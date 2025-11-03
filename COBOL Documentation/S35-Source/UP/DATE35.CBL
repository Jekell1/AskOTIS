       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DATE35.
       DATE-WRITTEN.  02-13-2024.
      ******************************************************************
      *
      *  IDENTIFICATION: UP/DATE35.C
      *
      *  DESCRIPTION   : UPGRADE A15 TO S35 FOR SELECT FILES THAT HAVE
      *                  DATE FIELDS IN THEIR PRIMARY KEYS.
      *
      *                  BMFILE, BYFILE, CJFILE, GTFILE, LMFILE
      *
      *  ASCII FILES ARE CREATED IN /USR/DATA/R1/ASCII; (TARGET)
      *                    ABWFILE.(GROUP) OR BEG-BR OR (ALL)
      *        WHEN ASCII FILE IS DONE, CREATES A TAG FILE IN
      *                            /USR/DATA/R1/ASCII/DONE
      *        WITH EXACT SAME SAME AS THE ASCII OUTPUT FILE
      *
      * GLOBALS:
      *   BY  [LP01BY] BRANCH STATS          
      *   GT+ [GL01GT] G/L TRANSACTIONS      
      *
      * LOCALS:
      *   BM+ [LP01BM] BORROWER MEMO
      *   CJ+ [SP01CJ] COLLECTOR JOURNAL
      *   LM+ [LP01LM] LOAN MEMO
      *
      *     + MEANS THAT ASCII EXTRACTION FILE AVAILABLE
      *
      *=================================================================
      * REV:
      * JKC 2024-0213 ***NEW*** BMFILE, BYFILE, CJFILE, GTFILE, LMFILE
      * JKC 2024-0223 ADDED TRACE LOGIC
      *
      * JKC 2024-0314 WITHIN CREATE-GLOBAL-DONE-TAGS SECTION AND
      *               CREATE-DONE-TAGS SECTION, NEED TO CHECK IF THE
      *               'DONE-PATH-BEG' AND 'DONE-PATH-GROUP' IS NOT EQUAL
      *               SPACES; IT WILL BE EQUAL SPACES IF A GIVEN FILE'S
      *               SOURCE FILE WAS NOT FOUND;  IF THE SOURCE FILE IS
      *               FOUND THEN IT WILL POPULATE THE 'DONE-PATH-GROUP';
      *               NEED THIS CHECKBECAUSE IT WILL STOP EXECUTION
      *               WITH A FILE IO-ERROR WINDOW.
      *
      * JKC 2024-0318 ADDED TO THE TRACE INITIALIZATION:
      *               + SQL DATABASE NAME
      *               + SQL SERVER NAME
      *               + BEGINNING DATE 
      *               + ENDING DATE 
      *
      * JKC 2024-0324 ADDED SYSTEM DATE TO THE "OTIS SESSION STARTED"
      *               LINE.
      *
      * JKC 2024-0325 CHANGED TO NOT MOVE THE A15 BRFILE TO BE OBRFILE;
      *     THIS WAS OLD ACU4GL LOGIC WHERE WE NEED TO MAKE IT OBRFILE;
      *     THIS OLD LOGIC CAUSED A LOCKING ISSUE WHEN RUNNING
      *     MULTIPLE SESSIONS VIA BATCHES (HAD 8 SESSIONS).
      *     NOW GOING TO USE DIRECTLY THE A15 BRFILE AS IS.
      *
      * JKC 2024-0326 CHANGED FOR GTFILE WHERE THE CHECK OF THE 
      *               RECORD IS WITHIN DATE RANGE; CHANGED TO USE
      *               REGULAR BEG-DATE & END-DATE (YYYYMMDD) TO
      *               A CONVERTED GT-DATE2 (YYYYMMDD-A15-GT-DATE).
      *
      * JKC 2024-0410 DO NOT UPGRADE ANY RECORDS WITH DATE = 99999999;
      *               THESE ARE BW CONTACT INFORMATION THAT IS NOW
      *               IN THE BWFILE; SEQ 1 THRU 10 WERE CONTACTS WHILE
      *               11 THRU 99 WERE FROM A GLITCH MADE IN THE
      *               MID-2000 FOR BRANCH TRANSFERS WHICH THE GLITCH
      *               WAS FIXED BUT DID NOT REMOVE BAD RECORDS.
      *
      * JKC 2024-0417 WE ARE FINDING RECORDS IN BMFILE & LMFILE
      *     THAT ARE OUTSIDE OF THE NORMAL RANGE (YEARS WAY BEFORE 1900
      *     AND BEYOND 2100); NEED TO SET BEG-DATE-MEMO-REVERSE &
      *     END-DATE-MEMO-REVERSE WITH ALL ZEROES OR 99999999 IF
      *     ENTERING A BEG TIME OF 01/01/50 AND END TIME OF 12/31/49.
      *
      * JKC 2024-0424 ADDED SOME LOGIC TO SUPPRESS SEND MESS OF 
      *     ERROR MESSAGES FOR THE ASCII DIRECTORIES CREATION;
      *     ALSO ADDED LOGIC TO PUT THOSE MESSAGES INTO THE TRACE LOG.
      *
      * JKC 2024-0502 FOUND RECORDS WITH ZERO DATE (99999999 REVERSED).
      *     A COUPLE OF LOAN RECORDS WE LOOKED AT IN BRANCH 1500 WE
      *     FOUND THESE WERE CREATED FROM AN ACQUISITION FROM ROYAL
      *     FINANCE IN 2002.  THE ACQUISITION DATA WAS NOT VERIFIED AND
      *     IMPORTED DIRECTLY SO THE MEMO DATA CAME IN AS ZEROES.
      *     KEVIN MULLEN SAID THAT WE SHOULD FORCE THESE DATES AS
      *     1970-01-01 (80299898 REVERSED).
      * JKC 2024-0503 FOUND RECORDS WITH ZERO ACCOUNT NUMBERS;
      *     PER KEVIN MULLEN WE CAN SKIP LOADING THESE RECORDS IN
      *     DATABASE.  1,326 LM RECORDS AS OF 2024-0331 WILL BE SKIPPED.
      * BAH 2024-0718 ADDED DATABASE NAME AND SERVER NAME TO RANGE LINES
      *                                                         S35Q-102
      * JKC 2024-1106 ADDED TO PRINTING RANGES IN THE END REPORT THE
      *     FOLLOWING RANGES: PRIORITY-MEMO, BEG-DATE, END-DATE
      *
      * JKC 2024-1206 ADDED TO OPTION OF PRIORITY-MEMO FOR (O)NLY.
      * JKC 2024-1212 ADDED MORE COUNTERS FOR PRIORITY MEMOS; TRYING
      *     FIND WHY WE ARE LOADING MORE MEMOS IN THE LMFILE
      *     THAN A15 RECORDS.
      *
      * JKC 2024-1216 SKIPPING BRANCH 9991 DUE TO A15 BRFILE SAYING
      *     IT IS A 'R1' WHEN S35 HAS IT AS 'R2'; THERE IS NO DATA
      *     FOR 'R2' BUT THERE IS FOR 'R1' WHICH IS THROWING OFF
      *     COMPARISONS AND COUNTS.
      *
      * JKC 2025-0218 FORCE GTFILE TO ONLY USE 'A' OR 'N'
      * BAH 2025-0423 ALLOW R2 GTFILE TO CONVERT FOR MID MONTH CONVERSION
      *               ONLY CONVERT MONTH AND YEAR OF THE SYSTEM DATE
      * JKC 2025-0423 DO NOT ALLOW R2 GTFILE TO CONVERT FOR BRANCH 9990;
      *          THESE CAME OVER AS DUPLICATES SINCE BRANCH 9990 IS
      *          FOR R1 & R2 IN A15; IN S35, BRANCH 9991 IS FOR R2 NOW.
      * JKC 2025-0423 QA OF THIS LOAD FOUND THAT R1 BRANCH 9990 GTFILE
      *          GOT WIPED OUT BY R2 BRANCH 9990 GTFILE; ADDED IF
      *          STATEMENT TO CHANGE R2 9990 TO BE 9991 (GT-BRANCH).
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBSP/SPFSSORT.CPY".

      * ASCII GLOBAL UPLOAD FILES
       COPY "LIBUP/UPFSABY.CPY".
       COPY "LIBUP/UPFSAGT.CPY".

      * ASCII LOCAL UPLOAD FILES
       COPY "LIBUP/UPFSABM.CPY".
       COPY "LIBUP/UPFSACJ.CPY".
       COPY "LIBUP/UPFSALM.CPY".

       COPY "LIBUP/UP1534_FS.CPY".
       COPY "LIBUP/UP1534_SPFSREPORT.CPY".

      * USING FOR ORIGINAL MACHINE
           SELECT OBR-FILE ASSIGN TO OBR-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY OBR-KEY
                  FILE STATUS FILE-STAT.

      *=================================================================
      *    MSEXCEL EXTRACTION FILE
      *=================================================================
           SELECT MSEXCEL-FILE ASSIGN OUTPUT MSEXCEL-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

      *=================================================================
      *    UPERROR EXTRACTION FILE
      *=================================================================
           SELECT UPERROR-FILE ASSIGN OUTPUT UPERROR-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

      ******************************************************************
      ******************************************************************
      ******************************************************************
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      * ASCII UPLOAD FILES
       COPY "LIBUP/UPFDABM.CPY".
       COPY "LIBUP/UPFDABY.CPY".
       COPY "LIBUP/UPFDACJ.CPY".
       COPY "LIBUP/UPFDAGT.CPY".
       COPY "LIBUP/UPFDALM.CPY".

       COPY "LIBUP/UP1534_FD.CPY".
       COPY "LIBUP/UP1534_SPFDREPORT.CPY".

       FD  OBR-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01BR.CPY" REPLACING LEADING "BR" BY "OBR".

      *=================================================================
      *    SORT FILE FOR CREATING MSEXCEL EXTRACTION FILE
      *    '-PR' IS FOR PRIORITY MEMOS
      *=================================================================
       COPY "LIBSP/SPFDSORT.CPY".
      *-----------------------------------------------------------------
       01  SORT-REC.
           03  SORT-KEY.
               05  SORT-FILENAME                     PIC X(08).
               05  SORT-FILEPATH                     PIC X(40).
               05  SORT-STATE                        PIC X(02).
               05  SORT-BRNO                         PIC 9(04).
           03  SORT-REC-INFO.
               05  SORT-RECS-READ                    PIC 9(15).
               05  SORT-RECS-WRITE                   PIC 9(15).
               05  SORT-RECS-SKIP                    PIC 9(15).
               05  SORT-TIME-BEG                     PIC 9V9(7).
               05  SORT-TIME-END                     PIC 9V9(7).
               05  SORT-TIME-ELAPSED                 PIC 9V9(7).
               05  SORT-RECS-READ-PR                 PIC 9(15).
               05  SORT-RECS-WRITE-PR                PIC 9(15).
               05  SORT-RECS-SKIP-PR                 PIC 9(15).

      *=================================================================
      *    MSEXCEL EXTRACTION FILE
      *=================================================================
       FD  MSEXCEL-FILE
           RECORD IS VARYING IN SIZE FROM 10 TO 200 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  MSEXCEL-REC.
           03  MSEXCEL-BYTE             PIC X OCCURS 200 TIMES.

      *=================================================================
      *    UPERROR EXTRACTION FILE
      *=================================================================
       FD  UPERROR-FILE
           RECORD IS VARYING IN SIZE FROM 10 TO 100 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  UPERROR-REC.
           03  UPERROR-BYTE             PIC X OCCURS 100 TIMES.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UP/DATE35.CBL S35 04/27/25-12:47:07 >".

      ******************************************************************
      *
      *    F I L E - P A T H - W O R K E R S   
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       COPY "LIBSP/SPWSSORT.CPY".
       COPY "LIBUP/UP1534_WS.CPY".
      * ASCII UPLOAD FILES

       COPY "LIBUP/UPWSABM.CPY".
       COPY "LIBUP/UPWSACJ.CPY".
       COPY "LIBUP/UPWSALM.CPY".

       COPY "LIBUP/UPWSABY.CPY".
       COPY "LIBUP/UPWSAGT.CPY".

       01  ASCII-DIRECTORY-PATH.
           03  ASCII-DIR-BEG           PIC X(9).
           03  ASCII-FILLER            PIC X    VALUE "/".
           03  ASCII-DIR-MACHINE       PIC X(2).
           03  ASCII-DIR-NAME          PIC X(6) VALUE "/ASCII".
           03  ASCII-DIR-DONE          PIC X(5).

       01  HOLD-ASCII-PATH-GROUP      PIC X(4).
       01  HOLD-ASCII-PATH-BRNO    REDEFINES HOLD-ASCII-PATH-GROUP
                                      PIC 9(4).

      *=================================================================
      *    EXAMPLE: /USR/TMP/DATE35-MSEXCEL-???????.TXT
      *=================================================================
       01  MSEXCEL-PATH                PIC X(35)     VALUE SPACES.
       01  FILLER                      REDEFINES MSEXCEL-PATH.
           03  MSEXCEL-PATH-SLASH-1     PIC X(01).    *> /
           03  MSEXCEL-PATH-DIR-1       PIC X(03).    *> USR
           03  MSEXCEL-PATH-SLASH-2     PIC X(01).    *> /
           03  MSEXCEL-PATH-DIR-2       PIC X(03).    *> TMP
           03  MSEXCEL-PATH-SLASH-3     PIC X(01).    *> /
           03  MSEXCEL-PATH-PREFIX      PIC X(15).    *> DATE35-MSEXCEL-
           03  MSEXCEL-PATH-SUFFIX      PIC X(07).    *> ???????
           03  MSEXCEL-PATH-EXT         PIC X(04).    *> .TXT

      *=================================================================
      *    EXAMPLE: /USR/TMP/DATE35-ERROR-???????
      *=================================================================
       01  UPERROR-PATH                 PIC X(35)     VALUE SPACES.
       01  FILLER                      REDEFINES UPERROR-PATH.
           03  UPERROR-PATH-SLASH-1     PIC X(01).    *> /
           03  UPERROR-PATH-DIR-1       PIC X(03).    *> USR
           03  UPERROR-PATH-SLASH-2     PIC X(01).    *> /
           03  UPERROR-PATH-DIR-2       PIC X(03).    *> TMP
           03  UPERROR-PATH-SLASH-3     PIC X(01).    *> /
           03  UPERROR-PATH-PREFIX      PIC X(13).    *> DATE35-ERROR-
           03  UPERROR-PATH-MKTEMP      PIC X(07).    *> ???????

       01  UPERROR-MESS                 PIC X(80)     VALUE SPACES.

      *=================================================================
       01  PR-IFN                      PIC X(2) VALUE "PR".
       01  UP1534-REPORT-KEY           PIC 9(11).
       01  UP1534-REPORT-PATH          PIC X(35).
       01  OIR-KEY                     PIC 99.
       01  OII-KEY                     PIC 99.
       01  OGB-KEY                     PIC 9.
       01  OBR-KEY                     PIC 9(4).
       01  OBR-PATH                    PIC X(35)     VALUE SPACES.
       01  FILENAME-EXT                PIC X(4)      VALUE ".VIX".

      * ASCII FILES FOR SQL
       01  ABYA-KEY                    PIC 9(12) VALUE 0.
       01  ABYB-KEY                    PIC 9(12) VALUE 0.
       01  ABM-KEY                     PIC 9(12) VALUE 0.
       01  ACJ-KEY                     PIC 9(12) VALUE 0.
       01  AGT-KEY                     PIC 9(12) VALUE 0.
       01  ALM-KEY                     PIC 9(12) VALUE 0.

      *-----------------------------------------------------------------
       01  HOLD-SORT-KEY.
           03  HOLD-SORT-FILENAME      PIC X(08).
           03  HOLD-SORT-FILEPATH      PIC X(40).
           03  HOLD-SORT-STATE         PIC X(02).
           03  HOLD-SORT-BRNO          PIC 9(04).

       01  SAVE-SORT-KEY.
           03  SAVE-SORT-FILENAME      PIC X(08).
           03  SAVE-SORT-FILEPATH      PIC X(40).
           03  SAVE-SORT-STATE         PIC X(02).
           03  SAVE-SORT-BRNO          PIC 9(04).

      *-----------------------------------------------------------------
       01  SAVE-A15-FILE-EXT           PIC X(4)      VALUE ".VIX".
       01  SAVE-A15-FILE-PATH-OLD      PIC X(40)     VALUE SPACES.
       01  SAVE-A15-FILE-PATH-NEW      PIC X(40)     VALUE SPACES.

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  DATE35-RECORD-COUNTERS                          VALUE ZEROES.
           03  DATE35-RECORD-COUNTER-READ-PR   PIC 9(10).
           03  DATE35-RECORD-COUNTER-WRITE-PR  PIC 9(10).
           03  DATE35-RECORD-COUNTER-SKIP-PR   PIC 9(10).

      *- - - - - - - - - - - - - - - - - -{ EDIT DISPLAY FORMAT - EDF }
       01  EDF-WORKERS.
           03  EDF-SORT-TIME-BEG       PIC 9.9(7).
           03  EDF-SORT-TIME-END       PIC 9.9(7).
           03  EDF-SORT-TIME-ELAPSED   PIC 9.9(7).

       01  ERRCD                       PIC X(01)     VALUE SPACES.

       01  LN-WORKERS.
           03  LN-FEED                 PIC 9(03)     VALUE ZEROES.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.

       01  ER-FEED                     PIC 9(01)     VALUE 1.
       01  PAGE-NUMBER                 PIC 9(04)     VALUE ZEROES.
       01  RECORDS-FOUND               PIC X(01).
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".


       01  BRANCH-TABLE.
           03  BRANCH-MACHINE          PIC XX OCCURS 9999 TIMES.

       01  TODAYS-DATE                 PIC 9(8).
       01  TODAYS-DATEX REDEFINES TODAYS-DATE.
           03  TODAYS-CC               PIC 99.
           03  TODAYS-YR               PIC 99.
           03  TODAYS-MO               PIC 99.
           03  TODAYS-DA               PIC 99.
       01  COMPARE-DATE                PIC 9(8).
       01  COMPARE-DATEX REDEFINES COMPARE-DATE.
           03  COMPARE-DATE-YYYY       PIC 9(4).
           03  FILLER                  PIC X(4).

       01  OLD-ACCTNO                  PIC 9(8).
       01  FILLER REDEFINES OLD-ACCTNO.
           03  NEW-ACCTNO              PIC 9(6).
           03  FILLER                  PIC 99.

       01  LITTLE-Z.
           03  FILLER           PIC X VALUE X"7A".

       01  HOLD-LN-MESSAGE      PIC X(30).
       01  HOLD-LN-MESSAGEX REDEFINES HOLD-LN-MESSAGE.
           03  HOLD-LN-MESS-BYTE PIC X OCCURS 30.
       01  HOLD-BYTE            PIC X.
           88  VALID-SPECIAL-CHARS  VALUE "(" ")"
                              "/" "\" "-" "?" "+" "%" "#" "[" "]"
                              "!" "<" ">" "$" "*" "&" "." """"  "="
                              "'" "_" ":" "@" "," ";" "~" "|" "{" "}".

      * MISC HOLD KEYS NEEDED FOR CLEAR ROUTINES, BUT NOT USED
       01  HOLD-KEY                    PIC X(18).
       01  HOLD-LN1-KEY REDEFINES HOLD-KEY
                                       PIC 9(18).
       01  HOLD-ES1-KEY REDEFINES HOLD-KEY
                                       PIC 9(18).
       01  HOLD-DL1-KEY REDEFINES HOLD-KEY
                                       PIC 9(18).
       01  HOLD-LH1-KEY REDEFINES HOLD-KEY
                                       PIC X(18).
       01  HOLD-EFFDATE REDEFINES HOLD-KEY
                                       PIC 9(18).

       01  HOLD-A15-ALPNAME            PIC X(3).
       01  FILLER REDEFINES HOLD-A15-ALPNAME.
           03  HOLD-A15-BYTE           PIC X(1) OCCURS 3 TIMES.

       01  HOLD-A30-ALPNAME            PIC X(4).
       01  FILLER REDEFINES HOLD-A30-ALPNAME.
           03  HOLD-A30-BYTE           PIC X(1) OCCURS 4 TIMES.

JKC    01  BEG-DATE-A15-BY-DATE        PIC 9(05)     VALUE ZEROES.
JKC    01  BEG-DATE-A15-CJ-DATE        PIC 9(05)     VALUE ZEROES.
JKC   *01  BEG-DATE-A15-GT-DATE        PIC 9(06)     VALUE ZEROES.
JKC    01  BEG-DATE-MEMO-REVERSE       PIC 9(08)     VALUE ZEROES.

JKC    01  END-DATE-A15-BY-DATE        PIC 9(05)     VALUE ZEROES.
JKC    01  END-DATE-A15-CJ-DATE        PIC 9(05)     VALUE ZEROES.
JKC   *01  END-DATE-A15-GT-DATE        PIC 9(06)     VALUE ZEROES.
JKC    01  END-DATE-MEMO-REVERSE       PIC 9(08)     VALUE ZEROES.

JKC    01  YYYYMMDD-A15-GT-DATE        PIC 9(08)     VALUE ZEROES.
       01  YYYYMMDD-A15-GT-DATEX REDEFINES YYYYMMDD-A15-GT-DATE.
           03 FILLER                  PIC 99.
           03 YYYYMMDD-A15-GT-YR       PIC 99.
           03 YYYYMMDD-A15-GT-MO       PIC 99.
           03 FILLER                   PIC 99.

       01  SUBSCRIPT-WORKERS.
           03  ELE                     PIC 9(03)     VALUE ZEROES.
           03  SUB                     PIC 9(03)     VALUE ZEROES.
           03  S1                      PIC 9(03)     VALUE ZEROES.
           03  SUB1      REDEFINES S1  PIC 9(03).
           03  BYSUB1    REDEFINES S1  PIC 9(03).
           03  SSSUB1    REDEFINES S1  PIC 9(03).
           03  DSSUB1    REDEFINES S1  PIC 9(03).
           03  TYSUB1    REDEFINES S1  PIC 9(03).
           03  S2                      PIC 9(03)     VALUE ZEROES.
           03  SUB2      REDEFINES S2  PIC 9(03).
           03  BYSUB2    REDEFINES S2  PIC 9(03).
           03  SSSUB2    REDEFINES S2  PIC 9(03).
           03  DSSUB2    REDEFINES S2  PIC 9(03).
           03  TYSUB2    REDEFINES S2  PIC 9(03).
           03  S3                      PIC 9(03)     VALUE ZEROES.
           03  SUB3      REDEFINES S3  PIC 9(03).
           03  BYSUB3    REDEFINES S3  PIC 9(03).
           03  SSSUB3    REDEFINES S3  PIC 9(03).
           03  DSSUB3    REDEFINES S3  PIC 9(03).

       01  TEST-DELQ-IB-AGE            PIC 999     VALUE ZEROS.
       01  DELQ-IB-MAX-DAYS-REM        PIC 999V99  VALUE ZEROS.
       01  TEST-DIVIDE-BY-30           PIC 999     VALUE ZEROS.
       01  TEST-DIVIDE-BY-30-REM       PIC 999V99  VALUE ZEROS.

      *===============================================[ HEX CHARACTERS ]
       01  HEX-CARRIAGE-RETURN         PIC X(01)     VALUE X"0D".
       01  HEX-LINE-FEED               PIC X(01)     VALUE X"0A".
       01  HEX-TAB                     PIC X(01)     VALUE X"09".

      ******************************************************************
      *
      *    M E S S A G E - W O R K E R S
      *
      ******************************************************************
       01  MESSAGE-INVALID-YN.
           03  FILLER PIC X(30) VALUE "INVALID: (Y)ES OR (N)O ONLY!!!".
           03  FILLER PIC X(30) VALUE "                              ".

       01  MESSAGE-INVALID-FILE-PATH.
           03  FILLER PIC X(30) VALUE "INVALID FILE PATH; PATH NOT FO".
           03  FILLER PIC X(30) VALUE "UND!!!                        ".

       01  MESSAGE-INVALID-SAME-PATHS.
           03  FILLER PIC X(30) VALUE "INVALID: SOURCE AND TARGET CAN".
           03  FILLER PIC X(30) VALUE "NOT BE THE SAME               ".

      ******************************************************************
      *
      *    P R I N T - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-1.
           03  H1-DATE              PIC X(08)   VALUE SPACES.
           03  FILLER               PIC X(02)   VALUE SPACES.
           03  H1-TIME              PIC X(08)   VALUE SPACES.
           03  FILLER               PIC X(28)   VALUE SPACES.
           03  H1-NAME              PIC X(40)   VALUE SPACES.
           03  FILLER               PIC X(37)   VALUE SPACES.
           03  FILLER               PIC X(05)   VALUE "PAGE".
           03  H1-PAGE-NO           PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEADER-LINE-2.
           03  H2-USERID-PROMPT     PIC X(11)   VALUE "USER ID  : ".
           03  H2-USERID            PIC X(10)   VALUE SPACES.
           03  FILLER               PIC X(22)   VALUE SPACES.
           03  H2-TITLE             PIC X(45)   VALUE
               "--------- UPGRADE A15 TO A34 REPORT ---------".
           03  FILLER               PIC X(44)   VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-RANGE-LINE        REDEFINES DETAIL-LINE.
           03  R1-DESC                        PIC X(34).
           03  FILLER                         PIC X(01).
           03  R1-FIELD                       PIC X(35).

           03  R1-DATE-RDF          REDEFINES R1-FIELD.
               05  R1-DATE                    PIC 99/99/99.
               05  FILLER                     PIC X(27).

           03  R1-MMYY-RDF          REDEFINES R1-FIELD.
               05  R1-MMYY                    PIC 99/99.
               05  FILLER                     PIC X(30).

           03  R1-NUMERIC-RDF       REDEFINES R1-FIELD.
               05  R1-NUMERIC                 PIC Z(09)9.
               05  FILLER                     PIC X(25).

           03  FILLER                  PIC X(62).

       01  WSS-DATE-YYYYSMM            PIC 9999/99.
       01  DEFAULT-GLOBAL-OPTIONS      PIC X(01)     VALUE SPACES.
       01  DEFAULT-LOCAL-OPTIONS       PIC X(01)     VALUE SPACES.

       01  SAVE-REPORT-COMMAND.
           03  SAVE-REPORT-CMD         PIC X(03)   VALUE "MV ".
           03  SAVE-REPORT-SOURCE      PIC X(35)   VALUE SPACES.
           03  FILLER                  PIC X(01)   VALUE SPACES.
           03  SAVE-REPORT-TARGET      PIC X(35)   VALUE SPACES.
           03  FILLER                  REDEFINES SAVE-REPORT-TARGET.
               05  FILLER              PIC X(09).
               05  SAVE-REPORT-TARGET-NEW
                                       PIC X(07).
               05  SAVE-REPORT-TARGET-MKTEMP
                                       PIC X(19).

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".
       01  TARGET-PATH-LIST            PIC X(07)     VALUE "T-PATH.".
       01  LEGEND                      PIC X(77)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".
       01  STARTING-TIME-LIST          PIC X(11)    VALUE "START-TIME.".
       01  STARTING-TIME-BUF.
           03  STARTING-TIME-RIGHT     PIC X(02)     VALUE SPACES.
           03  STARTING-TIME           PIC X(08)     VALUE SPACES.
           03  STARTING-TIME-LEFT      PIC X(02)     VALUE SPACES.

      *=================================================================
       01  LEGEND-PRINTER-OPTIONS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER PRINT".
           03  FILLER PIC X(30) VALUE "ER OPTIONS                    ".
           03  FILLER PIC X(17) VALUE "                 ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-SOURCE-PATH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER SOURC".
           03  FILLER PIC X(30) VALUE "E PATH                        ".
           03  FILLER PIC X(17) VALUE "                 ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-TARGET-PATH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER TARGET PATH               ".
           03  FILLER PIC X(17) VALUE "                 ".

       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, UP/DOWN, ENTER BEGINNING".
           03  FILLER PIC X(17) VALUE " BRANCH NUMBER   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER ENDING BRANCH NUMBER      ".
           03  FILLER PIC X(17) VALUE "                 ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-OPTION-FILES.
           03  FILLER PIC X(30) VALUE "F1 THRU F7, F8-HELP, UP/DOWN, ".
           03  FILLER PIC X(30) VALUE "ENTER (Y)ES/(N)O/(C)HECK FOR U".
           03  FILLER PIC X(17) VALUE "PDATING THIS FILE".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-PRIORITY-MEMO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER (Y)ES/(N)O/(O)NLY FOR PRIO".
           03  FILLER PIC X(17) VALUE "RITY MEMOS       ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-BEG-DATE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER DATE                      ".
           03  FILLER PIC X(17) VALUE "                 ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-ACCEPT.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER TO ACCEPT (YES/NO)        ".
           03  FILLER PIC X(17) VALUE "   ACCEPT (Y/N): ".

       01  FIL-LIST                    PIC X(11)    VALUE "DATA-SET:B.".
       01  FIL-BUF.
           03  FIL-PROMPT              PIC X(13)     VALUE SPACES.
           03  FIL-INFO                PIC X(35)     VALUE SPACES.

       01  RISC2                  PIC X(02)          VALUE "R2".
       01  SOURCE-MACHINE         PIC X(02).
       01  TARGET-MACHINE         PIC X(02).

      ***********************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      ***********************************************************
           03  CURRENT-MACHINE         PIC X(02).
           03  SOURCE-PATH             PIC X(35).
           03  TARGET-PATH             PIC X(35).

           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).

           03  GROUP-OPTION-WORKERS.
               05  GROUP-FLAG          PIC X(01).
               05  ENT-GROUP           PIC X(04).

           03  PRIORITY-MEMO           PIC X(01).
           03  BEG-DATE                PIC 9(08).
           03  END-DATE                PIC 9(08).

           03  FILE-OPTION-WORKERS.
      * GLOBALS
               05  FILE-OPTION-BY      PIC X(01).
               05  FILE-OPTION-GT      PIC X(01).
      * LOCALS
               05  FILE-OPTION-BM      PIC X(01).
               05  FILE-OPTION-CJ      PIC X(01).
               05  FILE-OPTION-LM      PIC X(01).

       01  VERSION-CONTROL             PIC X(01)     VALUE "A".
       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).

      *=================================================[ UPGRADE-STAT ]
       78  UPGRADE-STAT---GOOD                     VALUE "00".
       78  UPGRADE-STAT---BAD                      VALUE "XX".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       77  UPGRADE-STAT                PIC X(02)   VALUE SPACES.
           88  UPGRADE-STAT-GOOD                   VALUE "00".
           88  UPGRADE-STAT-BAD                    VALUE "  " THRU "//"
                                                         "01" THRU "~~".

      *=================================================================
       01  WSS-AM-APP-NUMBER           PIC 9(09)     VALUE ZEROES.
       01  FILLER            REDEFINES WSS-AM-APP-NUMBER.
           03  WSS-AM-APPNO            PIC 9(08).
           03  WSS-AM-SEQNO            PIC 9(01).
       01  WSS-MA-APP-NUMBER REDEFINES WSS-AM-APP-NUMBER
                                       PIC 9(09).
       01  FILLER            REDEFINES WSS-AM-APP-NUMBER.
           03  WSS-MA-APPNO            PIC 9(08).
           03  WSS-MA-SEQNO            PIC 9(01).

      *-----------------------------------------------------------------
       01  WSS-MSEXCEL-TIME-INPUT      PIC 9(6)      VALUE ZEROES.
       01  FILLER            REDEFINES WSS-MSEXCEL-TIME-INPUT.
           03  WSS-MSEXCEL-TIME-INPUT-HH   PIC 9(2).
           03  WSS-MSEXCEL-TIME-INPUT-MM   PIC 9(2).
           03  WSS-MSEXCEL-TIME-INPUT-SS   PIC 9(2).

       01  WSS-MSEXCEL-TIME-OUTPUT     PIC 9V9(7)    VALUE ZEROES.

      *-----------------------------------------------------------------
       01  WSS-PROGRAM-DIR-A15         PIC X(02)     VALUE SPACES.
       01  WSS-PROGRAM-NAME-A15        PIC X(10)     VALUE SPACES.
       01  WSS-PROGRAM-NAME-A30        PIC X(10)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  WSS-VENDOR-LONG-NAMES.
           03  WSS-VENDOR-LONG-NAME1   PIC X(40).
           03  WSS-VENDOR-LONG-NAME2   PIC X(40).

       01  WSS-VENDOR-LONG-NAME-FG     PIC 9(01)   VALUE ZEROES.

       01  CREATE-BX-RECORD            PIC X VALUE SPACES.
       COPY "LIBLP/TFRMEMOW.CPY".
      *=================================================================
       01  A15MEMO-MEMO                PIC X(60)     VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * BASE FORMAT - USED FOR IDENTIFYING WHAT TYPE OF MEMO.
      * TFR FR BR #### TO BR ####                                   [BM]
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  FILLER                      REDEFINES A15MEMO-MEMO.
           03  A15MEMO-PRIMARY         PIC X(09).
               88  A15MEMO-PRIMARY-FROM              VALUE "TFR FR BR".
               88  A15MEMO-PRIMARY-TO                VALUE "TFR TO BR".
           03  FILLER                  PIC X(01).
           03  A15MEMO-BRNO1           PIC 9(04).
           03  FILLER                  PIC X(01).
           03  A15MEMO-SECONDARY       PIC X(05).
               88  A15MEMO-SECONDARY-FROM            VALUE "FR BR".
               88  A15MEMO-SECONDARY-TO              VALUE "TO BR".
           03  FILLER                  PIC X(01).
           03  A15MEMO-BRNO2           PIC 9(04).
           03  FILLER                  PIC X(35).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TFR TO BR #### FR BR ####, RE: 123456 ---> 123456           [BM]
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  FILLER                      REDEFINES A15MEMO-MEMO.
           03  FILLER                  PIC X(25).
           03  A15MEMO-BM-RE           PIC X(06).
               88  A15MEMO-BM-RE-VALID               VALUE ", RE: ".
           03  A15MEMO-BM-ACCTNO1      PIC 9(06).
           03  FILLER                  PIC X(01).
           03  A15MEMO-BM-DIRECTION    PIC X(04).
               88  A15MEMO-BM-DIRECTION-VALID        VALUE "--->".
           03  FILLER                  PIC X(01).
           03  A15MEMO-BM-ACCTNO2      PIC 9(06).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TFR TO BR #### FR BR ####, FOR $ZZZZZ9.99 ---> 123456       [LM]
      * TFR FR BR #### TO BR ####, FOR $ZZZZZ9.99 <--- 123456       [LM]
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  FILLER                      REDEFINES A15MEMO-MEMO.
           03  FILLER                  PIC X(25).
           03  A15MEMO-LM-FOR          PIC X(07).
               88  A15MEMO-LM-FOR-VALID              VALUE ", FOR $".
           03  A15MEMO-LM-CURBAL       PIC ZZZZZ9.99.
           03  A15MEMO-LM-CURBALX      REDEFINES A15MEMO-LM-CURBAL
                                       PIC X(09).
           03  FILLER                  PIC X(01).
           03  A15MEMO-LM-DIRECTION    PIC X(04).
               88  A15MEMO-LM-DIRECTION-VALID        VALUE "--->"
                                                           "<---".
           03  FILLER                  PIC X(01).
           03  A15MEMO-LM-ACCTNO       PIC 9(06).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S 
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBUP/UP1534W.CPY".
       COPY "LIBWI/GRSCANW.CPY".

      *====================================================[ A30 ITEMS ]
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
      *------------------------------------------{ NEED TO BE TOGETHER }
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUP/DATE35_DEF.CPY".
       COPY "LIBUP/DATE35_WKS.CPY".
     
      ******************************************************************
      *
      *    L I N K A G E - S C R E E N - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
      *-----------------------------------------------------------------
     
      *=================================================================
       SCREEN SECTION.
       COPY "LIBUP/DATE35_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E    D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 REPORT-FILE OBR-FILE SORT-FILE
                 MSEXCEL-FILE UPERROR-FILE
      * ASCII OUTPUT FILES
                 ABM-FILE ABYA-FILE ABYB-FILE ACJ-FILE AGT-FILE ALM-FILE
      *- - - - - - - - - - - - - - - - - - - -{ SOURCE & TARGET FILES  }
      * GLOBALS
                 OBY-FILE   
                 OGT-FILE
      * LOCALS
                 OBM-FILE  
                 OCJ-FILE  
                 OLM-FILE.

      *-----------------------------------------------------------------
      *COPY "LIBUP/DECLARE.CPY".   *> ATTENTION THIS IS UNIQUE COPY
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.

      *-----------------------------------------------------------------
           CLOSE REPORT-FILE OBR-FILE SORT-FILE
                 MSEXCEL-FILE UPERROR-FILE
      * ASCII OUTPUT FILES
                 ABM-FILE ABYA-FILE ABYB-FILE ACJ-FILE AGT-FILE ALM-FILE
      *- - - - - - - - - - - - - - - - - - - -{ SOURCE & TARGET FILES  }
      * GLOBALS
                 OBY-FILE   
                 OGT-FILE
      * LOCALS
                 OBM-FILE  
                 OCJ-FILE  
                 OLM-FILE.

       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.

           PERFORM INITIALIZATION.

           IF ( ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM MISC-TRACE-INITIALIZATION.           *> ENABLE TRACE

      * CURRENT-MACHINE WILL BE RISC1, "R1"
           MOVE CURRENT-MACHINE TO SOURCE-MACHINE TARGET-MACHINE.
           CALL "C$TOLOWER" USING RISC2, VALUE 02.

      * LOAD A TABLE INDEXED BY BR-NO JUST TO KNOW THAT BR-MACHINE.
      * USING THE ORIGINAL A15 BRANCH FILE IN CASE YOU ALREADY CHANGED
      * THE NEW FILE TO ALL BE R1 BRANCHES!
      * BRANCH 9990 WILL BE R1
      * USE THIS FOR LOCALS ALSO, SO WE CAN FIX ERRORS AFTER R2 HAS BEEN
      * CHANGED
           PERFORM LOAD-BRANCH-TABLE.
           IF ( ERRCD NOT = SPACES )
              IF ( EXT-ROUTE-BUF = "00" )
                 MOVE "NEED ORIGINAL BRANCH FILE FOR MACHINE" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "ERROR: NEED ORIGINAL BRANCH FILE FOR MACHINE"
                   TO EXT-TCLP-TRACE-MESS
                 MOVE FORM-PATHNAME       TO EXT-TCLP-TRACE-FORM-PATH
                 PERFORM TRACE-SEND-MESS
              END-IF
              GO TO MAIN-PROGRAM-EXIT.
    
      * ADDED FILE EXTENSIONS FOR BRANCH/GROUP RANGES.
      *       F3-ALL       CREATES AXXFILE.ALL
      *       BRANCH RANGE CREATES AXXFILE.(BEGBR)
      *       GROUP        CREATES AXXFILE.(GROUP)
           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP TO HOLD-ASCII-PATH-GROUP
           ELSE
              IF (BEG-BRANCH = 0 AND END-BRANCH = 9999)
                 MOVE "ALL" TO HOLD-ASCII-PATH-GROUP
              ELSE
                 MOVE BEG-BRANCH TO HOLD-ASCII-PATH-BRNO.

      * CREATE ASCII AND DONE DIRECTORIES IF THEY DONT EXIST
            MOVE TARGET-PATH         TO ASCII-DIR-BEG.
            MOVE TARGET-MACHINE      TO ASCII-DIR-MACHINE.
            CALL "C$TOLOWER" USING ASCII-DIR-NAME, VALUE 06.

      * /USR/DATA/R1/ASCII
           MOVE ASCII-DIRECTORY-PATH TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING ASCII-DIRECTORY-PATH GIVING STAT-99
              IF STAT NOT = "00"
                 IF ( EXT-ROUTE-BUF = "00" )
                    MOVE "CAN'T CREATE ASCII DIRECTORY" TO MESS
                    PERFORM SEND-MESS
                 ELSE
                    MOVE "ERROR: CAN'T CREATE ASCII DIRECTORY"
                      TO EXT-TCLP-TRACE-MESS
                    MOVE FORM-PATHNAME      TO EXT-TCLP-TRACE-FORM-PATH
                    PERFORM TRACE-SEND-MESS
                 END-IF
                 GO TO MAIN-PROGRAM-EXIT.

      * /USR/DATA/R1/ASCII/DONE
           MOVE "/DONE" TO ASCII-DIR-DONE.
           CALL "C$TOLOWER" USING ASCII-DIR-DONE, VALUE 05.
           MOVE ASCII-DIRECTORY-PATH TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING ASCII-DIRECTORY-PATH GIVING STAT-99
              IF STAT NOT = "00"
                 IF ( EXT-ROUTE-BUF = "00" )
                    MOVE "CAN'T CREATE ASCII/DONE DIRECTORY" TO MESS
                    PERFORM SEND-MESS
                 ELSE
                    MOVE "ERROR: CAN'T CREATE ASCII/DONE DIRECTORY"
                      TO EXT-TCLP-TRACE-MESS
                    MOVE FORM-PATHNAME      TO EXT-TCLP-TRACE-FORM-PATH
                    PERFORM TRACE-SEND-MESS
                 END-IF
                 GO TO MAIN-PROGRAM-EXIT.

      * NEED TO OPEN OUTPUT BEFORE IT ALL GETS STARTED. OPEN I-O WILL
      * NOT CREATE THEM!
           PERFORM OPEN-OUTPUT-ALL-ASCII-FILES.

      *=================================================================
      * PROCESS RISC1 GLOBAL FILES
      *=================================================================

           MOVE "PROCESS RISC1 GLOBAL FILES" TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           PERFORM PROCESS-FILE-BY.
           PERFORM PROCESS-FILE-GT.

      *=================================================================
      * PROCESS RISC2 GLOBAL FILES TO MERGE/CONVERT
      *=================================================================

           MOVE "PROCESS RISC2 GLOBAL FILES TO MERGE/CONVERT" 
             TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           MOVE "R2" TO SOURCE-MACHINE.
           MOVE "R1" TO TARGET-MACHINE.
           CALL "C$TOLOWER" USING SOURCE-MACHINE, VALUE 02.
           CALL "C$TOLOWER" USING TARGET-MACHINE, VALUE 02.

      * NOT MERGING R2 AUFILE, KM SAID MAYBE NOT EVEN CONVERT 4/24/2023

      * BRANCH STATS, MERGE RISC2
           PERFORM PROCESS-FILE-BY.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%% ONLY DO RISC2 GTFILE POST-EOM; MIDSTREAM NEED TO DO!!!
      *%%% FOR GO LIVE DO *NOT* LOAD R2 GTFILE SINCE MERGED INTO R1
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           PERFORM PROCESS-FILE-GT.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      * CREATE DONE TAGS FOR GLOBAL FILES - EVEN IF NO FILES, NEED TO KNOW
      * ITS DONE. THE LOCALS WILL BE CREATED AT END OF PROGRAM

           PERFORM CREATE-GLOBAL-DONE-TAGS.

      * START LOCAL FILES USING BRANCH GROUP LOGIC

      * CURRENT-MACHINE WILL BE RISC1, "R1"
           MOVE CURRENT-MACHINE TO SOURCE-MACHINE TARGET-MACHINE.

           MOVE "PROCESS LOCAL FILES" TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME         TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       MAIN-PROGRAM-NEXT-GROUP.

           IF ( GROUP-FLAG = "N" )
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.
           IF ( WGR-SRESULT = ZEROES )
              GO TO MAIN-PROGRAM-EXIT.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO MAIN-PROGRAM-PROCESS-BRNO.

       MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO MAIN-PROGRAM-EXIT.

       MAIN-PROGRAM-PROCESS-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * JKC 2024-1216 SKIPPING BRANCH 9991 DUE TO A15 BRFILE SAYING
      *     IT IS A 'R1' WHEN S35 HAS IT AS 'R2'; THERE IS NO DATA
      *     FOR 'R2' BUT THERE IS FOR 'R1' WHICH IS THROWING OFF
      *     COMPARISONS AND COUNTS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( BR-NO = 9991 )
              GO TO MAIN-PROGRAM-NEXT-GROUP.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
      *    MOVE BR-MACHINE                 TO FILPATH-MACHINE
      *                                       SOURCE-MACHINE.
      * MADE CHANGE TO USE OLD BRANCH FILE TABLE MACHINE SO WE CAN RERUN
      * AFTER THE NEW BRANCH FILE GETS CHANGED TO ALL R1
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BRANCH-MACHINE(BR-NO)      TO SOURCE-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           PERFORM UP1534-CREATE-BRANCH-HEAD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * PROCESS LOCAL FILES FOR RISC1 & RISC2 BASED ON BR-REC
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM PROCESS-FILE-BM.
           PERFORM PROCESS-FILE-CJ.
           PERFORM PROCESS-FILE-LM.

           GO TO MAIN-PROGRAM-NEXT-GROUP.

      *=================================================================
       MAIN-PROGRAM-EXIT.

      * CREATE DONE TAGS EVEN IF NO RECORDS FOUND, NEED TO KNOW ITS DONE
           IF NOT EXIT-PRIOR-TO-EXECUTION
              PERFORM CREATE-DONE-TAGS.
           PERFORM PRINT-REPORT-MODULE.
           PERFORM EXTRACT-ROUTINE.

           GO TO END-PROGRAM.
 
      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DATE35"      TO VDU-FORM-NAME.

           MOVE "X"           TO RECORDS-FOUND.

           MOVE DATE35-QUEUE-POS           TO Q-POS.
           MOVE DATE35-COPIES-POS          TO C-POS.
           MOVE DATE35-PRU-POS             TO P-POS.
           MOVE SPACES                     TO Q-BUF  C-BUF  P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF ( BEG-BRANCH = ZEROES ) AND ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC     TO BEG-BRANCH.

           PERFORM GET-GPENV.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE GROUP-FLAG              TO SV-GROUP-FLAG
              MOVE ENT-GROUP               TO SV-ENT-GROUP
              MOVE ZEROES                  TO BEG-BRANCH
              MOVE 9999                    TO END-BRANCH
              MOVE "Y"                     TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP         TO ENT-GROUP
              MOVE EXT-EOBUF-GROUP-PRPATH  TO PRU-FILE.

           MOVE "N"                        TO RECORDS-FOUND.
           PERFORM SEND-STARTING-TIME.

           MOVE EXT-CONAME-CONAME          TO H1-NAME.
           MOVE EXT-CONAME-SYSDATE         TO H1-DATE
                                              ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
           MOVE DATE-YYYYMMDD              TO TODAYS-DATE.
           MOVE EXT-CONAME-USER-ID         TO H2-USERID.

JKC   *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
JKC        MOVE BEG-DATE                   TO DATE-YYYYMMDD.
JKC        PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
JKC        PERFORM CONVERT-MMDDYY-TO-YYYMM.
JKC        MOVE DATE-YYYMM                 TO BEG-DATE-A15-BY-DATE.

JKC   *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
JKC        MOVE END-DATE                   TO DATE-YYYYMMDD.
JKC        PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
JKC        PERFORM CONVERT-MMDDYY-TO-YYYMM.
JKC        MOVE DATE-YYYMM                 TO END-DATE-A15-BY-DATE.

JKC   *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
JKC        MOVE BEG-DATE                   TO DATE-YYYYMMDD.
JKC        MOVE DATE-YYYYMMDD              TO NUM-DATE.
JKC        PERFORM JUL.
JKC        MOVE JULIAN-DATE                TO BEG-DATE-A15-CJ-DATE.
JKC   *    MOVE DATE-YYYYMMDD              TO BEG-DATE-A15-GT-DATE.

JKC        IF ( BEG-DATE = 19500101 )
JKC           MOVE 99999999                TO BEG-DATE-MEMO-REVERSE
JKC        ELSE
JKC           COMPUTE BEG-DATE-MEMO-REVERSE = DATE-YYYYMMDD - 99999999.

JKC        MOVE END-DATE                   TO DATE-YYYYMMDD.
JKC        MOVE DATE-YYYYMMDD              TO NUM-DATE.
JKC        PERFORM JUL.
JKC        MOVE JULIAN-DATE                TO END-DATE-A15-CJ-DATE.
JKC   *    MOVE DATE-YYYYMMDD              TO END-DATE-A15-GT-DATE.

JKC        IF ( END-DATE = 20491231 )
JKC           MOVE ZEROES                  TO END-DATE-MEMO-REVERSE
JKC        ELSE
JKC           COMPUTE END-DATE-MEMO-REVERSE = DATE-YYYYMMDD - 99999999.

           MOVE MKTEMP-DEFAULT             TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF                 TO UP1534-REPORT-PATH.

           PERFORM OPEN-REPORT-FILE-OUTPUT.
           PERFORM CLOSE-REPORT-FILE.
           MOVE ZEROES TO UP1534-REPORT-KEY.

           PERFORM LOAD-SORT-FILE.
           PERFORM OPEN-SORT-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-REPORT-FILE.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "UPGRADE IN PROGRESS....................." TO LEGEND
              PERFORM SEND-LEGEND.
          
       EXIT-INITIALIZATION.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.
       ENTER-PARAMETERS-BEGIN.

           MOVE "S  A300TITLE:FR."               TO VDU.
           MOVE "[ 'A15' TO 'S35' CONVERSION  ]" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM SEND-FIL.

           MOVE SOURCE-PATH                TO WR-BUF.
           MOVE SOURCE-PATH-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE TARGET-PATH                TO WR-BUF.
           MOVE TARGET-PATH-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP-KEY )
              GO TO ENTER-OPTION-BY.

      *=================================================================
       ENTER-SOURCE-PATH.

           MOVE LEGEND-SOURCE-PATH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A090S-PATH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-SOURCE-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUBUF = SPACES )
              GO TO ENTER-SOURCE-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SOURCE FILE ACCESS CHECK:
      *    - TO PREVENT BOMBING OUT IF BRANCH'S FILES NOT ON THIS SYSTEM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO DIR-ACCESS-BUF SOURCE-PATH.
           PERFORM DIR-ACCESS-CALL.
           IF ( STAT-BAD )
              PERFORM INVALID-FILE-PATH-MESSAGE
              GO TO ENTER-SOURCE-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SOURCE FILE ACCESS CHECK:
      *    - TO PREVENT BOMBING OUT IF BRANCH'S FILES NOT ON THIS SYSTEM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                     TO DIR-ACCESS-BUF.

           STRING VDUBUF, "/", CURRENT-MACHINE
                  DELIMITED BY " "       INTO DIR-ACCESS-BUF.

           PERFORM DIR-ACCESS-CALL.

           IF ( STAT-BAD )
              PERFORM INVALID-FILE-PATH-MESSAGE
              GO TO ENTER-SOURCE-PATH.

           MOVE VDUBUF                TO SOURCE-PATH.

      *-----------------------------------------------------------------
       ENTER-TARGET-PATH.

           MOVE LEGEND-TARGET-PATH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A090T-PATH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-SOURCE-PATH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-TARGET-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUBUF = SPACES )
              GO TO ENTER-TARGET-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    TARGET FILE ACCESS CHECK:
      *    - TO PREVENT BOMBING OUT IF BRANCH'S FILES NOT ON THIS SYSTEM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                     TO DIR-ACCESS-BUF.

           STRING VDUBUF, "/", CURRENT-MACHINE
                  DELIMITED BY " "       INTO DIR-ACCESS-BUF.

           PERFORM DIR-ACCESS-CALL.

           IF ( STAT-BAD )
              MOVE MESSAGE-INVALID-FILE-PATH TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TARGET-PATH.

           MOVE VDUBUF                TO TARGET-PATH.

           IF ( SOURCE-PATH = TARGET-PATH )
              MOVE MESSAGE-INVALID-SAME-PATHS TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-SOURCE-PATH.

      *=================================================================
      *    ENTERING BRANCH NUMBER RANGE.
      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-TARGET-PATH.
           IF ( F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF ( F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF ( F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.

           IF ( F6-KEY )
              PERFORM SCAN-GR-FILE
              IF ( IO-BAD )
                 GO TO ENTER-BEG-BRANCH.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES               TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"                  TO GROUP-FLAG
              MOVE ZEROES               TO BEG-BRANCH
              MOVE 9999                 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.

           MOVE "N"                        TO GROUP-FLAG.
           MOVE WGR-SRESULT                TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH."       TO VDU.
           MOVE BEG-BRANCH                 TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENTER-END-BRANCH.

           IF ( UP-KEY           ) AND
              ( GROUP-FLAG = "Y" )
              GO TO ENTER-BEG-BRANCH.

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF ( F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF ( F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "N"                  TO GROUP-FLAG.
           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO VDUNUM0 BEG-BRANCH.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO VDUNUM0 END-BRANCH.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.

           PERFORM BR-RANGE-SECURITY.
           IF ( BR-SECURITY-OKAY = "N" )
              GO TO ENTER-BEG-BRANCH.

      *========================================{ SOURCE & TARGET FILES }
       ENTER-OPTION-BY.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-BY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-BY.

           MOVE VDUBUF TO FILE-OPTION-BY.

           IF NOT ( FILE-OPTION-BY = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-BY.

      *-----------------------------------------------------------------
       ENTER-OPTION-GT.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-GT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-BY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-GT.

           MOVE VDUBUF TO FILE-OPTION-GT.

      *    IF NOT ( FILE-OPTION-GT = "Y" OR "N" OR "A" )
      *       MOVE "INVALID : 'Y', 'N' OR 'A' ONLY" TO MESS
           IF NOT ( FILE-OPTION-GT = "N" OR "A" )
              MOVE "INVALID : 'N' OR 'A' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-OPTION-GT.

      *-----------------------------------------------------------------
       ENTER-OPTION-BM.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-BM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-GT.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-BM.

           MOVE VDUBUF TO FILE-OPTION-BM.

           IF NOT ( FILE-OPTION-BM = "Y" OR "N" OR "A" )
              MOVE "INVALID : 'Y', 'N' OR 'A' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-OPTION-BM.

      *-----------------------------------------------------------------
       ENTER-OPTION-CJ.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-CJ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-BM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-CJ.

           MOVE VDUBUF TO FILE-OPTION-CJ.

           IF NOT ( FILE-OPTION-CJ = "Y" OR "N" OR "A" )
              MOVE "INVALID : 'Y', 'N' OR 'A' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-OPTION-CJ.

      *-----------------------------------------------------------------
       ENTER-OPTION-LM.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-LM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-CJ.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-LM.

           MOVE VDUBUF TO FILE-OPTION-LM.

           IF NOT ( FILE-OPTION-LM = "Y" OR "N" OR "A" )
              MOVE "INVALID : 'Y', 'N' OR 'A' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-OPTION-LM.


           GO TO ENTER-PRIORITY-MEMO.

      *========================================{ SOURCE & TARGET FILES }
       ENTER-ALL-OPTIONS.

           IF ( F2-KEY )
              MOVE "C" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "C" TO DEFAULT-LOCAL-OPTIONS
           ELSE
           IF ( F3-KEY )
              MOVE "Y" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "Y" TO DEFAULT-LOCAL-OPTIONS
           ELSE
           IF ( F4-KEY ) 
              MOVE "N" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "N" TO DEFAULT-LOCAL-OPTIONS
           ELSE
           IF ( F5-KEY )
              MOVE "Y" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "N" TO DEFAULT-LOCAL-OPTIONS
           ELSE
           IF ( F9-KEY ) 
              MOVE "N" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "A" TO DEFAULT-LOCAL-OPTIONS
           ELSE
              MOVE "N" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "Y" TO DEFAULT-LOCAL-OPTIONS.

      * GLOBALS
           MOVE "S  A010OPTION-BY."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-BY.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-GT."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-GT.
           PERFORM SCREENIO.

      * LOCALS
           MOVE "S  A010OPTION-BM."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-BM.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-CJ."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-CJ.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-LM."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-LM.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PRIORITY-MEMO.

           MOVE LEGEND-PRIORITY-MEMO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRIORITY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-LM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-PRIORITY-MEMO.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-PRIORITY-MEMO.

           IF NOT ( VDUBUF = "Y" OR "N" OR "O" )
              DISPLAY MESSAGE BOX
                      "INVALID OPTION!!!:", X"0A",
                      "Y = YES",            X"0A",
                      "N = NO",             X"0A",
                      "O = ONLY"
              GO TO ENTER-PRIORITY-MEMO.

           MOVE VDUBUF TO PRIORITY-MEMO.

      *=================================================================
       ENTER-BEG-DATE.

           IF ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO ENTER-ACCEPT-YN.

           MOVE LEGEND-BEG-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRIORITY-MEMO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-BEG-DATE.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENTER-BEG-DATE.

           MOVE VDUNUM0 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           IF ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO ENTER-ACCEPT-YN.

           MOVE LEGEND-BEG-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-END-DATE.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENTER-END-DATE.

           MOVE VDUNUM0 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO END-DATE.

           IF ( BEG-DATE > END-DATE )
              MOVE "ERROR: INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

      *=================================================================
       ENTER-ACCEPT-YN.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    D I S P L A Y - P A R A M E T E R S
      *
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.

           IF ( GROUP-FLAG = "Y" )
              MOVE "S  A000BEG-BRANCH."   TO VDU
              MOVE ENT-GROUP              TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH."   TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH."   TO VDU
              MOVE BEG-BRANCH             TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH."   TO VDU
              MOVE END-BRANCH             TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "S  A090S-PATH."          TO VDU.
           MOVE SOURCE-PATH               TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A090T-PATH."          TO VDU.
           MOVE TARGET-PATH               TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-BY."       TO VDU.
           MOVE FILE-OPTION-BY            TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-GT."       TO VDU.
           MOVE FILE-OPTION-GT            TO VDUBUF.
           PERFORM SCREENIO.

      * LOCALS
           MOVE "S  A010OPTION-BM."       TO VDU.
           MOVE FILE-OPTION-BM            TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-CJ."       TO VDU.
           MOVE FILE-OPTION-CJ            TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-LM."       TO VDU.
           MOVE FILE-OPTION-LM            TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PRIORITY."        TO VDU.
           MOVE PRIORITY-MEMO             TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      ******************************************************************
       EO-EXEC-AUDIT SECTION.

           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
       OPEN-OUTPUT-ALL-ASCII-FILES SECTION.
      ******************************************************************
      * CURRENT-MACHINE WILL BE RISC1, "R1"
           MOVE CURRENT-MACHINE TO SOURCE-MACHINE TARGET-MACHINE.
           CALL "C$TOLOWER" USING RISC2, VALUE 02.

           MOVE UP1534-FILE-TYPE---GLOBAL   TO UP1534-FILE-TYPE.
           MOVE UP1534-FILE-ORDER---TARGET TO UP1534-FILE-ORDER.

           IF (FILE-OPTION-GT = "A")
              MOVE "GL/GTFILE" TO UP1534-FILE-INPUT
              PERFORM UP1534-LOAD-FILE-PATH
              MOVE UP1534-FILE-PATH TO AGT-PATH-BEG
              CALL "C$TOLOWER" USING AGT-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING AGT-DONE-FILENAME, VALUE 11
              OPEN OUTPUT AGT-FILE
              CLOSE AGT-FILE.

      * LOCAL FILES
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE UP1534-FILE-ORDER---TARGET TO UP1534-FILE-ORDER.
           IF (FILE-OPTION-BM = "A")
              MOVE "LP/BMFILE" TO UP1534-FILE-INPUT
              PERFORM UP1534-LOAD-FILE-PATH
              MOVE UP1534-FILE-PATH TO ABM-PATH-BEG
              CALL "C$TOLOWER" USING ABM-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ABM-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ABM-PATH-GROUP
              OPEN OUTPUT ABM-FILE
              CLOSE ABM-FILE.
           IF (FILE-OPTION-CJ = "A")
              MOVE "SP/CJFILE" TO UP1534-FILE-INPUT
              PERFORM UP1534-LOAD-FILE-PATH
              MOVE UP1534-FILE-PATH TO ACJ-PATH-BEG
              CALL "C$TOLOWER" USING ACJ-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ACJ-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ACJ-PATH-GROUP
              OPEN OUTPUT ACJ-FILE
              CLOSE ACJ-FILE.
           IF (FILE-OPTION-LM = "A")
              MOVE "LP/LMFILE" TO UP1534-FILE-INPUT
              PERFORM UP1534-LOAD-FILE-PATH
              MOVE UP1534-FILE-PATH TO ALM-PATH-BEG
              CALL "C$TOLOWER" USING ALM-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ALM-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ALM-PATH-GROUP
              OPEN OUTPUT ALM-FILE
              CLOSE ALM-FILE.

      ******************************************************************
      *
      *    L O A D - B R A N C H - T A B L E 
      *
      *=================================================================
      * LOAD A BRANCH TABLE ONE TIME INDEXED BY BR-NO FOR BR-MACHINE
      ******************************************************************
        LOAD-BRANCH-TABLE SECTION.

           MOVE SPACES TO ERRCD.

           MOVE "BR"                       TO UP1534-CURRENT-FILE.
           MOVE "GB/BRFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.

           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
      *KC  MOVE UP1534-FILE-PATH           TO UP1534-MV15-OLD-PATH.
      *KC  PERFORM UP1534-MV15-OLD-TO-NEW.
      *KC  MOVE UP1534-MV15-NEW-PATH       TO OBR-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * JKC 2024-0325 CHANGED TO NOT MOVE THE A15 BRFILE TO BE OBRFILE;
      *     THIS WAS OLD ACU4GL LOGIC WHERE WE NEED TO MAKE IT OBRFILE;
      *     THIS OLD LOGIC CAUSED A LOCKING ISSUE WHEN RUNNING
      *     MULTIPLE SESSIONS VIA BATCHES (HAD 8 SESSIONS).
      *     NOW GOING TO USE DIRECTLY THE A15 BRFILE AS IS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE UP1534-FILE-PATH           TO OBR-PATH.

      *KC  IF ( STAT-BAD )     *> MOVE OF OLD FILE TO NEW FILE WAS BAD
      *KC     MOVE "X" TO ERRCD
      *KC     GO TO LOAD-BRANCH-TABLE-EXIT.

           PERFORM OPEN-OBR-FILE.

           MOVE 1 TO OBR-KEY.
           PERFORM START-OBR-FILE.

        LOAD-NEXT-BRANCH.
           PERFORM READ-OBR-FILE-NEXT.
           IF IO-BAD 
              MOVE SPACES TO ERRCD
      *KC     MOVE OBR-PATH TO UP1534-MV15-NEW-PATH
      *KC     PERFORM UP1534-MV15-NEW-TO-OLD
              GO TO LOAD-BRANCH-TABLE-EXIT.

           MOVE OBR-MACHINE TO BRANCH-MACHINE(OBR-NO).

           GO TO LOAD-NEXT-BRANCH.

       LOAD-BRANCH-TABLE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - B Y
      *
      ******************************************************************
       PROCESS-FILE-BY SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-BY = "N" )
              GO TO PROCESS-FILE-BY-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO BY-PATH-OVERRIDE.
           MOVE "BY"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/BYFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.
           MOVE ZEROES                     TO DATE35-RECORD-COUNTERS.

           PERFORM UP1534-LOAD-FILE-BEG-TIME.
           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO UP1534-MV15-OLD-PATH.
           PERFORM UP1534-MV15-OLD-TO-NEW.
           MOVE UP1534-MV15-NEW-PATH       TO OBY-PATH.

           IF ( STAT-BAD )     *> MOVE OF OLD FILE TO NEW FILE WAS BAD
              MOVE STAT                    TO UPGRADE-STAT
              PERFORM UP1534-CREATE-FILE-ERROR
              GO TO PROCESS-FILE-BY-EXIT.

           MOVE UP1534-FILE-PATH           TO SAVE-A15-FILE-PATH-OLD.
           MOVE OBY-PATH                   TO SAVE-A15-FILE-PATH-NEW.
           PERFORM MISC-MV15-OLD-TO-NEW.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP TARGET PATHS }
           MOVE UP1534-FILE-ORDER---TARGET TO UP1534-FILE-ORDER.
           MOVE "LP/BYAFILE"               TO UP1534-FILE-INPUT.
           PERFORM UP1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-BY = "Y")
              MOVE UP1534-FILE-PATH           TO BYA-PATH.

           MOVE "LP/BYBFILE"               TO UP1534-FILE-INPUT.
           PERFORM UP1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-BY = "Y")
              MOVE UP1534-FILE-PATH           TO BYB-PATH
      * THIS WILL OPEN BYA-FILE AND BYB-FILE
              PERFORM OPEN-BY1-FILE.

           PERFORM MISC-TRACE-FILE-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ OPEN & START FILES }
           MOVE SPACES                     TO OBY1-KEY.
JKC   *    MOVE ZEROES                     TO OBY-DATE
JKC   *                                       OBY-BRNO
JKC   *                                       OBY-CLASS.
JKC        MOVE BEG-DATE-A15-BY-DATE       TO OBY-DATE.
JKC        MOVE ZEROES                     TO OBY-BRNO
JKC                                           OBY-CLASS.
           PERFORM OPEN-OBY1-FILE.
           PERFORM START-OBY1-FILE.

       PROCESS-FILE-BY-NEXT.
           PERFORM READ-OBY1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-BY-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

      * EMAIL FROM KM ON 9/13/2021 THAT WE CAN LIMIT UPGRADE TO 10 YEARS
           MOVE OBY-DATE                   TO DATE-YYYMM.
           PERFORM CONVERT-YYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
JKC        IF DATE-YYYYMM-YYYY < 2014
              ADD 1                        TO UP1534-RECORD-COUNTER-SKIP
              GO TO PROCESS-FILE-BY-NEXT.

JKC        IF ( OBY-DATE < BEG-DATE-A15-BY-DATE ) OR
JKC           ( OBY-DATE > END-DATE-A15-BY-DATE )
JKC           ADD 1                        TO UP1534-RECORD-COUNTER-SKIP
JKC           GO TO PROCESS-FILE-BY-NEXT.

           IF OBY-BRNO NOT NUMERIC
              ADD 1                        TO UP1534-RECORD-COUNTER-SKIP
              GO TO PROCESS-FILE-BY-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ VALID RECORD }
           PERFORM UPGRADE-FILE-BY.

           PERFORM WRITE-BY1-FILE.
           IF ( IO-BAD )
              ADD 1                        TO UP1534-RECORD-COUNTER-SKIP
              MOVE BY1-KEY                 TO UP1534-UPDATE-KEY
              MOVE FILE-STAT-DISP          TO UP1534-UPDATE-STAT
              MOVE "E"                     TO UP1534-UPDATE-TYPE
              IF ( FILE-STAT-DISP = "22,00" )
                 GO TO PROCESS-FILE-BY-NEXT
              ELSE
                 PERFORM UP1534-CREATE-FILE-UPDATE
                 GO TO IO-ERROR.

           ADD 1 TO UP1534-RECORD-COUNTER-WRITE.

           GO TO PROCESS-FILE-BY-NEXT.

      *=================================================================
       PROCESS-FILE-BY-END.

           PERFORM CLOSE-OBY1-FILE.
           MOVE OBY-PATH TO UP1534-MV15-NEW-PATH.
           PERFORM UP1534-MV15-NEW-TO-OLD.
           PERFORM MISC-MV15-NEW-TO-OLD.

           IF (FILE-OPTION-BY = "Y")
              PERFORM CLOSE-BY1-FILE.

           PERFORM UP1534-CREATE-FILE-TOTALS.

           MOVE "BYFILE"                   TO HOLD-SORT-FILENAME.
           MOVE UP1534-MV15-OLD-PATH       TO HOLD-SORT-FILEPATH.
           MOVE SPACES                     TO HOLD-SORT-STATE.
           MOVE ZEROES                     TO HOLD-SORT-BRNO.
           PERFORM UPDATE-SORT-MODULE.

       PROCESS-FILE-BY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - G T
      *
      ******************************************************************
       PROCESS-FILE-GT SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-GT = "N" )
              GO TO PROCESS-FILE-GT-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO GT-PATH-OVERRIDE.
           MOVE "GT"                       TO UP1534-CURRENT-FILE.
           MOVE "GL/GTFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.
           MOVE ZEROES                     TO DATE35-RECORD-COUNTERS.

           PERFORM UP1534-LOAD-FILE-BEG-TIME.
           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO UP1534-MV15-OLD-PATH.
           PERFORM UP1534-MV15-OLD-TO-NEW.
           MOVE UP1534-MV15-NEW-PATH       TO OGT-PATH.

           IF ( STAT-BAD )     *> MOVE OF OLD FILE TO NEW FILE WAS BAD
              MOVE STAT                    TO UPGRADE-STAT
              PERFORM UP1534-CREATE-FILE-ERROR
              GO TO PROCESS-FILE-GT-EXIT.

           MOVE UP1534-FILE-PATH           TO SAVE-A15-FILE-PATH-OLD.
           MOVE OGT-PATH                   TO SAVE-A15-FILE-PATH-NEW.
           PERFORM MISC-MV15-OLD-TO-NEW.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP TARGET PATHS }
           MOVE UP1534-FILE-ORDER---TARGET TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-GT = "Y")
              MOVE UP1534-FILE-PATH        TO GT-PATH
              PERFORM OPEN-GT1-FILE
           ELSE
              MOVE UP1534-FILE-PATH TO AGT-PATH-BEG
                                       AGT-DONE-PATH-BEG
              CALL "C$TOLOWER" USING AGT-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING AGT-DONE-FILENAME, VALUE 11
              PERFORM OPEN-AGT-FILE.

           PERFORM MISC-TRACE-FILE-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ OPEN & START FILES }
           MOVE SPACES                     TO OGT1-KEY.
           MOVE 0                          TO OGT-SOURCE
                                              OGT-DATE2
                                              OGT-BRANCH
                                              OGT-ACCTNO
                                              OGT-DATE3
                                              OGT-LINE.

           PERFORM OPEN-OGT1-FILE.
           PERFORM START-OGT1-FILE.

      *=================================================================
       PROCESS-FILE-GT-NEXT.

           PERFORM READ-OGT1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-GT-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

JKC        MOVE OGT-DATE2                  TO DATE-YYMMDD.
JKC        PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
JKC        PERFORM TEST-INVALID-DATE.
JKC        MOVE DATE-YYYYMMDD              TO YYYYMMDD-A15-GT-DATE.

JKC   *    IF ( OGT-DATE2 < BEG-DATE-A15-GT-DATE ) OR
JKC   *       ( OGT-DATE2 > END-DATE-A15-GT-DATE )
JKC        IF ( YYYYMMDD-A15-GT-DATE < BEG-DATE ) OR
JKC           ( YYYYMMDD-A15-GT-DATE > END-DATE )
JKC           ADD 1 TO UP1534-RECORD-COUNTER-SKIP
JKC           GO TO PROCESS-FILE-GT-NEXT.

      * DATE RANGE SHOULD BE THROUGH TODAY
      * BAH 4/23/2025 FOR MID MONTH CONVERSION, BRANCH CONSOLIDATION HAS NOT
      *               TAKEN PLACE
           IF SOURCE-MACHINE =  RISC2
              IF NOT ( (YYYYMMDD-A15-GT-YR = TODAYS-YR) AND
                       (YYYYMMDD-A15-GT-MO = TODAYS-MO) )
              ADD 1 TO UP1534-RECORD-COUNTER-SKIP
              GO TO PROCESS-FILE-GT-NEXT.

           IF ( SOURCE-MACHINE =  RISC2 ) AND
              ( OGT-BRANCH     = 9990   )
              MOVE 9991 TO OGT-BRANCH.
                  
           PERFORM UPGRADE-FILE-GT.

           IF (FILE-OPTION-GT = "Y")
              PERFORM WRITE-GT1-FILE
           ELSE
              ADD 1 TO AGT-KEY
              ACCEPT AGT-LTOUCH-DATE FROM CENTURY-DATE
              MOVE X"0D"              TO AGT-CR
              MOVE X"0A"              TO AGT-LF
              PERFORM WRITE-AGT-FILE.

           IF ( IO-BAD )
              ADD 1                        TO UP1534-RECORD-COUNTER-SKIP
              MOVE GT1-KEY                 TO UP1534-UPDATE-KEY
              MOVE FILE-STAT-DISP          TO UP1534-UPDATE-STAT
              MOVE "E"                     TO UP1534-UPDATE-TYPE
              IF ( FILE-STAT-DISP = "22,00" )
                 GO TO PROCESS-FILE-GT-NEXT
              ELSE
                 PERFORM UP1534-CREATE-FILE-UPDATE
                 GO TO IO-ERROR.

           ADD 1 TO UP1534-RECORD-COUNTER-WRITE.

           GO TO PROCESS-FILE-GT-NEXT.

      *=================================================================
       PROCESS-FILE-GT-END.

           PERFORM CLOSE-OGT1-FILE.
           MOVE OGT-PATH TO UP1534-MV15-NEW-PATH.
           PERFORM UP1534-MV15-NEW-TO-OLD.
           PERFORM MISC-MV15-NEW-TO-OLD.

           IF (FILE-OPTION-GT = "Y")
              PERFORM CLOSE-GT1-FILE
           ELSE
              PERFORM CLOSE-AGT-FILE.

           PERFORM UP1534-CREATE-FILE-TOTALS.

           MOVE "GTFILE"                   TO HOLD-SORT-FILENAME.
           MOVE UP1534-MV15-OLD-PATH       TO HOLD-SORT-FILEPATH.
           MOVE SPACES                     TO HOLD-SORT-STATE.
           MOVE ZEROES                     TO HOLD-SORT-BRNO.
           PERFORM UPDATE-SORT-MODULE.

      *-----------------------------------------------------------------
       PROCESS-FILE-GT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - B M
      *
      ******************************************************************
       UPGRADE-FILE-BM SECTION.

           IF FILE-OPTION-BM = "A"
              MOVE ALL X"09" TO ABM-REC
              MOVE SPACES TO ABM-LTOUCH-USERID ABM-MEMO ABM-MEMO-TYPE
           ELSE
              MOVE SPACES TO BM-REC.

           MOVE BR-NO    TO  BM-BRNO ABM-BRNO.

           MOVE OBM-SSNO TO BM-SSNO ABM-SSNO.
           MOVE OBM-DATE TO BM-DATE ABM-DATE-REVERSED.
           MOVE OBM-SEQ  TO BM-SEQ  ABM-SEQ.

      * WILL GET SET IN THE I/O WRITE ROUTINE
      *  BM-LTOUCH-DATE, BM-LTOUCH-TIME-HHMM, BM-LTOUCH-USERID
           MOVE OBM-MEMO      TO BM-MEMO      ABM-MEMO.
           MOVE OBM-MEMO-TYPE TO BM-MEMO-TYPE ABM-MEMO-TYPE.

      *=================================================================
      *    USING TRFMEMO WORKERS (LIBLP/TFRMEMOW) TO FIX A15 TRANSFER
      *    MEMOS FROM USING OLD ACCTNO (6 BYTES) TO NEW ACCTNO (8 BYTES)
      *
      *-----------------------------------------------------------------
      *
      * EXAMPLE A15 MEMOS CREATED [LP/BULKG2 & LP/BULKT2]
      * -------------------------------------------------
      *
      *          -BRNO-     -BRNO- -ACCTNO/CURBAL- -ACCTNO-
      * TFR FR BR #### TO BR ####                                   [BM]
      * TFR TO BR #### FR BR ####, RE: 123456 ---> 123456           [BM]
      *
      *-----------------------------------------------------------------
      *
      * EXAMPLE A30 MEMOS CREATED [LP/BULKG2 & LP/BULKT2]
      * -------------------------------------------------
      *
      *          -BRNO-     -BRNO-   -ACCTNO/CURBAL-   -ACCTNO-
      * TFR FR BR #### TO BR ####                                   [BM]
      * TFR TO BR #### FR BR ####, RE: 12345678 ---> 12345678       [BM]
      *
      *=================================================================

           IF ( OBM-MEMO-TYPE NOT = "9" )
              GO TO UPGRADE-FILE-BM-EXIT.

           MOVE OBM-MEMO                   TO A15MEMO-MEMO.

           IF ( A15MEMO-PRIMARY NOT = "TFR FR BR" ) AND
              ( A15MEMO-PRIMARY NOT = "TFR TO BR" )
              GO TO UPGRADE-FILE-BM-EXIT.

           IF ( A15MEMO-BM-DIRECTION NOT = "--->" )
              GO TO UPGRADE-FILE-BM-EXIT.

           MOVE SPACES                     TO TFRMEMO-MEMO.
           MOVE A15MEMO-PRIMARY            TO TFRMEMO-PRIMARY.
           MOVE A15MEMO-BRNO1              TO TFRMEMO-BRNO1.
           MOVE A15MEMO-SECONDARY          TO TFRMEMO-SECONDARY.
           MOVE A15MEMO-BRNO2              TO TFRMEMO-BRNO2.
           MOVE A15MEMO-BM-RE              TO TFRMEMO-BM-RE.
           MOVE A15MEMO-BM-ACCTNO1         TO TFRMEMO-BM-ACCTNO1.
           MOVE A15MEMO-BM-DIRECTION       TO TFRMEMO-BM-DIRECTION.
           MOVE A15MEMO-BM-ACCTNO2         TO TFRMEMO-BM-ACCTNO2.

           MOVE TFRMEMO-MEMO               TO BM-MEMO ABM-MEMO.

       UPGRADE-FILE-BM-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - C J
      *
      ******************************************************************
       UPGRADE-FILE-CJ SECTION.

           IF FILE-OPTION-CJ = "A"
              PERFORM CLEAR-ACJ-FILE
           ELSE
              PERFORM CLEAR-CJ-FILE.

           MOVE OCJ-BRNO        TO CJ-BRNO   ACJ-BRNO.
           MOVE OCJ-COLLID      TO CJ-COLLID ACJ-COLLID.

      *-       -       -       -       -       -       -       -       -
      *    OCJ-DATE IS STORED AS JULIAN IN A15
      *    FORCING CJ-DATE TO CCYYMMDD NOW INSTEAD OF LATER (A31).
      *-       -       -       -       -       -       -       -       -
           MOVE OCJ-DATE          TO JULIAN-DATE.
           PERFORM CJUL.
           MOVE NUM-DATE          TO CJ-DATE ACJ-DATE.

           IF OCJ-START-TIME NOT NUMERIC
              MOVE 0              TO CJ-START-TIME ACJ-START-TIME
           ELSE
              MOVE OCJ-START-TIME TO CJ-START-TIME ACJ-START-TIME.
           MOVE OCJ-SEQNO         TO CJ-SEQNO      ACJ-SEQNO.

           IF OCJ-ACCTNO NOT NUMERIC
              MOVE 0              TO OLD-ACCTNO
           ELSE
              MOVE OCJ-ACCTNO     TO OLD-ACCTNO.
           MOVE NEW-ACCTNO        TO CJ-ACCTNO     ACJ-ACCTNO.

           IF OCJ-END-TIME NOT NUMERIC
              MOVE 0              TO CJ-END-TIME    ACJ-END-TIME
           ELSE
              MOVE OCJ-END-TIME   TO CJ-END-TIME    ACJ-END-TIME.
           MOVE OCJ-ACTIVITY-CD   TO CJ-ACTIVITY-CD ACJ-ACTIVITY-CD.
           MOVE OCJ-RESULT-CD     TO CJ-RESULT-CD   ACJ-RESULT-CD.
           MOVE OCJ-ACTION-CD     TO CJ-ACTION-CD   ACJ-ACTION-CD.
           MOVE OCJ-NEW-COLLID    TO CJ-NEW-COLLID  ACJ-NEW-COLLID.
           MOVE OCJ-CON-STAT      TO CJ-CON-STAT    ACJ-CON-STAT.
           MOVE OCJ-REC-STAT      TO CJ-REC-STAT    ACJ-REC-STAT.
           IF OCJ-FORM-NO NOT NUMERIC
              MOVE 0              TO CJ-FORM-NO    ACJ-FORM-NO
           ELSE
              MOVE OCJ-FORM-NO    TO CJ-FORM-NO    ACJ-FORM-NO.

           MOVE OCJ-PROMISE-DATE1 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           MOVE DATE-YYYYMMDD     TO CJ-PROMISE-DATE1 ACJ-PROMISE-DATE1.

           IF OCJ-PROMISE-AMT NOT NUMERIC
              MOVE 0               TO CJ-PROMISE-AMT ACJ-PROMISE-AMT
           ELSE
              MOVE OCJ-PROMISE-AMT TO CJ-PROMISE-AMT ACJ-PROMISE-AMT.
           IF OCJ-BEG-LM-SEQ NOT NUMERIC
              MOVE 0               TO CJ-BEG-LM-SEQ ACJ-BEG-LM-SEQ
           ELSE
              MOVE OCJ-BEG-LM-SEQ  TO CJ-BEG-LM-SEQ ACJ-BEG-LM-SEQ.
           IF OCJ-END-LM-SEQ NOT NUMERIC
              MOVE 0               TO CJ-END-LM-SEQ ACJ-END-LM-SEQ
           ELSE
              MOVE OCJ-END-LM-SEQ  TO CJ-END-LM-SEQ ACJ-END-LM-SEQ.

           MOVE OCJ-PROMISE-DATE2 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           MOVE DATE-YYYYMMDD     TO CJ-PROMISE-DATE2 ACJ-PROMISE-DATE2.

           IF OCJ-CUR-BAL NOT NUMERIC
              MOVE 0               TO CJ-CUR-BAL      ACJ-CUR-BAL
           ELSE
              MOVE OCJ-CUR-BAL    TO CJ-CUR-BAL       ACJ-CUR-BAL.
           IF OCJ-PAST-DUE NOT NUMERIC
              MOVE 0               TO CJ-PAST-DUE     ACJ-PAST-DUE
           ELSE
              MOVE OCJ-PAST-DUE    TO CJ-PAST-DUE     ACJ-PAST-DUE.
           IF OCJ-POSTING-BR NOT NUMERIC
              MOVE 0               TO CJ-POSTING-BR   ACJ-POSTING-BR
           ELSE
              MOVE OCJ-POSTING-BR  TO CJ-POSTING-BR   ACJ-POSTING-BR.
           MOVE OCJ-QUEUE         TO CJ-QUEUE         ACJ-QUEUE.

       UPGRADE-FILE-CJ-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - L M
      *
      ******************************************************************
       UPGRADE-FILE-LM SECTION.

           MOVE SPACES TO LM-REC ALM-REC.
           IF FILE-OPTION-LM = "A"
              MOVE ALL X"09" TO ALM-REC
              MOVE SPACES TO ALM-LTOUCH-USERID ALM-MEMO ALM-MEMO-TYPE
           ELSE
              MOVE SPACES TO LM-REC.

           MOVE BR-NO      TO  LM-BRNO  ALM-BRNO.
           MOVE OLM-ACCTNO TO OLD-ACCTNO.
           MOVE NEW-ACCTNO TO LM-ACCTNO ALM-ACCTNO.
           MOVE OLM-DATE   TO LM-DATE   ALM-DATE-REVERSED.
           MOVE OLM-SEQ    TO LM-SEQ    ALM-SEQ.

      * LTOUCH FIELDS GET SET IN I/O WRITE
           INSPECT OLM-MEMO REPLACING ALL X"09" BY " ".
           INSPECT OLM-MEMO REPLACING ALL X"96" BY "-".
           MOVE OLM-MEMO      TO LM-MEMO      ALM-MEMO.
           MOVE OLM-MEMO-TYPE TO LM-MEMO-TYPE ALM-MEMO-TYPE.

      *=================================================================
      *    USING TRFMEMO WORKERS (LIBLP/TFRMEMOW) TO FIX A15 TRANSFER
      *    MEMOS FROM USING OLD ACCTNO (6 BYTES) TO NEW ACCTNO (8 BYTES)
      *
      *-----------------------------------------------------------------
      *
      * EXAMPLE A15 MEMOS CREATED [LP/BULKG2 & LP/BULKT2]
      * -------------------------------------------------
      *
      *
      *          -BRNO-     -BRNO- -ACCTNO/CURBAL- -ACCTNO-
      * TFR TO BR #### FR BR ####, FOR $ZZZZZ9.99 ---> 123456       [LM]
      * TFR FR BR #### TO BR ####, FOR $ZZZZZ9.99 <--- 123456       [LM]
      *
      *-----------------------------------------------------------------
      *
      * EXAMPLE A30 MEMOS CREATED [LP/BULKG2 & LP/BULKT2]
      * -------------------------------------------------
      *
      *          -BRNO-     -BRNO-   -ACCTNO/CURBAL-   -ACCTNO-
      * TFR TO BR #### FR BR ####, FOR $ZZZZZ9.99 ---> 12345678     [LM]
      * TFR FR BR #### TO BR ####, FOR $ZZZZZ9.99 <--- 12345678     [LM]
      *
      *=================================================================
       UPGRADE-FILE-LM-TFRMEMO.

           IF ( OLM-MEMO-TYPE NOT = "9" )
              GO TO UPGRADE-FILE-LM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE OLM-MEMO                   TO A15MEMO-MEMO.

           IF ( A15MEMO-PRIMARY NOT = "TFR FR BR" ) AND
              ( A15MEMO-PRIMARY NOT = "TFR TO BR" )
              GO TO UPGRADE-FILE-LM-EXIT.

           IF ( A15MEMO-LM-DIRECTION NOT = "--->" ) AND
              ( A15MEMO-LM-DIRECTION NOT = "<---" )
              GO TO UPGRADE-FILE-LM-EXIT.

           MOVE SPACES                     TO TFRMEMO-MEMO.
           MOVE A15MEMO-PRIMARY            TO TFRMEMO-PRIMARY.
           MOVE A15MEMO-BRNO1              TO TFRMEMO-BRNO1.
           MOVE A15MEMO-SECONDARY          TO TFRMEMO-SECONDARY.
           MOVE A15MEMO-BRNO2              TO TFRMEMO-BRNO2.
           MOVE A15MEMO-LM-FOR             TO TFRMEMO-LM-FOR.
           MOVE A15MEMO-LM-CURBALX         TO TFRMEMO-LM-CURBALX.
           MOVE A15MEMO-LM-DIRECTION       TO TFRMEMO-LM-DIRECTION.
           MOVE A15MEMO-LM-ACCTNO          TO TFRMEMO-LM-ACCTNO.

           MOVE TFRMEMO-MEMO               TO LM-MEMO ALM-MEMO.

       UPGRADE-FILE-LM-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - B Y
      *
      ******************************************************************
       UPGRADE-FILE-BY SECTION.

           PERFORM CLEAR-BY-FILE.

           MOVE OBY-DATE                   TO DATE-YYYMM.
           PERFORM CONVERT-YYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM                TO BY-DATE-CCYYMM
                                             ABYA-DATE-CCYYMM
                                             ABYB-DATE-CCYYMM.
           MOVE OBY-BRNO                   TO BY-BRNO ABYA-BRNO
                                                      ABYB-BRNO.
           IF OBY-CLASS NOT NUMERIC
              MOVE 0                       TO BY-CLASS 
                                             ABYA-CLASS ABYB-CLASS
           ELSE
           MOVE OBY-CLASS                  TO BY-CLASS
                                             ABYA-CLASS ABYB-CLASS.

           MOVE 0                          TO BY-LTOUCH-DATE
                                             ABYA-LTOUCH-DATE.

      * BYA
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                   AFTER   S2 FROM 1 BY 1 UNTIL S2 > 2
                   AFTER   S3 FROM 1 BY 1 UNTIL S3 > 11
                MOVE OBY-NO (S1 S2 S3) TO BY-NO   (S1 S2 S3)
                                         ABYA-NO  (S1 S2 S3)
                MOVE OBY-AMT(S1 S2 S3) TO BY-AMT  (S1 S2 S3)
                                         ABYA-AMT (S1 S2 S3)
                MOVE OBY-O2 (S1 S2 S3) TO BY-O2   (S1 S2 S3)
                                         ABYA-O2  (S1 S2 S3)
                MOVE OBY-INT(S1 S2 S3) TO BY-INT  (S1 S2 S3)
                                         ABYA-INT (S1 S2 S3)
           END-PERFORM.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                   AFTER   S2 FROM 1 BY 1 UNTIL S2 > 2
                   AFTER   S3 FROM 1 BY 1 UNTIL S3 > 5
              MOVE OBY-NO-REPO (S1 S2 S3) TO BY-NO-REPO  (S1 S2 S3)
                                            ABYA-NO-REPO (S1 S2 S3)
              MOVE OBY-AMT-REPO(S1 S2 S3) TO BY-AMT-REPO (S1 S2 S3)
                                            ABYA-AMT-REPO(S1 S2 S3)
              MOVE OBY-O2-REPO (S1 S2 S3) TO BY-O2-REPO  (S1 S2 S3)
                                            ABYA-O2-REPO (S1 S2 S3)
              MOVE OBY-INT-REPO(S1 S2 S3) TO BY-INT-REPO (S1 S2 S3)
                                            ABYA-INT-REPO(S1 S2 S3)
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                   AFTER   S2 FROM 1 BY 1 UNTIL S2 > 2
                   AFTER   S3 FROM 1 BY 1 UNTIL S3 > 10
               MOVE OBY-NO-PL (S1 S2 S3) TO BY-NO-PL  (S1 S2 S3)
                                           ABYA-NO-PL (S1 S2 S3)
               MOVE OBY-AMT-PL(S1 S2 S3) TO BY-AMT-PL (S1 S2 S3)
                                           ABYA-AMT-PL(S1 S2 S3)
               MOVE OBY-O2-PL (S1 S2 S3) TO BY-O2-PL  (S1 S2 S3)
                                           ABYA-O2-PL (S1 S2 S3)
               MOVE OBY-INT-PL(S1 S2 S3) TO BY-INT-PL (S1 S2 S3)
                                           ABYA-INT-PL(S1 S2 S3)
           END-PERFORM.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                   AFTER   S2 FROM 1 BY 1 UNTIL S2 > 2
                   AFTER   S3 FROM 1 BY 1 UNTIL S3 > 3
               MOVE OBY-DLRVPRIN(S1 S2 S3) TO BY-DLRVPRIN (S1 S2 S3)
                                             ABYA-DLRVPRIN(S1 S2 S3)
               MOVE OBY-DLRVCHGS(S1 S2 S3) TO BY-DLRVCHGS (S1 S2 S3)
                                             ABYA-DLRVCHGS(S1 S2 S3)
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                   AFTER   S2 FROM 1 BY 1 UNTIL S2 > 2
               MOVE OBY-DLPARVHELD(S1 S2) TO BY-DLPARVHELD (S1 S2)
                                            ABYA-DLPARVHELD(S1 S2)
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 02
              PERFORM VARYING S2 FROM 1 BY 1 UNTIL S2 > 05
                  MOVE OBY-PL-PRINCOL(S1 S2) TO BY-PL-PRINCOL (S1 S2)
                                               ABYB-PL-PRINCOL(S1 S2)
                  MOVE OBY-PL-INTCOL (S1 S2) TO BY-PL-INTCOL  (S1 S2)
                                               ABYB-PL-INTCOL (S1 S2)
                  MOVE OBY-PL-LATECOL(S1 S2) TO BY-PL-LATECOL (S1 S2)
                                               ABYB-PL-LATECOL(S1 S2)
                  MOVE OBY-PL-O2COL  (S1 S2) TO BY-PL-O2COL   (S1 S2)
                                               ABYB-PL-O2COL  (S1 S2)
              END-PERFORM
                  MOVE OBY-AMT-SOLD   (S1) TO BY-AMT-SOLD   (S1)
                                             ABYB-AMT-SOLD  (S1)
                  MOVE OBY-AMT-TRANS  (S1) TO BY-AMT-TRANS  (S1)
                                             ABYB-AMT-TRANS (S1)
                  MOVE OBY-AMT-PURCH  (S1) TO BY-AMT-PURCH  (S1)
                                             ABYB-AMT-PURCH (S1)
                  MOVE OBY-NO-SOLD    (S1) TO BY-NO-SOLD    (S1)
                                             ABYB-NO-SOLD   (S1)
                  MOVE OBY-NO-TRANS   (S1) TO BY-NO-TRANS   (S1)
                                             ABYB-NO-TRANS  (S1)
                   MOVE OBY-NO-PURCH   (S1) TO BY-NO-PURCH  (S1)
                                             ABYB-NO-PURCH  (S1)
                   MOVE OBY-OTHCOL     (S1) TO BY-OTHCOL    (S1)
                                              ABYB-OTHCOL   (S1)
                   MOVE OBY-OT2COL     (S1) TO BY-OT2COL    (S1)
                                              ABYB-OT2COL   (S1)
                   MOVE OBY-INTCOL     (S1) TO BY-INTCOL    (S1)
                                              ABYB-INTCOL   (S1)
                   MOVE OBY-LATECOL    (S1) TO BY-LATECOL    (S1)
                                              ABYB-LATECOL   (S1)
                   MOVE OBY-PRINCOL    (S1) TO BY-PRINCOL    (S1)
                                              ABYB-PRINCOL   (S1)
                   MOVE OBY-DEFCOL     (S1) TO BY-DEFCOL     (S1)
                                              ABYB-DEFCOL    (S1)
                   MOVE OBY-RNPRIN     (S1) TO BY-RNPRIN     (S1)
                                              ABYB-RNPRIN    (S1)
                   MOVE OBY-INTREB     (S1) TO BY-INTREB     (S1)
                                              ABYB-INTREB    (S1)
                   MOVE OBY-CLREB1     (S1) TO BY-CLREB1     (S1)
                                              ABYB-CLREB1    (S1)
                   MOVE OBY-CLREB2     (S1) TO BY-CLREB2     (S1)
                                              ABYB-CLREB2    (S1)
                   MOVE OBY-CLREB3     (S1) TO BY-CLREB3     (S1)
                                              ABYB-CLREB3    (S1)
                   MOVE OBY-CLREB4     (S1) TO BY-CLREB4     (S1)
                                              ABYB-CLREB4    (S1)
                   MOVE OBY-AHREB      (S1) TO BY-AHREB      (S1)
                                              ABYB-AHREB    (S1)
                   MOVE OBY-PPREB      (S1) TO BY-PPREB      (S1)
                                              ABYB-PPREB    (S1)
                   MOVE OBY-O1REB      (S1) TO BY-O1REB      (S1)
                                              ABYB-O1REB    (S1)
                   MOVE OBY-O2REB      (S1) TO BY-O2REB      (S1)
                                              ABYB-O2REB    (S1)
                   MOVE OBY-O3REB      (S1) TO BY-O3REB      (S1)
                                              ABYB-O3REB    (S1)
                   MOVE OBY-O4REB      (S1) TO BY-O4REB      (S1)
                                              ABYB-O4REB    (S1)
                   MOVE OBY-O5REB      (S1) TO BY-O5REB      (S1)
                                              ABYB-O5REB    (S1)
                   MOVE OBY-O6REB      (S1) TO BY-O6REB      (S1)
                                              ABYB-O6REB    (S1)
                   MOVE OBY-O7REB      (S1) TO BY-O7REB      (S1)
                                              ABYB-O7REB    (S1)
                   MOVE OBY-SCREB      (S1) TO BY-SCREB      (S1)
                                              ABYB-SCREB    (S1)
                   MOVE OBY-MFREB      (S1) TO BY-MFREB      (S1)
                                              ABYB-MFREB    (S1)
                   MOVE OBY-DFREB      (S1) TO BY-DFREB      (S1)
                                              ABYB-DFREB    (S1)
                   MOVE 0                   TO BY-UN7-REB    (S1)
                                              ABYB-UN7-REB    (S1)
                                               BY-UN8-REB    (S1)
                                              ABYB-UN8-REB    (S1)
                                               BY-UN9-REB    (S1)
                                              ABYB-UN9-REB    (S1)
                                               BY-UN10-REB   (S1)
                                              ABYB-UN10-REB   (S1)
                   MOVE OBY-PART-CHGOFF(S1) TO BY-PART-CHGOFF(S1)
                                              ABYB-PART-CHGOFF(S1)
                   MOVE OBY-ALLOTMENTS (S1) TO BY-ALLOTMENTS (S1)
                                              ABYB-ALLOTMENTS(S1)

                   PERFORM VARYING S2 FROM 1 BY 1 UNTIL S2 > 7
                      MOVE OBY-INS-NO-ELIG(S1 S2) TO 
                            BY-INS-NO-ELIG(S1 S2)
                           ABYB-INS-NO-ELIG(S1 S2)
                      MOVE OBY-INS-NO-WROTE(S1 S2) TO
                             BY-INS-NO-WROTE(S1 S2)
                           ABYB-INS-NO-WROTE(S1 S2)
                      MOVE OBY-INS-NO-NOTWROTE(S1 S2) TO
                            BY-INS-NO-NOTWROTE(S1 S2)
                           ABYB-INS-NO-NOTWROTE(S1 S2)
                      MOVE OBY-INS-AMT-WROTE(S1 S2) TO
                            BY-INS-AMT-WROTE(S1 S2)
                           ABYB-INS-AMT-WROTE(S1 S2)
                   END-PERFORM

                   PERFORM VARYING S2 FROM 8 BY 1 UNTIL S2 > 10
                      MOVE 0 TO BY-INS-NO-ELIG(S1 S2)
                               ABYB-INS-NO-ELIG(S1 S2)
                      MOVE 0 TO BY-INS-NO-WROTE(S1 S2)
                               ABYB-INS-NO-WROTE(S1 S2)
                      MOVE 0 TO BY-INS-NO-NOTWROTE(S1 S2)
                               ABYB-INS-NO-NOTWROTE(S1 S2)
                      MOVE 0 TO BY-INS-AMT-WROTE(S1 S2)
                               ABYB-INS-AMT-WROTE(S1 S2)
                   END-PERFORM

      * NONFILE, AD&D, ROADGARD WERE 8,9,10 IN INS TABLE, NOW THEY
      * ARE 1, 2, 3 IN FEE INS TABLE:

                   PERFORM VARYING S2 FROM 1 BY 1 UNTIL S2 > 3
                      MOVE OBY-INS-NO-ELIG(S1, S2 + 7) TO 
                            BY-FEE-NO-ELIG(S1 S2)
                           ABYB-FEE-NO-ELIG(S1 S2)
                      MOVE OBY-INS-NO-WROTE(S1, S2 + 7) TO
                            BY-FEE-NO-WROTE(S1 S2)
                           ABYB-FEE-NO-WROTE(S1 S2)
                      MOVE OBY-INS-NO-NOTWROTE(S1, S2 + 7) TO
                            BY-FEE-NO-NOTWROTE(S1 S2)
                           ABYB-FEE-NO-NOTWROTE(S1 S2)
                      MOVE OBY-INS-AMT-WROTE(S1, S2 + 7) TO
                            BY-FEE-AMT-WROTE(S1 S2)
                           ABYB-FEE-AMT-WROTE(S1 S2)
                   END-PERFORM

                  MOVE OBY-OTH-NO-COL(S1)      TO BY-OTH-NO-COL(S1)
                                                 ABYB-OTH-NO-COL(S1)
                  MOVE OBY-OT2-NO-COL(S1)      TO BY-OT2-NO-COL(S1)
                                                 ABYB-OT2-NO-COL(S1)
                  MOVE OBY-INT-NO-COL(S1)      TO BY-INT-NO-COL(S1)
                                                 ABYB-INT-NO-COL(S1)
                  MOVE OBY-LATE-NO-COL(S1)     TO BY-LATE-NO-COL(S1)
                                                 ABYB-LATE-NO-COL(S1)
                  MOVE OBY-OTH-NO-WAIVED(S1)   TO BY-OTH-NO-WAIVED(S1)
                                                 ABYB-OTH-NO-WAIVED(S1)
                  MOVE OBY-OT2-NO-WAIVED(S1)   TO BY-OT2-NO-WAIVED(S1)
                                                 ABYB-OT2-NO-WAIVED(S1)
                  MOVE OBY-INT-NO-WAIVED(S1)   TO BY-INT-NO-WAIVED(S1)
                                                 ABYB-INT-NO-WAIVED(S1)
                  MOVE OBY-LATE-NO-WAIVED(S1)  TO BY-LATE-NO-WAIVED(S1)
                                                 ABYB-LATE-NO-WAIVED(S1)
                  MOVE OBY-OTH-AMT-WAIVED(S1)  TO BY-OTH-AMT-WAIVED(S1)
                                                 ABYB-OTH-AMT-WAIVED(S1)
                  MOVE OBY-OT2-AMT-WAIVED(S1)  TO BY-OT2-AMT-WAIVED(S1)
                                                 ABYB-OT2-AMT-WAIVED(S1)
                  MOVE OBY-INT-AMT-WAIVED(S1)  TO BY-INT-AMT-WAIVED(S1)
                                                 ABYB-INT-AMT-WAIVED(S1)
                  MOVE OBY-LATE-AMT-WAIVED(S1) TO BY-LATE-AMT-WAIVED(S1)
                                                ABYB-LATE-AMT-WAIVED(S1)
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                     AFTER S2 FROM 1 BY 1 UNTIL S2 > 2
                     AFTER S3 FROM 1 BY 1 UNTIL S3 > 10
                  MOVE OBY-UNITS (S1 S2 S3) TO BY-UNITS (S1 S2 S3)
                                              ABYB-UNITS(S1 S2 S3)
                  MOVE OBY-OUTBAL(S1 S2 S3) TO BY-OUTBAL(S1 S2 S3)
                                              ABYB-OUTBAL(S1 S2 S3)
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                     AFTER S2 FROM 1 BY 1 UNTIL S2 > 2
                     AFTER S3 FROM 1 BY 1 UNTIL S3 > 10
                   MOVE OBY-UNITS-BNKRPT (S1 S2 S3) TO
                                      BY-UNITS-BNKRPT (S1 S2 S3)
                                     ABYB-UNITS-BNKRPT(S1 S2 S3)
                   MOVE OBY-OUTBAL-BNKRPT(S1 S2 S3) TO
                                      BY-OUTBAL-BNKRPT(S1 S2 S3)
                                     ABYB-OUTBAL-BNKRPT(S1 S2 S3)
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE OBY-SUMWGTTM               TO BY-SUMWGTTM
                                             ABYB-SUMWGTTM.
           MOVE OBY-SUMWGTAV               TO BY-SUMWGTAV
                                             ABYB-SUMWGTAV.
           MOVE OBY-SUMWGTTA               TO BY-SUMWGTTA
                                             ABYB-SUMWGTTA.
           MOVE OBY-IB-ACCRUED             TO BY-IB-ACCRUED
                                             ABYB-IB-ACCRUED.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 03
                MOVE OBY-DLDISC(S1) TO BY-DLDISC(S1)
                                      ABYB-DLDISC(S1)
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            MOVE OBY-STD-POS       TO BY-STD-POS
                                     ABYB-STD-POS.
            MOVE OBY-30-DAY-MOVED  TO BY-30-DAY-MOVED
                                     ABYB-30-DAY-MOVED.
            MOVE OBY-PROJ-INCREASE TO BY-PROJ-INCREASE
                                     ABYB-PROJ-INCREASE.
            MOVE OBY-PROJ-PROFIT   TO BY-PROJ-PROFIT
                                     ABYB-PROJ-PROFIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                     AFTER S2 FROM 1 BY 1 UNTIL S2 > 2
                     AFTER S3 FROM 1 BY 1 UNTIL S3 > 6
                   MOVE OBY-UNEARNED-AMT(S1 S2 S3) TO
                         BY-UNEARNED-AMT(S1 S2 S3)
                        ABYB-UNEARNED-AMT(S1 S2 S3)
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                     AFTER S2 FROM 1 BY 1 UNTIL S2 > 2
                     AFTER S3 FROM 7 BY 1 UNTIL S3 > 10
                   MOVE 0 TO BY-UNEARNED-AMT(S1 S2 S3)
                            ABYB-UNEARNED-AMT(S1 S2 S3)
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 2
                  MOVE OBY-PROCEEDS(S1)        TO BY-PROCEEDS(S1)
                                                 ABYB-PROCEEDS(S1)
                  MOVE OBY-LOCKBOX(S1)         TO BY-LOCKBOX(S1)
                                                 ABYB-LOCKBOX(S1)
           END-PERFORM.

       UPGRADE-FILE-BY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - G T
      *
      *=================================================================
      *    ****NOTE**** OGT-DATE1 OGT-DATE2 OGT-DATE3 ARE STORED IN
      *                 YYMMDD FORMAT.  NEED TO FIRST CONVERT BACK TO
      *                 MMDDYY FORMAT THEN USE
      *                 CONVERT-MMDDYY-TO-YYYYMMDD TO MAKE
      *                 GT-DATE1|2|3 IS ACTIVE CCYYMMDD IN A30.
      *
      ******************************************************************
       UPGRADE-FILE-GT SECTION.

           IF FILE-OPTION-GT = "A"
              PERFORM CLEAR-AGT-FILE
           ELSE
              PERFORM CLEAR-GT-FILE.

           MOVE OGT-FLAG                   TO GT-FLAG AGT-FLAG.
           MOVE OGT-REF                    TO GT-REF  AGT-REF.

           MOVE OGT-DATE1                  TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           MOVE DATE-YYYYMMDD              TO GT-DATE1  AGT-DATE1.

           IF OGT-SOURCE NOT NUMERIC
              MOVE 0                       TO GT-SOURCE AGT-SOURCE
           ELSE
           MOVE OGT-SOURCE                 TO GT-SOURCE AGT-SOURCE.

           MOVE OGT-DATE2                  TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           MOVE DATE-YYYYMMDD              TO GT-DATE2  AGT-DATE2.

           IF OGT-BRANCH NOT NUMERIC
              MOVE 0                       TO GT-BRANCH AGT-BRANCH
           ELSE
           MOVE OGT-BRANCH                 TO GT-BRANCH AGT-BRANCH.
           IF OGT-ACCTNO NOT NUMERIC
              MOVE 0                       TO GT-ACCTNO AGT-ACCTNO
           ELSE
           MOVE OGT-ACCTNO                 TO GT-ACCTNO AGT-ACCTNO.

           MOVE OGT-DATE3                  TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           MOVE DATE-YYYYMMDD              TO GT-DATE3  AGT-DATE3.

           MOVE OGT-DRCRFG                 TO GT-DRCRFG AGT-DRCRFG.
           IF OGT-LINE NOT NUMERIC
              MOVE 0                       TO GT-LINE AGT-LINE
           ELSE
           MOVE OGT-LINE                   TO GT-LINE   AGT-LINE.

           MOVE 0                          TO GT-LTOUCH-DATE
                                             AGT-LTOUCH-DATE.

           IF OGT-AMT NOT NUMERIC
              MOVE 0                TO GT-AMT         AGT-AMT
           ELSE
           MOVE OGT-AMT             TO GT-AMT         AGT-AMT.
           MOVE OGT-EXPL            TO GT-EXPL        AGT-EXPL.
           IF OGT-PERIOD NOT NUMERIC
              MOVE 0                TO GT-PERIOD      AGT-PERIOD
           ELSE
           MOVE OGT-PERIOD          TO GT-PERIOD      AGT-PERIOD.
           MOVE OGT-SMITH-SENT      TO GT-SMITH-SENT  AGT-SMITH-SENT.
           MOVE OGT-POST-USERID     TO GT-POST-USERID AGT-POST-USERID.

           IF OGT-POST-DATE NOT NUMERIC OR
                OGT-POST-DATE = 202020
              MOVE 0 TO GT-POST-DATE AGT-POST-DATE
           ELSE
              MOVE OGT-POST-DATE       TO DATE-MMDDYY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              PERFORM TEST-INVALID-DATE
              MOVE DATE-YYYYMMDD       TO GT-POST-DATE
                                         AGT-POST-DATE.

           IF OGT-POST-TIME NOT NUMERIC
              MOVE 0                TO GT-POST-TIME AGT-POST-TIME
           ELSE
           MOVE OGT-POST-TIME       TO GT-POST-TIME AGT-POST-TIME.

       UPGRADE-FILE-GT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - B M
      *
      ******************************************************************
       PROCESS-FILE-BM SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-BM = "N" )
              GO TO PROCESS-FILE-BM-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO BM-PATH-OVERRIDE.
           MOVE "BM"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/BMFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.
           MOVE ZEROES                     TO DATE35-RECORD-COUNTERS.

           PERFORM UP1534-LOAD-FILE-BEG-TIME.
           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO UP1534-MV15-OLD-PATH.
           PERFORM UP1534-MV15-OLD-TO-NEW.
           MOVE UP1534-MV15-NEW-PATH       TO OBM-PATH.

           IF ( STAT-BAD )     *> MOVE OF OLD FILE TO NEW FILE WAS BAD
              MOVE STAT                    TO UPGRADE-STAT
              PERFORM UP1534-CREATE-FILE-ERROR
              GO TO PROCESS-FILE-BM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP TARGET PATHS }
           MOVE UP1534-FILE-ORDER---TARGET TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-BM = "Y")
              MOVE UP1534-FILE-PATH           TO BM-PATH
              PERFORM OPEN-BM1-FILE
           ELSE
              PERFORM LOAD-BM1-FILE
              MOVE UP1534-FILE-PATH TO ABM-PATH-BEG
                                       ABM-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ABM-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ABM-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ABM-PATH-GROUP 
                                            ABM-DONE-PATH-GROUP
              PERFORM OPEN-ABM-FILE.

           PERFORM MISC-TRACE-FILE-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ OPEN & START FILES }
           MOVE ZEROES                     TO OBM-SSNO
                                              OBM-DATE
                                              OBM-SEQ.

           PERFORM OPEN-OBM1-FILE.
           PERFORM START-OBM1-FILE.

      *=================================================================
       PROCESS-FILE-BM-NEXT.

           PERFORM READ-OBM1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-BM-END.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           IF ( BR-NO = 91 ) AND
              ( OBM-SSNO = 260337890 )
      D       STOP MESS
              MOVE SPACES TO MESS.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      * THIS BRANCH BMFILE HAS A TRUE DUPLICATE RECORD IN IT. 
      * A VUTIL -R BMFILE REMOVES IT! PUT THIS IN FOR NOW
      *    IF (BR-NO = 482) AND
      *        (OBM-SSNO = 629056892) AND
      *          (OBM-DATE = 79969879) AND 
      *           (OBM-SEQ = 1) AND
      *            (SKIP-482-629056892-1 = "Y")
      *       MOVE "N" TO SKIP-482-629056892-1
      *       ADD 1 TO UP1534-RECORD-COUNTER-SKIP
      *       GO TO PROCESS-FILE-BM-NEXT.
               

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OBM-SSNO NOT NUMERIC OR
               OBM-DATE NOT NUMERIC OR
                OBM-SEQ NOT NUMERIC 
              ADD 1 TO UP1534-RECORD-COUNTER-SKIP
              GO TO PROCESS-FILE-BM-NEXT.

      * 99999999  1-10  BORROWER CONTACTS
      *    IF ( OBM-DATE = 99999999 ) AND
      *       ( OBM-SEQ  < 11       )
      *       ADD 1 TO UP1534-RECORD-COUNTER-SKIP
      *       GO TO PROCESS-FILE-BM-NEXT.

      * JKC 2024-0410 DO NOT UPGRADE ANY RECORDS WITH DATE = 99999999;
      *               THESE ARE BW CONTACT INFORMATION THAT IS NOW
      *               IN THE BWFILE; SEQ 1 THRU 10 WERE CONTACTS WHILE
      *               11 THRU 99 WERE FROM A GLITCH MADE IN THE
      *               MID-2000 FOR BRANCH TRANSFERS WHICH THE GLITCH
      *               WAS FIXED BUT DID NOT REMOVE BAD RECORDS.
           IF ( OBM-DATE = 99999999 )
              ADD 1 TO UP1534-RECORD-COUNTER-SKIP
              GO TO PROCESS-FILE-BM-NEXT.

JKC        IF ( OBM-DATE NOT  = ZEROES ) AND       *> NOT PRIORITY MEMO
JKC           ( PRIORITY-MEMO = "O"    )           *> PRIORITY ONLY!!!
JKC           ADD 1 TO UP1534-RECORD-COUNTER-SKIP
JKC           GO TO PROCESS-FILE-BM-NEXT.

JKC        IF ( OBM-DATE = ZEROES )                    *> PRIORITY MEMO
JKC           ADD 1 TO DATE35-RECORD-COUNTER-READ-PR
JKC           IF ( PRIORITY-MEMO = "N" )
JKC              ADD 1 TO DATE35-RECORD-COUNTER-SKIP-PR
JKC              ADD 1 TO UP1534-RECORD-COUNTER-SKIP
JKC              GO TO PROCESS-FILE-BM-NEXT
JKC           ELSE
JKC              NEXT SENTENCE
JKC        ELSE
JKC        IF ( OBM-DATE > BEG-DATE-MEMO-REVERSE ) OR
JKC           ( OBM-DATE < END-DATE-MEMO-REVERSE )
JKC           ADD 1 TO UP1534-RECORD-COUNTER-SKIP
JKC           GO TO PROCESS-FILE-BM-NEXT.

           PERFORM UPGRADE-FILE-BM.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           IF ( BR-NO = 91 ) AND
              ( BM-SSNO = 260337890 )
      D       STOP MESS
              MOVE SPACES TO MESS.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           IF (FILE-OPTION-BM = "Y")
              PERFORM WRITE-BM1-FILE
           ELSE
              ADD 1 TO ABM-KEY
              ACCEPT ABM-LTOUCH-DATE FROM CENTURY-DATE
              ACCEPT TIME-NOW FROM TIME
              MOVE H-AND-M            TO ABM-LTOUCH-TIME-HHMM
              MOVE EXT-CONAME-USER-ID TO ABM-LTOUCH-USERID
              MOVE X"0D"              TO ABM-CR
              MOVE X"0A"              TO ABM-LF
              PERFORM WRITE-ABM-FILE.

           IF ( IO-BAD )
JKC           IF ( OBM-DATE = ZEROES )                 *> PRIORITY MEMO
JKC              ADD 1 TO DATE35-RECORD-COUNTER-SKIP-PR
JKC           END-IF
              ADD 1                        TO UP1534-RECORD-COUNTER-SKIP
              MOVE BM1-KEY                 TO UP1534-UPDATE-KEY
              MOVE FILE-STAT-DISP          TO UP1534-UPDATE-STAT
              MOVE "E"                     TO UP1534-UPDATE-TYPE
              PERFORM UP1534-CREATE-FILE-UPDATE
              IF ( FILE-STAT-DISP = "22,00" )
                 GO TO PROCESS-FILE-BM-NEXT
              ELSE
                 GO TO IO-ERROR.

           ADD 1 TO UP1534-RECORD-COUNTER-WRITE.

JKC        IF ( OBM-DATE = ZEROES )                 *> PRIORITY MEMO
JKC           ADD 1 TO DATE35-RECORD-COUNTER-WRITE-PR.

           GO TO PROCESS-FILE-BM-NEXT.

      *=================================================================
       PROCESS-FILE-BM-END.

           PERFORM CLOSE-OBM1-FILE.
           MOVE OBM-PATH TO UP1534-MV15-NEW-PATH.
           PERFORM UP1534-MV15-NEW-TO-OLD.

    
           IF (FILE-OPTION-BM = "Y")
              PERFORM CLOSE-BM1-FILE
           ELSE
              PERFORM CLOSE-ABM-FILE.

           PERFORM UP1534-CREATE-FILE-TOTALS.

           MOVE "BMFILE"                   TO HOLD-SORT-FILENAME.
           MOVE UP1534-MV15-OLD-PATH       TO HOLD-SORT-FILEPATH.
           MOVE BM-PATH-OWNST              TO HOLD-SORT-STATE.
           MOVE BM-PATH-OWNBR              TO HOLD-SORT-BRNO.
           PERFORM UPDATE-SORT-MODULE.

       PROCESS-FILE-BM-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - C J
      *
      ******************************************************************
       PROCESS-FILE-CJ SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-CJ = "N" )
              GO TO PROCESS-FILE-CJ-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO CJ-PATH-OVERRIDE.
           MOVE "CJ"                       TO UP1534-CURRENT-FILE.
           MOVE "SP/CJFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.
           MOVE ZEROES                     TO DATE35-RECORD-COUNTERS.

           PERFORM UP1534-LOAD-FILE-BEG-TIME.
           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO UP1534-MV15-OLD-PATH.
           PERFORM UP1534-MV15-OLD-TO-NEW.
           MOVE UP1534-MV15-NEW-PATH       TO OCJ-PATH.

           IF ( STAT-BAD )     *> MOVE OF OLD FILE TO NEW FILE WAS BAD
              MOVE STAT                    TO UPGRADE-STAT
              PERFORM UP1534-CREATE-FILE-ERROR
              GO TO PROCESS-FILE-CJ-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP TARGET PATHS }
           MOVE UP1534-FILE-ORDER---TARGET TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-CJ = "Y")
              MOVE UP1534-FILE-PATH           TO CJ-PATH
              PERFORM OPEN-CJ1-FILE
           ELSE
              PERFORM LOAD-CJ1-FILE
              MOVE UP1534-FILE-PATH TO ACJ-PATH-BEG
                                       ACJ-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ACJ-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ACJ-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ACJ-PATH-GROUP 
                                            ACJ-DONE-PATH-GROUP
              PERFORM OPEN-ACJ-FILE.

           PERFORM MISC-TRACE-FILE-START.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                     TO OCJ1-KEY.
           MOVE ZEROES                     TO OCJ-BRNO
                                              OCJ-DATE
                                              OCJ-START-TIME
                                              OCJ-SEQNO.

           PERFORM OPEN-OCJ1-FILE.
           PERFORM START-OCJ1-FILE.

      *=================================================================
       PROCESS-FILE-CJ-NEXT.

           PERFORM READ-OCJ1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-CJ-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

      * CLEANSE BR 1500 HAD A BRNO 1303 WHICH DOESNT EXIST, COULDNT DELETE
      * AND RECORD WAS FULL OF GARBAGE CHARACTERS.
           IF BR-NO = 1500 AND OCJ-BRNO = 1303
              ADD 1              TO UP1534-RECORD-COUNTER-SKIP
              MOVE OCJ1-KEY      TO UP1534-UPDATE-KEY
              MOVE "CJBR "       TO UP1534-UPDATE-STAT
              MOVE "E"           TO UP1534-UPDATE-TYPE
              PERFORM UP1534-CREATE-FILE-UPDATE
              GO TO PROCESS-FILE-CJ-NEXT.


           IF OCJ-BRNO NOT NUMERIC OR
               OCJ-DATE NOT NUMERIC OR
                OCJ-START-TIME NOT NUMERIC OR
                 OCJ-SEQNO NOT NUMERIC
              ADD 1 TO UP1534-RECORD-COUNTER-SKIP
              GO TO PROCESS-FILE-CJ-NEXT.

JKC        IF ( OCJ-DATE < BEG-DATE-A15-CJ-DATE ) OR
JKC           ( OCJ-DATE > END-DATE-A15-CJ-DATE )
JKC           ADD 1 TO UP1534-RECORD-COUNTER-SKIP
JKC           GO TO PROCESS-FILE-CJ-NEXT.

           PERFORM UPGRADE-FILE-CJ.

           IF (FILE-OPTION-CJ = "Y")
              PERFORM WRITE-CJ1-FILE
           ELSE
              ADD 1 TO ACJ-KEY
              ACCEPT ACJ-LTOUCH-DATE FROM CENTURY-DATE
              MOVE X"0D"              TO ACJ-CR
              MOVE X"0A"              TO ACJ-LF
              PERFORM WRITE-ACJ-FILE.
           IF ( IO-BAD )
              ADD 1                        TO UP1534-RECORD-COUNTER-SKIP
              MOVE CJ1-KEY                 TO UP1534-UPDATE-KEY
              MOVE FILE-STAT-DISP          TO UP1534-UPDATE-STAT
              MOVE "E"                     TO UP1534-UPDATE-TYPE
              PERFORM UP1534-CREATE-FILE-UPDATE
              IF ( FILE-STAT-DISP = "22,00" )
                 GO TO PROCESS-FILE-CJ-NEXT
              ELSE
                 GO TO IO-ERROR.

           ADD 1 TO UP1534-RECORD-COUNTER-WRITE.

           GO TO PROCESS-FILE-CJ-NEXT.

      *=================================================================
       PROCESS-FILE-CJ-END.

           PERFORM CLOSE-OCJ1-FILE.
           MOVE OCJ-PATH TO UP1534-MV15-NEW-PATH.
           PERFORM UP1534-MV15-NEW-TO-OLD.

           IF (FILE-OPTION-CJ = "Y")
              PERFORM CLOSE-CJ1-FILE
           ELSE
              PERFORM CLOSE-ACJ-FILE.

           PERFORM UP1534-CREATE-FILE-TOTALS.

           MOVE "CJFILE"                   TO HOLD-SORT-FILENAME.
           MOVE UP1534-MV15-OLD-PATH       TO HOLD-SORT-FILEPATH.
           MOVE CJ-PATH-OWNST              TO HOLD-SORT-STATE.
           MOVE CJ-PATH-OWNBR              TO HOLD-SORT-BRNO.
           PERFORM UPDATE-SORT-MODULE.

       PROCESS-FILE-CJ-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - L M
      *
      ******************************************************************
       PROCESS-FILE-LM SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-LM = "N" )
              GO TO PROCESS-FILE-LM-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO LM-PATH-OVERRIDE.
           MOVE "LM"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/LMFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.
           MOVE ZEROES                     TO DATE35-RECORD-COUNTERS.

           PERFORM UP1534-LOAD-FILE-BEG-TIME.
           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO UP1534-MV15-OLD-PATH.
           PERFORM UP1534-MV15-OLD-TO-NEW.
           MOVE UP1534-MV15-NEW-PATH       TO OLM-PATH.

           IF ( STAT-BAD )     *> MOVE OF OLD FILE TO NEW FILE WAS BAD
              MOVE STAT                    TO UPGRADE-STAT
              PERFORM UP1534-CREATE-FILE-ERROR
              GO TO PROCESS-FILE-LM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP TARGET PATHS }
           MOVE UP1534-FILE-ORDER---TARGET TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           IF (FILE-OPTION-LM = "Y")
              MOVE UP1534-FILE-PATH           TO LM-PATH
              PERFORM OPEN-LM1-FILE
           ELSE
              PERFORM LOAD-LM1-FILE
              MOVE UP1534-FILE-PATH TO ALM-PATH-BEG
                                       ALM-DONE-PATH-BEG
              CALL "C$TOLOWER" USING ALM-FILENAME, VALUE 06
              CALL "C$TOLOWER" USING ALM-DONE-FILENAME, VALUE 11
              MOVE HOLD-ASCII-PATH-GROUP TO ALM-PATH-GROUP 
                                            ALM-DONE-PATH-GROUP
              PERFORM OPEN-ALM-FILE.

           PERFORM MISC-TRACE-FILE-START.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                     TO OLM1-KEY.
           MOVE ZEROES                     TO OLM-ACCTNO
                                              OLM-DATE
                                              OLM-SEQ.
           PERFORM OPEN-OLM1-FILE.
           PERFORM START-OLM1-FILE.


      *=================================================================
       PROCESS-FILE-LM-NEXT.

           PERFORM READ-OLM1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-LM-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OLM-ACCTNO NOT NUMERIC OR
               OLM-DATE NOT NUMERIC OR
                OLM-SEQ NOT NUMERIC 
              ADD 1 TO UP1534-RECORD-COUNTER-SKIP
              GO TO PROCESS-FILE-LM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * JKC 2024-0503 FOUND RECORDS WITH ZERO ACCOUNT NUMBERS;
      * PER KEVIN MULLEN WE CAN SKIP LOADING THESE RECORDS IN DATABASE;
      * 1,326 RECORDS AS OF 2024-0331 WILL BE SKIPPED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE OLM-ACCTNO TO OLD-ACCTNO.

           IF NEW-ACCTNO = ZEROES
              ADD 1 TO UP1534-RECORD-COUNTER-SKIP
              GO TO PROCESS-FILE-LM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * JKC 2024-0502 FOUND RECORDS WITH ZERO DATE (99999999 REVERSED).
      * A COUPLE OF LOAN RECORDS WE LOOKED AT IN BRANCH 1500 WE FOUND
      * THESE WERE CREATED FROM AN ACQUISITION FROM ROYAL FINANCE IN
      * 2002.  THE ACQUISITION DATA WAS NOT VERIFIED AND IMPORTED
      * DIRECTLY SO THE MEMO DATA CAME IN AS ZEROES.  KEVIN MULLEN SAID
      * THAT WE SHOULD FORCE THESE DATES AS 1970-01-01
      * (80299898 REVERSED).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
JKC        IF ( OLM-DATE = 99999999 )
JKC           MOVE 80299898 TO OLM-DATE.     *> 1970-01-01

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
JKC        IF ( OLM-DATE NOT  = ZEROES ) AND       *> NOT PRIORITY MEMO
JKC           ( PRIORITY-MEMO = "O"    )           *> PRIORITY ONLY!!!
JKC           ADD 1 TO UP1534-RECORD-COUNTER-SKIP
JKC           GO TO PROCESS-FILE-LM-NEXT.

JKC        IF ( OLM-DATE = ZEROES )                    *> PRIORITY MEMO
              ADD 1 TO DATE35-RECORD-COUNTER-READ-PR
JKC           IF ( PRIORITY-MEMO = "N" )
                 ADD 1 TO DATE35-RECORD-COUNTER-SKIP-PR
JKC              ADD 1 TO UP1534-RECORD-COUNTER-SKIP
JKC              GO TO PROCESS-FILE-LM-NEXT
JKC           ELSE
JKC              NEXT SENTENCE
JKC        ELSE
JKC        IF ( OLM-DATE > BEG-DATE-MEMO-REVERSE ) OR
JKC           ( OLM-DATE < END-DATE-MEMO-REVERSE )
JKC           ADD 1 TO UP1534-RECORD-COUNTER-SKIP
JKC           GO TO PROCESS-FILE-LM-NEXT.

           PERFORM UPGRADE-FILE-LM.

           IF (FILE-OPTION-LM = "Y")
              PERFORM WRITE-LM1-FILE
           ELSE
              ADD 1 TO ALM-KEY
              ACCEPT ALM-LTOUCH-DATE FROM CENTURY-DATE
              ACCEPT TIME-NOW FROM TIME
              MOVE H-AND-M            TO ALM-LTOUCH-TIME-HHMM
              MOVE EXT-CONAME-USER-ID TO ALM-LTOUCH-USERID
              MOVE X"0D"              TO ALM-CR
              MOVE X"0A"              TO ALM-LF
              PERFORM WRITE-ALM-FILE.

           IF ( IO-BAD )
JKC           IF ( OLM-DATE = ZEROES )                 *> PRIORITY MEMO
JKC              ADD 1 TO DATE35-RECORD-COUNTER-SKIP-PR
JKC           END-IF
              ADD 1                        TO UP1534-RECORD-COUNTER-SKIP
              MOVE LM1-KEY                 TO UP1534-UPDATE-KEY
              MOVE FILE-STAT-DISP          TO UP1534-UPDATE-STAT
              MOVE "E"                     TO UP1534-UPDATE-TYPE
              PERFORM UP1534-CREATE-FILE-UPDATE
              IF ( FILE-STAT-DISP = "22,00" )
                 GO TO PROCESS-FILE-LM-NEXT
              ELSE
                 GO TO IO-ERROR.

           ADD 1 TO UP1534-RECORD-COUNTER-WRITE.

JKC        IF ( OLM-DATE = ZEROES )                 *> PRIORITY MEMO
JKC           ADD 1 TO DATE35-RECORD-COUNTER-WRITE-PR.

           GO TO PROCESS-FILE-LM-NEXT.

      *=================================================================
       PROCESS-FILE-LM-END.

           PERFORM CLOSE-OLM1-FILE.
           MOVE OLM-PATH TO UP1534-MV15-NEW-PATH.
           PERFORM UP1534-MV15-NEW-TO-OLD.

           IF (FILE-OPTION-LM = "Y")
              PERFORM CLOSE-LM1-FILE
           ELSE
              PERFORM CLOSE-ALM-FILE.

           PERFORM UP1534-CREATE-FILE-TOTALS.

           MOVE "LMFILE"                   TO HOLD-SORT-FILENAME.
           MOVE UP1534-MV15-OLD-PATH       TO HOLD-SORT-FILEPATH.
           MOVE LM-PATH-OWNST              TO HOLD-SORT-STATE.
           MOVE LM-PATH-OWNBR              TO HOLD-SORT-BRNO.
           PERFORM UPDATE-SORT-MODULE.

       PROCESS-FILE-LM-EXIT.
           EXIT.

      ******************************************************************
      * CONVERT "  A" TO "   A"
      *         "BAC" TO " BAC"
      * CANT LEAVE SPACES AT THE END IN A30
      ******************************************************************
       CONVERT-A15-A30-ALPNAME SECTION.

           MOVE HOLD-A15-ALPNAME TO HOLD-A30-ALPNAME.
          
      * "AAA" TO "AAAA"
           IF HOLD-A15-ALPNAME = "AAA"
              MOVE "AAAA" TO HOLD-A30-ALPNAME
              GO TO EXIT-CONVERT-A15-A30-ALPNAME.
      * "~~~" TO "~~~~"
           IF HOLD-A15-ALPNAME = "~~~"
              MOVE "~~~~" TO HOLD-A30-ALPNAME
              GO TO EXIT-CONVERT-A15-A30-ALPNAME.
      * "ZZZ" TO "ZZZZ"
           IF HOLD-A15-BYTE(1) = LITTLE-Z AND
               HOLD-A15-BYTE(2) = LITTLE-Z AND
                HOLD-A15-BYTE(3) = LITTLE-Z 
              MOVE LITTLE-Z TO HOLD-A30-BYTE(1)
                               HOLD-A30-BYTE(2)
                               HOLD-A30-BYTE(3)
                               HOLD-A30-BYTE(4)
              GO TO EXIT-CONVERT-A15-A30-ALPNAME.
      * "  A" TO "   A"
      * "  J" TO "   J"
      * " AH" TO "  AH"
           IF HOLD-A15-BYTE(1) = SPACES
              MOVE HOLD-A15-ALPNAME TO HOLD-A30-ALPNAME
              CALL "C$JUSTIFY" USING HOLD-A30-ALPNAME, "R"
              GO TO EXIT-CONVERT-A15-A30-ALPNAME.
 
      * IF ALL 3 BYTES ARE FILLED WITH OTHER TYPE LETTER COMBOS,
      * TRY TO FIGURE IT OUT, THEN JUST GIVE UP AND MOVE AND JUSTIFY
      * "A~~" TO "A~~~"
      * "BZZ" TO "BZZZ"
           IF HOLD-A15-BYTE(3) = LITTLE-Z
              MOVE LITTLE-Z  TO HOLD-A30-BYTE(4) 
           ELSE
           IF HOLD-A15-BYTE(3) = "~"
              MOVE "~" TO HOLD-A30-BYTE(4) 
           ELSE
              MOVE HOLD-A15-ALPNAME TO HOLD-A30-ALPNAME
              CALL "C$JUSTIFY" USING HOLD-A30-ALPNAME, "R".

       EXIT-CONVERT-A15-A30-ALPNAME.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - M O D U L E
      *
      ******************************************************************
       PRINT-REPORT-MODULE SECTION.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "REPORT"                   TO UP1534-CURRENT-FILE.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ OPEN PRINTER }
           PERFORM OPEN-PR-FILE.

      *- - - - - - - - - - - - - - - - - - - - -{ SETUP 'REPORT' FILE  }
           MOVE ZEROES                     TO UP1534-REPORT-KEY.

           PERFORM OPEN-REPORT-FILE.
           PERFORM START-REPORT-FILE.

      *=================================================================
       PRINT-REPORT-NEXT-LINE.

           PERFORM READ-REPORT-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PRINT-REPORT-MODULE-END.

           PERFORM UP1534-VPI-DISPLAY.

           MOVE REPORT-DETAIL-LINE         TO DETAIL-LINE.
           MOVE REPORT-LN-FEED             TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-NEXT-LINE.

      *=================================================================
       PRINT-REPORT-MODULE-END.

           PERFORM CLOSE-REPORT-FILE.
           PERFORM PRINT-RANGES.

       PRINT-REPORT-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( EXT-CONAME-RPT-LINES > ZEROES )
              IF ( LN-WK GREATER THAN EXT-CONAME-RPT-LINES )
                 PERFORM WRITE-HEADER-LINES
              ELSE
                 NEXT SENTENCE
           ELSE
           IF ( LN-WK GREATER THAN 60 )
              PERFORM WRITE-HEADER-LINES.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED                     TO LN-COUNT.
           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.

       EXIT-WRITE-DETAIL-LINE.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R A N G E S
      *
      ******************************************************************
       PRINT-RANGES SECTION.

           MOVE 99 TO LN-COUNT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GROUP-FLAG = "Y" )
              MOVE "GROUP ID NAME                    :" TO R1-DESC
              MOVE ENT-GROUP                            TO R1-FIELD
              MOVE 2                                    TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH NUMBER          :" TO R1-DESC
              MOVE BEG-BRANCH                           TO R1-NUMERIC
              MOVE 2                                    TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH NUMBER             :" TO R1-DESC
              MOVE END-BRANCH                           TO R1-NUMERIC
              MOVE 1                                    TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE "DATABASE NAME                    :" TO R1-DESC.
           MOVE EXT-ACUSQL-DB-NAME                   TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SERVER NAME                      :" TO R1-DESC.
           MOVE EXT-ACUSQL-SERVER-NAME               TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SOURCE PATH                      :" TO R1-DESC.
           MOVE SOURCE-PATH                          TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TARGET PATH                      :" TO R1-DESC.
           MOVE TARGET-PATH                          TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "*********[ GLOBAL FILES ]*********" TO DETAIL-LINE.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BY-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-BY                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "GT-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-GT                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "*********[ LOCAL FILES ]**********" TO DETAIL-LINE.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BM-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-BM                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CJ-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-CJ                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LM-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-LM                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "*********[ DATE OPTION ]**********" TO DETAIL-LINE.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRIORITY MEMO (Y/N)              :" TO R1-DESC.
           MOVE FILE-OPTION-BM                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING DATE                   :" TO R1-DESC.
           MOVE BEG-DATE                             TO R1-DATE.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE                   :" TO R1-DESC.
           MOVE END-DATE                             TO R1-DATE.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME OF THE UPGRADE     :" TO R1-DESC.
           MOVE STARTING-TIME                        TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME OF THE UPGRADE     :" TO R1-DESC.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                            TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *=================================================================
       EXIT-PRINT-RANGES.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - H E A D E R - L I N E S
      *
      ******************************************************************
       WRITE-HEADER-LINES SECTION.

           ADD 01                          TO PAGE-NUMBER.
           MOVE PAGE-NUMBER                TO H1-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO H1-TIME.
           MOVE HEADER-LINE-1              TO PR-REC.
           MOVE 99                         TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEADER-LINE-2              TO PR-REC.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE ALL "-"                    TO PR-REC.
           MOVE 02                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 04                         TO LN-COUNT.
           MOVE 01                         TO LN-FEED.

       EXIT-WRITE-HEADER-LINES.
           EXIT.

      ******************************************************************
      *
      *    S C A N - G R - F I L E
      *
      ******************************************************************
       SCAN-GR-FILE SECTION.

           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF GRSCAN-STATUS NOT = "00"
              MOVE 9 TO IO-FG
              GO TO EXIT-SCAN-GR-FILE.

           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF GR1-KEY.
           PERFORM SCREENIO.

           MOVE "00"       TO VDUSTATUS.
           MOVE ZEROES     TO IO-FG.
           MOVE GRSCAN-KEY TO VDUBUF.

       EXIT-SCAN-GR-FILE.
           EXIT.

      ******************************************************************
      *
      * CREATE DONE TAGS FOR EACH GLOBAL ASCII FILE THAT WAS SELECTED
      *
      *=================================================================
      * NOTE: NEED TO CHECK IF THE 'DONE-PATH-BEG' IN NOT EQUAL
      *       SPACES; IT WILL BE EQUAL SPACES IF A GIVEN FILE'S SOURCE
      *       FILE WAS NOT FOUND;  IF THE SOURCE FILE IS FOUND THEN IT
      *       WILL POPULATE THE 'DONE-PATH-GROUP'; NEED THIS CHECK
      *       BECAUSE IT WILL STOP EXECUTION WITH A FILE IO-ERROR WINDOW
      ******************************************************************
       CREATE-GLOBAL-DONE-TAGS SECTION.

      *    IF ( FILE-OPTION-BY        = "A"    ) AND
      *       ( ABY-DONE-PATH-BEG NOT = SPACES )
      *       PERFORM OPEN-ABY-DONE-FILE
      *       PERFORM CLOSE-ABY-DONE-FILE.

           IF ( FILE-OPTION-GT        = "A"    ) AND
              ( AGT-DONE-PATH-BEG NOT = SPACES )
              PERFORM OPEN-AGT-DONE-FILE
              PERFORM CLOSE-AGT-DONE-FILE.

      ******************************************************************
      *
      * CREATE DONE TAGS FOR EACH LOCAL ASCII FILE THAT WAS SELECTED
      *
      *=================================================================
      * NOTE: NEED TO CHECK IF THE 'DONE-PATH-GROUP' IN NOT EQUAL
      *       SPACES; IT WILL BE EQUAL SPACES IF A GIVEN FILE'S SOURCE
      *       FILE WAS NOT FOUND;  IF THE SOURCE FILE IS FOUND THEN IT
      *       WILL POPULATE THE 'DONE-PATH-GROUP'; NEED THIS CHECK
      *       BECAUSE IT WILL STOP EXECUTION WITH A FILE IO-ERROR WINDOW
      ******************************************************************
       CREATE-DONE-TAGS SECTION.

           IF ( FILE-OPTION-BM        = "A"    ) AND
              ( ABM-DONE-PATH-BEG NOT = SPACES )
              PERFORM OPEN-ABM-DONE-FILE
              PERFORM CLOSE-ABM-DONE-FILE.

           IF ( FILE-OPTION-CJ        = "A"    ) AND
              ( ACJ-DONE-PATH-BEG NOT = SPACES )
              PERFORM OPEN-ACJ-DONE-FILE
              PERFORM CLOSE-ACJ-DONE-FILE.

           IF ( FILE-OPTION-LM        = "A"    ) AND
              ( ALM-DONE-PATH-BEG NOT = SPACES )
              PERFORM OPEN-ALM-DONE-FILE
              PERFORM CLOSE-ALM-DONE-FILE.

       EXIT-CREATE-DONE-TAGS.
           EXIT.

      ****************************************
       TEST-INVALID-DATE SECTION.
      ****************************************
           IF DATE-YYYYMMDD NOT NUMERIC
              MOVE 0     TO DATE-YYYYMMDD
              GO TO EXIT-TEST-INVALID-DATE
           ELSE
           IF DATE-YYYYMMDD = 0 OR 20202020
              MOVE 0     TO DATE-YYYYMMDD
              GO TO EXIT-TEST-INVALID-DATE.

      * CANT FIX BAD YEAR
           IF (DATE-YYYYMMDD-YYYY = 0) AND
               (DATE-YYYYMMDD-MM NOT = 0 OR DATE-YYYYMMDD-DD NOT = 0)
              MOVE 0 TO DATE-YYYYMMDD
              GO TO EXIT-TEST-INVALID-DATE.

      * 20000003  00/03/00 A15

      * 19638133
      * 19750000
      * 19970229

      * CANT FIX A BAD MONTH, SO FORCE IT TO 12 IF ONLY THING IS WRONG
      * DONT CARE IF DAY IS INVALID, IT WILL GET FIXED IN THE NEXT 'IF'
           IF (DATE-YYYYMMDD-YYYY NOT = 0) AND
                (DATE-YYYYMMDD-MM < 1 OR > 12) 
              MOVE 12 TO DATE-YYYYMMDD-MM.

           IF (DATE-YYYYMMDD-DD < 1 OR > 31) OR
              ((DATE-YYYYMMDD-MM = 02) AND (DATE-YYYYMMDD-DD > 28)) OR
              ((DATE-YYYYMMDD-DD > 30) AND
                            (DATE-YYYYMMDD-MM = 4 OR 6 OR 9 OR 11))
                 MOVE 33            TO DATE-YYYYMMDD-DD
                 MOVE DATE-YYYYMMDD TO NDTE-DATE
                 PERFORM NEW-DATE-CALCULATION
                 MOVE NDTE-DATE     TO DATE-YYYYMMDD.

       EXIT-TEST-INVALID-DATE.
           EXIT.

      ******************************************************************
      *
      *    E X T R A C T - R O U T I N E
      *
      ******************************************************************
       EXTRACT-ROUTINE SECTION.

           MOVE MKTEMP-BUF                 TO MSEXCEL-PATH.
           MOVE "DATE35-MSEXCEL-"          TO MSEXCEL-PATH-PREFIX.
           MOVE PROCESS-ID-BUF             TO MSEXCEL-PATH-SUFFIX.
           INSPECT MSEXCEL-PATH-SUFFIX REPLACING ALL SPACES BY "9".
           MOVE ".TXT"                     TO MSEXCEL-PATH-EXT.
           CALL "C$TOLOWER" USING MSEXCEL-PATH-EXT, VALUE 04.
           PERFORM OPEN-MSEXCEL-FILE-OUTPUT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO SORT-FILENAME
                          SORT-FILEPATH
                          SORT-STATE
           MOVE ZEROES TO SORT-BRNO.

           PERFORM CLOSE-SORT-FILE.
           PERFORM OPEN-SORT-FILE.
           PERFORM START-SORT-FILE.

      *=================================================================
       EXTRACT-ROUTINE-READ-NEXT.

           PERFORM READ-SORT-FILE-NEXT.

           IF ( IO-BAD )
              GO TO EXTRACT-ROUTINE-END.

           MOVE SORT-TIME-BEG              TO EDF-SORT-TIME-BEG.
           MOVE SORT-TIME-END              TO EDF-SORT-TIME-END.
           MOVE SORT-TIME-ELAPSED          TO EDF-SORT-TIME-ELAPSED.

           MOVE SPACES                     TO MSEXCEL-REC.

           STRING SORT-FILENAME,         HEX-TAB,
                  SORT-FILEPATH,         HEX-TAB,
                  SORT-STATE,            HEX-TAB,
                  SORT-BRNO,             HEX-TAB,
                  SORT-RECS-READ,        HEX-TAB,
                  SORT-RECS-WRITE,       HEX-TAB,
                  SORT-RECS-SKIP,        HEX-TAB,
                  EDF-SORT-TIME-BEG,     HEX-TAB,
                  EDF-SORT-TIME-END,     HEX-TAB,
                  EDF-SORT-TIME-ELAPSED, HEX-TAB,
                  SORT-RECS-READ-PR,     HEX-TAB,
                  SORT-RECS-WRITE-PR,    HEX-TAB,
                  SORT-RECS-SKIP-PR,     HEX-CARRIAGE-RETURN
                  DELIMITED BY "  " INTO MSEXCEL-REC.

           PERFORM WRITE-MSEXCEL-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO EXTRACT-ROUTINE-READ-NEXT.

      *=================================================================
       EXTRACT-ROUTINE-END.
           PERFORM CLOSE-MSEXCEL-FILE.
           PERFORM CLOSE-SORT-FILE.
           PERFORM REMOVE-SORT-FILE.

       EXTRACT-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - S O R T - M O D U L E
      *
      *=================================================================
      * IN  : HOLD-SORT-KEY
      *       UP1534-RECORD-COUNTER-READ
      *       UP1534-RECORD-COUNTER-WRITE
      *       UP1534-RECORD-COUNTER-SKIP
      *       UP1534-TIME-BEG ***
      *       UP1534-TIME-END ***
      *       UP1534-TIME-ELAPSED ***
      *       *** = SET WITHIN UP1534-CREATE-FILE-TOTALS SECTION
      * OUT : NOTHING
      * DESC: UPDATE THE SORT-FILE.  THE SORT FILE IS USED TO 
      *       CREATE THE MSEXCEL-FILE WHICH CAN BE IMPORTED INTO
      *       MS EXCEL.
      *
      *       THIS CREATES A DETAIL RECORD FOR EACH FILE UPGRADED.
      *       IT ALSO CREATES/UPDATES A TOTAL RECORD FOR ALL
      *       FILES (GLOBAL, ALL MACHINES; LOCAL, ALL BRANCHES).
      *
      *       FOR TIMES, THE INPUT IS IN A STANDARD FORMAT (HH:MM:SS)
      *       BUT BEFORE UPDATED IN THE SORT-REC THEY NEED TO BE
      *       CONVERTED.  THEY NEED TO BE CONVERTED TO SECONDS THEN
      *       DIVIDED BY 86400 (TOTAL OF SECOND FOR 24 HOURS).
      *       FOR EXAMPLE, 12:34:56 IS 45296 SECONDS WHICH DIVIDED BY
      *       86400 SECONDS IS 0.5242592 WHICH WITHIN MS EXCEL YOU CAN
      *       FORMAT THE WHOLE COLUMN TO A CUSTOM FORMAT OF HH:MM:SS.
      ******************************************************************
       UPDATE-SORT-MODULE SECTION.

           CALL "C$TOUPPER" USING HOLD-SORT-STATE, VALUE 02.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ FILE DETAIL }
           MOVE SPACES TO SORT-REC.
           MOVE HOLD-SORT-FILENAME          TO SORT-FILENAME.
           MOVE HOLD-SORT-FILEPATH          TO SORT-FILEPATH.
           MOVE HOLD-SORT-STATE             TO SORT-STATE.
           MOVE HOLD-SORT-BRNO              TO SORT-BRNO.
           MOVE UP1534-RECORD-COUNTER-READ  TO SORT-RECS-READ.
           MOVE UP1534-RECORD-COUNTER-WRITE TO SORT-RECS-WRITE.
           MOVE UP1534-RECORD-COUNTER-SKIP  TO SORT-RECS-SKIP.

           MOVE UP1534-TIME-BEG             TO WSS-MSEXCEL-TIME-INPUT.
           PERFORM MISC-CONVERT-MSEXCEL-TIME.
           MOVE WSS-MSEXCEL-TIME-OUTPUT     TO SORT-TIME-BEG.

           MOVE UP1534-TIME-END             TO WSS-MSEXCEL-TIME-INPUT.
           PERFORM MISC-CONVERT-MSEXCEL-TIME.
           MOVE WSS-MSEXCEL-TIME-OUTPUT     TO SORT-TIME-END.

           MOVE UP1534-TIME-ELAPSED         TO WSS-MSEXCEL-TIME-INPUT.
           PERFORM MISC-CONVERT-MSEXCEL-TIME.
           MOVE WSS-MSEXCEL-TIME-OUTPUT     TO SORT-TIME-ELAPSED.

           MOVE DATE35-RECORD-COUNTER-READ-PR  TO SORT-RECS-READ-PR.
           MOVE DATE35-RECORD-COUNTER-WRITE-PR TO SORT-RECS-WRITE-PR.
           MOVE DATE35-RECORD-COUNTER-SKIP-PR  TO SORT-RECS-SKIP-PR.
           PERFORM WRITE-SORT-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ FILE TOTAL }
           MOVE HOLD-SORT-FILENAME         TO SAVE-SORT-FILENAME.
           MOVE SPACES                     TO SAVE-SORT-FILEPATH.
           MOVE SPACES                     TO SAVE-SORT-STATE.
           MOVE 9999                       TO SAVE-SORT-BRNO.
           MOVE SAVE-SORT-KEY              TO SORT-KEY.
           PERFORM READ-SORT-FILE.

           IF ( IO-BAD )
              PERFORM UPDATE-SORT-FILE-WRITE.

           PERFORM UPDATE-SORT-REWRITE.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ BRANCH TOTAL }
           IF ( HOLD-SORT-BRNO = ZEROES )
              GO TO UPDATE-SORT-GRAND-TOTAL.

           MOVE "~ALL~"                    TO SAVE-SORT-FILENAME.
           MOVE "~BRANCH TOTAL~"           TO SAVE-SORT-FILEPATH.
           MOVE SPACES                     TO SAVE-SORT-STATE.
           MOVE HOLD-SORT-BRNO             TO SAVE-SORT-BRNO.
           MOVE SAVE-SORT-KEY              TO SORT-KEY.
           PERFORM READ-SORT-FILE.

           IF ( IO-BAD )
              PERFORM UPDATE-SORT-FILE-WRITE.

           PERFORM UPDATE-SORT-REWRITE.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ STATE TOTAL }
           IF ( HOLD-SORT-STATE = SPACES )
              GO TO UPDATE-SORT-GRAND-TOTAL.

           MOVE "~~ALL~~"                  TO SAVE-SORT-FILENAME.
           MOVE "~~STATE TOTAL~~"          TO SAVE-SORT-FILEPATH.
           MOVE HOLD-SORT-STATE            TO SAVE-SORT-STATE.
           MOVE ZEROES                     TO SAVE-SORT-BRNO.
           MOVE SAVE-SORT-KEY              TO SORT-KEY.
           PERFORM READ-SORT-FILE.

           IF ( IO-BAD )
              PERFORM UPDATE-SORT-FILE-WRITE.

           PERFORM UPDATE-SORT-REWRITE.

      *--------------------------------------------------{ GRAND TOTAL }
       UPDATE-SORT-GRAND-TOTAL.

           MOVE "~~~ALL~~~"                TO SAVE-SORT-FILENAME.
           MOVE "~~~GRAND TOTAL~~~"        TO SAVE-SORT-FILEPATH.
           MOVE "~~"                       TO SAVE-SORT-STATE.
           MOVE 9999                       TO SAVE-SORT-BRNO.
           MOVE SAVE-SORT-KEY              TO SORT-KEY.
           PERFORM READ-SORT-FILE.

           IF ( IO-BAD )
              PERFORM UPDATE-SORT-FILE-WRITE.

           PERFORM UPDATE-SORT-REWRITE.

      *=================================================================
       UPDATE-SORT-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - S O R T - R E W R I T E
      *
      *=================================================================
      * IN  : SORT-REC
      *       UP1534-RECORD-COUNTER-READ
      *       UP1534-RECORD-COUNTER-WRITE
      *       UP1534-RECORD-COUNTER-SKIP
      *       UP1534-TIME-BEG
      *       UP1534-TIME-END
      *       UP1534-TIME-ELAPSED
      * OUT : SORT-REC
      * DESC: STANDARD ROUTINE TO ADD TO TOTALS THEN REWRITE EXISTING
      *       SORT-REC. 
      ******************************************************************
       UPDATE-SORT-REWRITE SECTION.

           COMPUTE SORT-RECS-READ = SORT-RECS-READ
                                  + UP1534-RECORD-COUNTER-READ.
           COMPUTE SORT-RECS-WRITE = SORT-RECS-WRITE
                                  + UP1534-RECORD-COUNTER-WRITE.
           COMPUTE SORT-RECS-SKIP = SORT-RECS-SKIP
                                  + UP1534-RECORD-COUNTER-SKIP.


      *KC  MOVE UP1534-TIME-BEG             TO WSS-MSEXCEL-TIME-INPUT.
      *KC  PERFORM MISC-CONVERT-MSEXCEL-TIME.
      *KC  COMPUTE SORT-TIME-BEG = SORT-TIME-BEG
      *KC                        + WSS-MSEXCEL-TIME-OUTPUT.

      *KC  MOVE UP1534-TIME-END             TO WSS-MSEXCEL-TIME-INPUT.
      *KC  PERFORM MISC-CONVERT-MSEXCEL-TIME.
      *KC  COMPUTE SORT-TIME-END = SORT-TIME-END
      *KC                        + WSS-MSEXCEL-TIME-OUTPUT.

           MOVE UP1534-TIME-ELAPSED         TO WSS-MSEXCEL-TIME-INPUT.
           PERFORM MISC-CONVERT-MSEXCEL-TIME.
           COMPUTE SORT-TIME-ELAPSED = SORT-TIME-ELAPSED
                                     + WSS-MSEXCEL-TIME-OUTPUT.

           COMPUTE SORT-RECS-READ-PR  = SORT-RECS-READ-PR
                                      + DATE35-RECORD-COUNTER-READ-PR.
           COMPUTE SORT-RECS-WRITE-PR = SORT-RECS-WRITE-PR
                                      + DATE35-RECORD-COUNTER-WRITE-PR.
           COMPUTE SORT-RECS-SKIP-PR  = SORT-RECS-SKIP-PR
                                      + DATE35-RECORD-COUNTER-SKIP-PR.

           PERFORM REWRITE-SORT-FILE.

      *=================================================================
       UPDATE-SORT-REWRITE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - S O R T - W R I T E 
      *
      *=================================================================
      * IN  : SAVE-SORT-KEY
      * OUT : SORT-REC
      * DESC: STANDARD ROUTINE TO CREATE (WRITE) SORT-REC
      ******************************************************************
       UPDATE-SORT-FILE-WRITE SECTION.

           MOVE SPACES                     TO SORT-REC.
           MOVE SAVE-SORT-KEY              TO SORT-KEY.
           MOVE ZEROES                     TO SORT-RECS-READ
                                              SORT-RECS-WRITE
                                              SORT-RECS-SKIP
                                              SORT-TIME-BEG
                                              SORT-TIME-END
                                              SORT-TIME-ELAPSED
                                              SORT-RECS-READ-PR
                                              SORT-RECS-WRITE-PR
                                              SORT-RECS-SKIP-PR.
           PERFORM WRITE-SORT-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

      *=================================================================
       UPDATE-SORT-FILE-WRIT-EXIT.
           EXIT.

      ******************************************************************
       FORM-RESET SECTION.
      ******************************************************************
     
           MOVE EXT-CONAME-SYSDATE         TO DATE35-SYSDATE.
           DISPLAY DATE35-SCREEN        UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DATE35-FORM-FIELDS         TO WR-BUF.
           MOVE DATE35-FORM-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.
     
       FORM-RESET-EXIT.
           EXIT.
     
      ******************************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************************
     
           MOVE DATE35-HELP--FILE          TO HELP-LINK-FILE.
           MOVE DATE35-HELP--DIR           TO HELP-LINK-DIR.
     
       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************************
     
           MOVE STAT---GOOD                TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.
     
           EVALUATE TRUE
              COPY "LIBUP/DATE35_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.
     
       VDU-CONVERT-FIELD-EXIT.
           EXIT.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPTBNO.CPY".             *> NEEDED FOR LIBLP/LPSP1IN
       COPY "LIBUP/UP1534.CPY".
      *COPY "LIBUP/UPGRADE_PD.CPY".         *> UPGRADE-FILE-PD SECTION.

      *===================================[ TARGET FILE CLEAR ROUTINES ]
       COPY "LIBLP/LPBYCL.CPY".
       COPY "LIBUP/UPABYCL.CPY".
       COPY "LIBUP/UPAGTCL.CPY".
       COPY "LIBGL/GLGTCL.CPY".
       COPY "LIBSP/SPCJCL.CPY".
       COPY "LIBUP/UPACJCL.CPY".

      ******************************************************************
      *
      *     M I S C E L L A N E O U S - P A R A G R A P H S  
      *
      ******************************************************************
       MISC-ROUTINES SECTION.
      *=================================================================
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *=================================================================
       CONVERT-MMDDYY-TO-YYYMM.
           MOVE DATE-MMDDYY-MM TO DATE-YYYMM-MM.
           MOVE DATE-MMDDYY-YY TO DATE-YYYMM-YYY.

           IF ( DATE-YYYMM NOT = ZEROES )
              IF ( DATE-YYYMM-YYY < EXT-JULIAN-CC )
                 ADD 100 TO DATE-YYYMM-YYY.

       CONVERT-YYYMMDD-TO-MMDDYY.
           MOVE DATE-YYYMMDD-MM  TO DATE-MMDDYY-MM.
           MOVE DATE-YYYMMDD-DD  TO DATE-MMDDYY-DD.
           MOVE DATE-YYYMMDD-YYY TO DATE-MMDDYY-YY.

       CONVERT-YYYMM-TO-MMDDYY.
           MOVE DATE-YYYMM-MM  TO DATE-MMDDYY-MM.
           MOVE ZEROES         TO DATE-MMDDYY-DD.
           MOVE DATE-YYYMM-YYY TO DATE-MMDDYY-YY.

      *=================================================================
       INVALID-FILE-PATH-MESSAGE.
           MOVE MESSAGE-INVALID-FILE-PATH TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       INVALID-YN-MESSAGE.
           MOVE MESSAGE-INVALID-YN TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       SEND-STARTING-TIME.
           PERFORM GET-TIME.
           MOVE "[ "      TO STARTING-TIME-RIGHT.
           MOVE TIME-EDIT TO STARTING-TIME.
           MOVE " ]"      TO STARTING-TIME-LEFT.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE STARTING-TIME-BUF       TO WR-BUF
              MOVE STARTING-TIME-LIST      TO WR-LIST
              PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND                     TO WR-BUF.
           MOVE LEGEND-LIST                TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-FIL.
           MOVE "FIL"                      TO GETENV-BUF.
           PERFORM GETENV-CALL.

           MOVE SPACES                     TO UP1534-FILE-PATH.
           STRING "/", GETENV-BUF
                  DELIMITED BY "  "      INTO UP1534-FILE-PATH.
           MOVE UP1534-FILE-PATH-MACHINE   TO CURRENT-MACHINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHECK TO SEE IF THE SOURCE & TARGET PATHS ARE SET FROM BATCH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SOURCE-PATH = SPACES )
              MOVE UP1534-FILE-PATH        TO SOURCE-PATH
           ELSE
              MOVE SOURCE-PATH             TO DIR-ACCESS-BUF
              PERFORM DIR-ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE UP1534-FILE-PATH     TO SOURCE-PATH.
           IF ( TARGET-PATH = SPACES )
              MOVE UP1534-FILE-PATH        TO TARGET-PATH
           ELSE
              MOVE TARGET-PATH             TO DIR-ACCESS-BUF
              PERFORM DIR-ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE UP1534-FILE-PATH     TO TARGET-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CURRENT FIL: "            TO FIL-PROMPT.
           MOVE UP1534-FILE-PATH           TO FIL-INFO.

           MOVE FIL-BUF                    TO WR-BUF.
           MOVE FIL-LIST                   TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
      * IN  : WSS-MSEXCEL-TIME-INPUT
      * OUT : WSS-MSEXCEL-TIME-OUTPUT
      * DESC: CONVERT INPUT TIME IN STANDARD FORMAT (HH:MM:SS) TO 
      *       TO MS EXCEL DATE FORMAT. IT NEEDS TO BE CONVERTED TO
      *       SECONDS THEN DIVIDED BY 86400 (TOTAL OF SECOND FOR
      *       24 HOURS).  FOR EXAMPLE, 12:34:56 IS 45296 SECONDS WHICH
      *       DIVIDED BY 86400 SECONDS IS 0.5242592 WHICH WITHIN
      *       MS EXCEL YOU CAN FORMAT THE WHOLE COLUMN TO A
      *       CUSTOM FORMAT OF HH:MM:SS.
      *-----------------------------------------------------------------
       MISC-CONVERT-MSEXCEL-TIME.

           COMPUTE WSS-MSEXCEL-TIME-OUTPUT =
                     (( WSS-MSEXCEL-TIME-INPUT-HH * 3600 )
                   +  ( WSS-MSEXCEL-TIME-INPUT-MM * 60   )
                   +    WSS-MSEXCEL-TIME-INPUT-SS         )
                   / 86400.

      *-----------------------------------------------------------------
      * IN  : SAVE-A15-FILE-PATH-OLD
      *       SAVE-A15-FILE-PATH-NEW
      * OUT : NOTHING
      * DESC: MOVE A15 FILE (OLD) TO AN ANOTHER FILE (NEW; ADD "O" TO
      *       FRONT; OXXFILE) FOR A VARIETY FILE EXTENSION (.D01, .D02,
      *       .VIX, .V01).
      *       DO NOT NEED TO CHECK STAT ON THE MOVE (SYSTEM-CALL) SINCE
      *       THAT FILE EXTENSION MAY NOT EXIST.
      *-----------------------------------------------------------------
       MISC-MV15-OLD-TO-NEW.

           MOVE MV-CMD                     TO SYSTEM-BUF.
           MOVE ".D01"                     TO SAVE-A15-FILE-EXT.
           CALL "C$TOLOWER" USING SAVE-A15-FILE-EXT, VALUE 4.
           STRING SAVE-A15-FILE-PATH-OLD, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE1.
           STRING SAVE-A15-FILE-PATH-NEW, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE2.
           PERFORM SYSTEM-CALL.

           MOVE MV-CMD                     TO SYSTEM-BUF.
           MOVE ".D02"                     TO SAVE-A15-FILE-EXT.
           CALL "C$TOLOWER" USING SAVE-A15-FILE-EXT, VALUE 4.
           STRING SAVE-A15-FILE-PATH-OLD, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE1.
           STRING SAVE-A15-FILE-PATH-NEW, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE2.
           PERFORM SYSTEM-CALL.

           MOVE MV-CMD                     TO SYSTEM-BUF.
           MOVE ".VIX"                     TO SAVE-A15-FILE-EXT.
           CALL "C$TOLOWER" USING SAVE-A15-FILE-EXT, VALUE 4.
           STRING SAVE-A15-FILE-PATH-OLD, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE1.
           STRING SAVE-A15-FILE-PATH-NEW, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE2.
           PERFORM SYSTEM-CALL.

           MOVE MV-CMD                     TO SYSTEM-BUF.
           MOVE ".V01"                     TO SAVE-A15-FILE-EXT.
           CALL "C$TOLOWER" USING SAVE-A15-FILE-EXT, VALUE 4.
           STRING SAVE-A15-FILE-PATH-OLD, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE1.
           STRING SAVE-A15-FILE-PATH-NEW, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE2.
           PERFORM SYSTEM-CALL.

           MOVE MV-CMD                     TO SYSTEM-BUF.
           MOVE ".V02"                     TO SAVE-A15-FILE-EXT.
           CALL "C$TOLOWER" USING SAVE-A15-FILE-EXT, VALUE 4.
           STRING SAVE-A15-FILE-PATH-OLD, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE1.
           STRING SAVE-A15-FILE-PATH-NEW, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE2.
           PERFORM SYSTEM-CALL.

      *-----------------------------------------------------------------
      * IN  : SAVE-A15-FILE-PATH-OLD
      *       SAVE-A15-FILE-PATH-NEW
      * OUT : NOTHING
      * DESC: MOVE NEW A15 FILE ("O" AT THE FRONT; OXXFILE) BACK TO
      *       ORIGINAL FILE (OLD; NO "O" AT FRONT; XXFILE) FOR A VARIETY
      *       FILE EXTENSION (.D01, .D02, .VIX, .V01).
      *       DO NOT NEED TO CHECK STAT ON THE MOVE (SYSTEM-CALL) SINCE
      *       THAT FILE EXTENSION MAY NOT EXIST.
      *-----------------------------------------------------------------
       MISC-MV15-NEW-TO-OLD.

           MOVE MV-CMD                     TO SYSTEM-BUF.
           MOVE ".D01"                     TO SAVE-A15-FILE-EXT.
           CALL "C$TOLOWER" USING SAVE-A15-FILE-EXT, VALUE 4.
           STRING SAVE-A15-FILE-PATH-NEW, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE1.
           STRING SAVE-A15-FILE-PATH-OLD, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE2.
           PERFORM SYSTEM-CALL.

           MOVE MV-CMD                     TO SYSTEM-BUF.
           MOVE ".D02"                     TO SAVE-A15-FILE-EXT.
           CALL "C$TOLOWER" USING SAVE-A15-FILE-EXT, VALUE 4.
           STRING SAVE-A15-FILE-PATH-NEW, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE1.
           STRING SAVE-A15-FILE-PATH-OLD, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE2.
           PERFORM SYSTEM-CALL.

           MOVE MV-CMD                     TO SYSTEM-BUF.
           MOVE ".VIX"                     TO SAVE-A15-FILE-EXT.
           CALL "C$TOLOWER" USING SAVE-A15-FILE-EXT, VALUE 4.
           STRING SAVE-A15-FILE-PATH-NEW, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE1.
           STRING SAVE-A15-FILE-PATH-OLD, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE2.
           PERFORM SYSTEM-CALL.

           MOVE MV-CMD                     TO SYSTEM-BUF.
           MOVE ".V01"                     TO SAVE-A15-FILE-EXT.
           CALL "C$TOLOWER" USING SAVE-A15-FILE-EXT, VALUE 4.
           STRING SAVE-A15-FILE-PATH-NEW, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE1.
           STRING SAVE-A15-FILE-PATH-OLD, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE2.
           PERFORM SYSTEM-CALL.

           MOVE MV-CMD                     TO SYSTEM-BUF.
           MOVE ".V02"                     TO SAVE-A15-FILE-EXT.
           CALL "C$TOLOWER" USING SAVE-A15-FILE-EXT, VALUE 4.
           STRING SAVE-A15-FILE-PATH-NEW, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE1.
           STRING SAVE-A15-FILE-PATH-OLD, SAVE-A15-FILE-EXT
                        DELIMITED BY " " INTO MV-FILE2.
           PERFORM SYSTEM-CALL.

      *=================================================================
      *  IN  : WSS-PROGRAM-DIR-A15
      *        WSS-PROGRAM-NAME-A15
      *  OUT : WSS-PROGRAM-NAME-A30
      *  DESC: CONVERT A15 XX-PROGRAM FIELDS TO A30 XX-PROGRAM FIELDS
      *        IF NEEDED; NO LONGER USING DIRECTORIES IN /USR/A30/CO
      *        WHICH MEANS CANNOT HAVE DUPLICATE PROGRAM NAMES THUS
      *        THIS WILL SET THOSE TO THEIR NEW A30 PROGRAM NAMES.
      *=================================================================
       MISC-VERIFY-PROGRAM-NAME.

           MOVE WSS-PROGRAM-NAME-A15       TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "AP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "ATMAIN" )
              MOVE "APATMN"                TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "AP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "CKLIST" )
              MOVE "APCKLS"                TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "AP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "CKMAIN" )
              MOVE "APCKMN"                TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "AP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "CKSCAN" )
              MOVE "APCKSC"                TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "AP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "CLRCHK" )
              MOVE "APCLCK"                TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "FX"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "PAYSCH" )
              MOVE "PAYSC2"                TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "LP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "CHCRED" )
              MOVE "CHCRE2"                TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "LP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "CHLEGL" )
              MOVE "CHLEG2"                TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "LP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "FOLLOW" )
              MOVE "FOLRPT"                TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "NP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "TRSCAN" )
              MOVE "TRSCN2"                TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "NP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "TRCMAN" )
              MOVE "TRCMN2"                TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "MN"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "TRMNMU" )
              MOVE "MNTRMU"                TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "NP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "LEDGER" )
              MOVE "NPLEDG"                TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "WI"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "GLSCAN" )
              MOVE "GLSCN2"                TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "LP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "BYINQ3" )
              MOVE "BYINQ"                 TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "LP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "DSINQ3" )
              MOVE "DSINQ"                 TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "MN"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "FDDBMN" )
              MOVE "FDBMAN"                 TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "MN"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "FDDBLS" )
              MOVE "FDBLST"                 TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "SP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "1099MI" )
              MOVE "MI1099"                 TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "AP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "CKPR01" )
              MOVE "CKPRNT"                 TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "AP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "CKPR03" )
              MOVE "CKPRNT"                 TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "LP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "1ONPI" )
              MOVE "DISBI0"                 TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "LP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "2ONPI" )
              MOVE "CHCKI0"                 TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "LP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "3ONPI" )
              MOVE "VOIDI0"                 TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "LP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "4ONPI" )
              MOVE "BNKDI0"                 TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "LP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "5ONPI" )
              MOVE "BNKWI0"                 TO WSS-PROGRAM-NAME-A30.

           IF ( WSS-PROGRAM-DIR-A15  = "LP"     ) AND
              ( WSS-PROGRAM-NAME-A15 = "6ONPI" )
              MOVE "BCHKI0"                 TO WSS-PROGRAM-NAME-A30.

      *=================================================================
       MISC-TRACE-END-PROGRAM.

           MOVE ZEROES TO EXT-TCLP-TRACE-LEVEL.
           MOVE SPACES TO EXT-TCLP-TRACE-FORM-PATH
                          EXT-TCLP-TRACE-MESS.

           MOVE "N"    TO EXT-TCLP-TRACE-SKIP-YN.   *> LOG EVENT
           MOVE "N"    TO EXT-TCLP-TRACE.           *> DISABLE TRACE

      *-----------------------------------------------------------------
       MISC-TRACE-FILE-START.

           STRING "+ FILE: ", UP1534-CURRENT-FILE, "[", BR-NO, "]"
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

      *-----------------------------------------------------------------
      *    FORCE UPGRADE PROGRAM TO USE TRACE LOGIC; LOG SETTINGS
      *-----------------------------------------------------------------
       MISC-TRACE-INITIALIZATION.

           MOVE ZEROES TO EXT-TCLP-TRACE-LEVEL.
           MOVE SPACES TO EXT-TCLP-TRACE-FORM-PATH
                          EXT-TCLP-TRACE-MESS.

           MOVE "N"    TO EXT-TCLP-TRACE-SKIP-YN.   *> LOG EVENT
           MOVE "Y"    TO EXT-TCLP-TRACE.           *> ENABLE NON-BRANCH

           MOVE ALL "="                    TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           ACCEPT DATE-YYYYMMDD FROM CENTURY-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMSDDSYY.

           STRING "OTIS SESSION STARTED /", FORM-REL,
                  " | ", DATE-MMSDDSYY
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "DATABASE NAME = ", EXT-ACUSQL-DB-NAME
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "SQL SERVER    = ", EXT-ACUSQL-SERVER-NAME
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "BATCH CODE    = ", EXT-EOBUF-PCODE
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           MOVE EXT-EOBUF-DATE1            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMSDDSYY.
           STRING "BEG DATE      = ", DATE-MMSDDSYY
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           MOVE EXT-EOBUF-DATE2            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMSDDSYY.
           STRING "END DATE      = ", DATE-MMSDDSYY
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "SOURCE PATH   = ", SOURCE-PATH
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "TARGET PATH   = ", TARGET-PATH
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "BEG BRANCH    = ", BEG-BRANCH
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "END BRANCH    = ", END-BRANCH
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "GROUP FLAG    = ", GROUP-FLAG
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "ENT GROUP     = ", ENT-GROUP
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "PRIORITY MEMO = ", PRIORITY-MEMO
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           MOVE BEG-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMSDDSY4.
           STRING "BEG DATE      = ", DATE-MMSDDSYYYY
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           MOVE END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMSDDSY4.
           STRING "END DATE      = ", DATE-MMSDDSYYYY
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
      *COPY "LIBGB/CONNECT_SQL.CPY".  *> HARDCODED FOR ERROR TAG FILE%%%
      * COPYMEMBER: LIBGB/CONNECT_SQL.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBGB/CONNECT_SQL
      * DESCRIPTION   : SQL SERVER CONNECTION ROUTINES
      *
      * NEEDED COPYS  : LIBGB/FILEWK
      *                 LIBGB/DECLARE   -OR-
      *                 LIBGB/DECLAREOD -OR-
      *                 LIBGB/DECLRP
      *
      *=================================================================
      * REV:
      * JKC 2021-0115 ***NEW***
      * JKC 2021-0223 CLEANING UP SOME OF THE CODE.
      * JKC 2021-0324 ADDED SQL-GET-DATE & SQL-SET-DATE.
      * JKC 2021-0430 CHANGES MADE WHILE WORKING ON DECLARE ROUTINES.
      * JKC 2021-0511 CHANGED NAME FROM SQLCONNECT TO CONNECT_SQL
      * JKC 2021-0714 ADDED DISPLAY OF PROGRAM NAME IN SQL-ERROR;
      *               FORM-PATHNAME(16:6)
      * JKC 2020-0726 ADDED EXT-SQL-CONNECT-STAT FOR CHECKING IF 
      *               CONNECTION TO SQL SERVER HAS ALREADY BEEN MADE;
      *               USED TO ONLY CONNECT ONE-TIME INSTEAD OF MULTIPLE
      *               TIMES IN EVERY PROGRAM.
      *
      * JKC 2021-0816 CHANGED SQL-GET-DATE AND SQL-SET-DATE TO CHECK
      *               FOR ZERO DATES & '1900-01-01'.
      *
      * JKC 2021-0817 UPDATED FROM XPC/IOCONN.PRE FOR LATEST CHANGES.
      * JKC 2021-0927 CHANGED SQL-ERROR SECTION TO ALWAYS DISPLAY
      *               E-KEYX NO MATTER WHAT IO TYPE (E-MSG4).
      *
      * JKC 2022-0209 MADE CHANGE TO SET SQL CONNECTION FIELDS AT THE
      *               START OF THE 'SQL-CONNECT' PARAGRAPH FOR POTENTIAL
      *               FUTURE USE OF HAVING MULTIPLE CONNECTIONS.
      *               DID SOME CLEANUP OF UNNEED CODE THAT WAS USED
      *               FOR A TWO-STEP COMPILE PROCESS; WE USE A ONE-STEP
      *               COMPILE GOING FORWARD AT THIS POINT.
      *
      * JKC 2022-0214 MADE CHANGE TO SQL-DISCONNECT TO 
      *               `MOVE STAT---BAD TO EXT-ACUSQL-CONNECT-STAT`
      *               DUE TO ISSUE FOUND WITH CLOSING CURSORS VIA
      *               SQL-ERROR;
      *               IN SQL-IO-VALIDATION ROUTINE ADDED A CHECK TO SEE
      *               IF THE SQL CONNECTION (EXT-ACUSQL-CONNECT-STAT)
      *               IS OPEN (GOOD) THEN `PERFORM CLOSE-FILES`;
      *               NEED TO ONLY CLOSE FILES/TABLES WHEN THERE IS
      *               AN SQL CONNECTION OTHERWISE YOU CAN GET INTO
      *               AN INIFITE LOOP DUE TO THE SQL-DISCONNECT IN
      *               SQL-ERROR WHICH THEN YOU CANNOT CLOSE CURSORS
      *               IF THE CONNECTION HAS BEEN CLOSED.
      *
      * JKC 2022-0622 MOVED THIS FROM LIBIO TO LIBGB; LIBIO WAS REMOVED
      * BAH 2022.0627 RENAMED EXT-ACUSQL-ODBC-NAME TO EXT-ACUSQL-DSN-NAME

      * JKC 2022-0804 REMOVED `SQL-CONNECT-VERIFY' PARAGRAPH; REPLACED
      *          IT WITH `SELECT 1 INTO :QGP-KEY`.
      *
      * JKC 2022-0805 CHANGED :QGP-KEY TO BE :SQL-CONNECT-VERIFY.
      *
      ******************************************************************
      *
      *    S Q L - C O N N E C T
      *
      *=================================================================
      * IN  : EXT-ACUSQL-CONNECT-WORKERS
      * OUT : SQL-CONNECT-WORKERS
      *       IO-FG    [0 = GOOD; 9 = BAD]
      * DESC: MAKE CONNECTION TO THE SQL SERVER:
      *       1. CHECK CONNECTION STATUS
      *          A. BAD THEN MAKE CONNECTION
      *          B. GOOD THEN CONTINUE
      *       2. VERIFY CONNECTION
      *          A. BAD THEN CONTINUE
      *          B. GOOD THEN EXIT PARAGRAPH
      *       3. MAKE CONNECTION
      *       4. VERIFY CONNECTIONSTATUS
      *          A. BAD THEN ERROR PROCEDURE
      *          B. GOOD THEN EXIT PARAGRAPH
      ******************************************************************
       SQL-CONNECT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET THE SQL CONNECTION FIELDS RIGHT AWAY;
      *    IF WE MAKE MULTIPLE CONNNECTIONS FOR EACH/SEPERATE TABLES
      *    THEN THESE SQL CONNECTION FIELDS NEED TO BE SET SO THAT
      *    THEY CAN BE USED FOR EACH IO ROUTINE NEEDED;
      *    SQL CONNECTION FIELDS WERE ONLY BEING POPULATED WHEN
      *    GOING INTO 'SQL-CONNECT-MAKE' WHICH MAY NOT BE DONE IF
      *    THE CONNECT STAT WAS GOOD.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EXT-ACUSQL-DSN-NAME  TO SQL-CONNECT-SERVER.
           MOVE EXT-ACUSQL-LOGIN     TO SQL-CONNECT-USERID.
           MOVE EXT-ACUSQL-PSWD      TO SQL-CONNECT-PSWD.
      *??? MOVE "WAAZA34SQL-DEV'     TO SQL-CONNECT-SERVER
      *??? MOVE "DB_OWNER"    	     TO SQL-CONNECT-OWNER
      *??? MOVE "OTIS"               TO SQL-CONNECT-DATABASE

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-ACUSQL-CONNECT-STAT-BAD )
              PERFORM SQL-CONNECT-MAKE
              IF ( SQLCODE < 0 )
                 PERFORM SQL-CONNECT-ERROR
                 EXIT PARAGRAPH.

           EXEC SQL              *> VERIFY SQL SERVER CONNECTION EXISTS
                SELECT 1 INTO :SQL-CONNECT-VERIFY
           END-EXEC.

           IF ( SQLCODE = 0 )
              PERFORM SQL-CONNECT-WHENEVER
              MOVE STAT---GOOD TO EXT-ACUSQL-CONNECT-STAT
              EXIT PARAGRAPH.

           PERFORM SQL-CONNECT-MAKE.

           IF ( SQLCODE < 0 )
              PERFORM SQL-CONNECT-ERROR
              MOVE STAT---BAD TO EXT-ACUSQL-CONNECT-STAT
              EXIT PARAGRAPH.

           PERFORM SQL-CONNECT-WHENEVER.
           MOVE STAT---GOOD TO EXT-ACUSQL-CONNECT-STAT.

      ******************************************************************
      *
      *    S Q L - C O N N E C T - E R R O R
      *
      *=================================================================
      * IN  : SQLCA WORKERS
      * OUT : NONE
      * DESC: PROCESS ERROR FOR SQL SERVER CONNECTION
      ******************************************************************
       SQL-CONNECT-ERROR.

           MOVE "CONNECTION"            TO E-MSG.
           MOVE "SQL SERVER CONNECTION" TO E-FILE.
           PERFORM SQL-ERROR.

      ******************************************************************
      *
      *    S Q L - C O N N E C T - M A K E
      *
      *=================================================================
      * IN  : SQL-CONNECT-WORKERS
      * OUT : SQLCA WORKERS
      * DESC: MAKE CONNECTION TO THE SQL SERVER
      ******************************************************************
       SQL-CONNECT-MAKE.

           EXEC SQL
                CONNECT TO :SQL-CONNECT-SERVER AS C1
                      USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

      ******************************************************************
      *
      *    S Q L - C O N N E C T - W H E N E V E R
      *
      *=================================================================
      * IN  : NONE
      * OUT : NONE
      * DESC: DECLARE PROCEDURE FOR SQL ERRORS WHEN THEY OCCUR
      *       GPFILE SINCE IT SHOULD BE QUICK WITH ONLY HAVING ONE ROW.
      * NOTE: WHENEVER SQLERROR DOES NOT CATCH ALL ERRORS (LIKE A
      *          CASTING ERROR) SO WE STILL DO THE SQL-IO-VALIDATION
      *          AFTER EACH EXEC SQL IN ALL IO COPY MEMBERS!!!
      ******************************************************************
       SQL-CONNECT-WHENEVER.

           EXEC SQL WHENEVER SQLERROR
              PERFORM SQL-IO-VALIDATION
           END-EXEC.

      ******************************************************************
      *
      *    S Q L - D I S C O N N E C T
      *
      *=================================================================
      * IN  : NOTHING
      * OUT : SQL-CONNECT-WORKERS
      * DESC: MAKE CONNECTION TO THE SQL SERVER
      *=================================================================
      * ***NOTE*** NEED TO HAVE SQL-DISCONNECT IN LIBGB/CONNECT_SQL AND
      *            LIBGB/DISCONNECT_SQL
      ******************************************************************
       SQL-DISCONNECT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO HAVE WHENEVER SQLERROR NOT CALL TO SQL-IO-VALIDATION
      *    FOR CLOSING THE CONNECTION. THE USE IN LIBGB/DISCONNECT_SQL
      *    WILL NOT HAVE SQL-IO-VALIDATION SO NEED TO SHUT IT OFF
      *    (CONTINUE) AND DISCONNECT THEN PUT BACK.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.

           EXEC SQL COMMIT END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.

      *KC  IF ( SQLCODE < 0 )     *> NO NEED TO CHECK SINCE EXITING
      *KC     PERFORM SQL-ERROR.  *> APPLICATION VIA `STOP RUN`

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED THE FOLLOWING FOR LIBGB/CONNECT_SQL
      *    NEED TO COMMENT OUT THE FOLLOWING FOR LIBGB/DISCONNECT_SQL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES     TO SQL-CONNECT-WORKERS.
           MOVE STAT---BAD TO EXT-ACUSQL-CONNECT-STAT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DECLARE PROCEDURE FOR SQL ERRORS WHEN THEY OCCUR
      *    NOTE: WHENEVER SQLERROR DOES NOT CATCH ALL ERRORS (LIKE A
      *          CASTING ERROR) SO WE STILL DO THE SQL-IO-VALIDATION
      *          AFTER EACH EXEC SQL IN ALL IO COPY MEMBERS!!!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           
           EXEC SQL WHENEVER SQLERROR
              PERFORM SQL-IO-VALIDATION
           END-EXEC.

      ******************************************************************
      *
      *    S Q L - D E B U G
      *
      *=================================================================
      * IN  : SQLCA
      * OUT : NOTHING
      * DESC: DISPLAY MESSAGE BOX OF SQLCA SETTINGS FOR DEBUG
      ******************************************************************
       SQL-DEBUG.

           MOVE SPACES                TO SQL-DEBUG-BUF
           MOVE "CODE : "             TO SQL-DEBUG-01-SQLCODEX
           MOVE SQLCODE               TO SQL-DEBUG-01-SQLCODE
           MOVE "CABC : "             TO SQL-DEBUG-01-SQLCABCX
           MOVE SQLCABC               TO SQL-DEBUG-01-SQLCABC
           MOVE "CAID: "              TO SQL-DEBUG-01-SQLCAIDX
           MOVE SQLCAID               TO SQL-DEBUG-01-SQLCAID
           MOVE "STATE: "             TO SQL-DEBUG-02-SQLSTATEX
           MOVE SQLSTATE              TO SQL-DEBUG-02-SQLSTATE
           MOVE "ERRML: "             TO SQL-DEBUG-02-SQLERRMLX
           MOVE SQLERRML              TO SQL-DEBUG-02-SQLERRML
           MOVE "ERRPL: "             TO SQL-DEBUG-02-SQLERRPLX
           MOVE SQLERRPL              TO SQL-DEBUG-02-SQLERRPL
           MOVE "WARN: "              TO SQL-DEBUG-02-SQLWARNX
           MOVE SQLWARN0              TO SQL-DEBUG-02-SQLWARN0
           MOVE SQLWARN1              TO SQL-DEBUG-02-SQLWARN1
           MOVE SQLWARN2              TO SQL-DEBUG-02-SQLWARN2
           MOVE SQLWARN3              TO SQL-DEBUG-02-SQLWARN3
           MOVE SQLWARN4              TO SQL-DEBUG-02-SQLWARN4
           MOVE SQLWARN5              TO SQL-DEBUG-02-SQLWARN5
           MOVE SQLWARN6              TO SQL-DEBUG-02-SQLWARN6
           MOVE SQLWARN7              TO SQL-DEBUG-02-SQLWARN7
           MOVE SQLWARN8              TO SQL-DEBUG-02-SQLWARN8
           MOVE SQLWARN9              TO SQL-DEBUG-02-SQLWARN9
           MOVE SQLWARNA              TO SQL-DEBUG-02-SQLWARNA
           MOVE "ERRD : "             TO SQL-DEBUG-03-SQLERRDX
           MOVE SQLERRD(1)            TO SQL-DEBUG-03-SQLERRD-1
           MOVE SQLERRD(2)            TO SQL-DEBUG-03-SQLERRD-2
           MOVE SQLERRD(3)            TO SQL-DEBUG-03-SQLERRD-3
           MOVE SQLERRD(4)            TO SQL-DEBUG-04-SQLERRD-4
           MOVE SQLERRD(5)            TO SQL-DEBUG-04-SQLERRD-5
           MOVE SQLERRD(6)            TO SQL-DEBUG-04-SQLERRD-6
           STRING "------------------------------",
                  "[ ERRMC ]",
                  "-------------------------------"
                  DELIMITED BY SIZE INTO SQL-DEBUG-05
           MOVE SQLERRMC              TO SQL-DEBUG-06
           STRING "------------------------------",
                  "[ ERRPC ]",
                  "-------------------------------"
                  DELIMITED BY SIZE INTO SQL-DEBUG-07
           MOVE SQLERRPC              TO SQL-DEBUG-08.

           DISPLAY MESSAGE BOX SQL-DEBUG-01, X"0A"
                               SQL-DEBUG-02, X"0A"
                               SQL-DEBUG-03, X"0A"
                               SQL-DEBUG-04, X"0A"
                               SQL-DEBUG-05, X"0A"
                               SQL-DEBUG-06, X"0A"
                               SQL-DEBUG-07, X"0A"
                               SQL-DEBUG-08.

      ******************************************************************
      *
      *    S Q L - E R R O R
      *
      *=================================================================
      * IN  : E-MSG
      *       E-KEY
      *       SQLCA
      *       SQL-CONNECT-WORKERS
      * OUT : IO-FG   [0=GOOD; BAD!=0]
      * DESC: DISPLAY MESSAGE BOX OF SQL SERVER CONNECTION ERROR;
      *       DISCONNECT FROM SERVER
      ******************************************************************
       SQL-ERROR.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           MOVE MKTEMP-BUF                 TO UPERROR-PATH.
           MOVE "DATE35-ERROR-"            TO UPERROR-PATH-PREFIX.
           MOVE PROCESS-ID-BUF             TO UPERROR-PATH-MKTEMP.
           PERFORM OPEN-UPERROR-FILE-OUTPUT.

           PERFORM GET-TIME.
           MOVE TIME-EDIT                            TO R1-FIELD.

           MOVE SPACES                     TO UPERROR-REC.

           STRING "TIME:",               HEX-TAB,
                  TIME-EDIT,             HEX-CARRIAGE-RETURN
                  DELIMITED BY "  " INTO UPERROR-REC.

           PERFORM WRITE-UPERROR-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           MOVE SPACES TO SQL-ERROR-BUF.

           STRING "STOP!!!  ",
                  "CALL HOME OFFICE FOR IT SUPPORT IMMEDIATELY!!!"
                  DELIMITED BY SIZE INTO SQL-ERROR-01.

           STRING "STOP!!!  ",
                  "AN ERROR HAS OCCURED --- DO NOT PROCEED!!!"
                  DELIMITED BY SIZE INTO SQL-ERROR-02.

           STRING E-MSG, " --- FILE: ", E-FILE,
                  " --- PROGRAM: ", E-PROG
                  DELIMITED BY "  " INTO SQL-ERROR-04.
           MOVE SQL-ERROR-04 TO UPERROR-MESS.
           PERFORM SQL-ERROR-UP1534-CAT.

           MOVE "CODE: "  TO SQL-ERROR-05-SQLCODEX.
           MOVE SQLCODE   TO SQL-ERROR-05-SQLCODE.
           MOVE "STATE: " TO SQL-ERROR-05-SQLSTATEX.
           MOVE SQLSTATE  TO SQL-ERROR-05-SQLSTATE.
           MOVE "ERRML: " TO SQL-ERROR-05-SQLERRMLX.
           MOVE SQLERRML  TO SQL-ERROR-05-SQLERRML.
           MOVE "ERRPC: " TO SQL-ERROR-05-SQLERRPLX.
           MOVE SQLERRPL  TO SQL-ERROR-05-SQLERRPL.
           MOVE SQL-ERROR-05 TO UPERROR-MESS.
           PERFORM SQL-ERROR-UP1534-CAT.

           MOVE "KEY : "  TO SQL-ERROR-06-KEYX.
           MOVE E-KEYX    TO SQL-ERROR-06-KEY.
           MOVE SQL-ERROR-06 TO UPERROR-MESS.
           PERFORM SQL-ERROR-UP1534-CAT.

           MOVE SQLERRMC  TO SQL-ERROR-07.
           MOVE SQL-ERROR-07 TO UPERROR-MESS.
           PERFORM SQL-ERROR-UP1534-CAT.

           MOVE SQLERRPC  TO SQL-ERROR-08.
           MOVE SQL-ERROR-08 TO UPERROR-MESS.
           PERFORM SQL-ERROR-UP1534-CAT.

           DISPLAY MESSAGE BOX SQL-ERROR-01, X"0A"
                               SQL-ERROR-02, X"0A"
                               SQL-ERROR-03, X"0A"
                               SQL-ERROR-04, X"0A"
                               SQL-ERROR-05, X"0A"
                               SQL-ERROR-06, X"0A"
                               SQL-ERROR-07, X"0A"
                               SQL-ERROR-08.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  NOTE: WE ARE DOING A SQL-DISCONNECT AT THE END OF SQL-ERROR
      *        TO FORCE PROGRAMS TO STOP EXECUTING.  FOR EXAMPLE,
      *        LP/ACRERN CALLS TO LP/AC1ERN WHICH IF AC1ERN ERRORS
      *        WE WANT WHEN RETURNING BACK TO ACRERN TO HAVE THE
      *        CONNECTION CLOSED ("THE CONNECTION DOES NOT EXIST") THUS
      *        CAUSING AN ERROR ON ANY IO OPERATIONS (BLOCK UPDATES;
      *        WRITE/REWRITE).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM SQL-DISCONNECT.
           MOVE 9 TO IO-FG.          

      *-----------------------------------------------------------------
       SQL-ERROR-UP1534-CAT.

           MOVE SPACES                     TO UPERROR-REC.

           STRING UPERROR-MESS,          HEX-CARRIAGE-RETURN
                  DELIMITED BY "  " INTO UPERROR-REC.

           PERFORM WRITE-UPERROR-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

      ******************************************************************
      *
      *    S Q L - D A T E - D E C R E M E N T
      *
      *=================================================================
      * IN  : SQL-DATE-YYYYMMDD
      * OUT : SQL-DATE-YYYY-MM-DD
      * DESC: TAKE INPUT DATE THEN DECREMENT IT BY ONE DAY THEN
      *       FORMAT IT THEN RETURN IT BACK;
      *       TO GET A START NOT GREATER THAN TO WORK IN SQL LIKE IT
      *       DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO
      *       SUBTRACT ONE TO THE WHERE SCAN BEG (WSBEG).
      ******************************************************************
       SQL-DATE-DECREMENT.
           MOVE SQL-DATE-YYYYMMDD          TO NDTE-DATE.
           MOVE -1                         TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE                  TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.

      ******************************************************************
      *
      *    S Q L - D A T E - I N C R E M E N T
      *
      *=================================================================
      * IN  : SQL-DATE-YYYYMMDD
      * OUT : SQL-DATE-YYYY-MM-DD
      * DESC: TAKE INPUT DATE THEN INCREMENT IT BY ONE DAY THEN
      *       FORMAT IT THEN RETURN IT BACK;
      *       TO GET A START TO WORK IN SQL LIKE IT DOES IN VISION FILES
      *       FOR MULTIPLE SCAN PAGES, NEED TO ADD ONE TO THE
      *       WHERE SCAN BEG (WSBEG).
      ******************************************************************
       SQL-DATE-INCREMENT.
           MOVE SQL-DATE-YYYYMMDD          TO NDTE-DATE.
           MOVE 1                          TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE                  TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.

      ******************************************************************
      *
      *    S Q L - G E T - D A T E
      *
      *=================================================================
      * IN  : SQL-DATE-YYYY-MM-DD
      * OUT : SQL-DATE-YYYYMMDD
      * DESC: CONVERT DATE FROM YYYY-MM-DD TO YYYYMMDD FORMAT;
      *       NEEDED FOR 'GET' FIELDS ROUTINES.
      ******************************************************************
       SQL-GET-DATE.

           IF ( SQL-DATE-YYYY-MM-DD = "1900-01-01" )
              MOVE ZEROES TO SQL-DATE-YYYYMMDD
           ELSE
              INSPECT SQL-DATE-YYYY-MM-DD REPLACING ALL SPACES BY ZEROES
              MOVE SQL-DATE-YYYY-MM-DD-YYYY TO SQL-DATE-YYYYMMDD-YYYY
              MOVE SQL-DATE-YYYY-MM-DD-MM   TO SQL-DATE-YYYYMMDD-MM
              MOVE SQL-DATE-YYYY-MM-DD-DD   TO SQL-DATE-YYYYMMDD-DD.

      ******************************************************************
      *
      *    S Q L - S E T - D A T E
      *
      *=================================================================
      * IN  : SQL-DATE-YYYYMMDD
      * OUT : SQL-DATE-YYYY-MM-DD
      * DESC: CONVERT DATE FROM YYYYMMDD TO YYYY-MM-DD FORMAT;
      *       NEEDED FOR 'SET' FIELDS ROUTINES.
      ******************************************************************
       SQL-SET-DATE.

           IF ( SQL-DATE-YYYYMMDD = ZEROES )
              MOVE "1900-01-01" TO SQL-DATE-YYYY-MM-DD
           ELSE
              MOVE SQL-DATE-YYYYMMDD TO SQL-DATE-YYYY-MM-DD-NUM
              INSPECT SQL-DATE-YYYY-MM-DD-NUM REPLACING ALL "/" BY "-".

      ******************************************************************
      *
      *    S Q L - T E X T - D E C R E M E N T
      *
      *=================================================================
      * IN  : SQL-TEXT
      * OUT : SQL-TEXT
      * DESC: USED TO DECREMENT THE LAST CHARACTER OF A STRING SO THAT
      *       A START-NOT-GREATER PARAGRAPH (DECLARE CURSOR) WORKS LIKE
      *       A VISION FILE IN A MULTI-PAGE SCAN WINDOW;
      *       FIND THE LAST CHARACTER IN A TEXT STRING THEN DECREMENT
      *       IT BY ONE.  USING SQL-TEXT-ASCII WORKERS, WORK BACKWARDS:
      *
      *         " "     BECOMES " " (REMAINS SPACE; FIRST CHARACTER)
      *         "!"     BECOMES " "
      *         "1"     BECOMES "0"
      *         "B"     BECOMES "A" (UPPER B AND A)
      *         "~"     BECOMES "}"
      *
      *         "   "   BECOMES "   " (REMAINS SPACES; NO WHERE TO GO)
      *         "!  "   BECOMES "   "
      *         " ! "   BECOMES "   "
      *         "012"   BECOMES "011"
      *         "ABC"   BECOMES "ABB" (UPPER)
      *         "~~ "   BECOMES "~} "
      *         "~~!"   BECOMES "~~ "
      *         "~~~"   BECOMES "~~}"
      *
      ******************************************************************
       SQL-TEXT-DECREMENT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF TEXT FIELD IS SPACES, THEN KEEP IT SPACES (NO WHERE TO GO)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT = SPACES )
              MOVE ZEROES TO SQL-TEXT-SIZE
              EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FIND THE LAST CHARACTER IN THE SIZE OF THE TEXT STRING BY
      *    SEARCHING FOR THE LAST CHARACTER THAT IS NOT A SPACE THEN
      *    USE THAT SUBSCRIPT TO SET SQL-TEXT-SIZE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES TO SQL-TEXT-SIZE.

           PERFORM VARYING SQL-TEXT-S1 FROM SQL-TEXT-MAX BY -1
                        UNTIL SQL-TEXT-S1 <= ZEROES

                   IF ( SQL-TEXTX(SQL-TEXT-S1) NOT = SPACES )
                      MOVE SQL-TEXT-S1 TO SQL-TEXT-SIZE
                      MOVE ZEROES      TO SQL-TEXT-S1
                   END-IF

           END-PERFORM.

           IF ( SQL-TEXT-SIZE = ZEROES )
              EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHANGE LAST CHARACTER FOUND BACKWARDS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SQL-TEXTX(SQL-TEXT-SIZE) TO SQL-TEXT-CURRENT.
           PERFORM SQL-TEXT-FIND-ASCII.
           MOVE SQL-TEXT-BEFORE          TO SQL-TEXTX(SQL-TEXT-SIZE).

      ******************************************************************
      *
      *    S Q L - T E X T - I N C R E M E N T
      *
      *=================================================================
      * IN  : SQL-TEXT
      *       SQL-TEXT-SIZE [SIZE OF THE COBOL FIELD SIZE = PIC X(##)]
      * OUT : SQL-TEXT
      * DESC: USED TO INCREMENT TEXT STRING.  NEED THE SIZE OF THE COBOL
      *       FIELD TO KNOW WHICH CHARACTER NEEDS TO BE INCREMENTED TO.
      *
      *       IF THE LAST CHARACTER (SQL-TEXT-SIZE) IS NOT SPACE, THEN
      *       INCREMENT THAT CHARACTER:
      *
      *         " "     BECOMES "!"
      *         "/"     BECOMES "0"
      *         "9"     BECOMES ":"
      *         "@"     BECOMES "A" (UPPER A)
      *         "Z"     BECOMES "[" (UPPER Z)
      *         "`"     BECOMES "A" (LOWER A)
      *         "Z"     BECOMES "{" (LOWER Z)
      *         "~"     BECOMES "~" (~ REMAINS ~; LAST CHARACTER)
      *
      *         "AB!"   BECOMES "AB"" (UPPER AB)
      *         "AB/"   BECOMES "AB0" (UPPER AB)
      *         "AB9"   BECOMES "AB:" (UPPER AB)
      *         "AB@"   BECOMES "ABA" (UPPER ABA)
      *         "ABZ"   BECOMES "AB[" (UPPER ABZ)
      *         "AB`"   BECOMES "ABA" (UPPER AB LOWER A)
      *         "ABZ"   BECOMES "AB{" (UPPER AB LOWER Z)
      *         "AB~"   BECOMES "AC " (UPPER AB~ || UPPER AC)
      *         "AZZ"   BECOMES "AZ[" (UPPER AZZ)
      *         "AZ~"   BECOMES "A[ " (UPPER AZ~)
      *         "A~~"   BECOMES "B  " (UPPER A~~)
      *         "~~~"   REMAINS "~~~" (NO WHERE TO GO TO)
      *
      *         "AB!  " BECOMES "AB!! " (UPPER AB)
      *         "AB/  " BECOMES "AB/! " (UPPER AB)
      *         "AB9  " BECOMES "AB9! " (UPPER AB)
      *         "AB@  " BECOMES "AB@! " (UPPER AB)
      *         "ABZ  " BECOMES "ABZ! " (UPPER ABZ)
      *         "AB`  " BECOMES "AB`! " (UPPER AB)
      *         "AB~  " BECOMES "AB~! " (UPPER AB)
      *         "~~~  " BECOMES "~~~! "
      *
      *         "A !"   BECOMES "A "" (UPPER A)
      *         "A /"   BECOMES "A 0" (UPPER A)
      *         "A 9"   BECOMES "A :" (UPPER A)
      *         "A @"   BECOMES "A A" (UPPER A A)
      *         "A Z"   BECOMES "A [" (UPPER ABZ)
      *         "A `"   BECOMES "A A" (UPPER A SPACE LOWER A)
      *         "A ~"   BECOMES "A! " (UPPER A)
      *         "~  "   BECOMES "~! " 
      *         "~~ "   BECOMES "~~!" 
      *
      ******************************************************************
       SQL-TEXT-INCREMENT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF TEXT FIELD IS SPACES, THEN MAKE IT A LEADING ! ("!     ")
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT = SPACES )
              MOVE SQL-TEXT-ASCIIX(2) TO SQL-TEXT        *> ! CHARACTER
              MOVE ZEROES             TO SQL-TEXT-SIZE
              EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF SQL-TEXT-SIZE IS NOT SET THEN FIND THE SIZE OF THE TEXT
      *    STRING BY SEARCHING FOR THE LAST CHARACTER THAT IS NOT A
      *    SPACE THEN USE THAT SUBSCRIPT TO SET SQL-TEXT-SIZE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT-SIZE NOT NUMERIC ) OR
              ( SQL-TEXT-SIZE = ZEROES    )
              MOVE 01 TO SQL-TEXT-SIZE
              PERFORM VARYING SQL-TEXT-S1 FROM SQL-TEXT-MAX BY -1
                        UNTIL SQL-TEXT-S1 <= ZEROES

                      IF ( SQL-TEXTX(SQL-TEXT-S1) NOT = SPACES )
                         MOVE SQL-TEXT-S1 TO SQL-TEXT-SIZE
                         MOVE ZEROES      TO SQL-TEXT-S1
                      END-IF

              END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF TEXT SIZE = 1, INCREMENT EXECPT IF A `~`
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT-SIZE = 1 )
              IF ( SQL-TEXTX(1) = "~" )
                 MOVE ZEROES         TO SQL-TEXT-SIZE
                 EXIT PARAGRAPH
              ELSE
                 MOVE SQL-TEXTX(1)   TO SQL-TEXT-CURRENT
                 PERFORM SQL-TEXT-FIND-ASCII
                 MOVE SQL-TEXT-AFTER TO SQL-TEXTX(1)
                 MOVE ZEROES         TO SQL-TEXT-SIZE
                 EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FIND LAST CHARACTER THAT IS NOT A SPACE FROM THE SIZE OF
      *    STRING; NEED TO KNOW IF THE LAST CHARACTER IS ALSO THE
      *    LAST IN THE SIZE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SQL-TEXT-SIZE TO SQL-TEXT-LAST.

           PERFORM VARYING SQL-TEXT-S1 FROM SQL-TEXT-SIZE BY -1
                     UNTIL SQL-TEXT-S1 <= 0

                   IF ( SQL-TEXTX(SQL-TEXT-S1) NOT = SPACES )
                      MOVE SQL-TEXT-S1 TO SQL-TEXT-LAST
                      MOVE ZEROES      TO SQL-TEXT-S1
                   END-IF

           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF LAST CHARACTER (NON-SPACE; SQL-TEXT-LAST) IS ALSO THE
      *    LAST POSITON OF THE STRING (SQL-TEXT-SIZE), THEN INCREMENT
      *    THAT POSITION EXCEPT IF A `~`
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT-LAST                = SQL-TEXT-SIZE ) AND
              ( SQL-TEXTX(SQL-TEXT-LAST) NOT = "~"           )
              MOVE SQL-TEXTX(SQL-TEXT-LAST) TO SQL-TEXT-CURRENT
              PERFORM SQL-TEXT-FIND-ASCII
              MOVE SQL-TEXT-AFTER TO SQL-TEXTX(SQL-TEXT-LAST)
              MOVE ZEROES         TO SQL-TEXT-SIZE
              EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF LAST CHARACTER (NON-SPACE; SQL-TEXT-LAST) IS ***NOT*** THE
      *    LAST POSITON OF THE STRING (SQL-TEXT-SIZE), THEN:
      *     - IF CHARACTER IS NOT `~`, THEN INCREMENT THAT POSITION
      *     - IF CHARACTER IS `~`, THEN INCREMENT THE NEXT POSITION
      *       (SHOULD BE A SPACE; BECOMES A "!")
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT-LAST NOT = SQL-TEXT-SIZE )
              IF ( SQL-TEXTX(SQL-TEXT-LAST) NOT = "~" )
                 MOVE SQL-TEXTX(SQL-TEXT-LAST) TO SQL-TEXT-CURRENT
                 PERFORM SQL-TEXT-FIND-ASCII
                 MOVE SQL-TEXT-AFTER TO SQL-TEXTX(SQL-TEXT-LAST)
              ELSE 
                 MOVE SQL-TEXT-ASCIIX(2) TO SQL-TEXTX(SQL-TEXT-LAST + 1)
                 MOVE ZEROES             TO SQL-TEXT-SIZE
                 EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LAST CHARACTER (SQL-TEXT-LAST) IS EQUAL TO LAST POSITION OF
      *    THE LAST POSITION OF THE STRING (SQL-TEXT-SIZE) AND IS EQUAL
      *    TO A `~`.  NEED TO MAKE THAT POSITION A SPACE, THEN WORK
      *    BACKWARDS AND INCREMENT THAT POSITION UNLESS IT ALSO IS A
      *    `~` WHICH IF SO MAKE IT ALSO A SPACE AND REPEAT BACKWARDS
      *    TILL NON ~ CHARACTER FOUND:
      *
      *         "AZ~"   BECOMES "A[ " (UPPER AZ~)
      *         "A~~"   BECOMES "B  " (UPPER A~~)
      *         "A ~"   BECOMES "A! " (UPPER A)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SQL-TEXT-S1 FROM SQL-TEXT-LAST BY -1
                     UNTIL SQL-TEXT-S1 <= 0

                   IF ( SQL-TEXTX(SQL-TEXT-S1) = "~" )
                      MOVE SPACES TO SQL-TEXTX(SQL-TEXT-S1)
                   ELSE
                      MOVE SQL-TEXTX(SQL-TEXT-S1) TO SQL-TEXT-CURRENT
                      PERFORM SQL-TEXT-FIND-ASCII
                      MOVE SQL-TEXT-AFTER TO SQL-TEXTX(SQL-TEXT-S1)
                      MOVE ZEROES         TO SQL-TEXT-S1
                   END-IF

           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF SQL-TEXT EQUAL SPACES, THEN FILL IT WITH ALL `~`;
      *    THE PREVIOUS `PERFORM VARYING` CLEARED (SPACES) TO SQL-TEXT
      *    SO NEED TO POPULATE IT WITH `~` SINCE THERE NEEDS TO GET TO
      *    MAX VALUE TO SIGNAL THERE WAS NO WHERE TO GO:
      *
      *         "~~~"   REMAINS "~~~" (NO WHERE TO GO TO)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT = SPACES )
              MOVE ALL "~" TO SQL-TEXT.

      ******************************************************************
      *
      *    S Q L - T E X T - F I N D - A S C I I
      *
      *=================================================================
      * IN  : SQL-TEXT-CURRENT
      * OUT : SQL-TEXT-AFTER
      *       SQL-TEXT-BEFORE
      * DESC: FIND THE CURRENT CHARACTER IN SQL-TEXT-ASCII THEN RETURN
      *       BACK THE CHARACTER BEFORE & AFTER THE CURRENT CHARACTER.
      ******************************************************************
       SQL-TEXT-FIND-ASCII.

           MOVE ZEROES TO SQL-TEXT-S4.

           PERFORM VARYING SQL-TEXT-S3 FROM 1 BY 1
                     UNTIL SQL-TEXT-S3 > SQL-TEXT-ASCII-MAX

                  IF ( SQL-TEXT-ASCIIX(SQL-TEXT-S3) = SQL-TEXT-CURRENT )
                     MOVE SQL-TEXT-S3        TO SQL-TEXT-S4
                     MOVE SQL-TEXT-ASCII-MAX TO SQL-TEXT-S3
                  END-IF

           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT-S4 = ZEROES )
              MOVE SPACES TO SQL-TEXT-AFTER
                             SQL-TEXT-BEFORE
              EXIT PARAGRAPH.

           IF ( SQL-TEXT-S4 = 1 )
              MOVE SQL-TEXT-ASCIIX(2)               TO SQL-TEXT-AFTER
              MOVE SPACES                           TO SQL-TEXT-BEFORE
           ELSE
           IF ( SQL-TEXT-S4 = SQL-TEXT-ASCII-MAX )
              MOVE SPACES                           TO SQL-TEXT-AFTER
              MOVE SQL-TEXT-ASCIIX(SQL-TEXT-S4 - 1) TO SQL-TEXT-BEFORE
           ELSE
              MOVE SQL-TEXT-ASCIIX(SQL-TEXT-S4 + 1) TO SQL-TEXT-AFTER
              MOVE SQL-TEXT-ASCIIX(SQL-TEXT-S4 - 1) TO SQL-TEXT-BEFORE.


      *=================================================================
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".

       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       COPY "LIBSP/SPIOSORT.CPY".

      * FILES FOR SQL ASCII
      *COPY "LIBUP/UPABYI.CPY".
       COPY "LIBUP/UPABMI.CPY".
       COPY "LIBUP/UPACJI.CPY".
       COPY "LIBUP/UPAGTI.CPY".
       COPY "LIBUP/UPALMI.CPY".

       COPY "LIBUP/UP1534_IO.CPY".
       COPY "LIBUP/UP1534_SPIOREPORT.CPY".

      *=================================================================
      * COPYMEMBER: LIBGB/GBBRRN
      *=================================================================
       OPEN-OBR-FILE.
           PERFORM OPEN-IT.
           MOVE OBR-PATH TO E-FILE.
           OPEN INPUT OBR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBR-FILE.
           IF IO-FG = 7
              CLOSE OBR-FILE
              GO TO OPEN-OBR-FILE.

      *-----------------------------------------------------------------
       READ-OBR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBR-PATH TO E-FILE.
           MOVE OBR-KEY TO E-KEYX.
           READ OBR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBR-FILE-NEXT.

      *-----------------------------------------------------------------
       START-OBR-FILE.
           PERFORM START-IT.
           MOVE OBR-PATH TO E-FILE.
           MOVE OBR-KEY TO E-KEYX.
           START OBR-FILE KEY NOT < OBR-KEY.

      *-----------------------------------------------------------------
       CLOSE-OBR-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBR-FILE.

      *=================================================================
       OPEN-MSEXCEL-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE MSEXCEL-PATH TO E-FILE.
           OPEN OUTPUT MSEXCEL-FILE.

           IF ( IO-FG = 8 )
              GO TO OPEN-MSEXCEL-FILE-OUTPUT.

           IF ( IO-FG = 7 )
              CLOSE MSEXCEL-FILE
              GO TO OPEN-MSEXCEL-FILE-OUTPUT.

      *-----------------------------------------------------------------
       READ-MSEXCEL-FILE.
           PERFORM READ-IT.
           MOVE MSEXCEL-PATH TO E-FILE.
           READ MSEXCEL-FILE.

           IF ( IO-FG = 8 )
              GO TO READ-MSEXCEL-FILE.

      *-----------------------------------------------------------------
       WRITE-MSEXCEL-FILE.
           PERFORM WRITE-IT.
           MOVE MSEXCEL-PATH TO E-FILE.
           WRITE MSEXCEL-REC.

           IF ( IO-FG = 8 )
              GO TO WRITE-MSEXCEL-FILE.

      *-----------------------------------------------------------------
       CLOSE-MSEXCEL-FILE.
           PERFORM CLOSE-IT.
           CLOSE MSEXCEL-FILE.

      *=================================================================
       OPEN-UPERROR-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE UPERROR-PATH TO E-FILE.
           OPEN OUTPUT UPERROR-FILE.

           IF ( IO-FG = 8 )
              GO TO OPEN-UPERROR-FILE-OUTPUT.

           IF ( IO-FG = 7 )
              CLOSE UPERROR-FILE
              GO TO OPEN-UPERROR-FILE-OUTPUT.

      *-----------------------------------------------------------------
       READ-UPERROR-FILE.
           PERFORM READ-IT.
           MOVE UPERROR-PATH TO E-FILE.
           READ UPERROR-FILE.

           IF ( IO-FG = 8 )
              GO TO READ-UPERROR-FILE.

      *-----------------------------------------------------------------
       WRITE-UPERROR-FILE.
           PERFORM WRITE-IT.
           MOVE UPERROR-PATH TO E-FILE.
           WRITE UPERROR-REC.

           IF ( IO-FG = 8 )
              GO TO WRITE-UPERROR-FILE.

      *-----------------------------------------------------------------
       CLOSE-UPERROR-FILE.
           PERFORM CLOSE-IT.
           CLOSE UPERROR-FILE.

      *===========================================[ NOTE: MUST BE LAST ]
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
 
      ******************************************************************
      *
      *    E N D - P R O G R A M 
      *
      ******************************************************************
       END-PROGRAM SECTION.

           PERFORM CLOSE-FILES.
           PERFORM MISC-TRACE-END-PROGRAM.             *> DISABLE TRACE

           MOVE SORT-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF EXIT-PRIOR-TO-EXECUTION
              GO TO EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVING REPORT FILE IN /USR/TMP TO /USR/TMP/UP1534.MKTEMP
      *    SAVING REPORT FILE IN CASE OF ANY OF THE UPGRADE PROGRAMS
      *    BOMB OUT SO YOU CAN SEE WHICH FILE WAS PROCESSED LAST.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "MV "                      TO SAVE-REPORT-CMD.
      *    CALL "C$TOLOWER" USING SAVE-REPORT-CMD, VALUE 03.

      *    MOVE UP1534-REPORT-PATH         TO MKTEMP-BUF
      *                                       SAVE-REPORT-SOURCE
      *                                       SAVE-REPORT-TARGET.

      *    MOVE "UP1534."                  TO SAVE-REPORT-TARGET-NEW.
      *    CALL "C$TOLOWER" USING SAVE-REPORT-TARGET-NEW, VALUE 07.

      *    MOVE MKTEMP-NAME                TO SAVE-REPORT-TARGET-MKTEMP.

      *    MOVE SAVE-REPORT-COMMAND        TO SYSTEM-BUF.
      *    PERFORM SYSTEM-CALL.

      *-----------------------------------------------------------------
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
              MOVE EXIT-PATH               TO FORM-NAM
              MOVE FORM-PATH               TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              PERFORM FORM-RESET
              DESTROY DATE35-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
 
