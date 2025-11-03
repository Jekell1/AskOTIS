       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PCPG01.
       AUTHOR.        KEC.
       DATE-WRITTEN.  06-30-2006.
      ******************************************************************
      *
      * IDENTIFICATION:  MN/PCPG01.C
      *
      * DESCRIPTION   :  SCRATCH PAD COLLATERAL FILE (PC) MAINTENANCE
      *                  --- PAGE ONE
      *
      *=================================================================
      * REV:
      * KEC 2006.0630 {PR#00127} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      * MJD 131203 A30 CONVERSION COMPLETED
      * KEC 2017.0324 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED COMP-3 FROM PC1-KEY.
      *  CS 170503 ACTIVATE 8 DIGIT DATES
      * BAH 190114 GLOBAL PCFILE
      *  CS 200130 FIXED SCANKEY 
      * BAH 20200617 CHANGED PC-LT-RECORD TO PC-LTA-RECORD AND PC-LTC-RECORD
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *=================================================================
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/PCPG01.CBL S35 02/21/24-12:36:07 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01PC.CPY".
       COPY "LIBLP/LP01PC_SQL.CPY".
       COPY "LIBLP/LPWSPC.CPY".
       COPY "LIBLP/LP01WK.CPY".

      *=================================================================

      *-----------------------------------------------------------------
       01  HOLD-PC1-KEY.
           03  HOLD-PC-ID              PIC X(01).
           03  HOLD-PC-BRANCH          PIC 9(04).
           03  HOLD-PC-SOC-SEC         PIC 9(10).

       01  WORK-PC1-KEY.
           03  WORK-PC-ID              PIC X(01).
           03  WORK-PC-SOC-SEC         PIC 9(10).


      *-----------------------------------------------------------------
       01  RECORD-SIZES-WORKERS.
           03  ORIGINAL-PC-REC         PIC X(PC-SIZE).
           03  CHANGED-PC-REC          PIC X(PC-SIZE).

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************

      ******************************************************************
      *
      *    D I S P L A Y - W O R K E R S
      *
      ******************************************************************
       01  DISPLAY-BUF.

           03  F01                     PIC 99/99/99.
           03  F02                     PIC ZZ9.
           03  F03                     PIC ZZZ,ZZ9.99.
           03  F04                     PIC X(01).
           03  F05                     PIC 9(04).
           03  F06                     PIC X(33).

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER PIC X(20) VALUE "010 020 030 040 050 ".
           03  FILLER PIC X(04) VALUE "060.".

      *=================================================================
       78  MAX---DISPLAY-INFO                             VALUE 13.

      *-----------------------------------------------------------------
       01  DISPLAY-INFO-BUF            PIC X(1040)        VALUE SPACES.
       01  FILLER                      REDEFINES DISPLAY-INFO-BUF.
           03  DISPLAY-INFO-ELE        OCCURS MAX---DISPLAY-INFO
                                       PIC X(80).

      *-----------------------------------------------------------------
       01  DISPLAY-INFO-LIST.
           03  FILLER PIC X(20) VALUE "D01 D02 D03 D04 D05 ".
           03  FILLER PIC X(20) VALUE "D06 D07 D08 D09 D10 ".
           03  FILLER PIC X(12) VALUE "D11 D12 D13.".

      *=================================================================
       01  DISPLAY-KEY-BUF.
           03  DISPLAY-KEY-ID          PIC X(01)          VALUE SPACES.
           03  DISPLAY-KEY-SSNO        PIC 9(10)          VALUE ZEROES.

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-LIST.
           03  FILLER PIC X(07) VALUE "KEY-ID ".
           03  FILLER PIC X(09) VALUE "KEY-SSNO.".

      *=================================================================
       01  DETAIL-LINE                 PIC X(80)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01-A            REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(04).
           03  D01-A-1-PROMPT          PIC X(14).
           03  D01-A-1-TEXT            PIC X(25).
           03  D01-A-1-AMT             REDEFINES D01-A-1-TEXT
                                       PIC ZZZ,ZZ9.99.
           03  D01-A-1-DATE            REDEFINES D01-A-1-TEXT
                                       PIC 99/99/99.
           03  D01-A-1-NUMBER          REDEFINES D01-A-1-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.
           03  D01-A-1-PHONE           REDEFINES D01-A-1-TEXT
                                       PIC 999/999/9999.

           03  FILLER                  PIC X(04).
           03  D01-A-2-PROMPT          PIC X(13).
           03  D01-A-2-TEXT            PIC X(15).
           03  D01-A-2-AMT             REDEFINES D01-A-2-TEXT
                                       PIC ZZZ,ZZ9.99.
           03  D01-A-2-DATE            REDEFINES D01-A-2-TEXT
                                       PIC 99/99/99.
           03  D01-A-2-NUMBER          REDEFINES D01-A-2-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.
           03  D01-A-2-PHONE           REDEFINES D01-A-2-TEXT
                                       PIC 999/999/9999.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02-C            REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(04).
           03  D02-C-1-PROMPT          PIC X(13).
           03  D02-C-1-TEXT            PIC X(20).
           03  D02-C-1-AMT             REDEFINES D02-C-1-TEXT
                                       PIC ZZZ,ZZ9.99.
           03  D02-C-1-NUMBER          REDEFINES D02-C-1-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.
           03  FILLER                  PIC X(06).
           03  D02-C-2-PROMPT          PIC X(13).
           03  D02-C-2-TEXT            PIC X(20).
           03  D02-C-2-AMT             REDEFINES D02-C-2-TEXT
                                       PIC ZZZ,ZZ9.99.
           03  D02-C-2-NUMBER          REDEFINES D02-C-2-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.

      ******************************************************************
      *
      *    E L E M E N T - W O R K E R S
      *
      ******************************************************************
       78  MAX                                       VALUE 06.
       77  ONE                         PIC S9(1)     VALUE ZEROES.
       77  ELE                         PIC 9(02)     VALUE ZEROES.
           88  ELE-VALID                             VALUE 01 THRU MAX.

      *=================================================================
       01  ELEMENT-TABLE.
           03  FILLER      PIC X(8)   VALUE "ERNM060 ".
           03  FILLER      PIC X(8)   VALUE "ENNN030 ".
           03  FILLER      PIC X(8)   VALUE "E NN062 ".
           03  FILLER      PIC X(8)   VALUE "E  A010 ".
           03  FILLER      PIC X(8)   VALUE "ENNN040 ".
           03  FILLER      PIC X(8)   VALUE "E  A330 ".

      *=================================================================
       01  ELEMENT-TABLE-RDF           REDEFINES ELEMENT-TABLE.
           03  ELEMENT-TABLE-OCCURS    OCCURS MAX.
               05  ELEMENT-VDU         PIC X(07).
               05  ELEMENT-LOCK        PIC X(01).

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(74)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-ENTER-ELE.
           03  FILLER PIC X(30) VALUE "F1-RESET, ENTER FIELD INFORMAT".
           03  FILLER PIC X(30) VALUE "ION                           ".
           03  FILLER PIC X(14) VALUE "              ".

       01  LEGEND-LOAD-KEY-ID.
           03  FILLER PIC X(30) VALUE "F1-RESET, F6-SCAN, F7-EXIT, EN".
           03  FILLER PIC X(30) VALUE "TER ID 'C' OR 'A'             ".
           03  FILLER PIC X(14) VALUE "              ".

       01  LEGEND-LOAD-KEY-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F6-SCAN, F7-EXIT, EN".
           03  FILLER PIC X(30) VALUE "TER BRANCH NUMBER             ".
           03  FILLER PIC X(14) VALUE "              ".

       01  LEGEND-LOAD-KEY-SSNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F6-SCAN, F7-EXIT, EN".
           03  FILLER PIC X(30) VALUE "TER SOCIAL SECURITY NUMBER    ".
           03  FILLER PIC X(14) VALUE "              ".

       01  LEGEND-MODIFY-ROUTINE.
           03  FILLER PIC X(30) VALUE "                 F1-RESET, ENT".
           03  FILLER PIC X(30) VALUE "ER-SAVE ,P2 , ENTER FIELD NUMB".
           03  FILLER PIC X(14) VALUE "ER TO CHANGE: ".

       01  LEGEND-SEARCH-DRIVER.
           03  FILLER PIC X(30) VALUE "F1-RESET, F6-SCAN NEXT, <ENTER".
           03  FILLER PIC X(30) VALUE "> TO EDIT CURRENT RECORD, ENTE".
           03  FILLER PIC X(14) VALUE "R KEY TO START".

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBMN/COMMONW.CPY".
       COPY "LIBMN/PCPGRMW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/PCPG01_DEF.CPY".
       COPY "LIBMN/PCPG01_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/PCPG01_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           PERFORM INITIALIZATION.

       MAIN-MODULE.

           PERFORM LOAD-MODULE.

           IF ( SELECT-RESET ) GO TO MAIN-MODULE.
           IF ( SELECT-EXIT  ) MOVE "X" TO ERRCD
                               GO TO MAIN-PROGRAM-EXIT.

      *-----------------------------------------------------------------
       MAIN-MODULE-CONTINUE.

           PERFORM ENTRY-MODULE.

           IF ( SELECT-RESET  ) GO TO MAIN-MODULE.
           IF ( SELECT-DELETE ) PERFORM DELETE-MODULE.
           IF ( SELECT-SAVE   ) PERFORM SAVE-MODULE.
           IF ( SELECT-EDIT   ) GO TO MAIN-MODULE-CONTINUE.

           IF ( NEW-RECORD = "Y" )
              MOVE PC-ID                   TO PROGRAM-FORM-TYPE
              MOVE 02                      TO PROGRAM-FORM-PGNO.

           IF ( PROGRAM-FORM-PAGE-VALID )
              MOVE PC1-KEY                 TO SAVEKEY-PC1-KEY
              MOVE PROGRAM-FORM-PAGE       TO PAGE-SELECTION
              GO TO MAIN-PROGRAM-EXIT.

           MOVE "N" TO NEW-RECORD.

           GO TO MAIN-MODULE.

      *=================================================================
       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
           MOVE SELECT---CONTINUE          TO SELECT-OPTION.

           MOVE FORM-LINK-AREA TO PCPGRM-LINK-BUF.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PCPG01" TO VDU-FORM-NAME.

           PERFORM GET-LOCK-KEY.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C R E A T E - D R I V E R
      *
      ******************************************************************
       CREATE-DRIVER SECTION.

           MOVE ZEROES                     TO ELE.
           MOVE "Y"                        TO NEW-RECORD.
           MOVE SET-RECORD-NEW-ENTRY       TO RECORD-STATUS.

           MOVE "S  A000NEW."              TO VDU.
           MOVE "*** NEW RECORD ***"       TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-PC-FILE.

           MOVE HOLD-PC1-KEY               TO PC1-KEY.

           IF ( PC-ID = "A" )
              MOVE SPACES                  TO WK-LTA-RECORD
              MOVE ZEROES                  TO WK-LTA-PHONE
                                              WK-LTA-EFF-DATE
                                              WK-LTA-EXP-DATE
                                              WK-LTA-CAN-DATE
                                              WK-LTA-CONF-DATE
                                              WK-LTA-LAST-FORM
                                              WK-LTA-BIL-COV
                                              WK-LTA-PDL-COV
                                              WK-LTA-PIP-COV
                                              WK-LTA-UMB-COV
                                              WK-LTA-COL-COV
                                              WK-LTA-COM-COV
              MOVE WK-LTA-RECORD          TO PC-LTA-AUTO-RECORD
           ELSE
              MOVE SPACES                  TO WK-LT-RECORD
              MOVE ZEROES                  TO WK-MILEAGE-1
                                              WK-VALUE-1
                                              WK-SELLPR-1
                                              WK-MISC-AMT-1
                                              WK-MILEAGE-2
                                              WK-VALUE-2
                                              WK-SELLPR-2
                                              WK-MISC-AMT-2
              MOVE WK-HHGS-1    TO PC-LTC-HHGS(1)
              MOVE WK-HHGS-2    TO PC-LTC-HHGS(2)
              MOVE WK-HHGS-3    TO PC-LTC-HHGS(3)
              MOVE WK-AUTO-1    TO PC-LTC-AUTO(1)
              MOVE WK-AUTO-2    TO PC-LTC-AUTO(2)
              MOVE WK-AUTO-3    TO PC-LTC-AUTO(3).

       CREATE-DRIVER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    D E L E T E - M O D U L E
      *
      ******************************************************************
       DELETE-MODULE SECTION.

           MOVE SELECT---CONTINUE          TO SELECT-OPTION.

           IF ( NOT RECORD-EXIST-MODIFY )
              GO TO DELETE-MODULE-EXIT.

           PERFORM OPEN-PC1-FILE.
           PERFORM READ-PC1-FILE.
           IF ( IO-BAD )
              PERFORM MESSAGE-UNDERGONE-CHANGE
              PERFORM MESSAGE-RECORD-DELETED
              GO TO DELETE-MODULE-END.

           IF ( PC-REC NOT = ORIGINAL-PC-REC )
              PERFORM MESSAGE-UNDERGONE-CHANGE
              PERFORM MESSAGE-RECORD-CHANGED
              GO TO DELETE-MODULE-END.

           PERFORM DELETE-PC1-FILE.
           PERFORM CLOSE-PC1-FILE.

       DELETE-MODULE-END.

           MOVE SELECT---RESET             TO SELECT-OPTION.

       DELETE-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    D I S P L A Y - D R I V E R
      *
      ******************************************************************
       DISPLAY-DRIVER SECTION.

           MOVE SPACES                     TO DISPLAY-BUF.

           MOVE PC-LOANDATE                TO DATE-YYYYMMDD
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F01.

           MOVE PC-ORGTERM                 TO F02.
           MOVE PC-REGPYAMT                TO F03.
           MOVE PC2-ID                     TO F04.
           MOVE PC2-BRANCH                 TO F05.
           MOVE PC2-BWNAME                 TO F06.

      *=================================================================
       DISPLAY-INFO-DRIVER.

           IF PC-ID = "C"
              MOVE PC-LTC-HHGS(1) TO WK-HHGS-1
              MOVE PC-LTC-HHGS(2) TO WK-HHGS-2
              MOVE PC-LTC-HHGS(3) TO WK-HHGS-3
              MOVE PC-LTC-AUTO(1) TO WK-AUTO-1
              MOVE PC-LTC-AUTO(2) TO WK-AUTO-2
              MOVE PC-LTC-AUTO(3) TO WK-AUTO-3
           ELSE
              MOVE PC-LTA-AUTO-RECORD      TO WK-LTA-RECORD.

           MOVE SPACES                     TO DISPLAY-INFO-BUF.
           MOVE ZEROES                     TO ELE.

      *-----------------------------------------------------------------
       DISPLAY-INFO-TYPE-A.

           IF ( PC-ID NOT = "A" )
              GO TO DISPLAY-INFO-TYPE-C.

           MOVE "AUTO INS CO : "           TO D01-A-1-PROMPT.
           MOVE "DEALER REQ.: "            TO D01-A-2-PROMPT.
           MOVE WK-LTA-INS-CO             TO D01-A-1-TEXT.
           MOVE WK-LTA-DLR-REQ            TO D01-A-2-TEXT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "AGENCY NAME : "           TO D01-A-1-PROMPT.
           MOVE "EFF. DATE  : "            TO D01-A-2-PROMPT.
           MOVE WK-LTA-AGENCY             TO D01-A-1-TEXT.
           MOVE WK-LTA-EFF-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO D01-A-2-DATE.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "AGENT NAME  : "           TO D01-A-1-PROMPT.
           MOVE "EXP. DATE  : "            TO D01-A-2-PROMPT.
           MOVE WK-LTA-AGENT              TO D01-A-1-TEXT.
           MOVE WK-LTA-EXP-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO D01-A-2-DATE.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "ADDRESS 1   : "           TO D01-A-1-PROMPT.
           MOVE "CANCEL DATE: "            TO D01-A-2-PROMPT.
           MOVE WK-LTA-ADR1               TO D01-A-1-TEXT.
           MOVE WK-LTA-CAN-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO D01-A-2-DATE.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "ADDRESS 2   : "           TO D01-A-1-PROMPT.
           MOVE "CNFIRM DATE: "            TO D01-A-2-PROMPT.
           MOVE WK-LTA-ADR2               TO D01-A-1-TEXT.
           MOVE WK-LTA-CONF-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO D01-A-2-DATE.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "CITY        : "           TO D01-A-1-PROMPT.
           MOVE "BIL LIMIT  : "            TO D01-A-2-PROMPT.
           MOVE WK-LTA-CITY               TO D01-A-1-TEXT.
           MOVE WK-LTA-BIL-COV            TO D01-A-2-AMT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "STATE       : "           TO D01-A-1-PROMPT.
           MOVE "PDL LIMIT  : "            TO D01-A-2-PROMPT.
           MOVE WK-LTA-STATE              TO D01-A-1-TEXT.
           MOVE WK-LTA-PDL-COV            TO D01-A-2-AMT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "ZIP CODE    : "           TO D01-A-1-PROMPT.
           MOVE "PIP LIMIT  : "            TO D01-A-2-PROMPT.
           MOVE WK-LTA-ZIP                TO D01-A-1-TEXT.
           MOVE WK-LTA-PIP-COV            TO D01-A-2-AMT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "PHONE NUMBER: "           TO D01-A-1-PROMPT.
           MOVE "UMB. LIMIT : "            TO D01-A-2-PROMPT.
           MOVE WK-LTA-PHONE              TO D01-A-1-PHONE.
           INSPECT D01-A-1-PHONE REPLACING ALL "/" BY "-".
           MOVE WK-LTA-UMB-COV            TO D01-A-2-AMT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "POLICY NO.  : "           TO D01-A-1-PROMPT.
           MOVE "COLL. LIMIT: "            TO D01-A-2-PROMPT.
           MOVE WK-LTA-POLICY-NO          TO D01-A-1-TEXT.
           MOVE WK-LTA-COL-COV            TO D01-A-2-AMT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "COVERAGE TYP: "           TO D01-A-1-PROMPT.
           MOVE "COMP. LIMIT: "            TO D01-A-2-PROMPT.
           MOVE WK-LTA-COV-TYPE           TO D01-A-1-TEXT.
           MOVE WK-LTA-COM-COV            TO D01-A-2-AMT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "LOSS PAYEE  : "           TO D01-A-1-PROMPT.
           MOVE "LAST FORM #: "            TO D01-A-2-PROMPT.
           MOVE WK-LTA-LOSS-PAYEE         TO D01-A-1-TEXT.
           MOVE WK-LTA-LAST-FORM          TO D01-A-2-NUMBER.
           PERFORM LOAD-DETAIL-LINE.

           PERFORM LOAD-DETAIL-LINE.

       DISPLAY-INFO-TYPE-C.

           IF ( PC-ID NOT = "C" )
              GO TO DISPLAY-DRIVER-END.

           MOVE "DESCRIPTION: "            TO D02-C-1-PROMPT.
           MOVE "DESCRIPTION: "            TO D02-C-2-PROMPT.
           MOVE WK-DESC-1                  TO D02-C-1-TEXT.
           MOVE WK-DESC-2                  TO D02-C-2-TEXT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "MAKE       : "            TO D02-C-1-PROMPT.
           MOVE "MAKE       : "            TO D02-C-2-PROMPT.
           MOVE WK-MAKE-1                  TO D02-C-1-TEXT.
           MOVE WK-MAKE-2                  TO D02-C-2-TEXT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "MODEL      : "            TO D02-C-1-PROMPT.
           MOVE "MODEL      : "            TO D02-C-2-PROMPT.
           MOVE WK-MODEL-1                 TO D02-C-1-TEXT.
           MOVE WK-MODEL-2                 TO D02-C-2-TEXT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "YEAR       : "            TO D02-C-1-PROMPT.
           MOVE "YEAR       : "            TO D02-C-2-PROMPT.
           MOVE WK-YEAR-1                  TO D02-C-1-TEXT.
           MOVE WK-YEAR-2                  TO D02-C-2-TEXT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "STYLE      : "            TO D02-C-1-PROMPT.
           MOVE "STYLE      : "            TO D02-C-2-PROMPT.
           MOVE WK-STYLE-1                 TO D02-C-1-TEXT.
           MOVE WK-STYLE-2                 TO D02-C-2-TEXT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "CLASS      : "            TO D02-C-1-PROMPT.
           MOVE "CLASS      : "            TO D02-C-2-PROMPT.
           MOVE WK-CLASS-1                 TO D02-C-1-TEXT.
           MOVE WK-CLASS-2                 TO D02-C-2-TEXT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "ID#        : "            TO D02-C-1-PROMPT.
           MOVE "ID#        : "            TO D02-C-2-PROMPT.
           MOVE WK-VIN-1                   TO D02-C-1-TEXT.
           MOVE WK-VIN-2                   TO D02-C-2-TEXT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "TITLE NO   : "            TO D02-C-1-PROMPT.
           MOVE "TITLE NO   : "            TO D02-C-2-PROMPT.
           MOVE WK-TITLE-NO-1              TO D02-C-1-TEXT.
           MOVE WK-TITLE-NO-2              TO D02-C-2-TEXT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "MILEAGE    : "            TO D02-C-1-PROMPT.
           MOVE "MILEAGE    : "            TO D02-C-2-PROMPT.
           MOVE WK-MILEAGE-1               TO D02-C-1-NUMBER.
           MOVE WK-MILEAGE-2               TO D02-C-2-NUMBER.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "VALUE      : "            TO D02-C-1-PROMPT.
           MOVE "VALUE      : "            TO D02-C-2-PROMPT.
           MOVE WK-VALUE-1                 TO D02-C-1-AMT.
           MOVE WK-VALUE-2                 TO D02-C-2-AMT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "SELL PRICE : "            TO D02-C-1-PROMPT.
           MOVE "SELL PRICE : "            TO D02-C-2-PROMPT.
           MOVE WK-SELLPR-1                TO D02-C-1-AMT.
           MOVE WK-SELLPR-2                TO D02-C-2-AMT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE "MISC AMT   : "            TO D02-C-1-PROMPT.
           MOVE "MISC AMT   : "            TO D02-C-2-PROMPT.
           MOVE WK-MISC-AMT-1              TO D02-C-1-AMT.
           MOVE WK-MISC-AMT-2              TO D02-C-2-AMT.
           PERFORM LOAD-DETAIL-LINE.

           MOVE WK-HHGS-1             TO DETAIL-LINE.
           PERFORM LOAD-DETAIL-LINE.

       DISPLAY-DRIVER-END.

           PERFORM SEND-DISPLAY.
           PERFORM SEND-DISPLAY-INFO.

       DISPLAY-DRIVER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    D I S P L A Y - K E Y - D R I V E R
      *
      ******************************************************************
       DISPLAY-KEY-DRIVER SECTION.

           MOVE SPACES                     TO DISPLAY-KEY-BUF.

           MOVE PC-ID                      TO DISPLAY-KEY-ID
           MOVE PC-SOC-SEC                 TO DISPLAY-KEY-SSNO

           PERFORM SEND-DISPLAY-KEY.

       DISPLAY-KEY-DRIVER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *-----------------------------------------------------------------
      *    RECORD-STATUS = 0 - MODIFY EXISTING RECORD
      *                    1 - INITIAL NEW RECORD ENTRY
      *                    2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.

           MOVE SELECT---CONTINUE          TO SELECT-OPTION.
           MOVE 1                          TO ONE.
           MOVE ZEROES                     TO ELE.

       CREATE-ROUTINE.

           IF ( NOT RECORD-NEW-ENTRY ) GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE < 1   ) MOVE 1 TO ELE ONE.
           IF ( ELE-VALID ) GO TO ENTER-ELE.

           MOVE SET-RECORD-NEW-MODIFY      TO RECORD-STATUS.

       MODIFY-ROUTINE.

           MOVE LEGEND-MODIFY-ROUTINE      TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL."              TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTRY-MODULE-END.
           IF (        F6-KEY ) GO TO ENTRY-MODULE-END.
           IF ( NOT ENTER-KEY ) GO TO MODIFY-ROUTINE.

           MOVE VDUBUF                     TO SEL-ENTRY.

           IF ( SEL-ENTRY = SPACES )
              MOVE SELECT---SAVE           TO SELECT-OPTION
              GO TO ENTRY-MODULE-END.

           IF ( SEL-ENTRY = "DELETE" )
              MOVE SELECT---DELETE         TO SELECT-OPTION
              GO TO ENTRY-MODULE-END.

       ENTRY-PAGE-CHANGE.

           MOVE SPACES                     TO PROGRAM-FORM-PAGE.

           IF NOT (( SEL-SINGLE-PAGE-KEY = SPACES   )  AND
                   ( SEL-SINGLE-PAGE-P   = "P"      )  AND
                   ( SEL-SINGLE-PAGE-NUMBER NUMERIC ))
              GO TO CHECK-SEL-ELE.

      * VERIFY REQUEST 
           IF ( SEL-SINGLE-PAGE-NUMBER = 01 )
              MOVE ZEROES                  TO PROGRAM-FORM-TYPE
              MOVE SEL-SINGLE-PAGE-NUMBER  TO PROGRAM-FORM-PGNO
           ELSE
              MOVE PC-ID                   TO PROGRAM-FORM-TYPE
              MOVE SEL-SINGLE-PAGE-NUMBER  TO PROGRAM-FORM-PGNO.

           MOVE FORM-PATHNAME              TO FORM-PATH.

           IF ( FORM-NAM NOT = PROGRAM-FORM ) AND
              ( PROGRAM-FORM-PAGE-VALID     )
              MOVE SELECT---SAVE           TO SELECT-OPTION
              GO TO ENTRY-MODULE-END
           ELSE
              GO TO MODIFY-ROUTINE.

       CHECK-SEL-ELE.

           INSPECT SEL-ENTRY REPLACING ALL SPACES BY ZEROES.

           IF ( SEL-ELE NOT NUMERIC ) GO TO MODIFY-ROUTINE.
           IF ( SEL-ELE = ZEROES    ) GO TO MODIFY-ROUTINE.
           IF ( SEL-ELE > MAX       ) GO TO MODIFY-ROUTINE.

           MOVE SEL-ELE                    TO ELE.

       ENTER-ELE.

           MOVE LEGEND-ENTER-ELE           TO LEGEND.
           PERFORM SEND-LEGEND.

           IF ( SEL-LOCK-KEY  NOT = ZEROES ) AND
              ( ELEMENT-LOCK(ELE) = SPACES ) AND
              ( NOT RECORD-NEW-ENTRY       )
              GO TO MODIFY-ROUTINE.

           MOVE ELEMENT-VDU(ELE)           TO VDU.
           MOVE ELE                        TO VDUROW.
           MOVE ZEROES                     TO VDUCOL     CURRENT-VDUCOL.
           MOVE "."                        TO VDUDECIMAL.
           PERFORM SCREENIO.

       ENTRY-UP-KEY.

           IF ( UP-KEY           ) AND
              ( RECORD-NEW-ENTRY )
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

       ENTRY-F1-KEY.

           IF ( F1-KEY )
              GO TO ENTRY-MODULE-END.

       ENTRY-ENTER-KEY.

           IF ( NOT ENTER-KEY )
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

       GOTO-FIELD.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06
                 DEPENDING ON ELE.

           GO TO ENTRY-MODULE-END.

      *********************************************
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *********************************************
       F-01.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENTER-ELE.

           MOVE VDU-DATE-YYYYMMDD TO PC-LOANDATE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENTER-ELE.

           MOVE VDUNUM0 TO PC-ORGTERM.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.

           IF ( VDUNUM2 = ZEROES )
              GO TO ENTER-ELE.

           MOVE VDUNUM2 TO PC-REGPYAMT.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-ELE.

           IF ( VDUBUF NOT = PC-ID )
              MOVE "ERROR: ID IS NOT EQUAL TO THE KEY" TO MESS
              PERFORM SEND-MESS
              MOVE ELEMENT-VDU(ELE)           TO VDU
              MOVE "S"                        TO VDUACTION
              MOVE ELE                        TO VDUROW
              MOVE ZEROES                     TO VDUCOL  CURRENT-VDUCOL
              MOVE "."                        TO VDUDECIMAL
              MOVE PC-ID                      TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-ELE.

           MOVE VDUBUF TO PC2-ID.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENTER-ELE.

           IF ( VDUNUM0 NOT = PC-BRANCH )
              MOVE "ERROR: NUMBER IS NOT EQUAL TO THE KEY" TO MESS
              PERFORM SEND-MESS
              MOVE ELEMENT-VDU(ELE)           TO VDU
              MOVE "S"                        TO VDUACTION
              MOVE ELE                        TO VDUROW
              MOVE ZEROES                     TO VDUCOL  CURRENT-VDUCOL
              MOVE "."                        TO VDUDECIMAL
              MOVE PC-BRANCH                  TO VDUNUM0
              PERFORM SCREENIO
              GO TO ENTER-ELE.

           MOVE VDUNUM0 TO PC2-BRANCH.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-ELE.

           MOVE VDUBUF TO PC2-BWNAME.

           GO TO CREATE-ROUTINE.

      *=================================================================
       ENTRY-MODULE-END.

           IF ( F1-KEY ) MOVE SELECT---RESET  TO SELECT-OPTION.
           IF ( F6-KEY ) MOVE SELECT---LAUNCH TO SELECT-OPTION.

      *-----------------------------------------------------------------
       ENTRY-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    L O A D - M O D U L E
      *
      ******************************************************************
       LOAD-MODULE SECTION.

           MOVE SELECT---CONTINUE          TO SELECT-OPTION.

           MOVE "N"                        TO NEW-RECORD.
           MOVE SET-RECORD-EXIST-MODIFY    TO RECORD-STATUS.

           PERFORM FORM-RESET.

           IF ( SAVEKEY-PC1-KEY NOT = SPACES )
              MOVE SAVEKEY-PC1-KEY         TO PC1-KEY   HOLD-PC1-KEY
              MOVE SPACES                  TO SAVEKEY-PC1-KEY
              PERFORM DISPLAY-KEY-DRIVER
              GO TO LOAD-RECORD-FROM-KEY.

           MOVE EXT-FILPATH-BRNO TO BR-NO PC-BRANCH HOLD-PC-BRANCH.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              MOVE "INVALID: BRANCH NUMBER IS NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO LOAD-MODULE-END.   

      *=================================================================
       LOAD-KEY-ID.

           MOVE LEGEND-LOAD-KEY-ID         TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010KEY-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO LOAD-MODULE-END.
           IF (        F6-KEY ) PERFORM SEARCH-DRIVER
                                IF ( F1-KEY )
                                   GO TO LOAD-MODULE-END
                                ELSE
                                   GO TO LOAD-RECORD-FROM-KEY.
           IF (        F7-KEY ) GO TO LOAD-MODULE-END.
           IF ( NOT ENTER-KEY ) GO TO LOAD-KEY-ID.

           MOVE VDUBUF TO PC-ID HOLD-PC-ID.

           IF NOT ( PC-ID = "A" OR "C" )
              MOVE "ERROR: ONLY (A)UTO INS. OR (C)OLLATERAL" TO MESS
              PERFORM SEND-MESS
              GO TO LOAD-KEY-ID.

      *-----------------------------------------------------------------
       LOAD-KEY-SSNO.

           MOVE "ENNN100KEY-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO LOAD-MODULE-END.
           IF (        F6-KEY ) PERFORM SEARCH-DRIVER
                                IF ( F1-KEY )
                                   GO TO LOAD-MODULE-END
                                ELSE
                                   GO TO LOAD-RECORD-FROM-KEY.
           IF (        F7-KEY ) GO TO LOAD-MODULE-END.
           IF ( NOT ENTER-KEY ) GO TO LOAD-KEY-SSNO.

           IF ( VDUNUM0 = ZEROES )
              GO TO LOAD-KEY-SSNO.

           MOVE VDUNUM0 TO PC-SOC-SEC HOLD-PC-SOC-SEC.

       LOAD-RECORD-FROM-KEY.

           PERFORM DISPLAY-KEY-DRIVER.

           PERFORM OPEN-PC1-FILE.
           PERFORM READ-PC1-FILE.
           PERFORM CLOSE-PC1-FILE.
           IF ( IO-GOOD )
              MOVE PC-REC                  TO ORIGINAL-PC-REC
              PERFORM DISPLAY-DRIVER
            ELSE
              PERFORM CREATE-DRIVER.

       LOAD-MODULE-END.

           IF ( F1-KEY ) MOVE SELECT---RESET TO SELECT-OPTION.
           IF ( F7-KEY ) MOVE SELECT---EXIT  TO SELECT-OPTION.

       LOAD-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S A V E - M O D U L E
      *
      ******************************************************************
       SAVE-MODULE SECTION.

           MOVE SELECT---CONTINUE          TO SELECT-OPTION.

           IF ( NOT RECORD-EXIST-MODIFY )
              GO TO SAVE-MODULE-WRITE.

       SAVE-MODULE-REWRITE.

           PERFORM OPEN-PC1-FILE.
           MOVE PC-REC                     TO CHANGED-PC-REC.

           IF ( ORIGINAL-PC-REC = CHANGED-PC-REC )
              GO TO SAVE-MODULE-END.

           PERFORM READ-PC1-FILE.
           IF ( IO-BAD )
              PERFORM MESSAGE-UNDERGONE-CHANGE
              PERFORM MESSAGE-RECORD-DELETED
              GO TO SAVE-MODULE-END.

           IF ( PC-REC NOT = ORIGINAL-PC-REC )
              PERFORM MESSAGE-UNDERGONE-CHANGE
              PERFORM MESSAGE-CHANGE-AGAIN
              GO TO SAVE-MODULE-END.

           MOVE CHANGED-PC-REC             TO PC-REC.
           PERFORM REWRITE-PC1-FILE.

           GO TO SAVE-MODULE-END.

       SAVE-MODULE-WRITE.

           PERFORM OPEN-PC1-FILE.

           PERFORM WRITE-PC1-FILE.
           IF ( IO-BAD )
              PERFORM MESSAGE-ALREADY-CREATED.

       SAVE-MODULE-END.

           PERFORM CLOSE-PC1-FILE.

           MOVE SELECT---RESET             TO SELECT-OPTION.

       SAVE-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E A R C H - D R I V E R
      *
      ******************************************************************
       SEARCH-DRIVER SECTION.

           MOVE LEGEND-SEARCH-DRIVER TO LEGEND.
           PERFORM SEND-LEGEND.

      * "A" AUTO INSURACE
      * "C" COLLATERAL

           MOVE "A"                 TO PC-ID.
           MOVE EXT-FILPATH-BRANCH  TO PC-BRANCH.
           MOVE ZEROES              TO PC-SOC-SEC.

           PERFORM OPEN-PC1-FILE.

           PERFORM START-PC1-FILE.

       SEARCH-SCANKEY-ENTRY.

           MOVE "E RA110SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO SEARCH-DRIVER-END.
           IF (        F6-KEY ) GO TO SEARCH-READ-NEXT.
           IF ( NOT ENTER-KEY ) GO TO SEARCH-SCANKEY-ENTRY.

           IF (VDUBUF = SPACES)
              IF ( FIRST-SCAN NOT = 0 )
                 MOVE PC1-KEY TO HOLD-PC1-KEY
                 MOVE PC-REC  TO ORIGINAL-PC-REC
                 GO TO SEARCH-DRIVER-END
              ELSE
                 GO TO SEARCH-SCANKEY-ENTRY.

           MOVE VDUBUF             TO WORK-PC1-KEY.

           MOVE WORK-PC-ID      TO PC-ID.
           MOVE HOLD-PC-BRANCH  TO PC-BRANCH.
           IF WORK-PC-SOC-SEC NOT NUMERIC
              GO TO SEARCH-SCANKEY-ENTRY 
           ELSE
              MOVE WORK-PC-SOC-SEC TO PC-SOC-SEC HOLD-PC-SOC-SEC.

           PERFORM START-PC1-FILE.

       SEARCH-READ-NEXT.

           PERFORM READ-PC1-FILE-NEXT.
           IF IO-BAD
              MOVE VDU---F1-KEY TO VDUSTATUS
              GO TO SEARCH-DRIVER-END.

           IF PC-BRANCH NOT = PC-PATH-OWNBR
              IF HOLD-PC-ID = "A"
      * GO RESTART NOW WITH "C" AND LOOK FOR COLLATERAL RECORDS
                 MOVE "C"          TO HOLD-PC-ID
                 MOVE 0            TO HOLD-PC-SOC-SEC
                 MOVE HOLD-PC1-KEY TO PC1-KEY
                 PERFORM START-PC1-FILE
                 GO TO SEARCH-READ-NEXT
              ELSE
                 MOVE VDU---F1-KEY TO VDUSTATUS
                 GO TO SEARCH-DRIVER-END.

      *    UNLOCK PC-FILE.

           MOVE PC1-KEY TO HOLD-PC1-KEY.

           PERFORM DISPLAY-KEY-DRIVER.
           PERFORM DISPLAY-DRIVER.

           MOVE 1 TO FIRST-SCAN.

           GO TO SEARCH-SCANKEY-ENTRY.

       SEARCH-DRIVER-END.

           IF ( F1-KEY )
              PERFORM FORM-RESET.

           MOVE ZEROES TO FIRST-SCAN.
           PERFORM CLOSE-PC1-FILE.

       SEARCH-DRIVER-EXIT.
           EXIT.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO PCPG01-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PCPG01-SYSDATE.
           DISPLAY SPACES UPON PCPGRM-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PCPG01-SCREEN.
           DISPLAY PCPG01-SCREEN UPON PCPGRM-WINDOW-HANDLE.
           MOVE PCPG01-FORM-FIELDS TO WR-BUF.
           MOVE PCPG01-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE PCPG01-HELP--FILE TO HELP-LINK-FILE.
           MOVE PCPG01-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/PCPG01_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPPCCL.CPY".
       COPY "LIBMN/COMMON.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
       COPY "LIBGB/ACCESS.CPY".
      *-----------------------------------------------------------------
       LOAD-DETAIL-LINE.

           ADD  01                         TO ELE.
           MOVE DETAIL-LINE                TO DISPLAY-INFO-ELE(ELE).
           MOVE SPACES                     TO DETAIL-LINE.

      *-----------------------------------------------------------------
       SEND-DISPLAY.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-DISPLAY-INFO.
           MOVE DISPLAY-INFO-BUF TO WR-BUF.
           MOVE DISPLAY-INFO-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-DISPLAY-KEY.
           MOVE DISPLAY-KEY-BUF TO WR-BUF.
           MOVE DISPLAY-KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-PC1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPPCGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPPC1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.

           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PCPG01-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE PCPGRM-LINK-BUF TO FORM-LINK-AREA.
           EXIT PROGRAM.
