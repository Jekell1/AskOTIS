       IDENTIFICATION DIVISION.
       PROGRAM-ID.    UP15VY.
       DATE-WRITTEN.  05-13-2020.
      ******************************************************************
      *
      *  IDENTIFICATION: UP/UP15VY.C
      *
      *  DESCRIPTION   : VERIFY NUMERICS IN FILES
      *
      * GLOBALS:
      *    AL [GL01AL] G/L ALLOCATION        
      *    BR [GB01BR] BRANCH          
      *    BY [LP01BY] BRANCH STATS          
      *    CD [LP01CD] CODES                  (CDB NEW)
      *    CL [LP01CL] CAN LOAN               SKIP "OK" AND "OKL" ON RISC2
      *    CP [SP01CP] COLLECTOR POOLS        (LOCAL AND GLOBAL MERGED)
      *                (THIS PROGRAM CONVERTS GLOBAL CPFILE)
      *    EA [LP01EA] EXTRACTION ACTION     
      *    FS [GL01FS] FINANCIAL STATEMENT   >> THEY DONT WANT CONVERTED
      *    GI [GL01GI] G/L INTERFACE         
      *    GL [GL01GL] G/L MASTER            
      *    GT [GL01GT] G/L TRANSACTIONS      
      *    II [SP01II] IRS IDENTIFICATION    
      *    IR [LP01IR] INSURANCE RATE TABLE 
      *    LC [LP01LC] LOAN CLASS            
      *    PE [GB01PE] PASSWORD EXCEPTIONS    
      *    PW [GB01PW] PASSWORD  - CANNOT CONVERT HASHED PASSWORD
      *    RB [LP01RB] REBATE/CANCEL 
      *    RC [GB01RC] DAILY STATUS           SKIP 9990 ON RISC2
      *    SE [GB01SE] USER SECURITY         
      *    SN [GB01SN] SECURITY PROGRAM NAME 
      *    SM [LP01SM] STATE PARAM MEMO      
      *    SP [LP01SP] STATE PARAMETER       
      *    SS [LP01SS] CREDIT SCORE STATS    
      *    ST [LP01ST] STATE FILE             (SU TRAILER)
      *    TRWINFO [SPFDTRWINFO] CREDIT BUREAU INFO FILE
      *    TZ [LP01TZ] TIME ZONE             
      *    WR [SP01WR] RATING SHEET        
      *
      * LOCALS:
      *    BM [LP01BM] BORROWER MEMO
      *    BW [LP01BW] BORROWER
      *    BX [LP01BX] BORROWER TRAILER (KYC)
      *    CA [LP01CA] CASH DRAWER
      *    CI [LP01CI] CUSTOMER INFO
      *    CJ [SP01CJ] COLLECTOR JOURNAL
      *    CK [LP01CK] CHECK MASTER
      *    DL [LP01DL] DEALER MASTER
      *    DT [LP01DT] DEALER TRANS
      *    ES [LP01ES] ESCROW
      *    GB [GB01GB] GLOBAL 
      *    IC [LP01IC] INSURANCE COMPANY
      *    IX [LP01IX] ALTERNATE VIN INDEX
      *    LE [LP01LE] LOAN ENDORSER
      *    LH [LP01LH] LOAN HISTORY
      *    LM [LP01LM] LOAN MEMO
      *    LN [LP01LN] LOAN MASTER
      *    LP [LP01LP] LOAN PAYMENT
      *    LS [LP01LS] LOAN TRAILER (STATUS) SPLIT OUT
      *    LT [LP01LT] LOAN TRAILER SPLIT OUT
      *    LX [LP01LX] LOAN PAYMENT TRAILER 
      *    MP [LP01MP] MEMO PAYMENT
      *    TY [LP01TY] TODAYS TOTALS
      *
      *=================================================================
      * REV:
      * JKC 2021-1118 REMOVED CD-ELUNITPER-CD SINCE CHANGED TO PIC X.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       COPY "LIBUP/UP1534_FS.CPY".

      * USING FOR ORIGINAL MACHINE
           SELECT OBR-FILE ASSIGN TO OBR-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY OBR-KEY
                  FILE STATUS FILE-STAT.
           SELECT OB2-FILE ASSIGN TO OB2-PATH
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OB21-KEY
                  FILE STATUS FILE-STAT.


      ******************************************************************
      ******************************************************************
      ******************************************************************
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

       COPY "LIBUP/UP1534_FD.CPY".

       FD  OBR-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01BR.CPY" REPLACING LEADING "BR" BY "OBR".

       FD  OB2-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01B2.CPY" REPLACING LEADING "B2" BY "OB2".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UP/UP15VY.CBL S35 06/28/22-10:13:13 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       COPY "LIBUP/UP1534_WS.CPY".

      *=================================================================
      *   OWK-REC FROM 'REPLACING LEADING' ON WK-REC.
      *   NEED A15 & A30 VERSIONS OF LP01WK
      *    - A15: OWK-LT-RECORD       A30: WK-LT-RECORD
      *    - A15: OWK-LT-A-RECORD     A30: WK-LTA-RECORD
      *=================================================================
       COPY "LIBLP/LP01WK.CPY".
       COPY "LIBUP/A15SRC/LP01WK.CPY" REPLACING LEADING "WK" BY "OWK".
      * NEEDED TO VERIFY CANNED LOANS (CL)
       COPY "LIBUP/A15SRC/LP01PD.CPY" REPLACING LEADING "PD" BY "OPD".

      *=================================================================
       01  PR-IFN                      PIC X(2) VALUE "PR".
       01  OIR-KEY                     PIC 99.
       01  OII-KEY                     PIC 99.
       01  OGB-KEY                     PIC 9.
       01  OBR-KEY                     PIC 9(4).
       01  OBR-PATH                    PIC X(35)     VALUE SPACES.
       01  OB2-PATH                    PIC X(35)     VALUE SPACES.

       01  HOLD-CK1-KEY.
           03  HOLD-CK-BRNO     PIC 9(4).
           03  HOLD-CK-NO       PIC 9(6).
       01  HOLD-LT1-KEY.
           03  HOLD-LT-ACCTNO   PIC 9(8).
           03  HOLD-LT-CODE     PIC X.
           03  HOLD-LT-SEQNO    PIC 9(3).
       01  HOLD-IX1-KEY.
           03  HOLD-IX-LT-ID-ACCTNO   PIC 9(8).
           03  HOLD-IX-LT-ID-SEQNO    PIC 9(3).
           03  HOLD-IX-LT-ID-TYPE     PIC 99.
           03  HOLD-IX-LT-ID-FILLER   PIC X(1).

       01  HOLD-LE1-KEY.
           03  HOLD-LE-ACCTNO           PIC 9(8).
           03  HOLD-LE-TYPE             PIC X.
           03  HOLD-LE-SSNO             PIC 9(9).
           03  HOLD-LE-OSACCTNO         PIC 9(8).

       01  HOLD-CJ1-KEY.
           03  HOLD-CJ-BRNO              PIC 9(4).
           03  HOLD-CJ-COLLID            PIC XXX.
           03  HOLD-CJ-DATE              PIC 9(5).
           03  HOLD-CJ-START-TIME        PIC 9(4).
           03  HOLD-CJ-SEQNO             PIC 99.
         
      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************

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
       01  COMPARE-DATE                PIC 9(8).
       01  COMPARE-DATEX REDEFINES COMPARE-DATE.
           03  COMPARE-DATE-YYYY       PIC 9(4).
           03  FILLER                  PIC X(4).

       01  SUBSCRIPT-WORKERS.
           03  ELE                     PIC 9(03)     VALUE ZEROES.
           03  SUB                     PIC 9(03)     VALUE ZEROES.
           03  S1                      PIC 9(03)     VALUE ZEROES.
           03  SUB1      REDEFINES S1  PIC 9(03).
           03  BYSUB1    REDEFINES S1  PIC 9(03).
           03  SSSUB1    REDEFINES S1  PIC 9(03).
           03  TYSUB1    REDEFINES S1  PIC 9(03).
           03  S2                      PIC 9(03)     VALUE ZEROES.
           03  SUB2      REDEFINES S2  PIC 9(03).
           03  BYSUB2    REDEFINES S2  PIC 9(03).
           03  SSSUB2    REDEFINES S2  PIC 9(03).
           03  TYSUB2    REDEFINES S2  PIC 9(03).
           03  S3                      PIC 9(03)     VALUE ZEROES.
           03  SUB3      REDEFINES S3  PIC 9(03).
           03  BYSUB3    REDEFINES S3  PIC 9(03).
           03  SSSUB3    REDEFINES S3  PIC 9(03).



       01  TEST-TRCD                   PIC X(02).
           88  ADDON-TRCD              VALUE "RT" "LP" "NP" "ET" "ST".
           88  DEFER-TRCD              VALUE "DF" "D2" "D3" "D4" "D5"
                                             "D6" "D7" "D8" "D9".
           88  PMT-TRCD                VALUE "PY" "PA".
           88  REBATE-TRCD             VALUE "RC" "RA" "RP" "R1" "R2"
                                             "R3" "RI" "RS" "RM" "RD"
                                             "CC" "CA" "CP" "C1" "C2"
                                             "C3" "CD".
           88  REVOLV-TRCD             VALUE "AV" "PU" "ST" "PY".
           88  RESERVE-TRCD            VALUE "PR" "PC" "PP".
           88  RESERVE-P4P5-TRCD       VALUE "P4" "P5".

      ******************************************************************
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
           03  FILLER               PIC X(5)   VALUE SPACES.
           03  H1-NAME              PIC X(40)   VALUE SPACES.
           03  FILLER               PIC X(5)   VALUE SPACES.
           03  FILLER               PIC X(05)   VALUE "PAGE".
           03  H1-PAGE-NO           PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEADER-LINE-2.
           03  H2-USERID-PROMPT     PIC X(11)   VALUE "USER ID  : ".
           03  H2-USERID            PIC X(10)   VALUE SPACES.
           03  FILLER               PIC X(2)   VALUE SPACES.
           03  H2-TITLE             PIC X(45)   VALUE
               " VERIFY A15 NUMERICS REPORT".

      *=================================================================
       01  DETAIL-LINE                        PIC X(80)   VALUE SPACES.
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-MACHINE                   PIC XX.
           03  FILLER                      PIC X(2).
           03  D-FILE                      PIC XX.
           03  FILLER                      PIC X(2).
           03  D-BRNO                      PIC 9(4).
           03  FILLER                      PIC X(2).
           03  D-KEY                       PIC X(25).
           03  FILLER                      PIC X(2).
           03  D-FIELD                     PIC X(25).
           03  FILLER                      PIC X(2).
           03  D-VALUE                     PIC 9(12).999.
           03  D-VALUEX REDEFINES D-VALUE  PIC X(16).

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


       01  WSS-DATE-YYYYSMM            PIC 9999/99.
       01  DEFAULT-GLOBAL-OPTIONS      PIC X(01)     VALUE SPACES.
       01  DEFAULT-LOCAL-OPTIONS       PIC X(01)     VALUE SPACES.

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

           03  FILE-OPTION-WORKERS.
      * GLOBALS
               05  FILE-OPTION-AL      PIC X(01).
               05  FILE-OPTION-BY      PIC X(01).
               05  FILE-OPTION-CD      PIC X(01).
               05  FILE-OPTION-CL      PIC X(01).
               05  FILE-GLOBAL-CP      PIC X(01).
               05  FILE-OPTION-EA      PIC X(01).
               05  FILE-OPTION-FS      PIC X(01).
               05  FILE-OPTION-GI      PIC X(01).
               05  FILE-OPTION-GL      PIC X(01).
               05  FILE-OPTION-GT      PIC X(01).
               05  FILE-OPTION-II      PIC X(01).
               05  FILE-OPTION-IR      PIC X(01).
               05  FILE-OPTION-LC      PIC X(01).
               05  FILE-OPTION-PE      PIC X(01).
               05  FILE-OPTION-PW      PIC X(01).
               05  FILE-OPTION-RC      PIC X(01).
               05  FILE-OPTION-SE      PIC X(01).
               05  FILE-OPTION-SN      PIC X(01).
               05  FILE-OPTION-SM      PIC X(01).
               05  FILE-OPTION-SP      PIC X(01).
               05  FILE-OPTION-SS      PIC X(01).
               05  FILE-OPTION-ST      PIC X(01).
               05  FILE-OPTION-TRWINFO PIC X(01).
               05  FILE-OPTION-TZ      PIC X(01).
               05  FILE-OPTION-WR      PIC X(01).
               05  FILE-OPTION-BR      PIC X(01).
      * LOCALS
               05  FILE-OPTION-BM      PIC X(01).
               05  FILE-OPTION-BW      PIC X(01).
               05  FILE-OPTION-CA      PIC X(01).
               05  FILE-OPTION-CI      PIC X(01).
               05  FILE-OPTION-CJ      PIC X(01).
               05  FILE-OPTION-CK      PIC X(01).
               05  FILE-OPTION-CP      PIC X(01).
               05  FILE-OPTION-CX      PIC X(01).
               05  FILE-OPTION-DL      PIC X(01).
               05  FILE-OPTION-DT      PIC X(01).
               05  FILE-OPTION-GB      PIC X(01).
               05  FILE-OPTION-IC      PIC X(01).
               05  FILE-OPTION-IX      PIC X(01).
               05  FILE-OPTION-LE      PIC X(01).
               05  FILE-OPTION-LH      PIC X(01).
               05  FILE-OPTION-LM      PIC X(01).
               05  FILE-OPTION-LN      PIC X(01).
               05  FILE-OPTION-LP      PIC X(01).
               05  FILE-OPTION-LS      PIC X(01).
               05  FILE-OPTION-LT      PIC X(01).
               05  FILE-OPTION-LX      PIC X(01).
               05  FILE-OPTION-RX      PIC X(01).
               05  FILE-OPTION-TY      PIC X(01).

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
       COPY "LIBUP/UP15VY_DEF.CPY".
       COPY "LIBUP/UP15VY_WKS.CPY".
     
      ******************************************************************
      *
      *    L I N K A G E - S C R E E N - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
      *-----------------------------------------------------------------
     
      *=================================================================
       SCREEN SECTION.
       COPY "LIBUP/UP15VY_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E    D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE
                 OBR-FILE OB2-FILE
      *- - - - - - - - - - - - - - - - - - - -{ SOURCE & TARGET FILES  }
                 OAL-FILE  OBY-FILE OCD-FILE OCL-FILE OCP-FILE
                 OEA-FILE  OGI-FILE OGL-FILE OGT-FILE  
                 OII-FILE  OIR-FILE OLC-FILE OPE-FILE OPW-FILE  
                 ORC-FILE  OSE-FILE OSN-FILE OSM-FILE OSP-FILE 
                 OSS-FILE  OST-FILE OSU-FILE OTZ-FILE OTRW-INFO-FILE 
                 OWR-FILE  
      * LOCALS
                 OBW-FILE  OBM-FILE OBX-FILE  
                 OCA-FILE  OCI-FILE OCJ-FILE OCK-FILE  
                 ODL-FILE  ODT-FILE OGB-FILE OIC-FILE  
                 OIX-FILE  OLE-FILE OLH-FILE OLM-FILE  
                 OLN-FILE  OLP-FILE OLS-FILE  
                 OLT-FILE  OLX-FILE 
                  OTY-FILE.
      *-----------------------------------------------------------------
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
      *-----------------------------------------------------------------
           CLOSE OBR-FILE OB2-FILE
      *- - - - - - - - - - - - - - - - - - - -{ SOURCE & TARGET FILES  }
                 OAL-FILE  OBY-FILE  OCD-FILE  OCL-FILE OCP-FILE
                 OEA-FILE  OGI-FILE  OGL-FILE OGT-FILE
                 OII-FILE  OIR-FILE  OLC-FILE  OPE-FILE OPW-FILE 
                 ORC-FILE  OSE-FILE  OSN-FILE  OSM-FILE OSP-FILE 
                 OSS-FILE  OST-FILE  OSU-FILE  OTZ-FILE OTRW-INFO-FILE
                 OWR-FILE  
      * LOCALS
                 OBW-FILE OBM-FILE OBX-FILE OCA-FILE  
                 OCI-FILE OCJ-FILE OCK-FILE 
                 ODL-FILE ODT-FILE OGB-FILE OIC-FILE  
                 OIX-FILE OLE-FILE OLH-FILE OLM-FILE
                 OLN-FILE OLP-FILE OLS-FILE 
                 OLT-FILE OLX-FILE 
                 OTY-FILE.

       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           PERFORM INITIALIZATION.

           IF ( ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

      * CURRENT-MACHINE WILL BE RISC1, "R1"
           MOVE CURRENT-MACHINE TO SOURCE-MACHINE TARGET-MACHINE.
           CALL "C$TOLOWER" USING RISC2, VALUE 02.

      * LOAD A TABLE INDEXED BY BR-NO JUST TO KNOW THAT BR-MACHINE.
      * USING THE ORIGINAL A15 BRANCH FILE IN CASE YOU ALREADY CHANGED
      * THE NEW FILE TO ALL BE R1 BRANCHES!
      * BRANCH 9990 WILL BE R1
           PERFORM LOAD-BRANCH-TABLE.
           IF ( ERRCD NOT = SPACES )
              MOVE "NEED ORIGINAL BRANCH FILE FOR MACHINE" TO MESS
              PERFORM SEND-MESS
              GO TO MAIN-PROGRAM-EXIT.

      *=================================================================
      * PROCESS RISC1 GLOBAL FILES
      *=================================================================
           PERFORM PROCESS-FILE-AL.
           PERFORM PROCESS-FILE-BY.
           PERFORM PROCESS-FILE-CD.
           PERFORM PROCESS-FILE-CL.
           PERFORM PROCESS-FILE-GLOBAL-CP.
           PERFORM PROCESS-FILE-EA.
      *    PERFORM PROCESS-FILE-FS.
           PERFORM PROCESS-FILE-GI.
           PERFORM PROCESS-FILE-GL.
           PERFORM PROCESS-FILE-GT.
           PERFORM PROCESS-FILE-II.
           PERFORM PROCESS-FILE-IR.
           PERFORM PROCESS-FILE-LC.
           PERFORM PROCESS-FILE-PE.
           PERFORM PROCESS-FILE-PW.
           PERFORM PROCESS-FILE-RC.
           PERFORM PROCESS-FILE-SE.
           PERFORM PROCESS-FILE-SM.
           PERFORM PROCESS-FILE-SN.
           PERFORM PROCESS-FILE-SP.
           PERFORM PROCESS-FILE-SS.
           PERFORM PROCESS-FILE-TRWINFO.
           PERFORM PROCESS-FILE-TZ.
           PERFORM PROCESS-FILE-WR.

      *=================================================================
      * PROCESS RISC2 GLOBAL FILES TO MERGE/CONVERT
      *=================================================================

           MOVE "R2" TO SOURCE-MACHINE.
           MOVE "R1" TO TARGET-MACHINE.
           CALL "C$TOLOWER" USING SOURCE-MACHINE, VALUE 02.
           CALL "C$TOLOWER" USING TARGET-MACHINE, VALUE 02.

      * BRANCH STATS, MERGE RISC2
           PERFORM PROCESS-FILE-BY.

      * CODE FILE, "GC" AND "CN", IF AN R2 BRANCH, OVERWRITE IT
           PERFORM PROCESS-FILE-CD.

      * CAN LOANS, SKIPPING "OK" FROM RISC2, THOSE WERE THE ONLY DUPS
           PERFORM PROCESS-FILE-CL.

      * EXTRACTION, 79,586 DUPLICATES, IF AN R2 BRANCH, OVERWRITE IT
           PERFORM PROCESS-FILE-EA.

      * G/L CONTROL FILE, 2841 DUPLICATES, IF AN R2 BRANCH, OVERWRITE IT
           PERFORM PROCESS-FILE-GI.

      * INSURANCE RATE, 1 DUPLICATE, THEY ARE CHANGING 74 ON R2 TO BE 84
           PERFORM PROCESS-FILE-IR.

      * PASSWORD EXCEPTION, SKIP R2 DUPLICATES
           PERFORM PROCESS-FILE-PE.

      * DAILY STATUS FILE, 938 DIFFERENT ALL BRANCH 9990, SKIP THESE FROM RISC2
           PERFORM PROCESS-FILE-RC.

      * SPR MEMO, NO DUPLICATES
           PERFORM PROCESS-FILE-SM.

      * SPR, NO DUPLICATES
           PERFORM PROCESS-FILE-SP.

      * CREDIT SCORE STATS, NO DUPLICATES
           PERFORM PROCESS-FILE-SS.

      * TIME ZONE, NO DUPLICATES
           PERFORM PROCESS-FILE-TZ.

      * THESE SHOULD BE ALL DUPLICATES FROM RISC2 BUT IT MAKES ME NERVOUS
      * IF IT DOESNT HAPPEN AT EOM, SO JUST SKIP DUPLICATES.
      * KM - SINCE THIS IS STATS ONLY NOW, AND WE ARE RUNNING THE BYFILE
      * MERGE BEFORE CREATING THE WRFILE ON RISC1, IT APPEARS THEN WE HAVE
      * EVERYTHING WE NEED ON RISC1, SO JUST NEED TO CONVERT RISC1 AFTER
      * UPDATING BYFILE TO RISC1 AS PART OF EOM.
      *    PERFORM PROCESS-FILE-WR.

      *=================================================================
      * START LOCAL FILES USING BRANCH GROUP LOGIC
      *=================================================================

      * CURRENT-MACHINE WILL BE RISC1, "R1"
           MOVE CURRENT-MACHINE TO SOURCE-MACHINE TARGET-MACHINE.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *=================================================================
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

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE
                                              SOURCE-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * PROCESS LOCAL FILES FOR RISC1 & RISC2 BASED ON BR-REC
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM PROCESS-FILE-BM.
           PERFORM PROCESS-FILE-BW.
           PERFORM PROCESS-FILE-CA.
           PERFORM PROCESS-FILE-CI.
           PERFORM PROCESS-FILE-CJ.
           PERFORM PROCESS-FILE-CK.
           PERFORM PROCESS-FILE-LOCAL-CP.
           PERFORM PROCESS-FILE-DL.
           PERFORM PROCESS-FILE-DT.
           PERFORM PROCESS-FILE-GB.
           PERFORM PROCESS-FILE-IC.
           PERFORM PROCESS-FILE-IX.
           PERFORM PROCESS-FILE-LE.
           PERFORM PROCESS-FILE-LH.
           PERFORM PROCESS-FILE-LM.
           PERFORM PROCESS-FILE-LN.
           PERFORM PROCESS-FILE-LP.
           PERFORM PROCESS-FILE-LS.
           PERFORM PROCESS-FILE-LT.
           PERFORM PROCESS-FILE-LX.
           PERFORM PROCESS-FILE-TY.

           GO TO MAIN-PROGRAM-NEXT-GROUP.

      *=================================================================
       MAIN-PROGRAM-EXIT.

           PERFORM PRINT-RANGES.
           GO TO END-PROGRAM.
 
      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "UP15VY"      TO VDU-FORM-NAME.

           MOVE "X"           TO RECORDS-FOUND.

           MOVE UP15VY-QUEUE-POS           TO Q-POS.
           MOVE UP15VY-COPIES-POS          TO C-POS.
           MOVE UP15VY-PRU-POS             TO P-POS.
           MOVE SPACES                     TO Q-BUF  C-BUF  P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF ( BEG-BRANCH = ZEROES ) AND ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC     TO BEG-BRANCH

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
           MOVE EXT-CONAME-SYSDATE         TO H1-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD              TO TODAYS-DATE.
           MOVE EXT-CONAME-USER-ID         TO H2-USERID.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.


       EXIT-INITIALIZATION.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.
       ENTER-PARAMETERS-BEGIN.

           PERFORM SEND-FIL.

           MOVE SOURCE-PATH                TO WR-BUF.
           MOVE SOURCE-PATH-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE TARGET-PATH                TO WR-BUF.
           MOVE TARGET-PATH-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP-KEY )
              GO TO ENTER-OPTION-AL.

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
       ENTER-OPTION-AL.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-AL." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-AL.

           MOVE VDUBUF TO FILE-OPTION-AL.

           IF NOT ( FILE-OPTION-AL = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-AL.

       ENTER-OPTION-BY.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-BY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-AL.
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

       ENTER-OPTION-CD.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-CD." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-BY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-CD.

           MOVE VDUBUF TO FILE-OPTION-CD.

           IF NOT ( FILE-OPTION-CD = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-CD.

       ENTER-OPTION-CL.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-CL." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-CD.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-CL.

           MOVE VDUBUF TO FILE-OPTION-CL.

           IF NOT ( FILE-OPTION-CL = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-CL.

       ENTER-GLOBAL-CP.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010GLOBAL-CP." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-CL.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-GLOBAL-CP.

           MOVE VDUBUF TO FILE-GLOBAL-CP.

           IF NOT ( FILE-GLOBAL-CP = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-GLOBAL-CP.

       ENTER-OPTION-EA.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-EA." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-GLOBAL-CP.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-EA.

           MOVE VDUBUF TO FILE-OPTION-EA.

           IF NOT ( FILE-OPTION-EA = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-EA.

       ENTER-OPTION-FS.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-FS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-EA.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-FS.

           MOVE VDUBUF TO FILE-OPTION-FS.

           IF NOT ( FILE-OPTION-FS = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-FS.

       ENTER-OPTION-GI.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-GI." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-FS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-GI.

           MOVE VDUBUF TO FILE-OPTION-GI.

           IF NOT ( FILE-OPTION-GI = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-GI.

       ENTER-OPTION-GL.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-GL." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-GI.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-GL.

           MOVE VDUBUF TO FILE-OPTION-GL.

           IF NOT ( FILE-OPTION-GL = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-GL.

       ENTER-OPTION-GT.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-GT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-GL.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-GT.

           MOVE VDUBUF TO FILE-OPTION-GT.

           IF NOT ( FILE-OPTION-GT = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-GT.

       ENTER-OPTION-II.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-II." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-GT.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-II.

           MOVE VDUBUF TO FILE-OPTION-II.

           IF NOT ( FILE-OPTION-II = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-II.

       ENTER-OPTION-IR.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-IR." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-II.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-IR.

           MOVE VDUBUF TO FILE-OPTION-IR.

           IF NOT ( FILE-OPTION-IR = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-IR.


       ENTER-OPTION-LC.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-LC." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-IR.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-LC.

           MOVE VDUBUF TO FILE-OPTION-LC.

           IF NOT ( FILE-OPTION-LC = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-LC.

       ENTER-OPTION-PE.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-PE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-LC.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-PE.

           MOVE VDUBUF TO FILE-OPTION-PE.

           IF NOT ( FILE-OPTION-PE = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-PE.

       ENTER-OPTION-PW.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-PW." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-PE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-PW.

           MOVE VDUBUF TO FILE-OPTION-PW.

           IF NOT ( FILE-OPTION-PW = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-PW.

       ENTER-OPTION-RC.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-RC." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-PW.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-RC.

           MOVE VDUBUF TO FILE-OPTION-RC.

           IF NOT ( FILE-OPTION-RC = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-RC.

       ENTER-OPTION-SE.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-SE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-RC.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-SE.

           MOVE VDUBUF TO FILE-OPTION-SE.

           IF NOT ( FILE-OPTION-SE = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-SE.

       ENTER-OPTION-SM.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-SM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-SE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-SM.

           MOVE VDUBUF TO FILE-OPTION-SM.

           IF NOT ( FILE-OPTION-SM = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-SM.

       ENTER-OPTION-SN.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-SN." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-SM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-SN.

           MOVE VDUBUF TO FILE-OPTION-SN.

           IF NOT ( FILE-OPTION-SN = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-SN.


       ENTER-OPTION-SP.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-SP." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-SN.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-SP.

           MOVE VDUBUF TO FILE-OPTION-SP.

           IF NOT ( FILE-OPTION-SP = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-SP.

       ENTER-OPTION-SS.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-SS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-SP.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-SS.

           MOVE VDUBUF TO FILE-OPTION-SS.

           IF NOT ( FILE-OPTION-SS = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-SS.

       ENTER-OPTION-ST.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-ST." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-SS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-ST.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-ST.

           IF NOT ( FILE-OPTION-ST = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-ST.

       ENTER-OPTION-TRWINFO.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-TRW." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-ST.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-TRWINFO.

           MOVE VDUBUF TO FILE-OPTION-TRWINFO.

           IF NOT ( FILE-OPTION-TRWINFO = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-TRWINFO.

       ENTER-OPTION-TZ.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-TZ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-TRWINFO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-TZ.

           MOVE VDUBUF TO FILE-OPTION-TZ.

           IF NOT ( FILE-OPTION-TZ = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-TZ.

       ENTER-OPTION-WR.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-WR." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-TZ.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-WR.

           MOVE VDUBUF TO FILE-OPTION-WR.

           IF NOT ( FILE-OPTION-WR = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-WR.

       ENTER-OPTION-BR.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-BR." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-WR.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-BR.

           MOVE VDUBUF TO FILE-OPTION-BR.

           IF NOT ( FILE-OPTION-BR = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-BR.

       ENTER-OPTION-BM.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-BM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-BR.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-BM.

           MOVE VDUBUF TO FILE-OPTION-BM.

           IF NOT ( FILE-OPTION-BM = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-BM.

       ENTER-OPTION-BW.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-BW." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-BM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-BW.

           MOVE VDUBUF TO FILE-OPTION-BW.

           IF NOT ( FILE-OPTION-BW = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-BW.


       ENTER-OPTION-CA.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-CA." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-BW.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-CA.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-CA.

           IF NOT ( FILE-OPTION-CA = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-CA.

       ENTER-OPTION-CI.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-CI." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-CA.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-CI.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-CI.

           IF NOT ( FILE-OPTION-CI = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-CI.

       ENTER-OPTION-CJ.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-CJ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-CI.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-CJ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-CJ.

           IF NOT ( FILE-OPTION-CJ = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-CJ.

       ENTER-OPTION-CK.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-CK." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-CJ.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-CK.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-CK.

           IF NOT ( FILE-OPTION-CK = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-CK.

       ENTER-OPTION-CP.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-CP." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-CK.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-CP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-CP.

           IF NOT ( FILE-OPTION-CP = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-CP.

       ENTER-OPTION-CX.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-CX." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-CP.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-CX.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-CX.

           IF NOT ( FILE-OPTION-CX = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-CX.

       ENTER-OPTION-DL.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-DL." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-CX.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-DL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-DL.

           IF NOT ( FILE-OPTION-DL = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-DL.

       ENTER-OPTION-DT.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-DT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-DL.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-DT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-DT.

           IF NOT ( FILE-OPTION-DT = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-DT.

       ENTER-OPTION-GB.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-GB." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-DT.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-GB.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-GB.

           IF NOT ( FILE-OPTION-GB = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-GB.

       ENTER-OPTION-IC.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-IC." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-GB.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-IC.

           MOVE VDUBUF TO FILE-OPTION-IC.
           IF NOT ( FILE-OPTION-IC = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-IC.

       ENTER-OPTION-IX.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-IX." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-IC.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-IX.

           MOVE VDUBUF TO FILE-OPTION-IX.
           IF NOT ( FILE-OPTION-IX = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-IX.

       ENTER-OPTION-LE.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-LE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-IX.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-LE.

           MOVE VDUBUF TO FILE-OPTION-LE.
           IF NOT ( FILE-OPTION-LE = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-LE.

       ENTER-OPTION-LH.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-LH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-LE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-LH.

           MOVE VDUBUF TO FILE-OPTION-LH.
           IF NOT ( FILE-OPTION-LH = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-LH.

       ENTER-OPTION-LM.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-LM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-LH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-LM.

           MOVE VDUBUF TO FILE-OPTION-LM.
           IF NOT ( FILE-OPTION-LM = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-LM.

       ENTER-OPTION-LN.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-LN." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-LM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-LN.

           MOVE VDUBUF TO FILE-OPTION-LN.
           IF NOT ( FILE-OPTION-LN = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-LN.

       ENTER-OPTION-LP.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-LP." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-LN.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-LP.

           MOVE VDUBUF TO FILE-OPTION-LP.
           IF NOT ( FILE-OPTION-LP = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-LP.

       ENTER-OPTION-LS.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-LS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-LP.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-LS.

           MOVE VDUBUF TO FILE-OPTION-LS.
           IF NOT ( FILE-OPTION-LS = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-LS.

       ENTER-OPTION-LT.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-LT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-LS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-LT.

           MOVE VDUBUF TO FILE-OPTION-LT.
           IF NOT ( FILE-OPTION-LT = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-LT.

       ENTER-OPTION-LX.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-LX." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-LT.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-LX.

           MOVE VDUBUF TO FILE-OPTION-LX.
           IF NOT ( FILE-OPTION-LX = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-LX.

      *ENTER-OPTION-PA.

      *    MOVE LEGEND-OPTION-FILES TO LEGEND.
      *    PERFORM SEND-LEGEND.

      *    MOVE "E  A010OPTION-PA." TO VDU.
      *    PERFORM SCREENIO.

      *    IF (        UP-KEY ) GO TO ENTER-OPTION-LX.
      *    IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
      *    IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
      *    IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
      *    IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
      *    IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
      *    IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
      *    IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
      *    IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-PA.

      *    MOVE VDUBUF TO FILE-OPTION-PA.
      *    IF NOT ( FILE-OPTION-PA = "Y" OR "N" )
      *       PERFORM INVALID-YN-MESSAGE
      *       GO TO ENTER-OPTION-PA.

       ENTER-OPTION-RX.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-RX." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-LX.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-RX.

           MOVE VDUBUF TO FILE-OPTION-RX.
           IF NOT ( FILE-OPTION-RX = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-RX.

       ENTER-OPTION-TY.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-TY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-RX.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-TY.

           MOVE VDUBUF TO FILE-OPTION-TY.
           IF NOT ( FILE-OPTION-TY = "Y" OR "N" )
              PERFORM INVALID-YN-MESSAGE
              GO TO ENTER-OPTION-TY.

           GO TO ENTER-ACCEPT-YN.

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
              MOVE "N" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "Y" TO DEFAULT-LOCAL-OPTIONS.

      * GLOBALS
           MOVE "S  A010OPTION-AL."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-AL.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-BY."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-BY.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-CD."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-CD.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-CL."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-CL.
           PERFORM SCREENIO.
           MOVE "S  A010GLOBAL-CP."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-GLOBAL-CP.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-EA."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-EA.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-FS."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-FS.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-GI."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-GI.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-GL."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-GL.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-GT."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-GT.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-II."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-II.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-IR."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-IR.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-LC."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-LC.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-PE."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-PE.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-PW."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-PW.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-RC."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-RC.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-SE."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-SE.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-SN."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-SN.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-SM."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-SM.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-SP."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-SP.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-SS."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-SS.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-ST."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-ST.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-TRW."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-TRWINFO.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-TZ."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-TZ.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-WR."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-WR.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-BR."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-BR.
           PERFORM SCREENIO.
      * LOCALS
           MOVE "S  A010OPTION-BM."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-BM.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-BW."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-BW.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-CA."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-CA.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-CI."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-CI.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-CJ."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-CJ.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-CK."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-CK.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-CP."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-CP.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-CX."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-CX.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-DL."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-DL.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-DT."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-DT.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-GB."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-GB.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-IC."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-IC.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-IX."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-IX.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-LE."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-LE.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-LH."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-LH.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-LM."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-LM.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-LN."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-LN.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-LP."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-LP.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-LS."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-LS.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-LT."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-LT.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-LX."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-LX.
           PERFORM SCREENIO.
      *    MOVE "S  A010OPTION-PA."    TO VDU.
      *    MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-PA.
      *    PERFORM SCREENIO.
           MOVE "S  A010OPTION-RX."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-RX.
           PERFORM SCREENIO.
           MOVE "S  A010OPTION-TY."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS TO VDUBUF FILE-OPTION-TY.
           PERFORM SCREENIO.

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

           MOVE "S  A090S-PATH."          TO VDU.
           MOVE SOURCE-PATH               TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A090T-PATH."          TO VDU.
           MOVE TARGET-PATH               TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      ******************************************************************
       EO-EXEC-AUDIT SECTION.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    L O A D - B R A N C H - T A B L E 
      *
      *=================================================================
      * LOAD A BRANCH TABLE ONE TIME INDEXED BY BR-NO FOR BR-MACHINE
      ******************************************************************
        LOAD-BRANCH-TABLE SECTION.

           MOVE SPACES TO ERRCD.

           MOVE "B2"                       TO UP1534-CURRENT-FILE.
           MOVE "GB/B2FILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OB2-PATH.

           MOVE "BR"                       TO UP1534-CURRENT-FILE.
           MOVE "GB/BRFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OBR-PATH.

           PERFORM OPEN-OBR-FILE.
           PERFORM OPEN-OB21-FILE.

           MOVE 1 TO OBR-KEY.
           PERFORM START-OBR-FILE.

        LOAD-NEXT-BRANCH.
           PERFORM READ-OBR-FILE-NEXT.
           IF IO-BAD 
              MOVE SPACES TO ERRCD
              GO TO LOAD-BRANCH-TABLE-EXIT.

           MOVE OBR-MACHINE TO BRANCH-MACHINE(OBR-NO).

           IF ( FILE-OPTION-BR = "N" )
              GO TO LOAD-NEXT-BRANCH.

           IF OBR-PHONE NOT NUMERIC OR
               (OBR-PHONE = 2020202020 OR 0202020202)
              MOVE "BR-PHONE" TO D-FIELD
              MOVE OBR-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-RETAIN NOT NUMERIC OR
               (OBR-RETAIN = 20202020202 OR 02020202020)
              MOVE "BR-RETAIN" TO D-FIELD
              MOVE OBR-RETAIN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-NEXT-CHECK NOT NUMERIC OR
               (OBR-NEXT-CHECK = 202020 OR 020202)
              MOVE "BR-NEXT-CHECK" TO D-FIELD
              MOVE OBR-NEXT-CHECK  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              IF OBR-PRINTER(SUB) NOT NUMERIC 
                  MOVE "BR-PRINTER(SUB)" TO D-FIELD
                  MOVE OBR-PRINTER(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OBR-FEE-GL-BRNO NOT NUMERIC 
              MOVE "BR-FEE-GL-BRNO" TO D-FIELD
              MOVE OBR-FEE-GL-BRNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
      D    STOP MESS.
           IF OBR-INS-PRODUCER-NUM NOT NUMERIC 
              MOVE "BR-INS-PRODUCER-NUM" TO D-FIELD
              MOVE OBR-INS-PRODUCER-NUM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-BP-NEXT-BATCH NOT NUMERIC OR
               (OBR-BP-NEXT-BATCH = 20 OR 02)
              MOVE "BR-BP-NEXT-BATCH" TO D-FIELD
              MOVE OBR-BP-NEXT-BATCH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-BP-MAXCON NOT NUMERIC OR
               (OBR-BP-MAXCON = 202 OR 020)
              MOVE "BR-BP-MAXCON" TO D-FIELD
              MOVE OBR-BP-MAXCON  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-BP-MAXREC NOT NUMERIC OR
               (OBR-BP-MAXREC = 202 OR 020)
              MOVE "BR-BP-MAXREC" TO D-FIELD
              MOVE OBR-BP-MAXREC  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-BP-BRANCH NOT NUMERIC OR
               (OBR-BP-BRANCH = 20202 OR 02020)
              MOVE "BR-BP-BRANCH" TO D-FIELD
              MOVE OBR-BP-BRANCH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-LI-DLCOMM-CMSI-PP-MAX NOT NUMERIC 
              MOVE "BR-LI-DLCOMM-CMSI-PP-MAX" TO D-FIELD
              MOVE OBR-LI-DLCOMM-CMSI-PP-MAX  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 9
              IF OBR-OPEN-BAL(SUB) NOT NUMERIC  OR
                 (OBR-OPEN-BAL(SUB) = 2020202.02 OR
                                      0202020.20)
                  MOVE "BR-OPEN-BAL(SUB)" TO D-FIELD
                  MOVE OBR-OPEN-BAL(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OBR-DEFAULT-SC-CLASS NOT NUMERIC 
              MOVE "BR-DEFAULT-SC-CLASS" TO D-FIELD
              MOVE OBR-DEFAULT-SC-CLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-REGION-NUMBER NOT NUMERIC 
              MOVE "BR-REGION-NUMBER" TO D-FIELD
              MOVE OBR-REGION-NUMBER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              IF OBR-REB-METHOD(SUB) NOT NUMERIC  
                  MOVE "BR-REB-METHOD(SUB)" TO D-FIELD
                  MOVE OBR-REB-METHOD(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OBR-REB-METHOD-2(SUB) NOT NUMERIC  
                  MOVE "BR-REB-METHOD-2(SUB)" TO D-FIELD
                  MOVE OBR-REB-METHOD-2(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OBR-REBDAYS-MONTH(SUB) NOT NUMERIC  
                  MOVE "BR-REBDAYS-MONTH(SUB)" TO D-FIELD
                  MOVE OBR-REBDAYS-MONTH(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OBR-REBDAYS-MONTH-2(SUB) NOT NUMERIC  
                  MOVE "BR-REBDAYS-MONTH-2(SUB)" TO D-FIELD
                  MOVE OBR-REBDAYS-MONTH-2(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OBR-ACCRUAL-MONTHS(SUB) NOT NUMERIC  
                  MOVE "BR-ACCRUAL-MONTHS(SUB)" TO D-FIELD
                  MOVE OBR-ACCRUAL-MONTHS(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OBR-ACCRUAL-MONTHS-2(SUB) NOT NUMERIC  
                  MOVE "BR-ACCRUAL-MONTHS-2(SUB)" TO D-FIELD
                  MOVE OBR-ACCRUAL-MONTHS-2(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OBR-GLOVERSHORT NOT NUMERIC OR
               (OBR-GLOVERSHORT = 20202020202 OR 02020202020)
              MOVE "BR-GLOVERSHORT" TO D-FIELD
              MOVE OBR-GLOVERSHORT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-GLBANK NOT NUMERIC OR
               (OBR-GLBANK = 20202020202 OR 02020202020)
              MOVE "BR-GLBANK" TO D-FIELD
              MOVE OBR-GLBANK  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-GLONHAND NOT NUMERIC OR
               (OBR-GLONHAND = 20202020202 OR 02020202020)
              MOVE "BR-GLONHAND" TO D-FIELD
              MOVE OBR-GLONHAND  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-BANKBAL NOT NUMERIC OR
               (OBR-BANKBAL = 202020202.02 OR 020202020.20)
              MOVE "BR-BANKBAL" TO D-FIELD
              MOVE OBR-BANKBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-RETAIN-AMT NOT NUMERIC OR
               (OBR-RETAIN-AMT = 202020202.02 OR 020202020.20)
              MOVE "BR-RETAIN-AMT" TO D-FIELD
              MOVE OBR-RETAIN-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-LI-DLCOMM-CMSI-O3-MAX NOT NUMERIC 
              MOVE "BR-LI-DLCOMM-CMSI-O3-MAX" TO D-FIELD
              MOVE OBR-LI-DLCOMM-CMSI-O3-MAX  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-BANK-ROUTING-NO NOT NUMERIC OR
               (OBR-BANK-ROUTING-NO = 202020202 OR 020202020)
              MOVE "BR-BANK-ROUTING-NO" TO D-FIELD
              MOVE OBR-BANK-ROUTING-NO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-BANK-CLEARING-GL NOT NUMERIC OR
               (OBR-BANK-CLEARING-GL = 20202020202 OR 02020202020)
              MOVE "BR-BANK-CLEARING-GL" TO D-FIELD
              MOVE OBR-BANK-CLEARING-GL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-GLBANK-DEPOSIT NOT NUMERIC OR
               (OBR-GLBANK-DEPOSIT = 20202020202 OR 02020202020)
              MOVE "BR-GLBANK-DEPOSIT" TO D-FIELD
              MOVE OBR-GLBANK-DEPOSIT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-DEFAULT-SC-SUBCLASS NOT NUMERIC 
              MOVE "BR-DEFAULT-SC-SUBCLASS" TO D-FIELD
              MOVE OBR-DEFAULT-SC-SUBCLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-BYINQ-START-PAGE NOT NUMERIC 
              MOVE "BR-BYINQ-START-PAGE" TO D-FIELD
              MOVE OBR-BYINQ-START-PAGE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-DSINQ-START-PAGE NOT NUMERIC 
              MOVE "BR-DSINQ-START-PAGE" TO D-FIELD
              MOVE OBR-DSINQ-START-PAGE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-TIME-ZONE-FACTOR NOT NUMERIC 
              MOVE "BR-TIME-ZONE-FACTOR" TO D-FIELD
              MOVE OBR-TIME-ZONE-FACTOR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-NO-FT-EMP NOT NUMERIC OR
               (OBR-NO-FT-EMP = 202 OR 020)
              MOVE "BR-NO-FT-EMP" TO D-FIELD
              MOVE OBR-NO-FT-EMP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-NO-PT-EMP NOT NUMERIC OR
               (OBR-NO-PT-EMP = 20.20 OR 02.02)
              MOVE "BR-NO-PT-EMP" TO D-FIELD
              MOVE OBR-NO-PT-EMP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-NODEF-1ST-X-MONTHS NOT NUMERIC 
              MOVE "BR-NODEF-1ST-X-MONTHS" TO D-FIELD
              MOVE OBR-NODEF-1ST-X-MONTHS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-NEXT-SOLICIT-NO NOT NUMERIC OR
               (OBR-NEXT-SOLICIT-NO = 202020 OR 020202)
              MOVE "BR-NEXT-SOLICIT-NO" TO D-FIELD
              MOVE OBR-NEXT-SOLICIT-NO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-FAX-NUMBER NOT NUMERIC OR
               (OBR-FAX-NUMBER = 2020202020 OR 0202020202)
              MOVE "BR-FAX-NUMBER" TO D-FIELD
              MOVE OBR-FAX-NUMBER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

      * NOT USED
      *  BR-REVOLVING-CONV-DATE    PIC 9(6)     COMP-3.

           IF OBR-TAX-PREBOOK-CUST NOT NUMERIC OR
               (OBR-TAX-PREBOOK-CUST = 2020 OR 0202)
              MOVE "BR-TAX-PREBOOK-CUST" TO D-FIELD
              MOVE OBR-TAX-PREBOOK-CUST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-TAX-PREBOOK-NONCUST NOT NUMERIC OR
               (OBR-TAX-PREBOOK-NONCUST = 2020 OR 0202)
              MOVE "BR-TAX-PREBOOK-NONCUST" TO D-FIELD
              MOVE OBR-TAX-PREBOOK-NONCUST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-LBOX-MAXCON NOT NUMERIC OR
               (OBR-LBOX-MAXCON = 202 OR 020)
              MOVE "BR-LBOX-MAXCON" TO D-FIELD
              MOVE OBR-LBOX-MAXCON  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBR-LBOX-MAXREC NOT NUMERIC OR
               (OBR-LBOX-MAXREC = 202 OR 020)
              MOVE "BR-LBOX-MAXREC" TO D-FIELD
              MOVE OBR-LBOX-MAXREC  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           MOVE OBR-NO TO OB2-BRNO.
           PERFORM READ-OB21-FILE.
           IF IO-BAD
              GO TO LOAD-NEXT-BRANCH.

           IF OB2-COLLECT-PHONE NOT NUMERIC OR
               (OB2-COLLECT-PHONE = 2020202020 OR 0202020202)
              MOVE "B2-COLLECT-PHONE" TO D-FIELD
              MOVE OB2-COLLECT-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OB2-COLLECT-EXT NOT NUMERIC OR
               (OB2-COLLECT-EXT = 2020 OR 0202)
              MOVE "B2-COLLECT-EXT" TO D-FIELD
              MOVE OB2-COLLECT-EXT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OB2-MIN-MONTHS-DEF NOT NUMERIC 
              MOVE "B2-MIN-MONTHS-DEF" TO D-FIELD
              MOVE OB2-MIN-MONTHS-DEF  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OB2-MIN-MONTHS-D2 NOT NUMERIC 
              MOVE "B2-MIN-MONTHS-D2" TO D-FIELD
              MOVE OB2-MIN-MONTHS-D2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OB2-GL-ESCROW-BALANCE NOT NUMERIC OR
             (OB2-GL-ESCROW-BALANCE = 20202020202 OR 02020202020)
              MOVE "B2-GL-ESCROW-BALANCE" TO D-FIELD
              MOVE OB2-GL-ESCROW-BALANCE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OB2-GL-ESCROW-PAYABLE NOT NUMERIC OR
             (OB2-GL-ESCROW-PAYABLE = 20202020202 OR 02020202020)
              MOVE "B2-GL-ESCROW-PAYABLE" TO D-FIELD
              MOVE OB2-GL-ESCROW-PAYABLE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OB2-ACH-BANK-NO NOT NUMERIC 
              MOVE "B2-ACH-BANK-NO" TO D-FIELD
              MOVE OB2-ACH-BANK-NO  TO D-VALUEX
              PERFORM WRITE-DETAIL-LINE.

           IF OB2-CHECK-DEPOSIT-GL NOT NUMERIC OR
             (OB2-CHECK-DEPOSIT-GL = 20202020202 OR 02020202020)
              MOVE "B2-CHECK-DEPOSIT-GL" TO D-FIELD
              MOVE OB2-CHECK-DEPOSIT-GL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OB2-GL-DEBIT-CARD-PROCEEDS NOT NUMERIC OR
             (OB2-GL-DEBIT-CARD-PROCEEDS = 20202020202 OR
                                           02020202020)
              MOVE "B2-GL-DEBIT-CARD-PROCEEDS" TO D-FIELD
              MOVE OB2-GL-DEBIT-CARD-PROCEEDS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OB2-WKDAY-BANKID NOT NUMERIC OR
             (OB2-WKDAY-BANKID = 2020 OR 0202)
              MOVE "B2-WKDAY-BANKID" TO D-FIELD
              MOVE OB2-WKDAY-BANKID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO LOAD-NEXT-BRANCH.

       LOAD-BRANCH-TABLE-EXIT.
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

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OBM-PATH.

           MOVE ZEROES                     TO OBM-SSNO
                                              OBM-DATE
                                              OBM-SEQ.

           PERFORM OPEN-OBM1-FILE.
           PERFORM START-OBM1-FILE.

       PROCESS-FILE-BM-NEXT.
           PERFORM READ-OBM1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-BM-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OBM-SSNO NOT NUMERIC
              MOVE "BM-SSNO" TO D-FIELD
              MOVE OBM-SSNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
      * CANT TEST VALID DATE, SUBTRACTED FROM 9'S
           IF OBM-DATE NOT NUMERIC
              MOVE "BM-DATE" TO D-FIELD
              MOVE OBM-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBM-SEQ NOT NUMERIC
              MOVE "BM-SEQ" TO D-FIELD
              MOVE OBM-SEQ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

      * BM-MEMO PIC X(60) AND BM-MEMO-TYPE PIC X, REALLY CAN HAVE ANYTHING
      * DONT KNOW HOW TO TEST FOR JUST GARBAGE, THEY CAN USE # SIGN, $ %
           GO TO PROCESS-FILE-BM-NEXT.

       PROCESS-FILE-BM-END.

           PERFORM CLOSE-OBM1-FILE.

       PROCESS-FILE-BM-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - B W
      *
      ******************************************************************
       PROCESS-FILE-BW SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-BW = "N" )
              GO TO PROCESS-FILE-BW-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO BW-PATH-OVERRIDE.
           MOVE "BW"                       TO UP1534-CURRENT-FILE.
           MOVE "GB/BWFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OBW-PATH.

           MOVE 0 TO OBW-SSNO.
           PERFORM OPEN-OBW1-FILE.
           PERFORM START-OBW1-FILE.

       PROCESS-FILE-BW-NEXT.
           PERFORM READ-OBW1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-BW-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OBW-SSNO NOT NUMERIC
              MOVE "BW-SSNO" TO D-FIELD
              MOVE OBW-SSNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-REFDATE NOT NUMERIC 
              MOVE "BW-REFDATE" TO D-FIELD
              MOVE OBW-REFDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OBW-REFDATE NOT = 0
                 MOVE OBW-REFDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "BW-REFDATE" TO D-FIELD
                    MOVE OBW-REFDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OBW-AGE NOT NUMERIC 
              MOVE "BW-AGE" TO D-FIELD
              MOVE OBW-AGE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-PHONE NOT NUMERIC OR
               (OBW-PHONE = 20202020200202)
              MOVE "BW-PHONE" TO D-FIELD
              MOVE OBW-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-DEPEND NOT NUMERIC OR
               (OBW-DEPEND = 20 )
              MOVE "BW-DEPEND" TO D-FIELD
              MOVE OBW-DEPEND  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-ORGSLSMN NOT NUMERIC OR
               (OBW-ORGSLSMN = 202020)
              MOVE "BW-ORGSLSMN" TO D-FIELD
              MOVE OBW-ORGSLSMN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-SPSSNO NOT NUMERIC OR
               (OBW-SPSSNO = 202020202020)
              MOVE "BW-SPSSNO" TO D-FIELD
              MOVE OBW-SPSSNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-EMPPHONE NOT NUMERIC OR
               (OBW-EMPPHONE = 20202020200202)
              MOVE "BW-EMPPHONE" TO D-FIELD
              MOVE OBW-EMPPHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-EMPEXT NOT NUMERIC
              MOVE "BW-EMPEXT" TO D-FIELD
              MOVE OBW-EMPEXT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-EMPYEARS NOT NUMERIC 
              MOVE "BW-EMPYEARS" TO D-FIELD
              MOVE OBW-EMPYEARS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-EMPPAY NOT NUMERIC OR
               (OBW-EMPPAY = 202020.20 OR 020202.02)
              MOVE "BW-EMPPAY" TO D-FIELD
              MOVE OBW-EMPPAY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-EMPPAYOT NOT NUMERIC OR
               (OBW-EMPPAYOT = 202020.20 OR 020202.02)
              MOVE "BW-EMPPAYOT" TO D-FIELD
              MOVE OBW-EMPPAYOT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-LOANS NOT NUMERIC 
              MOVE "BW-LOANS" TO D-FIELD
              MOVE OBW-LOANS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OBW-INTYTD NOT NUMERIC OR
               (OBW-INTYTD = 2020202.02 OR 0202020.20)
              MOVE "BW-INTYTD" TO D-FIELD
              MOVE OBW-INTYTD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-INTLYR NOT NUMERIC OR
               (OBW-INTLYR = 2020202.02 OR 0202020.20)
              MOVE "BW-INTLYR" TO D-FIELD
              MOVE OBW-INTLYR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-LIASMLN NOT NUMERIC OR
               (OBW-LIASMLN = 2020202.02 OR 0202020.20)
              MOVE "BW-LIASMLN" TO D-FIELD
              MOVE OBW-LIASMLN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-LIAOTH NOT NUMERIC OR
               (OBW-LIAOTH = 2020202.02 OR 0202020.20)
              MOVE "BW-LIAOTH" TO D-FIELD
              MOVE OBW-LIAOTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-LSTPODATE NOT NUMERIC 
              MOVE "BW-LSTPODATE" TO D-FIELD
              MOVE OBW-LSTPODATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OBW-LSTPODATE NOT = 0
                 MOVE OBW-LSTPODATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "BW-LSTPODATE" TO D-FIELD
                    MOVE OBW-LSTPODATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OBW-SPAGE NOT NUMERIC OR
               (OBW-SPAGE = 202020)
              MOVE "BW-SPAGE" TO D-FIELD
              MOVE OBW-SPAGE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-PRLOANS NOT NUMERIC 
              MOVE "BW-PRLOANS" TO D-FIELD
              MOVE OBW-PRLOANS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-RE-INTYTD NOT NUMERIC OR
               (OBW-RE-INTYTD = 20202.02 OR 02020.20)
              MOVE "BW-RE-INTYTD" TO D-FIELD
              MOVE OBW-RE-INTYTD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-RE-INTLYR NOT NUMERIC OR
               (OBW-RE-INTLYR = 20202.02 OR 02020.20)
              MOVE "BW-RE-INTLYR" TO D-FIELD
              MOVE OBW-RE-INTLYR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-CREDIT-SCORE NOT NUMERIC OR
               (OBW-CREDIT-SCORE = 2020 OR 0202)
              MOVE "BW-CREDIT-SCORE" TO D-FIELD
              MOVE OBW-CREDIT-SCORE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-LAST-FORM-NO NOT NUMERIC OR
               (OBW-LAST-FORM-NO = 202 OR 020)
              MOVE "BW-LAST-FORM-NO" TO D-FIELD
              MOVE OBW-LAST-FORM-NO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-LAST-FORM-DATE NOT NUMERIC 
              MOVE "BW-LAST-FORM-DATE" TO D-FIELD
              MOVE OBW-LAST-FORM-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OBW-LAST-FORM-DATE NOT = 0
                 MOVE OBW-LAST-FORM-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "BW-LAST-FORM-DATE" TO D-FIELD
                    MOVE OBW-LAST-FORM-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OBW-ADR-PHONE-CHG-DATE NOT NUMERIC 
              MOVE "BW-ADR-PHONE-CHG-DATE" TO D-FIELD
              MOVE OBW-ADR-PHONE-CHG-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OBW-ADR-PHONE-CHG-DATE NOT = 0
                 MOVE OBW-ADR-PHONE-CHG-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "BW-ADR-PHONE-CHG-DATE" TO D-FIELD
                    MOVE OBW-ADR-PHONE-CHG-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OBW-PRIOR-RN-FOR10 NOT NUMERIC OR
               (OBW-PRIOR-RN-FOR10 = 202020)
              MOVE "BW-PRIOR-RN-FOR10" TO D-FIELD
              MOVE OBW-PRIOR-RN-FOR10  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-LAST-RN-FOR10 NOT NUMERIC OR
               (OBW-LAST-RN-FOR10 = 202020)
              MOVE "BW-LAST-RN-FOR10" TO D-FIELD
              MOVE OBW-LAST-RN-FOR10  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.


           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OBW-MISC-PHONE(SUB) NOT NUMERIC OR
                 (OBW-MISC-PHONE(SUB) = 2020202020 OR 0202020202)
                  MOVE "BW-MISC-PHONE(SUB)" TO D-FIELD
                  MOVE OBW-MISC-PHONE(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OBW-MISC-EXT(SUB) NOT NUMERIC OR
                 (OBW-MISC-EXT(SUB) = 2020 OR 0202)
                  MOVE "BW-MISC-EXT(SUB)" TO D-FIELD
                  MOVE OBW-MISC-EXT(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OBW-CREDIT-LIMIT NOT NUMERIC OR
               (OBW-CREDIT-LIMIT = 202020.20 OR 020202.02)
              MOVE "BW-CREDIT-LIMIT" TO D-FIELD
              MOVE OBW-CREDIT-LIMIT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-CELL-PHONE NOT NUMERIC OR
               (OBW-CELL-PHONE = 2020202020 OR 0202020202)
              MOVE "BW-CELL-PHONE" TO D-FIELD
              MOVE OBW-CELL-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBW-DEALTRACK-CHG-DATE NOT NUMERIC 
              MOVE "BW-DEALTRACK-CHG-DATE" TO D-FIELD
              MOVE OBW-DEALTRACK-CHG-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OBW-DEALTRACK-CHG-DATE NOT = 0
                 MOVE OBW-DEALTRACK-CHG-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "BW-DEALTRACK-CHG-DATE" TO D-FIELD
                    MOVE OBW-DEALTRACK-CHG-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.


           GO TO PROCESS-FILE-BW-NEXT.

       PROCESS-FILE-BW-END.

           PERFORM CLOSE-OBW1-FILE.

       PROCESS-FILE-BW-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - C A
      *
      ******************************************************************
       PROCESS-FILE-CA SECTION.
           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-CA = "N" )
              GO TO PROCESS-FILE-CA-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO CA-PATH-OVERRIDE.
           MOVE "CA"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/CAFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OCA-PATH.

           MOVE 0 TO OCA-BRNO OCA-DATE OCA-TILLNO.
           PERFORM OPEN-OCA1-FILE.
           PERFORM START-OCA1-FILE.

       PROCESS-FILE-CA-NEXT.
           PERFORM READ-OCA1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-CA-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OCA-BRNO NOT NUMERIC
              MOVE "CA-BRNO" TO D-FIELD
              MOVE OCA-BRNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCA-DATE NOT NUMERIC 
              MOVE "CA-DATE" TO D-FIELD
              MOVE OCA-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OCA-DATE NOT = 0
                 MOVE OCA-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "CA-DATE" TO D-FIELD
                    MOVE OCA-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OCA-TILLNO NOT ALPHABETIC  
              IF OCA-TILLNO NOT NUMERIC
                 MOVE "CA-TILLNO" TO D-FIELD
                 MOVE OCA-TILLNO  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE.

      * CA-TILLNO-ZERO PIC 9, NOT USED IN UPGRADE
      * CA-DESC        PIC X(30). CAN BE ANYTHING
      * CA-PASSWORD    PIC X(4). CAN BE ANYTHING

           IF OCA-BEGBAL NOT NUMERIC OR
               (OCA-BEGBAL = 2020202.02 OR 0202020.20)
              MOVE "CA-BEGBAL" TO D-FIELD
              MOVE OCA-BEGBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCA-OPENED NOT NUMERIC 
              MOVE "CA-OPENED" TO D-FIELD
              MOVE OCA-OPENED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCA-CLOSED NOT NUMERIC 
              MOVE "CA-CLOSED" TO D-FIELD
              MOVE OCA-CLOSED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCA-ENDBAL NOT NUMERIC OR
               (OCA-ENDBAL = 2020202.02 OR 0202020.20)
              MOVE "CA-ENDBAL" TO D-FIELD
              MOVE OCA-ENDBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCA-RUNNING NOT NUMERIC OR
               (OCA-RUNNING = 2020202.02 OR 0202020.20)
              MOVE "CA-RUNNING" TO D-FIELD
              MOVE OCA-RUNNING  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCA-DEPOSIT NOT NUMERIC OR
               (OCA-DEPOSIT = 2020202.02 OR 0202020.20)
              MOVE "CA-DEPOSIT" TO D-FIELD
              MOVE OCA-DEPOSIT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCA-NET-CASH-NOTES NOT NUMERIC OR
               (OCA-NET-CASH-NOTES = 2020202.02 OR 0202020.20)
              MOVE "CA-NET-CASH-NOTES" TO D-FIELD
              MOVE OCA-NET-CASH-NOTES  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCA-CHECK-DEPOSIT NOT NUMERIC OR
               (OCA-CHECK-DEPOSIT = 2020202.02 OR 0202020.20)
              MOVE "CA-CHECK-DEPOSIT" TO D-FIELD
              MOVE OCA-CHECK-DEPOSIT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-CA-NEXT.

       PROCESS-FILE-CA-END.

           PERFORM CLOSE-OCA1-FILE.

       PROCESS-FILE-CA-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - C I
      *
      ******************************************************************
       PROCESS-FILE-CI SECTION.
           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-CI = "N" )
              GO TO PROCESS-FILE-CI-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO CI-PATH-OVERRIDE.
           MOVE "CI"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/CIFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OCI-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE SPACES TO UP1534-CURRENT-FILE
              MOVE "CI"        TO D-FILE
              MOVE "MISSING"   TO D-KEY
              MOVE "CIFILE"    TO D-FIELD
              MOVE 0           TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
              GO TO PROCESS-FILE-CI-END.

           MOVE 0                     TO OCI-ACCTNO.
           PERFORM OPEN-OCI1-FILE.
           PERFORM START-OCI1-FILE.

       PROCESS-FILE-CI-NEXT.
           PERFORM READ-OCI1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-CI-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OCI-ACCTNO NOT NUMERIC 
              MOVE "CI-ACCTNO" TO D-FIELD
              MOVE OCI-ACCTNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCI-SSNO NOT NUMERIC 
              MOVE "CI-SSNO" TO D-FIELD
              MOVE OCI-SSNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCI-SSNO-2 NOT NUMERIC 
              MOVE "CI-SSNO-2" TO D-FIELD
              MOVE OCI-SSNO-2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
      * MMDDYYYY TRULY STORED AS 8 DIGIT DATE
           IF OCI-DATE-UPDATED NOT NUMERIC OR
               (OCI-DATE-UPDATED = 20202020 OR 02020202)
              MOVE "CI-DATE-UPDATED" TO D-FIELD
              MOVE OCI-DATE-UPDATED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OCI-DATE-UPDATED NOT = 0
                 MOVE OCI-DATE-UPDATED TO DATE-MMDDYYYY
                 PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "CI-DATE-UPDATED" TO D-FIELD
                    MOVE OCI-DATE-UPDATED  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OCI-AMT-REQUESTED NOT NUMERIC  OR
                (OCI-AMT-REQUESTED = 2020202 OR 0202020)
              MOVE "CI-AMT-REQUESTED" TO D-FIELD
              MOVE OCI-AMT-REQUESTED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCI-MARITAL-STAT NOT ALPHABETIC 
              IF OCI-MARITAL-STAT NOT NUMERIC
                 MOVE "CI-MARITAL-STAT" TO D-FIELD
                 MOVE OCI-MARITAL-STAT  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE.
           IF OCI-YEARS-IN-AREA NOT NUMERIC 
              MOVE "CI-YEARS-IN-AREA" TO D-FIELD
              MOVE OCI-YEARS-IN-AREA  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
      * MMDDYYYY TRULY STORED AS 8 DIGIT DATE
           IF OCI-DATE-AT-PRESENT-ADR NOT NUMERIC OR
               (OCI-DATE-AT-PRESENT-ADR = 20202020 OR 02020202)
              MOVE "CI-DATE-AT-PRESENT-ADR" TO D-FIELD
              MOVE OCI-DATE-AT-PRESENT-ADR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OCI-DATE-AT-PRESENT-ADR NOT = 0
                 MOVE OCI-DATE-AT-PRESENT-ADR TO DATE-MMDDYYYY
                 PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "CI-DATE-AT-PRESENT-ADR" TO D-FIELD
                    MOVE OCI-DATE-AT-PRESENT-ADR  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OCI-MONTHLY-RENT-MTG NOT NUMERIC  OR
                (OCI-MONTHLY-RENT-MTG = 2020202 OR 0202020)
              MOVE "CI-MONTHLY-RENT-MTG" TO D-FIELD
              MOVE OCI-MONTHLY-RENT-MTG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCI-FAIR-MARKET-VALUE NOT NUMERIC  OR
                (OCI-FAIR-MARKET-VALUE = 2020202 OR 0202020)
              MOVE "CI-FAIR-MARKET-VALUE" TO D-FIELD
              MOVE OCI-FAIR-MARKET-VALUE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCI-MTG-BALANCE NOT NUMERIC  OR
                (OCI-MTG-BALANCE = 2020202 OR 0202020)
              MOVE "CI-MTG-BALANCE" TO D-FIELD
              MOVE OCI-MTG-BALANCE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCI-CHECKING-ACCOUNT NOT ALPHABETIC
              MOVE "CI-CHECKING-ACCOUNT" TO D-FIELD
              MOVE OCI-CHECKING-ACCOUNT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCI-SAVINGS-ACCOUNT NOT ALPHABETIC
              MOVE "CI-SAVINGS-ACCOUNT" TO D-FIELD
              MOVE OCI-SAVINGS-ACCOUNT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCI-TOTAL-MO-INCOME NOT NUMERIC  OR
                (OCI-TOTAL-MO-INCOME = 2020202 OR 0202020)
              MOVE "CI-TOTAL-MO-INCOME" TO D-FIELD
              MOVE OCI-TOTAL-MO-INCOME  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCI-LESS-MO-PAY NOT NUMERIC  OR
                (OCI-LESS-MO-PAY = 2020202 OR 0202020)
              MOVE "CI-LESS-MO-PAY" TO D-FIELD
              MOVE OCI-LESS-MO-PAY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
      * MMDDYYYY TRULY STORED AS 8 DIGIT DATE
           IF OCI-DATE-HIRE-1 NOT NUMERIC OR
               (OCI-DATE-HIRE-1 = 20202020 OR 02020202)
              MOVE "CI-DATE-HIRE-1" TO D-FIELD
              MOVE OCI-DATE-HIRE-1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OCI-DATE-HIRE-1 NOT = 0
                 MOVE OCI-DATE-HIRE-1 TO DATE-MMDDYYYY
                 PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "CI-DATE-HIRE-1" TO D-FIELD
                    MOVE OCI-DATE-HIRE-1  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
      * BR 18 ACCT 216, HAS "10   " AND ALPAHBETIC AND NUMERIC DONT WORK
      *    IF OCI-POSITION-CODE-1 NOT ALPHABETIC 
      *          MOVE "CI-POSITION-CODE-1" TO D-FIELD
      *          MOVE OCI-POSITION-CODE-1  TO D-VALUE
      *          PERFORM WRITE-DETAIL-LINE.
           IF OCI-GROSS-MO-INCOME-1 NOT NUMERIC 
              MOVE "CI-GROSS-MO-INCOME-1" TO D-FIELD
              MOVE OCI-GROSS-MO-INCOME-1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

      * MMDDYYYY TRULY STORED AS 8 DIGIT DATE
           IF OCI-DATE-HIRE-2 NOT NUMERIC OR
               (OCI-DATE-HIRE-2 = 20202020 OR 02020202)
              MOVE "CI-DATE-HIRE-2" TO D-FIELD
              MOVE OCI-DATE-HIRE-2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OCI-DATE-HIRE-2 NOT = 0
                 MOVE OCI-DATE-HIRE-2 TO DATE-MMDDYYYY
                 PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "CI-DATE-HIRE-2" TO D-FIELD
                    MOVE OCI-DATE-HIRE-2  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
      *
      *    IF OCI-POSITION-CODE-2 NOT ALPHABETIC 
      *       IF OCI-POSITION-CODE-2 NOT NUMERIC
      *          MOVE "CI-POSITION-CODE-2" TO D-FIELD
      *          MOVE OCI-POSITION-CODE-2  TO D-VALUE
      *          PERFORM WRITE-DETAIL-LINE.
           IF OCI-GROSS-MO-INCOME-2 NOT NUMERIC 
              MOVE "CI-GROSS-MO-INCOME-2" TO D-FIELD
              MOVE OCI-GROSS-MO-INCOME-2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-CI-NEXT.

       PROCESS-FILE-CI-END.

           PERFORM CLOSE-OCI1-FILE.

       PROCESS-FILE-CI-EXIT.
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

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OCJ-PATH.

           MOVE SPACES                     TO OCJ1-KEY.
           MOVE ZEROES                     TO OCJ-BRNO
                                              OCJ-DATE
                                              OCJ-START-TIME
                                              OCJ-SEQNO.

           PERFORM OPEN-OCJ1-FILE.
           PERFORM START-OCJ1-FILE.

       PROCESS-FILE-CJ-NEXT.

           PERFORM READ-OCJ1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-CJ-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OCJ-BRNO NOT NUMERIC
              MOVE "CJ-BRNO" TO D-FIELD
              MOVE OCJ-BRNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
      *    IF OCJ-COLLID NOT ALPHABETIC 
      *       IF OCJ-COLLID NOT NUMERIC
      *          MOVE "CJ-COLLID" TO D-FIELD
      *          MOVE OCJ-COLLID  TO D-VALUE
      *          PERFORM WRITE-DETAIL-LINE.
           IF OCJ-DATE NOT NUMERIC OR
               (OCJ-DATE = 20202 OR 02020)
              MOVE "CJ-DATE" TO D-FIELD
              MOVE OCJ-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OCJ-DATE NOT = 0
                 MOVE OCJ-DATE TO JULIAN-DATE
                 PERFORM CJUL
                 MOVE NUM-DATE TO DATE-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "CJ-DATE" TO D-FIELD
                    MOVE OCJ-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OCJ-START-TIME NOT NUMERIC
              MOVE "CJ-START-TIME" TO D-FIELD
              MOVE OCJ-START-TIME  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCJ-SEQNO NOT NUMERIC
              MOVE "CJ-SEQNO" TO D-FIELD
              MOVE OCJ-SEQNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCJ-ACCTNO NOT NUMERIC
              MOVE "CJ-ACCTNO" TO D-FIELD
              MOVE OCJ-ACCTNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCJ-END-TIME NOT NUMERIC
              MOVE "CJ-END-TIME" TO D-FIELD
              MOVE OCJ-END-TIME  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
      * THESE SOMETIMES HAVE # AND * AND ?
      *    IF OCJ-ACTIVITY-CD NOT ALPHABETIC 
      *       IF OCJ-ACTIVITY-CD NOT NUMERIC
      *          MOVE "CJ-ACTIVITY-CD" TO D-FIELD
      *          MOVE OCJ-ACTIVITY-CD  TO D-VALUE
      *          PERFORM WRITE-DETAIL-LINE.
      *    IF OCJ-RESULT-CD NOT ALPHABETIC 
      *       IF OCJ-RESULT-CD NOT NUMERIC
      *          MOVE "CJ-RESULT-CD" TO D-FIELD
      *          MOVE OCJ-RESULT-CD  TO D-VALUE
      *          PERFORM WRITE-DETAIL-LINE.
      *    IF OCJ-ACTION-CD NOT ALPHABETIC 
      *       IF OCJ-ACTION-CD NOT NUMERIC
      *          MOVE "CJ-ACTION-CD" TO D-FIELD
      *          MOVE OCJ-ACTION-CD  TO D-VALUE
      *          PERFORM WRITE-DETAIL-LINE.
      *    IF OCJ-NEW-COLLID NOT ALPHABETIC 
      *       IF OCJ-NEW-COLLID NOT NUMERIC
      *          MOVE "CJ-NEW-COLLID" TO D-FIELD
      *          MOVE OCJ-NEW-COLLID  TO D-VALUE
      *          PERFORM WRITE-DETAIL-LINE.
      *    IF OCJ-CON-STAT NOT ALPHABETIC 
      *       IF OCJ-CON-STAT NOT NUMERIC
      *          MOVE "CJ-CON-STAT" TO D-FIELD
      *          MOVE OCJ-CON-STAT  TO D-VALUE
      *          PERFORM WRITE-DETAIL-LINE.
      *    IF OCJ-REC-STAT NOT ALPHABETIC 
      *       IF OCJ-REC-STAT NOT NUMERIC
      *          MOVE "CJ-REC-STAT" TO D-FIELD
      *          MOVE OCJ-REC-STAT  TO D-VALUE
      *          PERFORM WRITE-DETAIL-LINE.
           IF OCJ-FORM-NO NOT NUMERIC 
              MOVE "CJ-FORM-NO" TO D-FIELD
              MOVE OCJ-FORM-NO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCJ-PROMISE-DATE1 NOT NUMERIC OR
               (OCJ-PROMISE-DATE1 = 202020)
              MOVE "CJ-PROMISE-DATE1" TO D-FIELD
              MOVE OCJ-PROMISE-DATE1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OCJ-PROMISE-DATE1 NOT = 0
                 MOVE OCJ-PROMISE-DATE1 TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "CJ-PROMISE-DATE1" TO D-FIELD
                    MOVE OCJ-PROMISE-DATE1  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OCJ-PROMISE-AMT NOT NUMERIC OR
               (OCJ-PROMISE-AMT = 020202.02 OR 202020.20)
              MOVE "CJ-PROMISE-AMT" TO D-FIELD
              MOVE OCJ-PROMISE-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCJ-BEG-LM-SEQ NOT NUMERIC 
              MOVE "CJ-BEG-LM-SEQ" TO D-FIELD
              MOVE OCJ-BEG-LM-SEQ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCJ-END-LM-SEQ NOT NUMERIC 
              MOVE "CJ-END-LM-SEQ" TO D-FIELD
              MOVE OCJ-END-LM-SEQ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCJ-PROMISE-DATE2 NOT NUMERIC OR
               (OCJ-PROMISE-DATE2 = 202020)
              MOVE "CJ-PROMISE-DATE2" TO D-FIELD
              MOVE OCJ-PROMISE-DATE2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OCJ-PROMISE-DATE2 NOT = 0
                 MOVE OCJ-PROMISE-DATE2 TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "CJ-PROMISE-DATE2" TO D-FIELD
                    MOVE OCJ-PROMISE-DATE2  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OCJ-CUR-BAL NOT NUMERIC OR
               (OCJ-CUR-BAL = 020202.02 OR 202020.20)
              MOVE "CJ-CUR-BAL" TO D-FIELD
              MOVE OCJ-CUR-BAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCJ-PAST-DUE NOT NUMERIC OR
               (OCJ-PAST-DUE = 020202.02 OR 202020.20)
              MOVE "CJ-PAST-DUE" TO D-FIELD
              MOVE OCJ-PAST-DUE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCJ-POSTING-BR NOT NUMERIC 
              MOVE "CJ-POSTING-BR" TO D-FIELD
              MOVE OCJ-POSTING-BR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
      *    IF OCJ-QUEUE NOT ALPHABETIC
      *       MOVE "CJ-QUEUE" TO D-FIELD
      *       MOVE OCJ-QUEUE  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.


           GO TO PROCESS-FILE-CJ-NEXT.

       PROCESS-FILE-CJ-END.

           PERFORM CLOSE-OCJ1-FILE.

       PROCESS-FILE-CJ-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - C K
      *
      ******************************************************************
       PROCESS-FILE-CK SECTION.
           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-CK = "N" )
              GO TO PROCESS-FILE-CK-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO CK-PATH-OVERRIDE.
           MOVE "CK"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/CKFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OCK-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 0 TO OCK-BRNO OCK-NO.
           PERFORM OPEN-OCK1-FILE.
           PERFORM START-OCK1-FILE.

       PROCESS-FILE-CK-NEXT.
           PERFORM READ-OCK1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-CK-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OCK-DATE NOT NUMERIC 
              MOVE "CK-DATE" TO D-FIELD
              MOVE OCK-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OCK-DATE NOT = 0
                 MOVE OCK-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "CK-DATE" TO D-FIELD
                    MOVE OCK-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OCK-AMT NOT NUMERIC OR
               (OCK-AMT = 2020202.02 OR 0202020.20)
              MOVE "CK-AMT" TO D-FIELD
              MOVE OCK-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCK-REF NOT NUMERIC OR
               (OCK-REF = 202020)
              MOVE "CK-REF" TO D-FIELD
              MOVE OCK-REF  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              IF OCK-GLNO(SUB) NOT NUMERIC OR
                  (OCK-GLNO(SUB) = 20202020202 OR 02020202020)
                 MOVE "CK-GLNO(SUB)" TO D-FIELD
                 MOVE OCK-GLNO(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OCK-GLAMT(SUB) NOT NUMERIC OR
                  (OCK-GLAMT(SUB) = 2020202.02 OR 0202020.20)
                 MOVE "CK-GLAMT(SUB)" TO D-FIELD
                 MOVE OCK-GLAMT(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OCK-CLASS NOT NUMERIC 
              MOVE "CK-CLASS" TO D-FIELD
              MOVE OCK-CLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCK-DLNO NOT NUMERIC OR
               (OCK-DLNO = 2020.20 OR 0202.02)
              MOVE "CK-DLNO" TO D-FIELD
              MOVE OCK-DLNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCK-VOID-DATE NOT NUMERIC 
              MOVE "CK-VOID-DATE" TO D-FIELD
              MOVE OCK-VOID-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OCK-VOID-DATE NOT = 0
                 MOVE OCK-VOID-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "CK-VOID-DATE" TO D-FIELD
                    MOVE OCK-VOID-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OCK-ACCTNO NOT NUMERIC OR
               (OCK-ACCTNO = 20202020 OR 02020202)
              MOVE "CK-ACCTNO" TO D-FIELD
              MOVE OCK-ACCTNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCK-DLRVNO NOT NUMERIC 
              MOVE "CK-DLRVNO" TO D-FIELD
              MOVE OCK-DLRVNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCK-SSNO NOT NUMERIC OR
               (OCK-SSNO = 202020202 OR 020202020)
              MOVE "CK-SSNO" TO D-FIELD
              MOVE OCK-SSNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCK-BATCHID NOT NUMERIC OR
               (OCK-BATCHID = 2020202020 OR 0202020202)
              MOVE "CK-BATCHID" TO D-FIELD
              MOVE OCK-BATCHID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCK-CLEARED-DATE NOT NUMERIC 
              MOVE "CK-CLEARED-DATE" TO D-FIELD
              MOVE OCK-CLEARED-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OCK-CLEARED-DATE NOT = 0
                 MOVE OCK-CLEARED-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "CK-CLEARED-DATE" TO D-FIELD
                    MOVE OCK-CLEARED-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OCK-PAYEE-NO NOT NUMERIC OR
               (OCK-PAYEE-NO = 202020)
              MOVE "CK-PAYEE-NO" TO D-FIELD
              MOVE OCK-PAYEE-NO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-CK-NEXT.

       PROCESS-FILE-CK-END.

           PERFORM CLOSE-OCK1-FILE.

       PROCESS-FILE-CK-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - L O C A L - C P
      *
      *-----------------------------------------------------------------
      *    THIS IS THE LOCAL CP
      ******************************************************************
       PROCESS-FILE-LOCAL-CP SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-CP = "N" )
              GO TO PROCESS-FILE-LOCAL-CP-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO CP-PATH-OVERRIDE.
           MOVE "CP"                       TO UP1534-CURRENT-FILE.
           MOVE "SP/CPFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.


           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OCP-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO OCP-ID.
           PERFORM OPEN-OCP1-FILE.
           PERFORM START-OCP1-FILE.

       PROCESS-FILE-LOCAL-CP-NEXT.
           PERFORM READ-OCP1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-LOCAL-CP-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OCP-TOTAL-ACCOUNTS NOT NUMERIC
              MOVE "CP-TOTAL-ACCOUNTS" TO D-FIELD
              MOVE OCP-TOTAL-ACCOUNTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCP-COLLECTOR-COUNT NOT NUMERIC 
              MOVE "CP-COLLECTOR-COUNT" TO D-FIELD
              MOVE OCP-COLLECTOR-COUNT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCP-LAST-SUBSCRIPT NOT NUMERIC 
              MOVE "CP-LAST-SUBSCRIPT" TO D-FIELD
              MOVE OCP-LAST-SUBSCRIPT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-LOCAL-CP-NEXT.

       PROCESS-FILE-LOCAL-CP-END.

           PERFORM CLOSE-OCP1-FILE.

       PROCESS-FILE-LOCAL-CP-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - D L
      *
      ******************************************************************
       PROCESS-FILE-DL SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-DL = "N" )
              GO TO PROCESS-FILE-DL-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO DL-PATH-OVERRIDE.
           MOVE "DL"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/DLFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO ODL-PATH.

           MOVE 0                     TO ODL-NUM.
           PERFORM OPEN-ODL1-FILE.
           PERFORM START-ODL1-FILE.

       PROCESS-FILE-DL-NEXT.
           PERFORM READ-ODL1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-DL-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF ODL-NUM NOT NUMERIC
              MOVE "DL-NUM" TO D-FIELD
              MOVE ODL-NUM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-PHONE NOT NUMERIC 
              MOVE "DL-PHONE" TO D-FIELD
              MOVE ODL-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-BRANCH NOT NUMERIC 
              MOVE "DL-BRANCH" TO D-FIELD
              MOVE ODL-BRANCH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
 
           IF ODL-ORIGDATE NOT NUMERIC OR
               (ODL-ORIGDATE = 202020)
              MOVE "DL-ORIGDATE" TO D-FIELD
              MOVE ODL-ORIGDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF ODL-ORIGDATE NOT = 0
                 MOVE ODL-ORIGDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "DL-ORIGDATE" TO D-FIELD
                    MOVE ODL-ORIGDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF ODL-TERMDATE NOT NUMERIC OR
               (ODL-TERMDATE = 202020)
              MOVE "DL-TERMDATE" TO D-FIELD
              MOVE ODL-TERMDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF ODL-TERMDATE NOT = 0
                 MOVE ODL-TERMDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "DL-TERMDATE" TO D-FIELD
                    MOVE ODL-TERMDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF ODL-LC-CODE NOT NUMERIC 
              MOVE "DL-LC-CODE" TO D-FIELD
              MOVE ODL-LC-CODE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-FULLCEIL NOT NUMERIC 
              MOVE "DL-FULLCEIL" TO D-FIELD
              MOVE ODL-FULLCEIL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-PARTCEIL NOT NUMERIC 
              MOVE "DL-PARTCEIL" TO D-FIELD
              MOVE ODL-PARTCEIL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-NONCEIL NOT NUMERIC 
              MOVE "DL-NONCEIL" TO D-FIELD
              MOVE ODL-NONCEIL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-MAXPURTRM NOT NUMERIC 
              MOVE "DL-MAXPURTRM" TO D-FIELD
              MOVE ODL-MAXPURTRM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-MAXPURPER NOT NUMERIC 
              MOVE "DL-MAXPURPER" TO D-FIELD
              MOVE ODL-MAXPURPER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-EARNCD NOT NUMERIC 
              MOVE "DL-EARNCD" TO D-FIELD
              MOVE ODL-EARNCD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-EARNFAC NOT NUMERIC 
              MOVE "DL-EARNFAC" TO D-FIELD
              MOVE ODL-EARNFAC  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-DISC-FRMLA NOT NUMERIC 
              MOVE "DL-DISC-FRMLA" TO D-FIELD
              MOVE ODL-DISC-FRMLA  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF ODL-DISC-TERM(SUB) NOT NUMERIC 
                 MOVE "DL-DISC-TERM(SUB)" TO D-FIELD
                 MOVE ODL-DISC-TERM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF ODL-DISC-RATE(SUB) NOT NUMERIC 
                 MOVE "DL-DISC-RATE(SUB)" TO D-FIELD
                 MOVE ODL-DISC-RATE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
          
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF ODL-RVPRIN-FRMLA(SUB) NOT NUMERIC 
                 MOVE "DL-RVPRIN-FRMLA(SUB)" TO D-FIELD
                 MOVE ODL-RVPRIN-FRMLA(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF ODL-RVPRIN-ADDON(SUB) NOT NUMERIC 
                 MOVE "DL-RVPRIN-ADDON(SUB)" TO D-FIELD
                 MOVE ODL-RVPRIN-ADDON(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF ODL-RVCHGS-FRMLA(SUB) NOT NUMERIC 
                 MOVE "DL-RVCHGS-FRMLA(SUB)" TO D-FIELD
                 MOVE ODL-RVCHGS-FRMLA(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF ODL-RVCHGS-ADDON(SUB) NOT NUMERIC 
                 MOVE "DL-RVCHGS-ADDON(SUB)" TO D-FIELD
                 MOVE ODL-RVCHGS-ADDON(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 6
                 IF ODL-RVPRIN-TERM(SUB, SUB2) NOT NUMERIC 
                    MOVE "DL-RVPRIN-TERM(SUB, SUB2)" TO D-FIELD
                    MOVE ODL-RVPRIN-TERM(SUB, SUB2)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE
                 END-IF
                 IF ODL-RVPRIN-RATE(SUB, SUB2) NOT NUMERIC 
                    MOVE "DL-RVPRIN-RATE(SUB, SUB2)" TO D-FIELD
                    MOVE ODL-RVPRIN-RATE(SUB, SUB2)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE
                 END-IF
                 IF ODL-RVCHGS-TERM(SUB, SUB2) NOT NUMERIC 
                    MOVE "DL-RVCHGS-TERM(SUB, SUB2)" TO D-FIELD
                    MOVE ODL-RVCHGS-TERM(SUB, SUB2)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE
                 END-IF
                 IF ODL-RVCHGS-RATE(SUB, SUB2) NOT NUMERIC 
                    MOVE "DL-RVCHGS-RATE(SUB, SUB2)" TO D-FIELD
                    MOVE ODL-RVCHGS-RATE(SUB, SUB2)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE
                 END-IF
              END-PERFORM
           END-PERFORM.

           IF ODL-PARVHELD-FRMLA NOT NUMERIC 
              MOVE "DL-PARVHELD-FRMLA" TO D-FIELD
              MOVE ODL-PARVHELD-FRMLA  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-PARVHELD-ADDON NOT NUMERIC 
              MOVE "DL-PARVHELD-ADDON" TO D-FIELD
              MOVE ODL-PARVHELD-ADDON  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF ODL-PARVHELD-TERM(SUB) NOT NUMERIC 
                 MOVE "DL-PARVHELD-TERM(SUB)" TO D-FIELD
                 MOVE ODL-PARVHELD-TERM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF ODL-PARVHELD-RATE(SUB) NOT NUMERIC 
                 MOVE "DL-PARVHELD-RATE(SUB)" TO D-FIELD
                 MOVE ODL-PARVHELD-RATE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF ODL-PARVPAID-FRMLA NOT NUMERIC 
              MOVE "DL-PARVPAID-FRMLA" TO D-FIELD
              MOVE ODL-PARVPAID-FRMLA  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
 
           IF ODL-PARVPAID-ADDON NOT NUMERIC 
              MOVE "DL-PARVPAID-ADDON" TO D-FIELD
              MOVE ODL-PARVPAID-ADDON  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF ODL-PARVPAID-TERM(SUB) NOT NUMERIC 
                 MOVE "DL-PARVPAID-TERM(SUB)" TO D-FIELD
                 MOVE ODL-PARVPAID-TERM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF ODL-PARVPAID-RATE(SUB) NOT NUMERIC 
                 MOVE "DL-PARVPAID-RATE(SUB)" TO D-FIELD
                 MOVE ODL-PARVPAID-RATE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF ODL-SVACCT-FRMLA NOT NUMERIC 
              MOVE "DL-SVACCT-FRMLA" TO D-FIELD
              MOVE ODL-SVACCT-FRMLA  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-SVACCT-AMT NOT NUMERIC 
              MOVE "DL-SVACCT-AMT" TO D-FIELD
              MOVE ODL-SVACCT-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-SVACCT-RATE NOT NUMERIC 
              MOVE "DL-SVACCT-RATE" TO D-FIELD
              MOVE ODL-SVACCT-RATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-SVACCT-LCHGPER NOT NUMERIC 
              MOVE "DL-SVACCT-LCHGPER" TO D-FIELD
              MOVE ODL-SVACCT-LCHGPER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-SVACCT-INTPER NOT NUMERIC 
              MOVE "DL-SVACCT-INTPER" TO D-FIELD
              MOVE ODL-SVACCT-INTPER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-SVACCT-DEFPER NOT NUMERIC 
              MOVE "DL-SVACCT-DEFPER" TO D-FIELD
              MOVE ODL-SVACCT-DEFPER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-FULLCEIL-BAL NOT NUMERIC 
              MOVE "DL-FULLCEIL-BAL" TO D-FIELD
              MOVE ODL-FULLCEIL-BAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-PARTCEIL-BAL NOT NUMERIC 
              MOVE "DL-PARTCEIL-BAL" TO D-FIELD
              MOVE ODL-PARTCEIL-BAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-NONCEIL-BAL NOT NUMERIC 
              MOVE "DL-NONCEIL-BAL" TO D-FIELD
              MOVE ODL-NONCEIL-BAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-FAX-NUMBER NOT NUMERIC 
              MOVE "DL-FAX-NUMBER" TO D-FIELD
              MOVE ODL-FAX-NUMBER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-BANK-ROUTING-NO NOT NUMERIC 
              MOVE "DL-BANK-ROUTING-NO" TO D-FIELD
              MOVE ODL-BANK-ROUTING-NO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-SAL-MISC-PCT NOT NUMERIC 
              MOVE "DL-SAL-MISC-PCT" TO D-FIELD
              MOVE ODL-SAL-MISC-PCT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODL-SAL-MISC-PCT2 NOT NUMERIC 
              MOVE "DL-SAL-MISC-PCT2" TO D-FIELD
              MOVE ODL-SAL-MISC-PCT2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-DL-NEXT.

       PROCESS-FILE-DL-END.

           PERFORM CLOSE-ODL1-FILE.

       PROCESS-FILE-DL-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - D T
      *
      ******************************************************************
       PROCESS-FILE-DT SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-DT = "N" )
              GO TO PROCESS-FILE-DT-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO DT-PATH-OVERRIDE.
           MOVE "DT"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/DTFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO ODT-PATH.

           MOVE ZEROES                     TO ODT-DLNO
                                              ODT-CLASS
                                              ODT-ACCTNO
                                              ODT-DATE
                                              ODT-SEQ.

           PERFORM OPEN-ODT1-FILE.
           PERFORM START-ODT1-FILE.

       PROCESS-FILE-DT-NEXT.
           PERFORM READ-ODT1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-DT-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

   
           IF ODT-NUM NOT NUMERIC 
              MOVE "DT-NUM" TO D-FIELD
              MOVE ODT-NUM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-CLASS NOT NUMERIC
              MOVE "DT-CLASS" TO D-FIELD
              MOVE ODT-CLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-ACCTNO NOT NUMERIC
              MOVE "DT-ACCTNO" TO D-FIELD
              MOVE ODT-ACCTNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-DATE NOT NUMERIC OR
               (ODT-DATE = 202020)
              MOVE "DT-DATE" TO D-FIELD
              MOVE ODT-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF ODT-DATE NOT = 0
                 MOVE ODT-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "DT-DATE" TO D-FIELD
                    MOVE ODT-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF ODT-SEQ NOT NUMERIC 
              MOVE "DT-SEQ" TO D-FIELD
              MOVE ODT-SEQ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-BRNO NOT NUMERIC 
              MOVE "DT-BRNO" TO D-FIELD
              MOVE ODT-BRNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-LPSEQNO NOT NUMERIC 
              MOVE "DT-LPSEQNO" TO D-FIELD
              MOVE ODT-LPSEQNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF ODT-DLRVPRIN(SUB) NOT NUMERIC 
                 MOVE "DT-DLRVPRIN(SUB)" TO D-FIELD
                 MOVE ODT-DLRVPRIN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF ODT-DLRVCHGS(SUB) NOT NUMERIC 
                 MOVE "DT-DLRVCHGS(SUB)" TO D-FIELD
                 MOVE ODT-DLRVCHGS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF ODT-DLPARVHELD NOT NUMERIC 
              MOVE "DT-DLPARVHELD" TO D-FIELD
              MOVE ODT-DLPARVHELD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-DLPARVPAID NOT NUMERIC 
              MOVE "ODT-DLPARVPAID" TO D-FIELD
              MOVE ODT-DLPARVPAID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-PROCEEDS NOT NUMERIC 
              MOVE "ODT-PROCEEDS" TO D-FIELD
              MOVE ODT-PROCEEDS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-RELEASED NOT NUMERIC 
              MOVE "ODT-RELEASED" TO D-FIELD
              MOVE ODT-RELEASED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-PAYMENTS NOT NUMERIC 
              MOVE "ODT-PAYMENTS" TO D-FIELD
              MOVE ODT-PAYMENTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-CKNO NOT NUMERIC 
              MOVE "ODT-CKNO" TO D-FIELD
              MOVE ODT-CKNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-CKDATE NOT NUMERIC OR
               (ODT-CKDATE = 202020)
              MOVE "DT-CKDATE" TO D-FIELD
              MOVE ODT-CKDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF ODT-CKDATE NOT = 0
                 MOVE ODT-CKDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "DT-CKDATE" TO D-FIELD
                    MOVE ODT-CKDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF ODT-BATCHID NOT NUMERIC 
              MOVE "DT-BATCHID" TO D-FIELD
              MOVE ODT-BATCHID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-BATCHID-SEQ NOT NUMERIC 
              MOVE "DT-BATCHID-SEQ" TO D-FIELD
              MOVE ODT-BATCHID-SEQ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-PURBAL NOT NUMERIC 
              MOVE "DT-PURBAL" TO D-FIELD
              MOVE ODT-PURBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-APCUR NOT NUMERIC 
              MOVE "DT-APCUR" TO D-FIELD
              MOVE ODT-APCUR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-DLRVNO NOT NUMERIC 
              MOVE "DT-DLRVNO" TO D-FIELD
              MOVE ODT-DLRVNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-DLRVPRIN-PAY NOT NUMERIC 
              MOVE "DT-DLRVPRIN-PAY" TO D-FIELD
              MOVE ODT-DLRVPRIN-PAY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-DLRVCHGS-PAY NOT NUMERIC 
              MOVE "DT-DLRVCHGS-PAY" TO D-FIELD
              MOVE ODT-DLRVCHGS-PAY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ODT-DLPARVHELD-PAY NOT NUMERIC 
              MOVE "DT-DLPARVHELD-PAY" TO D-FIELD
              MOVE ODT-DLPARVHELD-PAY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
 

           GO TO PROCESS-FILE-DT-NEXT.

       PROCESS-FILE-DT-END.

           PERFORM CLOSE-ODT1-FILE.

       PROCESS-FILE-DT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - G B
      *
      ******************************************************************
       PROCESS-FILE-GB SECTION.
           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-GB = "N" )
              GO TO PROCESS-FILE-GB-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO GB-PATH-OVERRIDE.
           MOVE "GB"                       TO UP1534-CURRENT-FILE.
           MOVE "GB/GBFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OGB-PATH.

           MOVE 1             TO OGB-KEY.
           PERFORM OPEN-OGB-FILE.
           PERFORM START-OGB-FILE.

       PROCESS-FILE-GB-NEXT.
           PERFORM READ-OGB-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-GB-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OGB-PHONE NOT NUMERIC 
              MOVE "GB-PHONE" TO D-FIELD
              MOVE OGB-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGB-NEXT-SF NOT NUMERIC 
              MOVE "GB-NEXT-SF" TO D-FIELD
              MOVE OGB-NEXT-SF  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGB-NEXT-SMALL NOT NUMERIC 
              MOVE "GB-NEXT-SMALL" TO D-FIELD
              MOVE OGB-NEXT-SMALL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGB-NEXT-OTHER NOT NUMERIC 
              MOVE "GB-NEXT-OTHER" TO D-FIELD
              MOVE OGB-NEXT-OTHER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGB-NEXT-NOTE NOT NUMERIC 
              MOVE "GB-NEXT-NOTE" TO D-FIELD
              MOVE OGB-NEXT-NOTE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
              IF OGB-HOLIDAY(SUB) NOT NUMERIC 
                 MOVE "GB-HOLIDAY(SUB)" TO D-FIELD
                 MOVE OGB-HOLIDAY(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OGB-RPT-LINES NOT NUMERIC 
              MOVE "GB-RPT-LINES" TO D-FIELD
              MOVE OGB-RPT-LINES  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGB-LAST-ACCRUE NOT NUMERIC 
              MOVE "GB-LAST-ACCRUE" TO D-FIELD
              MOVE OGB-LAST-ACCRUE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGB-NEXT-APPNO NOT NUMERIC 
              MOVE "GB-NEXT-APPNO" TO D-FIELD
              MOVE OGB-NEXT-APPNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGB-NEXT-RECEIPT NOT NUMERIC 
              MOVE "GB-NEXT-RECEIPT" TO D-FIELD
              MOVE OGB-NEXT-RECEIPT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGB-NEXT-SF-REVOL NOT NUMERIC 
              MOVE "GB-NEXT-SF-REVOL" TO D-FIELD
              MOVE OGB-NEXT-SF-REVOL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGB-NEXT-LN-REVOL NOT NUMERIC 
              MOVE "GB-NEXT-LN-REVOL" TO D-FIELD
              MOVE OGB-NEXT-LN-REVOL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-GB-NEXT.

       PROCESS-FILE-GB-END.

           PERFORM CLOSE-OGB-FILE.

       PROCESS-FILE-GB-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - I C
      *
      ******************************************************************
       PROCESS-FILE-IC SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-IC = "N" )
              GO TO PROCESS-FILE-IC-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO IC-PATH-OVERRIDE.
           MOVE "IC"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/ICFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OIC-PATH.

           MOVE ZEROES                     TO OIC-CODE.
           PERFORM OPEN-OIC1-FILE.
           PERFORM START-OIC1-FILE.

       PROCESS-FILE-IC-NEXT.
           PERFORM READ-OIC1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-IC-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.

           PERFORM UP1534-VPI-DISPLAY.
 
           IF OIC-PHONE NOT NUMERIC
              MOVE "IC-PHONE" TO D-FIELD
              MOVE OIC-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 13
              IF OIC-COMMRATE(SUB) NOT NUMERIC
                 MOVE "IC-COMMRATE(SUB)" TO D-FIELD
                 MOVE OIC-COMMRATE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
 

           GO TO PROCESS-FILE-IC-NEXT.

       PROCESS-FILE-IC-END.

           PERFORM CLOSE-OIC1-FILE.

       PROCESS-FILE-IC-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - I X
      *
      ******************************************************************
       PROCESS-FILE-IX SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-IX = "N" )
              GO TO PROCESS-FILE-IX-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO IX-PATH-OVERRIDE.
           MOVE "IX"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/IXFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OIX-PATH.

           MOVE SPACES                     TO OIX1-KEY.
           PERFORM OPEN-OIX1-FILE.
           PERFORM START-OIX1-FILE.

       PROCESS-FILE-IX-NEXT.
           PERFORM READ-OIX1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-IX-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.

           PERFORM UP1534-VPI-DISPLAY.

           IF OIX-LT-ID-ACCTNO NOT NUMERIC 
              MOVE "IX-LT-ID-ACCTNO" TO D-FIELD
              MOVE OIX-LT-ID-ACCTNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OIX-LT-ID-SEQNO NOT NUMERIC 
              MOVE "IX-LT-ID-SEQNO" TO D-FIELD
              MOVE OIX-LT-ID-SEQNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OIX-LT-ID-TYPE NOT NUMERIC 
              MOVE "IX-LT-ID-TYPE" TO D-FIELD
              MOVE OIX-LT-ID-TYPE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-IX-NEXT.

       PROCESS-FILE-IX-END.

           PERFORM CLOSE-OIX1-FILE.

       PROCESS-FILE-IX-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - L E
      *
      ******************************************************************
       PROCESS-FILE-LE SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-LE = "N" )
              GO TO PROCESS-FILE-LE-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO LE-PATH-OVERRIDE.
           MOVE "LE"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/LEFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OLE-PATH.

           MOVE SPACES                     TO OLE1-KEY.
           MOVE ZEROES                     TO OLE-ACCTNO
                                              OLE-OSACCTNO.

           PERFORM OPEN-OLE1-FILE.
           PERFORM START-OLE1-FILE.

       PROCESS-FILE-LE-NEXT.
           PERFORM READ-OLE1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-LE-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.

           PERFORM UP1534-VPI-DISPLAY.

           IF OLE-ACCTNO NOT NUMERIC
              MOVE "LE-ACCTNO" TO D-FIELD
              MOVE OLE-ACCTNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLE-OSACCTNO NOT NUMERIC
              MOVE "LE-OSACCTNO" TO D-FIELD
              MOVE OLE-OSACCTNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OLE-TYPE = "E"
              IF OLE-OSRATE NOT NUMERIC
                 MOVE "LE-OSRATE" TO D-FIELD
                 MOVE OLE-OSRATE  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLE-OSAMT NOT NUMERIC
                 MOVE "LE-OSAMT" TO D-FIELD
                 MOVE OLE-OSAMT  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLE-OSAMT NOT NUMERIC
                 MOVE "LE-OSAMT" TO D-FIELD
                 MOVE OLE-OSAMT  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLE-AMT NOT NUMERIC
                 MOVE "LE-AMT" TO D-FIELD
                 MOVE OLE-AMT  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              PERFORM CHECK-LE-DATES.
              
           IF OLE-TYPE = "R"
              PERFORM CHECK-LE-R-DATES
              IF OLE-LNAMT NOT NUMERIC 
                 MOVE "LE-LNAMT" TO D-FIELD
                 MOVE OLE-LNAMT  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLE-RESTRICTED-MONEY NOT NUMERIC 
                 MOVE "LE-RESTRICTED-MONEY" TO D-FIELD
                 MOVE OLE-RESTRICTED-MONEY  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLE-BALANCE NOT NUMERIC 
                 MOVE "LE-BALANCE" TO D-FIELD
                 MOVE OLE-BALANCE  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-LE-NEXT.

       PROCESS-FILE-LE-END.

           PERFORM CLOSE-OLE1-FILE.

       PROCESS-FILE-LE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - L H
      *
      ******************************************************************
       PROCESS-FILE-LH SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-LH = "N" )
              GO TO PROCESS-FILE-LH-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO LH-PATH-OVERRIDE.
           MOVE "LH"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/LHFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OLH-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                     TO OLH1-KEY.
           MOVE ZEROES                     TO OLH-SSNO
                                              OLH-ACCTNO.
           PERFORM OPEN-OLH1-FILE.
           PERFORM START-OLH1-FILE.

       PROCESS-FILE-LH-NEXT.
           PERFORM READ-OLH1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-LH-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.

           PERFORM UP1534-VPI-DISPLAY.

           IF OLH-SSNO NOT NUMERIC 
              MOVE "LH-SSNO" TO D-FIELD
              MOVE OLH-SSNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLH-ACCTNO NOT NUMERIC 
              MOVE "LH-ACCTNO" TO D-FIELD
              MOVE OLH-ACCTNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLH-CLASS NOT NUMERIC 
              MOVE "LH-CLASS" TO D-FIELD
              MOVE OLH-CLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLH-LAWCD NOT NUMERIC 
              MOVE "LH-LAWCD" TO D-FIELD
              MOVE OLH-LAWCD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLH-LOANDATE NOT NUMERIC OR
               (OLH-LOANDATE = 202020)
              MOVE "LH-LOANDATE" TO D-FIELD
              MOVE OLH-LOANDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLH-LOANDATE NOT = 0
                 MOVE OLH-LOANDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LH-LOANDATE" TO D-FIELD
                    MOVE OLH-LOANDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLH-PLDATE NOT NUMERIC OR
               (OLH-PLDATE = 202020)
              MOVE "LH-PLDATE" TO D-FIELD
              MOVE OLH-PLDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLH-PLDATE NOT = 0
                 MOVE OLH-PLDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LH-PLDATE" TO D-FIELD
                    MOVE OLH-PLDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLH-JDDATE NOT NUMERIC OR
               (OLH-JDDATE = 202020)
              MOVE "LH-JDDATE" TO D-FIELD
              MOVE OLH-JDDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLH-JDDATE NOT = 0
                 MOVE OLH-JDDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LH-JDDATE" TO D-FIELD
                    MOVE OLH-JDDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLH-PAIDOFF NOT NUMERIC OR
               (OLH-PAIDOFF = 202020)
              MOVE "LH-PAIDOFF" TO D-FIELD
              MOVE OLH-PAIDOFF  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLH-PAIDOFF NOT = 0
                 MOVE OLH-PAIDOFF TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LH-PAIDOFF" TO D-FIELD
                    MOVE OLH-PAIDOFF  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLH-LNAMT NOT NUMERIC 
              MOVE "LH-LNAMT" TO D-FIELD
              MOVE OLH-LNAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLH-ORGTERM NOT NUMERIC 
              MOVE "LH-ORGTERM" TO D-FIELD
              MOVE OLH-ORGTERM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLH-POTERMS NOT NUMERIC 
              MOVE "LH-POTERMS" TO D-FIELD
              MOVE OLH-POTERMS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLH-PYPROFILE NOT NUMERIC 
              MOVE "LH-PYPROFILE" TO D-FIELD
              MOVE OLH-PYPROFILE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLH-DLNO NOT NUMERIC 
              MOVE "LH-DLNO" TO D-FIELD
              MOVE OLH-DLNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-LH-NEXT.

       PROCESS-FILE-LH-END.

           PERFORM CLOSE-OLH1-FILE.

       PROCESS-FILE-LH-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - L N
      *
      ******************************************************************
       PROCESS-FILE-LN SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-LN = "N" )
              GO TO PROCESS-FILE-LN-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO LN-PATH-OVERRIDE.
           MOVE "LN"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/LNFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OLN-PATH.

           MOVE SPACES                     TO OLN1-KEY.
           MOVE ZEROES                     TO OLN-ACCTNO.
           PERFORM OPEN-OLN1-FILE.
           PERFORM START-OLN1-FILE.

       PROCESS-FILE-LN-NEXT.
           PERFORM READ-OLN1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-LN-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.

           PERFORM UP1534-VPI-DISPLAY.

           IF OLN-OWNBR NOT NUMERIC 
              MOVE "LN-OWNBR" TO D-FIELD
              MOVE OLN-OWNBR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-CLASS NOT NUMERIC 
              MOVE "LN-CLASS" TO D-FIELD
              MOVE OLN-CLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-ACCTNO NOT NUMERIC 
              MOVE "LN-ACCTNO" TO D-FIELD
              MOVE OLN-ACCTNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-SUBCLASS NOT NUMERIC 
              MOVE "LN-SUBCLASS" TO D-FIELD
              MOVE OLN-SUBCLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-LAWCODE NOT NUMERIC 
              MOVE "LN-LAWCODE" TO D-FIELD
              MOVE OLN-LAWCODE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              IF OLN-SSNO(SUB) NOT NUMERIC 
                 MOVE "LN-SSNO(SUB)" TO D-FIELD
                 MOVE OLN-SSNO(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OLN-ORGBR NOT NUMERIC 
              MOVE "LN-ORGBR" TO D-FIELD
              MOVE OLN-ORGBR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-SPRCLASS NOT NUMERIC 
              MOVE "LN-SPRCLASS" TO D-FIELD
              MOVE OLN-SPRCLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-LC-CODE NOT NUMERIC 
              MOVE "LN-LC-CODE" TO D-FIELD
              MOVE OLN-LC-CODE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
          
           IF OLN-LOANDATE NOT NUMERIC OR
               (OLN-LOANDATE = 202020)
              MOVE "LN-LOANDATE" TO D-FIELD
              MOVE OLN-LOANDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-LOANDATE NOT = 0
                 MOVE OLN-LOANDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-LOANDATE" TO D-FIELD
                    MOVE OLN-LOANDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLN-ENTDATE NOT NUMERIC OR
               (OLN-ENTDATE = 202020)
              MOVE "LN-ENTDATE" TO D-FIELD
              MOVE OLN-ENTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-ENTDATE NOT = 0
                 MOVE OLN-ENTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-ENTDATE" TO D-FIELD
                    MOVE OLN-ENTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLN-PURDATE NOT NUMERIC OR
               (OLN-PURDATE = 202020)
              MOVE "LN-PURDATE" TO D-FIELD
              MOVE OLN-PURDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-PURDATE NOT = 0
                 MOVE OLN-PURDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-PURDATE" TO D-FIELD
                    MOVE OLN-PURDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLN-INTDATE NOT NUMERIC OR
               (OLN-INTDATE = 202020)
              MOVE "LN-INTDATE" TO D-FIELD
              MOVE OLN-INTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-INTDATE NOT = 0
                 MOVE OLN-INTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-INTDATE" TO D-FIELD
                    MOVE OLN-INTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLN-CASHDATE NOT NUMERIC OR
               (OLN-CASHDATE = 202020)
              MOVE "LN-CASHDATE" TO D-FIELD
              MOVE OLN-CASHDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-CASHDATE NOT = 0
                 MOVE OLN-CASHDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-CASHDATE" TO D-FIELD
                    MOVE OLN-CASHDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLN-FILEDATE NOT NUMERIC OR
               (OLN-FILEDATE = 202020)
              MOVE "LN-FILEDATE" TO D-FIELD
              MOVE OLN-FILEDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-FILEDATE NOT = 0
                 MOVE OLN-FILEDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-FILEDATE" TO D-FIELD
                    MOVE OLN-FILEDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLN-ORIG-1STPAY NOT NUMERIC OR
               (OLN-ORIG-1STPAY = 202020)
              MOVE "LN-ORIG-1STPAY" TO D-FIELD
              MOVE OLN-ORIG-1STPAY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-ORIG-1STPAY NOT = 0
                 MOVE OLN-ORIG-1STPAY TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-ORIG-1STPAY" TO D-FIELD
                    MOVE OLN-ORIG-1STPAY  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLN-1STPYDATE NOT NUMERIC OR
               (OLN-1STPYDATE = 202020)
              MOVE "LN-1STPYDATE" TO D-FIELD
              MOVE OLN-1STPYDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-1STPYDATE NOT = 0
                 MOVE OLN-1STPYDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-1STPYDATE" TO D-FIELD
                    MOVE OLN-1STPYDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-JDDATE NOT NUMERIC OR
               (OLN-JDDATE = 202020)
              MOVE "LN-JDDATE" TO D-FIELD
              MOVE OLN-JDDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-JDDATE NOT = 0
                 MOVE OLN-JDDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-JDDATE" TO D-FIELD
                    MOVE OLN-JDDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-PLDATE NOT NUMERIC OR
               (OLN-PLDATE = 202020)
              MOVE "LN-PLDATE" TO D-FIELD
              MOVE OLN-PLDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-PLDATE NOT = 0
                 MOVE OLN-PLDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-PLDATE" TO D-FIELD
                    MOVE OLN-PLDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-ACCELDATE NOT NUMERIC OR
               (OLN-ACCELDATE = 202020)
              MOVE "LN-ACCELDATE" TO D-FIELD
              MOVE OLN-ACCELDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-ACCELDATE NOT = 0
                 MOVE OLN-ACCELDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-ACCELDATE" TO D-FIELD
                    MOVE OLN-ACCELDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-REDUDATE NOT NUMERIC OR
               (OLN-REDUDATE = 202020)
              MOVE "LN-REDUDATE" TO D-FIELD
              MOVE OLN-REDUDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-REDUDATE NOT = 0
                 MOVE OLN-REDUDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-REDUDATE" TO D-FIELD
                    MOVE OLN-REDUDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-POFFDATE NOT NUMERIC OR
               (OLN-POFFDATE = 202020)
              MOVE "LN-POFFDATE" TO D-FIELD
              MOVE OLN-POFFDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-POFFDATE NOT = 0
                 MOVE OLN-POFFDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-POFFDATE" TO D-FIELD
                    MOVE OLN-POFFDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLN-LEGALDATE NOT NUMERIC OR
               (OLN-LEGALDATE = 202020)
              MOVE "LN-LEGALDATE" TO D-FIELD
              MOVE OLN-LEGALDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-LEGALDATE NOT = 0
                 MOVE OLN-LEGALDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-LEGALDATE" TO D-FIELD
                    MOVE OLN-LEGALDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-BNKRPTDATE NOT NUMERIC OR
               (OLN-BNKRPTDATE = 202020)
              MOVE "LN-BNKRPTDATE" TO D-FIELD
              MOVE OLN-BNKRPTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-BNKRPTDATE NOT = 0
                 MOVE OLN-BNKRPTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-BNKRPTDATE" TO D-FIELD
                    MOVE OLN-BNKRPTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-DEBTAIDATE NOT NUMERIC OR
               (OLN-DEBTAIDATE = 202020)
              MOVE "LN-DEBTAIDATE" TO D-FIELD
              MOVE OLN-DEBTAIDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-DEBTAIDATE NOT = 0
                 MOVE OLN-DEBTAIDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-DEBTAIDATE" TO D-FIELD
                    MOVE OLN-DEBTAIDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-REPODATE NOT NUMERIC OR
               (OLN-REPODATE = 202020)
              MOVE "LN-REPODATE" TO D-FIELD
              MOVE OLN-REPODATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-REPODATE NOT = 0
                 MOVE OLN-REPODATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-REPODATE" TO D-FIELD
                    MOVE OLN-REPODATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-REDEEMDATE NOT NUMERIC OR
               (OLN-REDEEMDATE = 202020)
              MOVE "LN-REDEEMDATE" TO D-FIELD
              MOVE OLN-REDEEMDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-REDEEMDATE NOT = 0
                 MOVE OLN-REDEEMDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-REDEEMDATE" TO D-FIELD
                    MOVE OLN-REDEEMDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLN-SOLDDATE NOT NUMERIC OR
               (OLN-SOLDDATE = 202020)
              MOVE "LN-SOLDDATE" TO D-FIELD
              MOVE OLN-SOLDDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-SOLDDATE NOT = 0
                 MOVE OLN-SOLDDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-SOLDDATE" TO D-FIELD
                    MOVE OLN-SOLDDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-INS-CONFIRMATION NOT NUMERIC OR
               (OLN-INS-CONFIRMATION = 202020)
              MOVE "LN-INS-CONFIRMATION" TO D-FIELD
              MOVE OLN-INS-CONFIRMATION  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-INS-CONFIRMATION NOT = 0
                 MOVE OLN-INS-CONFIRMATION TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-INS-CONFIRMATION" TO D-FIELD
                    MOVE OLN-INS-CONFIRMATION  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLN-APRATE NOT NUMERIC 
              MOVE "LN-APRATE" TO D-FIELD
              MOVE OLN-APRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-EFFRATE NOT NUMERIC 
              MOVE "LN-EFFRATE" TO D-FIELD
              MOVE OLN-EFFRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-SMPRATE NOT NUMERIC 
              MOVE "LN-SMPRATE" TO D-FIELD
              MOVE OLN-SMPRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-YLDRATE NOT NUMERIC 
              MOVE "LN-YLDRATE" TO D-FIELD
              MOVE OLN-YLDRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-OVRDRATE NOT NUMERIC 
              MOVE "LN-OVRDRATE" TO D-FIELD
              MOVE OLN-OVRDRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-ENDORATE NOT NUMERIC 
              MOVE "LN-ENDORATE" TO D-FIELD
              MOVE OLN-ENDORATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-JDRATE NOT NUMERIC 
              MOVE "LN-JDRATE" TO D-FIELD
              MOVE OLN-JDRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-ACCRATE NOT NUMERIC 
              MOVE "LN-ACCRATE" TO D-FIELD
              MOVE OLN-ACCRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-REDURATE NOT NUMERIC 
              MOVE "LN-REDURATE" TO D-FIELD
              MOVE OLN-REDURATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-SERVPER NOT NUMERIC 
              MOVE "LN-SERVPER" TO D-FIELD
              MOVE OLN-SERVPER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-SELLPR NOT NUMERIC 
              MOVE "LN-SELLPR" TO D-FIELD
              MOVE OLN-SELLPR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-DOWNPY NOT NUMERIC 
              MOVE "LN-DOWNPY" TO D-FIELD
              MOVE OLN-DOWNPY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-TRADEIN NOT NUMERIC 
              MOVE "LN-TRADEIN" TO D-FIELD
              MOVE OLN-TRADEIN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-LNAMT NOT NUMERIC 
              MOVE "LN-LNAMT" TO D-FIELD
              MOVE OLN-LNAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-FINCHG NOT NUMERIC 
              MOVE "LN-FINCHG" TO D-FIELD
              MOVE OLN-FINCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-INTCHG NOT NUMERIC 
              MOVE "LN-INTCHG" TO D-FIELD
              MOVE OLN-INTCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-SERCHG NOT NUMERIC 
              MOVE "LN-SERCHG" TO D-FIELD
              MOVE OLN-SERCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-EXTCHG NOT NUMERIC 
              MOVE "LN-EXTCHG" TO D-FIELD
              MOVE OLN-EXTCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-MAINTFEE NOT NUMERIC 
              MOVE "LN-MAINTFEE" TO D-FIELD
              MOVE OLN-MAINTFEE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-POINTS NOT NUMERIC 
              MOVE "LN-POINTS" TO D-FIELD
              MOVE OLN-POINTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PROCEEDS NOT NUMERIC 
              MOVE "LN-PROCEEDS" TO D-FIELD
              MOVE OLN-PROCEEDS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-ORGTERM NOT NUMERIC 
              MOVE "LN-ORGTERM" TO D-FIELD
              MOVE OLN-ORGTERM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-1STPYAMT NOT NUMERIC 
              MOVE "LN-1STPYAMT" TO D-FIELD
              MOVE OLN-1STPYAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-NOREGPY NOT NUMERIC 
              MOVE "LN-NOREGPY" TO D-FIELD
              MOVE OLN-NOREGPY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-REGPYAMT NOT NUMERIC 
              MOVE "LN-REGPYAMT" TO D-FIELD
              MOVE OLN-REGPYAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-LASTPYAMT NOT NUMERIC 
              MOVE "LN-LASTPYAMT" TO D-FIELD
              MOVE OLN-LASTPYAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PROCASH NOT NUMERIC 
              MOVE "LN-PROCASH" TO D-FIELD
              MOVE OLN-PROCASH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-STATE-WORKER NOT NUMERIC 
              MOVE "LN-STATE-WORKER" TO D-FIELD
              MOVE OLN-STATE-WORKER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-STATE-WORKER2 NOT NUMERIC 
              MOVE "LN-STATE-WORKER2" TO D-FIELD
              MOVE OLN-STATE-WORKER2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF OLN-FEEAMT(SUB) NOT NUMERIC 
                 MOVE "LN-FEEAMT(SUB)" TO D-FIELD
                 MOVE OLN-FEEAMT(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              IF OLN-INSCOMP(SUB) NOT NUMERIC 
                 MOVE "LN-INSCOMP(SUB)" TO D-FIELD
                 MOVE OLN-INSCOMP(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLN-INSEFF(SUB) NOT NUMERIC 
                 MOVE "LN-INSEFF(SUB)" TO D-FIELD
                 MOVE OLN-INSEFF(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLN-INSEXP(SUB) NOT NUMERIC 
                 MOVE "LN-INSEXP(SUB)" TO D-FIELD
                 MOVE OLN-INSEXP(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLN-INSPREM(SUB) NOT NUMERIC 
                 MOVE "LN-INSPREM(SUB)" TO D-FIELD
                 MOVE OLN-INSPREM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLN-INSCOVR(SUB) NOT NUMERIC 
                 MOVE "LN-INSCOVR(SUB)" TO D-FIELD
                 MOVE OLN-INSCOVR(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLN-INSCOMM(SUB) NOT NUMERIC 
                 MOVE "LN-INSCOMM(SUB)" TO D-FIELD
                 MOVE OLN-INSCOMM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLN-INSANTICERN(SUB) NOT NUMERIC 
                 MOVE "LN-INSANTICERN(SUB)" TO D-FIELD
                 MOVE OLN-INSANTICERN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLN-INSANTICADJ(SUB) NOT NUMERIC 
                 MOVE "LN-INSANTICADJ(SUB)" TO D-FIELD
                 MOVE OLN-INSANTICADJ(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLN-INSUFERN(SUB) NOT NUMERIC 
                 MOVE "LN-INSUFERN(SUB)" TO D-FIELD
                 MOVE OLN-INSUFERN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLN-INSACCERN(SUB) NOT NUMERIC 
                 MOVE "LN-INSACCERN(SUB)" TO D-FIELD
                 MOVE OLN-INSACCERN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLN-INSEARN-ORDER(SUB) NOT NUMERIC 
                 MOVE "LN-INSEARN-ORDER(SUB)" TO D-FIELD
                 MOVE OLN-INSEARN-ORDER(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OLN-PYMMYY-RECDEL NOT NUMERIC 
              MOVE "LN-PYMMYY-RECDEL" TO D-FIELD
              MOVE OLN-PYMMYY-RECDEL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-MTD-PAYS-RECDEL NOT NUMERIC 
              MOVE "LN-MTD-PAYS-RECDEL" TO D-FIELD
              MOVE OLN-MTD-PAYS-RECDEL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-AD-D-INS-COVR NOT NUMERIC 
              MOVE "LN-AD-D-INS-COVR" TO D-FIELD
              MOVE OLN-AD-D-INS-COVR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-TOT-PARTIAL-CHGOFF NOT NUMERIC 
              MOVE "LN-TOT-PARTIAL-CHGOFF" TO D-FIELD
              MOVE OLN-TOT-PARTIAL-CHGOFF  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PRIOR-RN-FOR10 NOT NUMERIC 
              MOVE "LN-PRIOR-RN-FOR10" TO D-FIELD
              MOVE OLN-PRIOR-RN-FOR10  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-LAST-RN-FOR10 NOT NUMERIC 
              MOVE "LN-LAST-RN-FOR10" TO D-FIELD
              MOVE OLN-LAST-RN-FOR10  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-WCBC-CREDITLIM NOT NUMERIC 
              MOVE "LN-WCBC-CREDITLIM" TO D-FIELD
              MOVE OLN-WCBC-CREDITLIM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-MIP-MTD-CLAHPAID NOT NUMERIC 
              MOVE "LN-MIP-MTD-CLAHPAID" TO D-FIELD
              MOVE OLN-MIP-MTD-CLAHPAID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-MIP-CLPDTH NOT NUMERIC 
              MOVE "LN-MIP-CLPDTH" TO D-FIELD
              MOVE OLN-MIP-CLPDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-MIP-TOT-CLPAID NOT NUMERIC 
              MOVE "LN-MIP-TOT-CLPAID" TO D-FIELD
              MOVE OLN-MIP-TOT-CLPAID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-MIP-AHPDTH NOT NUMERIC 
              MOVE "LN-MIP-AHPDTH" TO D-FIELD
              MOVE OLN-MIP-AHPDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-MIP-TOT-AHPAID NOT NUMERIC 
              MOVE "LN-MIP-TOT-AHPAID" TO D-FIELD
              MOVE OLN-MIP-TOT-AHPAID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-DLNO NOT NUMERIC 
              MOVE "LN-DLNO" TO D-FIELD
              MOVE OLN-DLNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-DLPOOL NOT NUMERIC 
              MOVE "LN-DLPOOL" TO D-FIELD
              MOVE OLN-DLPOOL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-DLPARTRCAMT NOT NUMERIC 
              MOVE "LN-DLPARTRCAMT" TO D-FIELD
              MOVE OLN-DLPARTRCAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-ORIGPURAMT NOT NUMERIC 
              MOVE "LN-ORIGPURAMT" TO D-FIELD
              MOVE OLN-ORIGPURAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PURBAL NOT NUMERIC 
              MOVE "LN-PURBAL" TO D-FIELD
              MOVE OLN-PURBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OLN-DLRVPRIN(SUB) NOT NUMERIC 
                 MOVE "LN-DLRVPRIN(SUB)" TO D-FIELD
                 MOVE OLN-DLRVPRIN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLN-DLRVCHGS(SUB) NOT NUMERIC 
                 MOVE "LN-DLRVCHGS(SUB)" TO D-FIELD
                 MOVE OLN-DLRVCHGS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OLN-DLPARVHELD NOT NUMERIC 
              MOVE "LN-DLPARVHELD" TO D-FIELD
              MOVE OLN-DLPARVHELD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-DLPARVPAID NOT NUMERIC 
              MOVE "LN-DLPARVPAID" TO D-FIELD
              MOVE OLN-DLPARVPAID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-DLDISC NOT NUMERIC 
              MOVE "LN-DLDISC" TO D-FIELD
              MOVE OLN-DLDISC  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-LCPDTH NOT NUMERIC OR
               (OLN-LCPDTH = 202020)
              MOVE "LN-LCPDTH" TO D-FIELD
              MOVE OLN-LCPDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-LCPDTH NOT = 0
                 MOVE OLN-LCPDTH TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-LCPDTH" TO D-FIELD
                    MOVE OLN-LCPDTH  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-INTPDTH NOT NUMERIC OR
               (OLN-INTPDTH = 202020)
              MOVE "LN-INTPDTH" TO D-FIELD
              MOVE OLN-INTPDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-INTPDTH NOT = 0
                 MOVE OLN-INTPDTH TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-INTPDTH" TO D-FIELD
                    MOVE OLN-INTPDTH  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-CURBAL NOT NUMERIC 
              MOVE "LN-CURBAL" TO D-FIELD
              MOVE OLN-CURBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-INTBAL NOT NUMERIC 
              MOVE "LN-INTBAL" TO D-FIELD
              MOVE OLN-INTBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-LCBAL NOT NUMERIC 
              MOVE "LN-LCBAL" TO D-FIELD
              MOVE OLN-LCBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-OTHBAL NOT NUMERIC 
              MOVE "LN-OTHBAL" TO D-FIELD
              MOVE OLN-OTHBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-OT2BAL NOT NUMERIC 
              MOVE "LN-OT2BAL" TO D-FIELD
              MOVE OLN-OT2BAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-LCPAID NOT NUMERIC 
              MOVE "LN-LCPAID" TO D-FIELD
              MOVE OLN-LCPAID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-LCPARTIALS NOT NUMERIC 
              MOVE "LN-LCPARTIALS" TO D-FIELD
              MOVE OLN-LCPARTIALS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-STMT-COUNTER NOT NUMERIC 
              MOVE "LN-STMT-COUNTER" TO D-FIELD
              MOVE OLN-STMT-COUNTER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-MONTHEND-BAL NOT NUMERIC 
              MOVE "LN-MONTHEND-BAL" TO D-FIELD
              MOVE OLN-MONTHEND-BAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-ANTICTERM NOT NUMERIC 
              MOVE "LN-ANTICTERM" TO D-FIELD
              MOVE OLN-ANTICTERM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-ANTICTERM NOT NUMERIC 
              MOVE "LN-ANTICTERM" TO D-FIELD
              MOVE OLN-ANTICTERM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OLN-ANTICERN(SUB) NOT NUMERIC 
                 MOVE "LN-ANTICERN(SUB)" TO D-FIELD
                 MOVE OLN-ANTICERN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLN-ANTICADJ(SUB) NOT NUMERIC 
                 MOVE "LN-ANTICADJ(SUB)" TO D-FIELD
                 MOVE OLN-ANTICADJ(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLN-UFERN(SUB) NOT NUMERIC 
                 MOVE "LN-UFERN(SUB)" TO D-FIELD
                 MOVE OLN-UFERN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLN-ACCERN(SUB) NOT NUMERIC 
                 MOVE "LN-ACCERN(SUB)" TO D-FIELD
                 MOVE OLN-ACCERN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLN-EARN-ORDER(SUB) NOT NUMERIC 
                 MOVE "LN-EARN-ORDER(SUB)" TO D-FIELD
                 MOVE OLN-EARN-ORDER(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OLN-LMO-PRIOR-INTEREST NOT NUMERIC 
              MOVE "LN-LMO-PRIOR-INTEREST" TO D-FIELD
              MOVE OLN-LMO-PRIOR-INTEREST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PRIOR-UNEARNED NOT NUMERIC 
              MOVE "LN-PRIOR-UNEARNED" TO D-FIELD
              MOVE OLN-PRIOR-UNEARNED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-NONACRERN NOT NUMERIC 
              MOVE "LN-NONACRERN" TO D-FIELD
              MOVE OLN-NONACRERN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PRIOR-NONACRERN NOT NUMERIC 
              MOVE "LN-PRIOR-NONACRERN" TO D-FIELD
              MOVE OLN-PRIOR-NONACRERN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-ERN-IB-PDTH NOT NUMERIC OR
               (OLN-ERN-IB-PDTH = 202020)
              MOVE "LN-ERN-IB-PDTH" TO D-FIELD
              MOVE OLN-ERN-IB-PDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-ERN-IB-PDTH NOT = 0
                 MOVE OLN-ERN-IB-PDTH TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-ERN-IB-PDTH" TO D-FIELD
                    MOVE OLN-ERN-IB-PDTH  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-ERN-IB-PRINBAL NOT NUMERIC 
              MOVE "LN-ERN-IB-PRINBAL" TO D-FIELD
              MOVE OLN-ERN-IB-PRINBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-ERN-IB-INTOWE NOT NUMERIC 
              MOVE "LN-ERN-IB-INTOWE" TO D-FIELD
              MOVE OLN-ERN-IB-INTOWE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-TOTERN NOT NUMERIC 
              MOVE "LN-TOTERN" TO D-FIELD
              MOVE OLN-TOTERN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-TOTOTH NOT NUMERIC 
              MOVE "LN-TOTOTH" TO D-FIELD
              MOVE OLN-TOTOTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-TOTINT NOT NUMERIC 
              MOVE "LN-TOTINT" TO D-FIELD
              MOVE OLN-TOTINT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-TOTLCHG NOT NUMERIC 
              MOVE "LN-TOTLCHG" TO D-FIELD
              MOVE OLN-TOTLCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-TOTDEF NOT NUMERIC 
              MOVE "LN-TOTDEF" TO D-FIELD
              MOVE OLN-TOTDEF  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-TOTSERACCT NOT NUMERIC 
              MOVE "LN-TOTSERACCT" TO D-FIELD
              MOVE OLN-TOTSERACCT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-LCWAIVERS NOT NUMERIC 
              MOVE "LN-LCWAIVERS" TO D-FIELD
              MOVE OLN-LCWAIVERS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-INTWAIVERS NOT NUMERIC 
              MOVE "LN-INTWAIVERS" TO D-FIELD
              MOVE OLN-INTWAIVERS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-TOTPAYMNTD NOT NUMERIC 
              MOVE "LN-TOTPAYMNTD" TO D-FIELD
              MOVE OLN-TOTPAYMNTD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-TOTEXCPAYMNTD NOT NUMERIC 
              MOVE "LN-TOTEXCPAYMNTD" TO D-FIELD
              MOVE OLN-TOTEXCPAYMNTD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-TOTNODEF NOT NUMERIC 
              MOVE "LN-TOTNODEF" TO D-FIELD
              MOVE OLN-TOTNODEF  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-YTDNODEF NOT NUMERIC 
              MOVE "LN-YTDNODEF" TO D-FIELD
              MOVE OLN-YTDNODEF  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-TOTNOALDEL NOT NUMERIC 
              MOVE "LN-TOTNOALDEL" TO D-FIELD
              MOVE OLN-TOTNOALDEL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-YTDINTPD NOT NUMERIC 
              MOVE "LN-YTDINTPD" TO D-FIELD
              MOVE OLN-YTDINTPD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-LYRINTPD NOT NUMERIC 
              MOVE "LN-LYRINTPD" TO D-FIELD
              MOVE OLN-LYRINTPD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-TOTINTPD NOT NUMERIC 
              MOVE "LN-TOTINTPD" TO D-FIELD
              MOVE OLN-TOTINTPD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-RECASTINT NOT NUMERIC 
              MOVE "LN-RECASTINT" TO D-FIELD
              MOVE OLN-RECASTINT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-RECASTPRIN NOT NUMERIC 
              MOVE "LN-RECASTPRIN" TO D-FIELD
              MOVE OLN-RECASTPRIN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-TRW-DOFD NOT NUMERIC OR
               (OLN-TRW-DOFD = 202020)
              MOVE "LN-TRW-DOFD" TO D-FIELD
              MOVE OLN-TRW-DOFD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-TRW-DOFD NOT = 0
                 MOVE OLN-TRW-DOFD TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-TRW-DOFD" TO D-FIELD
                    MOVE OLN-TRW-DOFD  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-LAST-PYPA-DATE NOT NUMERIC OR
               (OLN-LAST-PYPA-DATE = 202020)
              MOVE "LN-LAST-PYPA-DATE" TO D-FIELD
              MOVE OLN-LAST-PYPA-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-LAST-PYPA-DATE NOT = 0
                 MOVE OLN-LAST-PYPA-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-LAST-PYPA-DATE" TO D-FIELD
                    MOVE OLN-LAST-PYPA-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-PROMDAYS-PASTDUE NOT NUMERIC 
              MOVE "LN-PROMDAYS-PASTDUE" TO D-FIELD
              MOVE OLN-PROMDAYS-PASTDUE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OLN-PROMDATE NOT NUMERIC OR
               (OLN-PROMDATE = 202020)
              MOVE "LN-PROMDATE" TO D-FIELD
              MOVE OLN-PROMDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-PROMDATE NOT = 0
                 MOVE OLN-PROMDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-PROMDATE" TO D-FIELD
                    MOVE OLN-PROMDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-PROMAMT NOT NUMERIC 
              MOVE "LN-PROMAMT" TO D-FIELD
              MOVE OLN-PROMAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-DATE-PAID-LAST NOT NUMERIC OR
               (OLN-DATE-PAID-LAST = 202020)
              MOVE "LN-DATE-PAID-LAST" TO D-FIELD
              MOVE OLN-DATE-PAID-LAST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-DATE-PAID-LAST NOT = 0
                 MOVE OLN-DATE-PAID-LAST TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-DATE-PAID-LAST" TO D-FIELD
                    MOVE OLN-DATE-PAID-LAST  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-AMT-PAID-LAST NOT NUMERIC 
              MOVE "LN-AMT-PAID-LAST" TO D-FIELD
              MOVE OLN-AMT-PAID-LAST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-LASTCFORMNO NOT NUMERIC 
              MOVE "LN-LASTCFORMNO" TO D-FIELD
              MOVE OLN-LASTCFORMNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-LASTCFORMDATE NOT NUMERIC OR
               (OLN-LASTCFORMDATE = 202020)
              MOVE "LN-LASTCFORMDATE" TO D-FIELD
              MOVE OLN-LASTCFORMDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-LASTCFORMDATE NOT = 0
                 MOVE OLN-LASTCFORMDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-LASTCFORMDATE" TO D-FIELD
                    MOVE OLN-LASTCFORMDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-DELCNT NOT NUMERIC 
              MOVE "LN-DELCNT" TO D-FIELD
              MOVE OLN-DELCNT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-DELCNT2 NOT NUMERIC 
              MOVE "LN-DELCNT2" TO D-FIELD
              MOVE OLN-DELCNT2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-CREDITLIM NOT NUMERIC 
              MOVE "LN-CREDITLIM" TO D-FIELD
              MOVE OLN-CREDITLIM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-CRSCORE NOT NUMERIC 
              MOVE "LN-CRSCORE" TO D-FIELD
              MOVE OLN-CRSCORE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-LASTSFORMNO NOT NUMERIC 
              MOVE "LN-LASTSFORMNO" TO D-FIELD
              MOVE OLN-LASTSFORMNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-LASTSFORMDATE NOT NUMERIC OR
               (OLN-LASTSFORMDATE = 202020)
              MOVE "LN-LASTSFORMDATE" TO D-FIELD
              MOVE OLN-LASTSFORMDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-LASTSFORMDATE NOT = 0
                 MOVE OLN-LASTSFORMDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-LASTSFORMDATE" TO D-FIELD
                    MOVE OLN-LASTSFORMDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-SOLCNT NOT NUMERIC 
              MOVE "LN-SOLCNT" TO D-FIELD
              MOVE OLN-SOLCNT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PROMDATE2 NOT NUMERIC OR
               (OLN-PROMDATE2 = 202020)
              MOVE "LN-PROMDATE2" TO D-FIELD
              MOVE OLN-PROMDATE2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-PROMDATE2 NOT = 0
                 MOVE OLN-PROMDATE2 TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-PROMDATE2" TO D-FIELD
                    MOVE OLN-PROMDATE2  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLN-OLDACCTNO NOT NUMERIC 
              MOVE "LN-OLDACCTNO" TO D-FIELD
              MOVE OLN-OLDACCTNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PRLNNO NOT NUMERIC 
              MOVE "LN-PRLNNO" TO D-FIELD
              MOVE OLN-PRLNNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PRBAL NOT NUMERIC 
              MOVE "LN-PRBAL" TO D-FIELD
              MOVE OLN-PRBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PRINTDUE NOT NUMERIC 
              MOVE "LN-PRINTDUE" TO D-FIELD
              MOVE OLN-PRINTDUE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PRFINCHGRB NOT NUMERIC 
              MOVE "LN-PRFINCHGRB" TO D-FIELD
              MOVE OLN-PRFINCHGRB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PRINSRB NOT NUMERIC 
              MOVE "LN-PRINSRB" TO D-FIELD
              MOVE OLN-PRINSRB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PRPODATE NOT NUMERIC 
              MOVE "LN-PRPODATE" TO D-FIELD
              MOVE OLN-PRPODATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PRCONPROFILE NOT NUMERIC 
              MOVE "LN-PRCONPROFILE" TO D-FIELD
              MOVE OLN-PRCONPROFILE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PRDELINQAMT NOT NUMERIC 
              MOVE "LN-PRDELINQAMT" TO D-FIELD
              MOVE OLN-PRDELINQAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-SUM-ORIGPURAMT NOT NUMERIC 
              MOVE "LN-SUM-ORIGPURAMT" TO D-FIELD
              MOVE OLN-SUM-ORIGPURAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-SUM-LNAMT NOT NUMERIC 
              MOVE "LN-SUM-LNAMT" TO D-FIELD
              MOVE OLN-SUM-LNAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-SUM-FINCHG NOT NUMERIC 
              MOVE "LN-SUM-FINCHG" TO D-FIELD
              MOVE OLN-SUM-FINCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-SUM-INTCHG NOT NUMERIC 
              MOVE "LN-SUM-INTCHG" TO D-FIELD
              MOVE OLN-SUM-INTCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-SUM-ORGTERM NOT NUMERIC 
              MOVE "LN-SUM-ORGTERM" TO D-FIELD
              MOVE OLN-SUM-ORGTERM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-SUM-REGPYAMT NOT NUMERIC 
              MOVE "LN-SUM-REGPYAMT" TO D-FIELD
              MOVE OLN-SUM-REGPYAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-SUM-EXTCHG NOT NUMERIC 
              MOVE "LN-SUM-EXTCHG" TO D-FIELD
              MOVE OLN-SUM-EXTCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-SUM-SERCHG NOT NUMERIC 
              MOVE "LN-SUM-SERCHG" TO D-FIELD
              MOVE OLN-SUM-SERCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-SUM-MAINTFEE NOT NUMERIC 
              MOVE "LN-SUM-MAINTFEE" TO D-FIELD
              MOVE OLN-SUM-MAINTFEE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-UNITPER-FREQ NOT NUMERIC 
              MOVE "LN-UNITPER-FREQ" TO D-FIELD
              MOVE OLN-UNITPER-FREQ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OLN-DAYS-IN-UNITPER NOT NUMERIC 
              MOVE "LN-DAYS-IN-UNITPER" TO D-FIELD
              MOVE OLN-DAYS-IN-UNITPER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-UNITPER-PER-YEAR NOT NUMERIC 
              MOVE "LN-UNITPER-PER-YEAR" TO D-FIELD
              MOVE OLN-UNITPER-PER-YEAR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OLN-O3INS-DLPROC NOT NUMERIC 
              MOVE "LN-O3INS-DLPROC" TO D-FIELD
              MOVE OLN-O3INS-DLPROC  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OLN-INSPREM-DLADJ NOT NUMERIC 
              MOVE "LN-INSPREM-DLADJ" TO D-FIELD
              MOVE OLN-INSPREM-DLADJ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PRLNAMT NOT NUMERIC 
              MOVE "LN-PRLNAMT" TO D-FIELD
              MOVE OLN-PRLNAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PRPDUE-DAYS NOT NUMERIC 
              MOVE "LN-PRPDUE-DAYS" TO D-FIELD
              MOVE OLN-PRPDUE-DAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PLAMT NOT NUMERIC 
              MOVE "LN-PLAMT" TO D-FIELD
              MOVE OLN-PLAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-MTD-PRIN NOT NUMERIC 
              MOVE "LN-MTD-PRIN" TO D-FIELD
              MOVE OLN-MTD-PRIN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-MTD-LC NOT NUMERIC 
              MOVE "LN-MTD-LC" TO D-FIELD
              MOVE OLN-MTD-LC  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-MTD-INT NOT NUMERIC 
              MOVE "LN-MTD-INT" TO D-FIELD
              MOVE OLN-MTD-INT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-MTD-OT NOT NUMERIC 
              MOVE "LN-MTD-OT" TO D-FIELD
              MOVE OLN-MTD-OT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-MTD-EX NOT NUMERIC 
              MOVE "LN-MTD-EX" TO D-FIELD
              MOVE OLN-MTD-EX  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-BNKRPT-CHAPTER NOT NUMERIC 
              MOVE "LN-BNKRPT-CHAPTER" TO D-FIELD
              MOVE OLN-BNKRPT-CHAPTER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-APPNO NOT NUMERIC 
              MOVE "LN-APPNO" TO D-FIELD
              MOVE OLN-APPNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-APPNO-SA NOT NUMERIC 
              MOVE "LN-APPNO-SA" TO D-FIELD
              MOVE OLN-APPNO-SA  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-GIFTNET-OUR-PO NOT NUMERIC 
              MOVE "LN-GIFTNET-OUR-PO" TO D-FIELD
              MOVE OLN-GIFTNET-OUR-PO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               IF OLN-CONPROFILE(SUB) NOT NUMERIC 
                  MOVE "LN-CONPROFILE(SUB)" TO D-FIELD
                  MOVE OLN-CONPROFILE(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OLN-RECPROFILE(SUB) NOT NUMERIC 
                  MOVE "LN-RECPROFILE(SUB)" TO D-FIELD
                  MOVE OLN-RECPROFILE(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
           END-PERFORM.

           IF OLN-FOLLOWUP-DATE NOT NUMERIC OR
               (OLN-FOLLOWUP-DATE = 202020)
              MOVE "LN-FOLLOWUP-DATE" TO D-FIELD
              MOVE OLN-FOLLOWUP-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-FOLLOWUP-DATE NOT = 0
                 MOVE OLN-FOLLOWUP-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-FOLLOWUP-DATE" TO D-FIELD
                    MOVE OLN-FOLLOWUP-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLN-PRDLDISC-UNEARN NOT NUMERIC 
              MOVE "LN-PRDLDISC-UNEARN" TO D-FIELD
              MOVE OLN-PRDLDISC-UNEARN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-YTDNOALDEL NOT NUMERIC 
              MOVE "LN-YTDNOALDEL" TO D-FIELD
              MOVE OLN-YTDNOALDEL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OLN-BNKRPT-DISMISSAL NOT NUMERIC OR
               (OLN-BNKRPT-DISMISSAL = 202020)
              MOVE "LN-BNKRPT-DISMISSAL" TO D-FIELD
              MOVE OLN-BNKRPT-DISMISSAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-BNKRPT-DISMISSAL NOT = 0
                 MOVE OLN-BNKRPT-DISMISSAL TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-BNKRPT-DISMISSAL" TO D-FIELD
                    MOVE OLN-BNKRPT-DISMISSAL  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           
           IF OLN-LAST-ACCRUAL-DATE NOT NUMERIC OR
               (OLN-LAST-ACCRUAL-DATE = 202020)
              MOVE "LN-LAST-ACCRUAL-DATE" TO D-FIELD
              MOVE OLN-LAST-ACCRUAL-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLN-LAST-ACCRUAL-DATE NOT = 0
                 MOVE OLN-LAST-ACCRUAL-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LN-LAST-ACCRUAL-DATE" TO D-FIELD
                    MOVE OLN-LAST-ACCRUAL-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLN-TOTNODEFACT NOT NUMERIC 
              MOVE "LN-TOTNODEFACT" TO D-FIELD
              MOVE OLN-TOTNODEFACT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-YTDNODEFACT NOT NUMERIC 
              MOVE "LN-YTDNODEFACT" TO D-FIELD
              MOVE OLN-YTDNODEFACT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-TOTNOLCHG NOT NUMERIC 
              MOVE "LN-TOTNOLCHG" TO D-FIELD
              MOVE OLN-TOTNOLCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-PRIOR-STMT-COUNTER NOT NUMERIC 
              MOVE "LN-PRIOR-STMT-COUNTER" TO D-FIELD
              MOVE OLN-PRIOR-STMT-COUNTER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
       
           IF OLN-CONRATE NOT NUMERIC 
              MOVE "LN-CONRATE" TO D-FIELD
              MOVE OLN-CONRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-EXTRA-PRIN-LFP NOT NUMERIC 
              MOVE "LN-EXTRA-PRIN-LFP" TO D-FIELD
              MOVE OLN-EXTRA-PRIN-LFP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-REMAIN-INT-SINCE-LFP NOT NUMERIC 
              MOVE "LN-REMAIN-INT-SINCE-LFP" TO D-FIELD
              MOVE OLN-REMAIN-INT-SINCE-LFP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-REMAIN-PRIN-SINCE-LFP NOT NUMERIC 
              MOVE "LN-REMAIN-PRIN-SINCE-LFP" TO D-FIELD
              MOVE OLN-REMAIN-PRIN-SINCE-LFP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OLN-DLCOMM-O3-CMSI NOT NUMERIC 
              MOVE "LN-DLCOMM-O3-CMSI" TO D-FIELD
              MOVE OLN-DLCOMM-O3-CMSI  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLN-DLCOMM-PP-CMSI NOT NUMERIC 
              MOVE "LN-DLCOMM-PP-CMSI" TO D-FIELD
              MOVE OLN-DLCOMM-PP-CMSI  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-LN-NEXT.

       PROCESS-FILE-LN-END.

           PERFORM CLOSE-OLN1-FILE.

       PROCESS-FILE-LN-EXIT.
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

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OLM-PATH.

           MOVE SPACES                     TO OLM1-KEY.
           MOVE ZEROES                     TO OLM-ACCTNO
                                              OLM-DATE
                                              OLM-SEQ.
           PERFORM OPEN-OLM1-FILE.
           PERFORM START-OLM1-FILE.

       PROCESS-FILE-LM-NEXT.

           PERFORM READ-OLM1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-LM-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.

           PERFORM UP1534-VPI-DISPLAY.

           IF OLM-ACCTNO NOT NUMERIC 
              MOVE "LM-ACCTNO" TO D-FIELD
              MOVE OLM-ACCTNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLM-DATE NOT NUMERIC 
              MOVE "LM-DATE" TO D-FIELD
              MOVE OLM-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OLM-SEQ NOT NUMERIC 
              MOVE "LM-SEQ" TO D-FIELD
              MOVE OLM-SEQ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-LM-NEXT.

       PROCESS-FILE-LM-END.

           PERFORM CLOSE-OLM1-FILE.

       PROCESS-FILE-LM-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - L P
      *
      ******************************************************************
       PROCESS-FILE-LP SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-LP = "N" )
              GO TO PROCESS-FILE-LP-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO LP-PATH-OVERRIDE.
           MOVE "LP"                       TO UP1534-CURRENT-FILE.
           MOVE "GB/LPFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OLP-PATH.

           MOVE SPACES                     TO OLP1-KEY.
           MOVE ZEROES                     TO OLP-ACCTNO
                                              OLP-SEQNO.
           PERFORM OPEN-OLP1-FILE.
           PERFORM START-OLP1-FILE.

      *=================================================================
       PROCESS-FILE-LP-NEXT.

           PERFORM READ-OLP1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-LP-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.

           PERFORM UP1534-VPI-DISPLAY.

        
           IF OLP-ACCTNO NOT NUMERIC 
              MOVE "LP-ACCTNO" TO D-FIELD
              MOVE OLP-ACCTNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-SEQNO NOT NUMERIC 
              MOVE "LP-SEQNO" TO D-FIELD
              MOVE OLP-SEQNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-POSTDATE NOT NUMERIC OR
               (OLP-POSTDATE = 202020)
              MOVE "LP-POSTDATE" TO D-FIELD
              MOVE OLP-POSTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLP-POSTDATE NOT = 0
                 MOVE OLP-POSTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LP-POSTDATE" TO D-FIELD
                    MOVE OLP-POSTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLP-POSTTIME NOT NUMERIC 
              MOVE "LP-POSTTIME" TO D-FIELD
              MOVE OLP-POSTTIME  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-TRDATE NOT NUMERIC OR
               (OLP-TRDATE = 202020)
              MOVE "LP-TRDATE" TO D-FIELD
              MOVE OLP-TRDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLP-TRDATE NOT = 0
                 MOVE OLP-TRDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LP-TRDATE" TO D-FIELD
                    MOVE OLP-TRDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLP-PAYDATE NOT NUMERIC OR
               (OLP-PAYDATE = 202020)
              MOVE "LP-PAYDATE" TO D-FIELD
              MOVE OLP-PAYDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLP-PAYDATE NOT = 0
                 MOVE OLP-PAYDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LP-PAYDATE" TO D-FIELD
                    MOVE OLP-PAYDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLP-TRAMT NOT NUMERIC 
              MOVE "LP-TRAMT" TO D-FIELD
              MOVE OLP-TRAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-APOTH NOT NUMERIC 
              MOVE "LP-APOTH" TO D-FIELD
              MOVE OLP-APOTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-APOT2 NOT NUMERIC 
              MOVE "LP-APOT2" TO D-FIELD
              MOVE OLP-APOT2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-APINT NOT NUMERIC 
              MOVE "LP-APINT" TO D-FIELD
              MOVE OLP-APINT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-APLC NOT NUMERIC 
              MOVE "LP-APLC" TO D-FIELD
              MOVE OLP-APLC  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-APCUR NOT NUMERIC 
              MOVE "LP-APCUR" TO D-FIELD
              MOVE OLP-APCUR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-OTHBAL NOT NUMERIC 
              MOVE "LP-OTHBAL" TO D-FIELD
              MOVE OLP-OTHBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-OT2BAL NOT NUMERIC 
              MOVE "LP-OT2BAL" TO D-FIELD
              MOVE OLP-OT2BAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-INTBAL NOT NUMERIC 
              MOVE "LP-INTBAL" TO D-FIELD
              MOVE OLP-INTBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-LCBAL NOT NUMERIC 
              MOVE "LP-LCBAL" TO D-FIELD
              MOVE OLP-LCBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-CURBAL NOT NUMERIC 
              MOVE "LP-CURBAL" TO D-FIELD
              MOVE OLP-CURBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-INTDUE NOT NUMERIC 
              MOVE "LP-INTDUE" TO D-FIELD
              MOVE OLP-INTDUE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-DLPROC NOT NUMERIC 
              MOVE "LP-DLPROC" TO D-FIELD
              MOVE OLP-DLPROC  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OLP-EARNED(SUB) NOT NUMERIC 
                 MOVE "LP-EARNED(SUB)" TO D-FIELD
                 MOVE OLP-EARNED(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLP-DLRVCHGS-ADJ(SUB) NOT NUMERIC 
                 MOVE "LP-DLRVCHGS-ADJ(SUB)" TO D-FIELD
                 MOVE OLP-DLRVCHGS-ADJ(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OLP-ACCTNO NOT NUMERIC 
              MOVE "LP-ACCTNO" TO D-FIELD
              MOVE OLP-ACCTNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-INTPAID NOT NUMERIC 
              MOVE "LP-INTPAID" TO D-FIELD
              MOVE OLP-INTPAID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-SSNO NOT NUMERIC 
              MOVE "LP-SSNO" TO D-FIELD
              MOVE OLP-SSNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-PDTH NOT NUMERIC OR
               (OLP-PDTH = 202020)
              MOVE "LP-PDTH" TO D-FIELD
              MOVE OLP-PDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLP-PDTH NOT = 0
                 MOVE OLP-PDTH TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LP-PDTH" TO D-FIELD
                    MOVE OLP-PDTH  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLP-LCPDTH NOT NUMERIC OR
               (OLP-LCPDTH = 202020)
              MOVE "LP-LCPDTH" TO D-FIELD
              MOVE OLP-LCPDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLP-LCPDTH NOT = 0
                 MOVE OLP-LCPDTH TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LP-LCPDTH" TO D-FIELD
                    MOVE OLP-LCPDTH  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLP-APINTOWE NOT NUMERIC 
              MOVE "LP-APINTOWE" TO D-FIELD
              MOVE OLP-APINTOWE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-DLPARVHELD-ADJ NOT NUMERIC 
              MOVE "LP-DLPARVHELD-ADJ" TO D-FIELD
              MOVE OLP-DLPARVHELD-ADJ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-REVSEQNO NOT NUMERIC 
              MOVE "LP-REVSEQNO" TO D-FIELD
              MOVE OLP-REVSEQNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-NEWSTAT-RATE NOT NUMERIC 
              MOVE "LP-NEWSTAT-RATE" TO D-FIELD
              MOVE OLP-NEWSTAT-RATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-LCWORK1 NOT NUMERIC 
              MOVE "LP-LCWORK1" TO D-FIELD
              MOVE OLP-LCWORK1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-LCWORK2 NOT NUMERIC 
              MOVE "LP-LCWORK2" TO D-FIELD
              MOVE OLP-LCWORK2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-DLPARVPAID-ADJ NOT NUMERIC 
              MOVE "LP-DLPARVPAID-ADJ" TO D-FIELD
              MOVE OLP-DLPARVPAID-ADJ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-DLNO NOT NUMERIC 
              MOVE "LP-DLNO" TO D-FIELD
              MOVE OLP-DLNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-WORKAMT1 NOT NUMERIC 
              MOVE "LP-WORKAMT1" TO D-FIELD
              MOVE OLP-WORKAMT1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-WORKAMT2 NOT NUMERIC 
              MOVE "LP-WORKAMT2" TO D-FIELD
              MOVE OLP-WORKAMT2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-POSTING-BR NOT NUMERIC 
              MOVE "LP-POSTING-BR" TO D-FIELD
              MOVE OLP-POSTING-BR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-NOLCHG NOT NUMERIC 
              MOVE "LP-NOLCHG" TO D-FIELD
              MOVE OLP-NOLCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-EXTRA-PRIN-LFP NOT NUMERIC 
              MOVE "LP-EXTRA-PRIN-LFP" TO D-FIELD
              MOVE OLP-EXTRA-PRIN-LFP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-REMAIN-INT-SINCE-LFP NOT NUMERIC 
              MOVE "LP-REMAIN-INT-SINCE-LFP" TO D-FIELD
              MOVE OLP-REMAIN-INT-SINCE-LFP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-REMAIN-PRIN-SINCE-LFP NOT NUMERIC 
              MOVE "LP-REMAIN-PRIN-SINCE-LFP" TO D-FIELD
              MOVE OLP-REMAIN-PRIN-SINCE-LFP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-APPLIED-TO-ESCROW NOT NUMERIC 
              MOVE "LP-APPLIED-TO-ESCROW" TO D-FIELD
              MOVE OLP-APPLIED-TO-ESCROW  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-LP-NEXT.

       PROCESS-FILE-LP-END.

           PERFORM CLOSE-OLP1-FILE.

       PROCESS-FILE-LP-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - L S
      *
      ******************************************************************
       PROCESS-FILE-LS SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-LS = "N" )
              GO TO PROCESS-FILE-LS-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO LSD-PATH-OVERRIDE
                                              LSF-PATH-OVERRIDE
                                              LSG-PATH-OVERRIDE
                                              LSJ-PATH-OVERRIDE
                                              LSL-PATH-OVERRIDE
                                              LSM-PATH-OVERRIDE
                                              LSP-PATH-OVERRIDE
                                              LSR-PATH-OVERRIDE
                                              LSX-PATH-OVERRIDE.

           MOVE "LS"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/LSFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OLS-PATH.

           MOVE SPACES                     TO OLS1-KEY.
           MOVE ZEROES                     TO OLS-ACCTNO.
           PERFORM OPEN-OLS1-FILE.
           PERFORM START-OLS1-FILE.

       PROCESS-FILE-LS-NEXT.
           PERFORM READ-OLS1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-LS-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

      *=================================================================
      *          "D" = DEFICIENCY
      *          "F" = FOLLOW
      *          "G" = GPS
      *          "J" = JUDGEMENT
      *          "L" = LEGAL
      *          "M" = MISC
      *          "P" = PDTH
      *          "R" = RESCHEDULE
      *          "X" = REPO
      *          "Y" = REPO
      ******************************************************************

           IF ( OLS-CODE = "D" ) PERFORM VERIFY-FILE-LS-D.
           IF ( OLS-CODE = "F" ) PERFORM VERIFY-FILE-LS-F.
           IF ( OLS-CODE = "G" ) PERFORM VERIFY-FILE-LS-G.
           IF ( OLS-CODE = "J" ) PERFORM VERIFY-FILE-LS-J.
           IF ( OLS-CODE = "L" ) PERFORM VERIFY-FILE-LS-L.
           IF ( OLS-CODE = "P" ) PERFORM VERIFY-FILE-LS-P.
           IF ( OLS-CODE = "R" ) PERFORM VERIFY-FILE-LS-R.
           IF ( OLS-CODE = "X" ) PERFORM VERIFY-FILE-LS-X.
           IF ( OLS-CODE = "Y" ) PERFORM VERIFY-FILE-LS-Y.

           GO TO PROCESS-FILE-LS-NEXT.

       PROCESS-FILE-LS-END.

           PERFORM CLOSE-OLS1-FILE.

       PROCESS-FILE-LS-EXIT.
           EXIT.

      ******************************************
       VERIFY-FILE-LS-D SECTION.
      ******************************************
           IF OLS-D-STOLEN-DATE NOT NUMERIC OR
               (OLS-D-STOLEN-DATE = 202020)
              MOVE "LS-D-STOLEN-DATE" TO D-FIELD
              MOVE OLS-D-STOLEN-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-D-STOLEN-DATE NOT = 0
                 MOVE OLS-D-STOLEN-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-D-STOLEN-DATE" TO D-FIELD
                    MOVE OLS-D-STOLEN-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLS-D-GAP-INS-PAID NOT NUMERIC 
              MOVE "LS-D-GAP-INS-PAID" TO D-FIELD
              MOVE OLS-D-GAP-INS-PAID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-OTHER-EXPENSE NOT NUMERIC 
              MOVE "LS-D-OTHER-EXPENSE" TO D-FIELD
              MOVE OLS-D-OTHER-EXPENSE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-MISC-AMT-EXPENSE NOT NUMERIC 
              MOVE "LS-D-MISC-AMT-EXPENSE" TO D-FIELD
              MOVE OLS-D-MISC-AMT-EXPENSE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-TOT-EXPENSE NOT NUMERIC 
              MOVE "LS-D-TOT-EXPENSE" TO D-FIELD
              MOVE OLS-D-TOT-EXPENSE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-INT-REB NOT NUMERIC 
              MOVE "LS-D-INT-REB" TO D-FIELD
              MOVE OLS-D-INT-REB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-CPP-REB NOT NUMERIC 
              MOVE "LS-D-CPP-REB" TO D-FIELD
              MOVE OLS-D-CPP-REB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-GAP-REB NOT NUMERIC 
              MOVE "LS-D-GAP-REB" TO D-FIELD
              MOVE OLS-D-GAP-REB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-WARRANTY-REB NOT NUMERIC 
              MOVE "LS-D-WARRANTY-REB" TO D-FIELD
              MOVE OLS-D-WARRANTY-REB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-MISC-AMT-CREDIT NOT NUMERIC 
              MOVE "LS-D-MISC-AMT-CREDIT" TO D-FIELD
              MOVE OLS-D-MISC-AMT-CREDIT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-TOT-CREDITS NOT NUMERIC 
              MOVE "LS-D-TOT-CREDITS" TO D-FIELD
              MOVE OLS-D-TOT-CREDITS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-PAID-TO-OTHER NOT NUMERIC 
              MOVE "LS-D-PAID-TO-OTHER" TO D-FIELD
              MOVE OLS-D-PAID-TO-OTHER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-AMT-FROM-OWING NOT NUMERIC 
              MOVE "LS-D-AMT-FROM-OWING" TO D-FIELD
              MOVE OLS-D-AMT-FROM-OWING  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-AMT-OF-SURPLUS NOT NUMERIC 
              MOVE "LS-D-AMT-OF-SURPLUS" TO D-FIELD
              MOVE OLS-D-AMT-OF-SURPLUS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-AMT-OF-DEFICIENCY NOT NUMERIC 
              MOVE "LS-D-AMT-OF-DEFICIENCY" TO D-FIELD
              MOVE OLS-D-AMT-OF-DEFICIENCY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-SEND-FMNO NOT NUMERIC 
              MOVE "LS-D-SEND-FMNO" TO D-FIELD
              MOVE OLS-D-SEND-FMNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-INSURANCE-PROCEEDS NOT NUMERIC 
              MOVE "LS-D-INSURANCE-PROCEEDS" TO D-FIELD
              MOVE OLS-D-INSURANCE-PROCEEDS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-DATE-CHECK-RECV NOT NUMERIC OR
               (OLS-D-DATE-CHECK-RECV = 202020)
              MOVE "LS-D-DATE-CHECK-RECV" TO D-FIELD
              MOVE OLS-D-DATE-CHECK-RECV  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-D-DATE-CHECK-RECV NOT = 0
                 MOVE OLS-D-DATE-CHECK-RECV TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-D-DATE-CHECK-RECV" TO D-FIELD
                    MOVE OLS-D-DATE-CHECK-RECV  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLS-D-INSURANCE-CHECK NOT NUMERIC 
              MOVE "LS-D-INSURANCE-CHECK" TO D-FIELD
              MOVE OLS-D-INSURANCE-CHECK  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-DEF-LETTER-SENT NOT NUMERIC OR
               (OLS-D-DEF-LETTER-SENT = 202020)
              MOVE "LS-D-DEF-LETTER-SENT" TO D-FIELD
              MOVE OLS-D-DEF-LETTER-SENT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-D-DEF-LETTER-SENT NOT = 0
                 MOVE OLS-D-DEF-LETTER-SENT TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-D-DEF-LETTER-SENT" TO D-FIELD
                    MOVE OLS-D-DEF-LETTER-SENT  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-SALES-DATE NOT NUMERIC OR
               (OLS-D-SALES-DATE = 202020)
              MOVE "LS-D-SALES-DATE" TO D-FIELD
              MOVE OLS-D-SALES-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-D-SALES-DATE NOT = 0
                 MOVE OLS-D-SALES-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-D-SALES-DATE" TO D-FIELD
                    MOVE OLS-D-SALES-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-AUCTN-TIME NOT NUMERIC 
              MOVE "LS-D-AUCTN-TIME" TO D-FIELD
              MOVE OLS-D-AUCTN-TIME  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-AUCTN-PHONE NOT NUMERIC 
              MOVE "LS-D-AUCTN-PHONE" TO D-FIELD
              MOVE OLS-D-AUCTN-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-REPO-CHARGE NOT NUMERIC 
              MOVE "LS-D-REPO-CHARGE" TO D-FIELD
              MOVE OLS-D-REPO-CHARGE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-STORAGE-CHARGE NOT NUMERIC 
              MOVE "LS-D-STORAGE-CHARGE" TO D-FIELD
              MOVE OLS-D-STORAGE-CHARGE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-REPAIR-CHARGE NOT NUMERIC 
              MOVE "LS-D-REPAIR-CHARGE" TO D-FIELD
              MOVE OLS-D-REPAIR-CHARGE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-OTHER-CHARGE NOT NUMERIC 
              MOVE "LS-D-OTHER-CHARGE" TO D-FIELD
              MOVE OLS-D-OTHER-CHARGE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-STORAGE-RATE NOT NUMERIC 
              MOVE "LS-D-STORAGE-RATE" TO D-FIELD
              MOVE OLS-D-STORAGE-RATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-EST-REBATE NOT NUMERIC 
              MOVE "LS-D-EST-REBATE" TO D-FIELD
              MOVE OLS-D-EST-REBATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-D-MINIMUM-BID NOT NUMERIC 
              MOVE "LS-D-MINIMUM-BID" TO D-FIELD
              MOVE OLS-D-MINIMUM-BID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.


       EXIT-VERIFY-FILE-LS-D.
           EXIT.

      ******************************************************************
       VERIFY-FILE-LS-F SECTION.
      ******************************************************************

           IF OLS-F-SKIP-START-DATE NOT NUMERIC OR
               (OLS-F-SKIP-START-DATE = 202020)
              MOVE "LS-F-SKIP-START-DATE" TO D-FIELD
              MOVE OLS-F-SKIP-START-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-F-SKIP-START-DATE NOT = 0
                 MOVE OLS-F-SKIP-START-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-F-SKIP-START-DATE" TO D-FIELD
                    MOVE OLS-F-SKIP-START-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-F-SKIP-END-DATE NOT NUMERIC OR
               (OLS-F-SKIP-END-DATE = 202020)
              MOVE "LS-F-SKIP-END-DATE" TO D-FIELD
              MOVE OLS-F-SKIP-END-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-F-SKIP-END-DATE NOT = 0
                 MOVE OLS-F-SKIP-END-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-F-SKIP-END-DATE" TO D-FIELD
                    MOVE OLS-F-SKIP-END-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-F-OFR-START-DATE NOT NUMERIC OR
               (OLS-F-OFR-START-DATE = 202020)
              MOVE "LS-F-OFR-START-DATE" TO D-FIELD
              MOVE OLS-F-OFR-START-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-F-OFR-START-DATE NOT = 0
                 MOVE OLS-F-OFR-START-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-F-OFR-START-DATE" TO D-FIELD
                    MOVE OLS-F-OFR-START-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-F-OFR-END-DATE NOT NUMERIC OR
               (OLS-F-OFR-END-DATE = 202020)
              MOVE "LS-F-OFR-END-DATE" TO D-FIELD
              MOVE OLS-F-OFR-END-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-F-OFR-END-DATE NOT = 0
                 MOVE OLS-F-OFR-END-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-F-OFR-END-DATE" TO D-FIELD
                    MOVE OLS-F-OFR-END-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-F-IMPOUND-START-DATE NOT NUMERIC OR
               (OLS-F-IMPOUND-START-DATE = 202020)
              MOVE "LS-F-IMPOUND-START-DATE" TO D-FIELD
              MOVE OLS-F-IMPOUND-START-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-F-IMPOUND-START-DATE NOT = 0
                 MOVE OLS-F-IMPOUND-START-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-F-IMPOUND-START-DATE" TO D-FIELD
                    MOVE OLS-F-IMPOUND-START-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-F-IMPOUND-END-DATE NOT NUMERIC OR
               (OLS-F-IMPOUND-END-DATE = 202020)
              MOVE "LS-F-IMPOUND-END-DATE" TO D-FIELD
              MOVE OLS-F-IMPOUND-END-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-F-IMPOUND-END-DATE NOT = 0
                 MOVE OLS-F-IMPOUND-END-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-F-IMPOUND-END-DATE" TO D-FIELD
                    MOVE OLS-F-IMPOUND-END-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-F-BNKRPT-START-DATE NOT NUMERIC OR
               (OLS-F-BNKRPT-START-DATE = 202020)
              MOVE "LS-F-BNKRPT-START-DATE" TO D-FIELD
              MOVE OLS-F-BNKRPT-START-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-F-BNKRPT-START-DATE NOT = 0
                 MOVE OLS-F-BNKRPT-START-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-F-BNKRPT-START-DATE" TO D-FIELD
                    MOVE OLS-F-BNKRPT-START-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-F-BNKRPT-END-DATE NOT NUMERIC OR
               (OLS-F-BNKRPT-END-DATE = 202020)
              MOVE "LS-F-BNKRPT-END-DATE" TO D-FIELD
              MOVE OLS-F-BNKRPT-END-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-F-BNKRPT-END-DATE NOT = 0
                 MOVE OLS-F-BNKRPT-END-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-F-BNKRPT-END-DATE" TO D-FIELD
                    MOVE OLS-F-BNKRPT-END-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-F-INSUR-START-DATE NOT NUMERIC OR
               (OLS-F-INSUR-START-DATE = 202020)
              MOVE "LS-F-INSUR-START-DATE" TO D-FIELD
              MOVE OLS-F-INSUR-START-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-F-INSUR-START-DATE NOT = 0
                 MOVE OLS-F-INSUR-START-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-F-INSUR-START-DATE" TO D-FIELD
                    MOVE OLS-F-INSUR-START-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-F-INSUR-END-DATE NOT NUMERIC OR
               (OLS-F-INSUR-END-DATE = 202020)
              MOVE "LS-F-INSUR-END-DATE" TO D-FIELD
              MOVE OLS-F-INSUR-END-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-F-INSUR-END-DATE NOT = 0
                 MOVE OLS-F-INSUR-END-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-F-INSUR-END-DATE" TO D-FIELD
                    MOVE OLS-F-INSUR-END-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-F-COLLECT-START-DATE NOT NUMERIC OR
               (OLS-F-COLLECT-START-DATE = 202020)
              MOVE "LS-F-COLLECT-START-DATE" TO D-FIELD
              MOVE OLS-F-COLLECT-START-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-F-COLLECT-START-DATE NOT = 0
                 MOVE OLS-F-COLLECT-START-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-F-COLLECT-START-DATE" TO D-FIELD
                    MOVE OLS-F-COLLECT-START-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-F-COLLECT-END-DATE NOT NUMERIC OR
               (OLS-F-COLLECT-END-DATE = 202020)
              MOVE "LS-F-COLLECT-END-DATE" TO D-FIELD
              MOVE OLS-F-COLLECT-END-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-F-COLLECT-END-DATE NOT = 0
                 MOVE OLS-F-COLLECT-END-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-F-COLLECT-END-DATE" TO D-FIELD
                    MOVE OLS-F-COLLECT-END-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

       EXIT-VERIFY-FILE-LS-F.
           EXIT.

      ******************************************************************
       VERIFY-FILE-LS-G SECTION.
      ******************************************************************
           IF OLS-G-LAST-COMMAND-DATE NOT NUMERIC OR
               (OLS-G-LAST-COMMAND-DATE = 202020)
              MOVE "LS-G-LAST-COMMAND-DATE" TO D-FIELD
              MOVE OLS-G-LAST-COMMAND-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-G-LAST-COMMAND-DATE NOT = 0
                 MOVE OLS-G-LAST-COMMAND-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-G-LAST-COMMAND-DATE" TO D-FIELD
                    MOVE OLS-G-LAST-COMMAND-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-G-LAST-AUTO-REPORT-DATE NOT NUMERIC OR
               (OLS-G-LAST-AUTO-REPORT-DATE = 202020)
              MOVE "LS-G-LAST-AUTO-REPORT-DATE" TO D-FIELD
              MOVE OLS-G-LAST-AUTO-REPORT-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-G-LAST-AUTO-REPORT-DATE NOT = 0
                 MOVE OLS-G-LAST-AUTO-REPORT-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-G-LAST-AUTO-REPORT-DATE" TO D-FIELD
                    MOVE OLS-G-LAST-AUTO-REPORT-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.


       EXIT-VERIFY-FILE-LS-G.
           EXIT.

      ******************************************************************
       VERIFY-FILE-LS-J SECTION.
      ******************************************************************
           IF OLS-J-FILEDATE NOT NUMERIC OR
               (OLS-J-FILEDATE = 202020)
              MOVE "LS-J-FILEDATE" TO D-FIELD
              MOVE OLS-J-FILEDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-J-FILEDATE NOT = 0
                 MOVE OLS-J-FILEDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-J-FILEDATE" TO D-FIELD
                    MOVE OLS-J-FILEDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-J-AMT NOT NUMERIC 
              MOVE "LS-J-AMT" TO D-FIELD
              MOVE OLS-J-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-J-CLERK-PHONE NOT NUMERIC 
              MOVE "LS-J-CLERK-PHONE" TO D-FIELD
              MOVE OLS-J-CLERK-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-J-SHERIFF-FEE NOT NUMERIC 
              MOVE "LS-J-SHERIFF-FEE" TO D-FIELD
              MOVE OLS-J-SHERIFF-FEE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-J-COURT-FEE NOT NUMERIC 
              MOVE "LS-J-COURT-FEE" TO D-FIELD
              MOVE OLS-J-COURT-FEE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-J-ATTY-FEE NOT NUMERIC 
              MOVE "LS-J-ATTY-FEE" TO D-FIELD
              MOVE OLS-J-ATTY-FEE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-J-DISMISS-DATE NOT NUMERIC OR
               (OLS-J-DISMISS-DATE = 202020)
              MOVE "LS-J-DISMISS-DATE" TO D-FIELD
              MOVE OLS-J-DISMISS-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-J-DISMISS-DATE NOT = 0
                 MOVE OLS-J-DISMISS-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-J-DISMISS-DATE" TO D-FIELD
                    MOVE OLS-J-DISMISS-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-J-GARN-DATE NOT NUMERIC OR
               (OLS-J-GARN-DATE = 202020)
              MOVE "LS-J-GARN-DATE" TO D-FIELD
              MOVE OLS-J-GARN-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-J-GARN-DATE NOT = 0
                 MOVE OLS-J-GARN-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-J-GARN-DATE" TO D-FIELD
                    MOVE OLS-J-GARN-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLS-J-ATTY-PHONE NOT NUMERIC 
              MOVE "LS-J-ATTY-PHONE" TO D-FIELD
              MOVE OLS-J-ATTY-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.


       EXIT-VERIFY-FILE-LS-J.
           EXIT.

      ******************************************************************
       VERIFY-FILE-LS-L SECTION.
      ******************************************************************
           IF OLS-L-DATE-SUIT-AUTH NOT NUMERIC OR
               (OLS-L-DATE-SUIT-AUTH = 202020)
              MOVE "LS-L-DATE-SUIT-AUTH" TO D-FIELD
              MOVE OLS-L-DATE-SUIT-AUTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-DATE-SUIT-AUTH NOT = 0
                 MOVE OLS-L-DATE-SUIT-AUTH TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-DATE-SUIT-AUTH" TO D-FIELD
                    MOVE OLS-L-DATE-SUIT-AUTH  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLS-L-ATTORNEY-PHONE NOT NUMERIC 
              MOVE "LS-L-ATTORNEY-PHONE" TO D-FIELD
              MOVE OLS-L-ATTORNEY-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-DATE-ASSIGNED NOT NUMERIC OR
               (OLS-L-DATE-ASSIGNED = 202020)
              MOVE "LS-L-DATE-ASSIGNED" TO D-FIELD
              MOVE OLS-L-DATE-ASSIGNED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-DATE-ASSIGNED NOT = 0
                 MOVE OLS-L-DATE-ASSIGNED TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-DATE-ASSIGNED" TO D-FIELD
                    MOVE OLS-L-DATE-ASSIGNED  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLS-L-PRIN-BAL-SENT NOT NUMERIC 
              MOVE "LS-L-PRIN-BAL-SENT" TO D-FIELD
              MOVE OLS-L-PRIN-BAL-SENT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-RECEIVED-ACKNOW NOT NUMERIC OR
               (OLS-L-RECEIVED-ACKNOW = 202020)
              MOVE "LS-L-RECEIVED-ACKNOW" TO D-FIELD
              MOVE OLS-L-RECEIVED-ACKNOW  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-RECEIVED-ACKNOW NOT = 0
                 MOVE OLS-L-RECEIVED-ACKNOW TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-RECEIVED-ACKNOW" TO D-FIELD
                    MOVE OLS-L-RECEIVED-ACKNOW  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLS-L-ATTORNEY-LETTER-SENT NOT NUMERIC OR
               (OLS-L-ATTORNEY-LETTER-SENT = 202020)
              MOVE "LS-L-ATTORNEY-LETTER-SENT" TO D-FIELD
              MOVE OLS-L-ATTORNEY-LETTER-SENT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-ATTORNEY-LETTER-SENT NOT = 0
                 MOVE OLS-L-ATTORNEY-LETTER-SENT TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-ATTORNEY-LETTER-SENT" TO D-FIELD
                    MOVE OLS-L-ATTORNEY-LETTER-SENT  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-HOLD-DATE NOT NUMERIC OR
               (OLS-L-HOLD-DATE = 202020)
              MOVE "LS-L-HOLD-DATE" TO D-FIELD
              MOVE OLS-L-HOLD-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-HOLD-DATE NOT = 0
                 MOVE OLS-L-HOLD-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-HOLD-DATE" TO D-FIELD
                    MOVE OLS-L-HOLD-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-RESPONSE-RECEIVED NOT NUMERIC OR
               (OLS-L-RESPONSE-RECEIVED = 202020)
              MOVE "LS-L-RESPONSE-RECEIVED" TO D-FIELD
              MOVE OLS-L-RESPONSE-RECEIVED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-RESPONSE-RECEIVED NOT = 0
                 MOVE OLS-L-RESPONSE-RECEIVED TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-RESPONSE-RECEIVED" TO D-FIELD
                    MOVE OLS-L-RESPONSE-RECEIVED  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-SUIT-DATE NOT NUMERIC OR
               (OLS-L-SUIT-DATE = 202020)
              MOVE "LS-L-SUIT-DATE" TO D-FIELD
              MOVE OLS-L-SUIT-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-SUIT-DATE NOT = 0
                 MOVE OLS-L-SUIT-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-SUIT-DATE" TO D-FIELD
                    MOVE OLS-L-SUIT-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-SUIT-FILED NOT NUMERIC OR
               (OLS-L-SUIT-FILED = 202020)
              MOVE "LS-L-SUIT-FILED" TO D-FIELD
              MOVE OLS-L-SUIT-FILED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-SUIT-FILED NOT = 0
                 MOVE OLS-L-SUIT-FILED TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-SUIT-FILED" TO D-FIELD
                    MOVE OLS-L-SUIT-FILED  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-SERVICE-DATE NOT NUMERIC OR
               (OLS-L-SERVICE-DATE = 202020)
              MOVE "LS-L-SERVICE-DATE" TO D-FIELD
              MOVE OLS-L-SERVICE-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-SERVICE-DATE NOT = 0
                 MOVE OLS-L-SERVICE-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-SERVICE-DATE" TO D-FIELD
                    MOVE OLS-L-SERVICE-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-ANSWER-FILED NOT NUMERIC OR
               (OLS-L-ANSWER-FILED = 202020)
              MOVE "LS-L-ANSWER-FILED" TO D-FIELD
              MOVE OLS-L-ANSWER-FILED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-ANSWER-FILED NOT = 0
                 MOVE OLS-L-ANSWER-FILED TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-ANSWER-FILED" TO D-FIELD
                    MOVE OLS-L-ANSWER-FILED  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-COURT-DATE NOT NUMERIC OR
               (OLS-L-COURT-DATE = 202020)
              MOVE "LS-L-COURT-DATE" TO D-FIELD
              MOVE OLS-L-COURT-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-COURT-DATE NOT = 0
                 MOVE OLS-L-COURT-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-COURT-DATE" TO D-FIELD
                    MOVE OLS-L-COURT-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-JUDGMENT-OBTAINED NOT NUMERIC OR
               (OLS-L-JUDGMENT-OBTAINED = 202020)
              MOVE "LS-L-JUDGMENT-OBTAINED" TO D-FIELD
              MOVE OLS-L-JUDGMENT-OBTAINED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-JUDGMENT-OBTAINED NOT = 0
                 MOVE OLS-L-JUDGMENT-OBTAINED TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-JUDGMENT-OBTAINED" TO D-FIELD
                    MOVE OLS-L-JUDGMENT-OBTAINED  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLS-L-JUDGMENT-AMT NOT NUMERIC 
              MOVE "LS-L-JUDGMENT-AMT" TO D-FIELD
              MOVE OLS-L-JUDGMENT-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-JUDGMENT-PRIN NOT NUMERIC 
              MOVE "LS-L-JUDGMENT-PRIN" TO D-FIELD
              MOVE OLS-L-JUDGMENT-PRIN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-POST-JUDG-INT-RATE NOT NUMERIC 
              MOVE "LS-L-POST-JUDG-INT-RATE" TO D-FIELD
              MOVE OLS-L-POST-JUDG-INT-RATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-JUDG-INT-AMT NOT NUMERIC 
              MOVE "LS-L-JUDG-INT-AMT" TO D-FIELD
              MOVE OLS-L-JUDG-INT-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-ATTORNEY-FEES-ALLOWED NOT NUMERIC 
              MOVE "LS-L-ATTORNEY-FEES-ALLOWED" TO D-FIELD
              MOVE OLS-L-ATTORNEY-FEES-ALLOWED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-OTHER-FEES-ALLOWED NOT NUMERIC 
              MOVE "LS-L-OTHER-FEES-ALLOWED" TO D-FIELD
              MOVE OLS-L-OTHER-FEES-ALLOWED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-JUDG-INT-RATE NOT NUMERIC 
              MOVE "LS-L-JUDG-INT-RATE" TO D-FIELD
              MOVE OLS-L-JUDG-INT-RATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-GARN-FILED-DATE NOT NUMERIC OR
               (OLS-L-GARN-FILED-DATE = 202020)
              MOVE "LS-L-GARN-FILED-DATE" TO D-FIELD
              MOVE OLS-L-GARN-FILED-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-GARN-FILED-DATE NOT = 0
                 MOVE OLS-L-GARN-FILED-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-GARN-FILED-DATE" TO D-FIELD
                    MOVE OLS-L-GARN-FILED-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLS-L-GARN-TERM-DAYS NOT NUMERIC 
              MOVE "LS-L-GARN-TERM-DAYS" TO D-FIELD
              MOVE OLS-L-GARN-TERM-DAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-INTERROGATORY-SENT NOT NUMERIC OR
               (OLS-L-INTERROGATORY-SENT = 202020)
              MOVE "LS-L-INTERROGATORY-SENT" TO D-FIELD
              MOVE OLS-L-INTERROGATORY-SENT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-INTERROGATORY-SENT NOT = 0
                 MOVE OLS-L-INTERROGATORY-SENT TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-INTERROGATORY-SENT" TO D-FIELD
                    MOVE OLS-L-INTERROGATORY-SENT  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-INTERROGATORY-ANSW NOT NUMERIC OR
               (OLS-L-INTERROGATORY-ANSW = 202020)
              MOVE "LS-L-INTERROGATORY-ANSW" TO D-FIELD
              MOVE OLS-L-INTERROGATORY-ANSW  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-INTERROGATORY-ANSW NOT = 0
                 MOVE OLS-L-INTERROGATORY-ANSW TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-INTERROGATORY-ANSW" TO D-FIELD
                    MOVE OLS-L-INTERROGATORY-ANSW  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-FIFA-SENT NOT NUMERIC OR
               (OLS-L-FIFA-SENT = 202020)
              MOVE "LS-L-FIFA-SENT" TO D-FIELD
              MOVE OLS-L-FIFA-SENT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-FIFA-SENT NOT = 0
                 MOVE OLS-L-FIFA-SENT TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-FIFA-SENT" TO D-FIELD
                    MOVE OLS-L-FIFA-SENT  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-FIFA-ANSWERED NOT NUMERIC OR
               (OLS-L-FIFA-ANSWERED = 202020)
              MOVE "LS-L-FIFA-ANSWERED" TO D-FIELD
              MOVE OLS-L-FIFA-ANSWERED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-FIFA-ANSWERED NOT = 0
                 MOVE OLS-L-FIFA-ANSWERED TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-FIFA-ANSWERED" TO D-FIELD
                    MOVE OLS-L-FIFA-ANSWERED  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLS-L-DATE-TO-RECV-PAY NOT NUMERIC OR
               (OLS-L-DATE-TO-RECV-PAY = 202020)
              MOVE "LS-L-DATE-TO-RECV-PAY" TO D-FIELD
              MOVE OLS-L-DATE-TO-RECV-PAY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-DATE-TO-RECV-PAY NOT = 0
                 MOVE OLS-L-DATE-TO-RECV-PAY TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-DATE-TO-RECV-PAY" TO D-FIELD
                    MOVE OLS-L-DATE-TO-RECV-PAY  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-FOLLOWUP-DATE NOT NUMERIC OR
               (OLS-L-FOLLOWUP-DATE = 202020)
              MOVE "LS-L-FOLLOWUP-DATE" TO D-FIELD
              MOVE OLS-L-FOLLOWUP-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-FOLLOWUP-DATE NOT = 0
                 MOVE OLS-L-FOLLOWUP-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-FOLLOWUP-DATE" TO D-FIELD
                    MOVE OLS-L-FOLLOWUP-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-FIFA-RENEWED NOT NUMERIC OR
               (OLS-L-FIFA-RENEWED = 202020)
              MOVE "LS-L-FIFA-RENEWED" TO D-FIELD
              MOVE OLS-L-FIFA-RENEWED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-FIFA-RENEWED NOT = 0
                 MOVE OLS-L-FIFA-RENEWED TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-FIFA-RENEWED" TO D-FIELD
                    MOVE OLS-L-FIFA-RENEWED  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-L-DATE-CLOSED NOT NUMERIC OR
               (OLS-L-DATE-CLOSED = 202020)
              MOVE "LS-L-DATE-CLOSED" TO D-FIELD
              MOVE OLS-L-DATE-CLOSED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-L-DATE-CLOSED NOT = 0
                 MOVE OLS-L-DATE-CLOSED TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-L-DATE-CLOSED" TO D-FIELD
                    MOVE OLS-L-DATE-CLOSED  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

       EXIT-VERIFY-FILE-LS-L.
           EXIT.

      ******************************************************************
       VERIFY-FILE-LS-P SECTION.
      ******************************************************************
           IF OLS-P-PERIODS-TO-DEFER NOT NUMERIC 
              MOVE "LS-P-PERIODS-TO-DEFER" TO D-FIELD
              MOVE OLS-P-PERIODS-TO-DEFER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-P-MORTGAGE-BALANCE NOT NUMERIC 
              MOVE "LS-P-MORTGAGE-BALANCE" TO D-FIELD
              MOVE OLS-P-MORTGAGE-BALANCE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-P-INFO-DATE NOT NUMERIC OR
               (OLS-P-INFO-DATE = 202020)
              MOVE "LS-P-INFO-DATE" TO D-FIELD
              MOVE OLS-P-INFO-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-P-INFO-DATE NOT = 0
                 MOVE OLS-P-INFO-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-P-INFO-DATE" TO D-FIELD
                    MOVE OLS-P-INFO-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-P-BR-MANAGER-DATE NOT NUMERIC OR
               (OLS-P-BR-MANAGER-DATE = 202020)
              MOVE "LS-P-BR-MANAGER-DATE" TO D-FIELD
              MOVE OLS-P-BR-MANAGER-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-P-BR-MANAGER-DATE NOT = 0
                 MOVE OLS-P-BR-MANAGER-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-P-BR-MANAGER-DATE" TO D-FIELD
                    MOVE OLS-P-BR-MANAGER-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-P-AVP-DATE NOT NUMERIC OR
               (OLS-P-AVP-DATE = 202020)
              MOVE "LS-P-AVP-DATE" TO D-FIELD
              MOVE OLS-P-AVP-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-P-AVP-DATE NOT = 0
                 MOVE OLS-P-AVP-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-P-AVP-DATE" TO D-FIELD
                    MOVE OLS-P-AVP-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-P-VP-DATE NOT NUMERIC OR
               (OLS-P-VP-DATE = 202020)
              MOVE "LS-P-VP-DATE" TO D-FIELD
              MOVE OLS-P-VP-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-P-VP-DATE NOT = 0
                 MOVE OLS-P-VP-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-P-VP-DATE" TO D-FIELD
                    MOVE OLS-P-VP-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.


       EXIT-VERIFY-FILE-LS-P.
           EXIT.

      ******************************************************************
       VERIFY-FILE-LS-R SECTION.
      ******************************************************************
           IF OLS-R-SCRA-START-DATE NOT NUMERIC OR
               (OLS-R-SCRA-START-DATE = 202020)
              MOVE "LS-R-SCRA-START-DATE" TO D-FIELD
              MOVE OLS-R-SCRA-START-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-R-SCRA-START-DATE NOT = 0
                 MOVE OLS-R-SCRA-START-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-R-SCRA-START-DATE" TO D-FIELD
                    MOVE OLS-R-SCRA-START-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-R-SCRA-END-DATE NOT NUMERIC OR
               (OLS-R-SCRA-END-DATE = 202020)
              MOVE "LS-R-SCRA-END-DATE" TO D-FIELD
              MOVE OLS-R-SCRA-END-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-R-SCRA-END-DATE NOT = 0
                 MOVE OLS-R-SCRA-END-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-R-SCRA-END-DATE" TO D-FIELD
                    MOVE OLS-R-SCRA-END-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-R-CURRENT-RESCHED-DATE NOT NUMERIC OR
               (OLS-R-CURRENT-RESCHED-DATE = 202020)
              MOVE "LS-R-CURRENT-RESCHED-DATE" TO D-FIELD
              MOVE OLS-R-CURRENT-RESCHED-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-R-CURRENT-RESCHED-DATE NOT = 0
                 MOVE OLS-R-CURRENT-RESCHED-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-R-CURRENT-RESCHED-DATE" TO D-FIELD
                    MOVE OLS-R-CURRENT-RESCHED-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-R-INITIAL-RESCHED-DATE NOT NUMERIC OR
               (OLS-R-INITIAL-RESCHED-DATE = 202020)
              MOVE "LS-R-INITIAL-RESCHED-DATE" TO D-FIELD
              MOVE OLS-R-INITIAL-RESCHED-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-R-INITIAL-RESCHED-DATE NOT = 0
                 MOVE OLS-R-INITIAL-RESCHED-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-R-INITIAL-RESCHED-DATE" TO D-FIELD
                    MOVE OLS-R-INITIAL-RESCHED-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-R-INITIAL-TDR-DATE NOT NUMERIC OR
               (OLS-R-INITIAL-TDR-DATE = 202020)
              MOVE "LS-R-INITIAL-TDR-DATE" TO D-FIELD
              MOVE OLS-R-INITIAL-TDR-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-R-INITIAL-TDR-DATE NOT = 0
                 MOVE OLS-R-INITIAL-TDR-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-R-INITIAL-TDR-DATE" TO D-FIELD
                    MOVE OLS-R-INITIAL-TDR-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-R-CURRENT-TDR-DATE NOT NUMERIC OR
               (OLS-R-CURRENT-TDR-DATE = 202020)
              MOVE "LS-R-CURRENT-TDR-DATE" TO D-FIELD
              MOVE OLS-R-CURRENT-TDR-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-R-CURRENT-TDR-DATE NOT = 0
                 MOVE OLS-R-CURRENT-TDR-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-R-CURRENT-TDR-DATE" TO D-FIELD
                    MOVE OLS-R-CURRENT-TDR-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.


       EXIT-VERIFY-FILE-LS-R.
           EXIT.

      ******************************************************************
       VERIFY-FILE-LS-X SECTION.
      ******************************************************************
           IF OLS-X-REPODATE NOT NUMERIC OR
               (OLS-X-REPODATE = 202020)
              MOVE "LS-X-REPODATE" TO D-FIELD
              MOVE OLS-X-REPODATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-REPODATE NOT = 0
                 MOVE OLS-X-REPODATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-REPODATE" TO D-FIELD
                    MOVE OLS-X-REPODATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-TITLE-MILEAGE NOT NUMERIC 
              MOVE "LS-X-TITLE-MILEAGE" TO D-FIELD
              MOVE OLS-X-TITLE-MILEAGE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-AUTHORIZED-DATE NOT NUMERIC OR
               (OLS-X-AUTHORIZED-DATE = 202020)
              MOVE "LS-X-AUTHORIZED-DATE" TO D-FIELD
              MOVE OLS-X-AUTHORIZED-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-AUTHORIZED-DATE NOT = 0
                 MOVE OLS-X-AUTHORIZED-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-AUTHORIZED-DATE" TO D-FIELD
                    MOVE OLS-X-AUTHORIZED-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-ASSIGNED-DATE NOT NUMERIC OR
               (OLS-X-ASSIGNED-DATE = 202020)
              MOVE "LS-X-ASSIGNED-DATE" TO D-FIELD
              MOVE OLS-X-ASSIGNED-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-ASSIGNED-DATE NOT = 0
                 MOVE OLS-X-ASSIGNED-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-ASSIGNED-DATE" TO D-FIELD
                    MOVE OLS-X-ASSIGNED-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-REPO-CKNO NOT NUMERIC 
              MOVE "LS-X-REPO-CKNO" TO D-FIELD
              MOVE OLS-X-REPO-CKNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-REPO-CKDATE NOT NUMERIC OR
               (OLS-X-REPO-CKDATE = 202020)
              MOVE "LS-X-REPO-CKDATE" TO D-FIELD
              MOVE OLS-X-REPO-CKDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-REPO-CKDATE NOT = 0
                 MOVE OLS-X-REPO-CKDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-REPO-CKDATE" TO D-FIELD
                    MOVE OLS-X-REPO-CKDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-REPO-CKAMT NOT NUMERIC 
              MOVE "LS-X-REPO-CKAMT" TO D-FIELD
              MOVE OLS-X-REPO-CKAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OLS-X-STORED-CKDATE NOT NUMERIC OR
               (OLS-X-STORED-CKDATE = 202020)
              MOVE "LS-X-STORED-CKDATE" TO D-FIELD
              MOVE OLS-X-STORED-CKDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-STORED-CKDATE NOT = 0
                 MOVE OLS-X-STORED-CKDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-STORED-CKDATE" TO D-FIELD
                    MOVE OLS-X-STORED-CKDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLS-X-STORED-CKNO NOT NUMERIC 
              MOVE "LS-X-STORED-CKNO" TO D-FIELD
              MOVE OLS-X-STORED-CKNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-STORED-CKAMT NOT NUMERIC 
              MOVE "LS-X-STORED-CKAMT" TO D-FIELD
              MOVE OLS-X-STORED-CKAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OLS-X-REDEEM-DATE NOT NUMERIC OR
               (OLS-X-REDEEM-DATE = 202020)
              MOVE "LS-X-REDEEM-DATE" TO D-FIELD
              MOVE OLS-X-REDEEM-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-REDEEM-DATE NOT = 0
                 MOVE OLS-X-REDEEM-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-REDEEM-DATE" TO D-FIELD
                    MOVE OLS-X-REDEEM-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-BUREAU-DATE NOT NUMERIC OR
               (OLS-X-BUREAU-DATE = 202020)
              MOVE "LS-X-BUREAU-DATE" TO D-FIELD
              MOVE OLS-X-BUREAU-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-BUREAU-DATE NOT = 0
                 MOVE OLS-X-BUREAU-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-BUREAU-DATE" TO D-FIELD
                    MOVE OLS-X-BUREAU-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-10DAY-DATE NOT NUMERIC OR
               (OLS-X-10DAY-DATE = 202020)
              MOVE "LS-X-10DAY-DATE" TO D-FIELD
              MOVE OLS-X-10DAY-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-10DAY-DATE NOT = 0
                 MOVE OLS-X-10DAY-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-10DAY-DATE" TO D-FIELD
                    MOVE OLS-X-10DAY-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-10DAY-COST NOT NUMERIC 
              MOVE "LS-X-10DAY-COST" TO D-FIELD
              MOVE OLS-X-10DAY-COST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-TITLE-APPL-DATE NOT NUMERIC OR
               (OLS-X-TITLE-APPL-DATE = 202020)
              MOVE "LS-X-TITLE-APPL-DATE" TO D-FIELD
              MOVE OLS-X-TITLE-APPL-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-TITLE-APPL-DATE NOT = 0
                 MOVE OLS-X-TITLE-APPL-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-TITLE-APPL-DATE" TO D-FIELD
                    MOVE OLS-X-TITLE-APPL-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-TITLE-APPL-COST NOT NUMERIC 
              MOVE "LS-X-TITLE-APPL-COST" TO D-FIELD
              MOVE OLS-X-TITLE-APPL-COST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-COND-MILEAGE NOT NUMERIC 
              MOVE "LS-X-COND-MILEAGE" TO D-FIELD
              MOVE OLS-X-COND-MILEAGE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-REPAIRS-COST NOT NUMERIC 
              MOVE "LS-X-REPAIRS-COST" TO D-FIELD
              MOVE OLS-X-REPAIRS-COST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OLS-X-ASSIGN-COMPLETE(1) NOT NUMERIC OR
               (OLS-X-ASSIGN-COMPLETE(1) = 202020)
              MOVE "LS-X-ASSIGN-COMPLETE(1)" TO D-FIELD
              MOVE OLS-X-ASSIGN-COMPLETE(1)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-ASSIGN-COMPLETE(1) NOT = 0
                 MOVE OLS-X-ASSIGN-COMPLETE(1) TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-ASSIGN-COMPLETE(1)" TO D-FIELD
                    MOVE OLS-X-ASSIGN-COMPLETE(1)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-ASSIGN-COMPLETE(2) NOT NUMERIC OR
               (OLS-X-ASSIGN-COMPLETE(2) = 202020)
              MOVE "LS-X-ASSIGN-COMPLETE(2)" TO D-FIELD
              MOVE OLS-X-ASSIGN-COMPLETE(2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-ASSIGN-COMPLETE(2) NOT = 0
                 MOVE OLS-X-ASSIGN-COMPLETE(2) TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-ASSIGN-COMPLETE(2)" TO D-FIELD
                    MOVE OLS-X-ASSIGN-COMPLETE(2)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-ASSIGN-CKDATE(1) NOT NUMERIC OR
               (OLS-X-ASSIGN-CKDATE(1) = 202020)
              MOVE "LS-X-ASSIGN-CKDATE(1)" TO D-FIELD
              MOVE OLS-X-ASSIGN-CKDATE(1)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-ASSIGN-CKDATE(1) NOT = 0
                 MOVE OLS-X-ASSIGN-CKDATE(1) TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-ASSIGN-CKDATE(1)" TO D-FIELD
                    MOVE OLS-X-ASSIGN-CKDATE(1)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-ASSIGN-CKDATE(2) NOT NUMERIC OR
               (OLS-X-ASSIGN-CKDATE(2) = 202020)
              MOVE "LS-X-ASSIGN-CKDATE(2)" TO D-FIELD
              MOVE OLS-X-ASSIGN-CKDATE(2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-ASSIGN-CKDATE(2) NOT = 0
                 MOVE OLS-X-ASSIGN-CKDATE(2) TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-ASSIGN-CKDATE(2)" TO D-FIELD
                    MOVE OLS-X-ASSIGN-CKDATE(2)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
              IF OLS-X-ASSIGN-CKNO(SUB) NOT NUMERIC 
                 MOVE "LS-X-ASSIGN-CKNO(SUB)" TO D-FIELD
                 MOVE OLS-X-ASSIGN-CKNO(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLS-X-ASSIGN-CKAMT(SUB) NOT NUMERIC 
                 MOVE "LS-X-ASSIGN-CKAMT(SUB)" TO D-FIELD
                 MOVE OLS-X-ASSIGN-CKAMT(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OLS-X-OTH-EXP-CKDATE(1) NOT NUMERIC OR
               (OLS-X-OTH-EXP-CKDATE(1) = 202020)
              MOVE "LS-X-OTH-EXP-CKDATE(1)" TO D-FIELD
              MOVE OLS-X-OTH-EXP-CKDATE(1)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-OTH-EXP-CKDATE(1) NOT = 0
                 MOVE OLS-X-OTH-EXP-CKDATE(1) TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-OTH-EXP-CKDATE(1)" TO D-FIELD
                    MOVE OLS-X-OTH-EXP-CKDATE(1)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-OTH-EXP-CKDATE(2) NOT NUMERIC OR
               (OLS-X-OTH-EXP-CKDATE(2) = 202020)
              MOVE "LS-X-OTH-EXP-CKDATE(2)" TO D-FIELD
              MOVE OLS-X-OTH-EXP-CKDATE(2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-OTH-EXP-CKDATE(2) NOT = 0
                 MOVE OLS-X-OTH-EXP-CKDATE(2) TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-OTH-EXP-CKDATE(2)" TO D-FIELD
                    MOVE OLS-X-OTH-EXP-CKDATE(2)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-OTH-EXP-CKDATE(3) NOT NUMERIC OR
               (OLS-X-OTH-EXP-CKDATE(3) = 202020)
              MOVE "LS-X-OTH-EXP-CKDATE(3)" TO D-FIELD
              MOVE OLS-X-OTH-EXP-CKDATE(3)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-OTH-EXP-CKDATE(3) NOT = 0
                 MOVE OLS-X-OTH-EXP-CKDATE(3) TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-OTH-EXP-CKDATE(3)" TO D-FIELD
                    MOVE OLS-X-OTH-EXP-CKDATE(3)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OLS-X-OTH-EXP-CKNO(SUB) NOT NUMERIC 
                 MOVE "LS-X-OTH-EXP-CKNO(SUB)" TO D-FIELD
                 MOVE OLS-X-OTH-EXP-CKNO(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLS-X-OTH-EXP-CKAMT(SUB) NOT NUMERIC 
                 MOVE "LS-X-OTH-EXP-CKAMT(SUB)" TO D-FIELD
                 MOVE OLS-X-OTH-EXP-CKAMT(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OLS-X-INS-RB-REQ-DATE NOT NUMERIC OR
               (OLS-X-INS-RB-REQ-DATE = 202020)
              MOVE "LS-X-INS-RB-REQ-DATE" TO D-FIELD
              MOVE OLS-X-INS-RB-REQ-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-INS-RB-REQ-DATE NOT = 0
                 MOVE OLS-X-INS-RB-REQ-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-INS-RB-REQ-DATE" TO D-FIELD
                    MOVE OLS-X-INS-RB-REQ-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-INS-RB-REC-DATE NOT NUMERIC OR
               (OLS-X-INS-RB-REC-DATE = 202020)
              MOVE "LS-X-INS-RB-REC-DATE" TO D-FIELD
              MOVE OLS-X-INS-RB-REC-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-INS-RB-REC-DATE NOT = 0
                 MOVE OLS-X-INS-RB-REC-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-INS-RB-REC-DATE" TO D-FIELD
                    MOVE OLS-X-INS-RB-REC-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLS-X-INS-RB-AMT NOT NUMERIC 
              MOVE "LS-X-INS-RB-AMT" TO D-FIELD
              MOVE OLS-X-INS-RB-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OLS-X-RB-REQ-DATE NOT NUMERIC OR
               (OLS-X-RB-REQ-DATE = 202020)
              MOVE "LS-X-RB-REQ-DATE" TO D-FIELD
              MOVE OLS-X-RB-REQ-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-RB-REQ-DATE NOT = 0
                 MOVE OLS-X-RB-REQ-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-RB-REQ-DATE" TO D-FIELD
                    MOVE OLS-X-RB-REQ-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-RB-REC-DATE NOT NUMERIC OR
               (OLS-X-RB-REC-DATE = 202020)
              MOVE "LS-X-RB-REC-DATE" TO D-FIELD
              MOVE OLS-X-RB-REC-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-RB-REC-DATE NOT = 0
                 MOVE OLS-X-RB-REC-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-RB-REC-DATE" TO D-FIELD
                    MOVE OLS-X-RB-REC-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLS-X-RB-AMT NOT NUMERIC 
              MOVE "LS-X-RB-AMT" TO D-FIELD
              MOVE OLS-X-RB-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-SIC-INS-FILE-DATE NOT NUMERIC OR
               (OLS-X-SIC-INS-FILE-DATE = 202020)
              MOVE "LS-X-SIC-INS-FILE-DATE" TO D-FIELD
              MOVE OLS-X-SIC-INS-FILE-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-SIC-INS-FILE-DATE NOT = 0
                 MOVE OLS-X-SIC-INS-FILE-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-SIC-INS-FILE-DATE" TO D-FIELD
                    MOVE OLS-X-SIC-INS-FILE-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-SIC-INS-PAID-DATE NOT NUMERIC OR
               (OLS-X-SIC-INS-PAID-DATE = 202020)
              MOVE "LS-X-SIC-INS-PAID-DATE" TO D-FIELD
              MOVE OLS-X-SIC-INS-PAID-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-SIC-INS-PAID-DATE NOT = 0
                 MOVE OLS-X-SIC-INS-PAID-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-SIC-INS-PAID-DATE" TO D-FIELD
                    MOVE OLS-X-SIC-INS-PAID-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-SIC-INS-PAID-AMT NOT NUMERIC 
              MOVE "LS-X-SIC-INS-PAID-AMT" TO D-FIELD
              MOVE OLS-X-SIC-INS-PAID-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-BOOK-VALUE NOT NUMERIC 
              MOVE "LS-X-BOOK-VALUE" TO D-FIELD
              MOVE OLS-X-BOOK-VALUE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-SOLD-TO-DATE NOT NUMERIC OR
               (OLS-X-SOLD-TO-DATE = 202020)
              MOVE "LS-X-SOLD-TO-DATE" TO D-FIELD
              MOVE OLS-X-SOLD-TO-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-SOLD-TO-DATE NOT = 0
                 MOVE OLS-X-SOLD-TO-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-SOLD-TO-DATE" TO D-FIELD
                    MOVE OLS-X-SOLD-TO-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-SOLD-TO-AMT NOT NUMERIC 
              MOVE "LS-X-SOLD-TO-AMT" TO D-FIELD
              MOVE OLS-X-SOLD-TO-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-DLNO NOT NUMERIC 
              MOVE "LS-X-DLNO" TO D-FIELD
              MOVE OLS-X-DLNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-DLR-RSV-CKNO NOT NUMERIC 
              MOVE "LS-X-DLR-RSV-CKNO" TO D-FIELD
              MOVE OLS-X-DLR-RSV-CKNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-DLR-RSV-CKAMT NOT NUMERIC 
              MOVE "LS-X-DLR-RSV-CKAMT" TO D-FIELD
              MOVE OLS-X-DLR-RSV-CKAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-PL-EXPENSE NOT NUMERIC 
              MOVE "LS-X-PL-EXPENSE" TO D-FIELD
              MOVE OLS-X-PL-EXPENSE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-NET-BAL-DUE NOT NUMERIC 
              MOVE "LS-X-NET-BAL-DUE" TO D-FIELD
              MOVE OLS-X-NET-BAL-DUE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-DEFICIENCY-BAL NOT NUMERIC 
              MOVE "LS-X-DEFICIENCY-BAL" TO D-FIELD
              MOVE OLS-X-DEFICIENCY-BAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-AUCTION-EXPENSE NOT NUMERIC 
              MOVE "LS-X-AUCTION-EXPENSE" TO D-FIELD
              MOVE OLS-X-AUCTION-EXPENSE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-CYLINDERS NOT NUMERIC 
              MOVE "LS-X-CYLINDERS" TO D-FIELD
              MOVE OLS-X-CYLINDERS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-AGENT-PHONE NOT NUMERIC 
              MOVE "LS-X-AGENT-PHONE" TO D-FIELD
              MOVE OLS-X-AGENT-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-AGENT-FAX NOT NUMERIC 
              MOVE "LS-X-AGENT-FAX" TO D-FIELD
              MOVE OLS-X-AGENT-FAX  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-X-TITLE-SENT-DATE NOT NUMERIC OR
               (OLS-X-TITLE-SENT-DATE = 202020)
              MOVE "LS-X-TITLE-SENT-DATE" TO D-FIELD
              MOVE OLS-X-TITLE-SENT-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-X-TITLE-SENT-DATE NOT = 0
                 MOVE OLS-X-TITLE-SENT-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-X-TITLE-SENT-DATE" TO D-FIELD
                    MOVE OLS-X-TITLE-SENT-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.


       EXIT-VERIFY-FILE-LS-X.
           EXIT.

      ******************************************************************
       VERIFY-FILE-LS-Y SECTION.
      ******************************************************************
           IF OLS-Y-NEXT-HLD-RVW-DATE NOT NUMERIC OR
               (OLS-Y-NEXT-HLD-RVW-DATE = 202020)
              MOVE "LS-Y-NEXT-HLD-RVW-DATE" TO D-FIELD
              MOVE OLS-Y-NEXT-HLD-RVW-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLS-Y-NEXT-HLD-RVW-DATE NOT = 0
                 MOVE OLS-Y-NEXT-HLD-RVW-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LS-Y-NEXT-HLD-RVW-DATE" TO D-FIELD
                    MOVE OLS-Y-NEXT-HLD-RVW-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLS-Y-CASE-ID NOT NUMERIC 
              MOVE "LS-Y-CASE-ID" TO D-FIELD
              MOVE OLS-Y-CASE-ID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-Y-SUPPLIER-ID NOT NUMERIC 
              MOVE "LS-Y-SUPPLIER-ID" TO D-FIELD
              MOVE OLS-Y-SUPPLIER-ID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLS-Y-AGENCY-PHONE NOT NUMERIC 
              MOVE "LS-Y-AGENCY-PHONE" TO D-FIELD
              MOVE OLS-Y-AGENCY-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

       EXIT-VERIFY-FILE-LS-Y.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - L T
      *
      ******************************************************************
       PROCESS-FILE-LT SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-LT = "N" )
              GO TO PROCESS-FILE-LT-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO LTA-PATH-OVERRIDE
                                              LTB-PATH-OVERRIDE
                                              LTC-PATH-OVERRIDE
                                              LTD-PATH-OVERRIDE
                                              LTF-PATH-OVERRIDE
                                              LTI-PATH-OVERRIDE
                                              LTL-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           MOVE "LT"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/LTFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OLT-PATH.

           MOVE SPACES                     TO OLT1-KEY.
           MOVE ZEROES                     TO OLT-ACCTNO
                                              OLT-SEQNO.

           PERFORM OPEN-OLT1-FILE.
           PERFORM START-OLT1-FILE.

       PROCESS-FILE-LT-NEXT.

           PERFORM READ-OLT1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-LT-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.

           PERFORM UP1534-VPI-DISPLAY.

           IF ( OLT-CODE = "A" ) PERFORM VERIFY-FILE-LT-A.
           IF ( OLT-CODE = "B" ) PERFORM VERIFY-FILE-LT-B.
           IF ( OLT-CODE = "C" ) PERFORM VERIFY-FILE-LT-C.
           IF ( OLT-CODE = "D" ) PERFORM VERIFY-FILE-LT-D.
           IF ( OLT-CODE = "F" ) PERFORM VERIFY-FILE-LT-F.
           IF ( OLT-CODE = "I" ) PERFORM VERIFY-FILE-LT-I.
           IF ( OLT-CODE = "L" ) PERFORM VERIFY-FILE-LT-L.
           IF ( OLT-CODE = "P" ) PERFORM VERIFY-FILE-LT-P.

           GO TO PROCESS-FILE-LT-NEXT.

       PROCESS-FILE-LT-END.

           PERFORM CLOSE-OLT1-FILE.

       PROCESS-FILE-LT-EXIT.
           EXIT.

      ******************************************
       VERIFY-FILE-LT-A SECTION.
      ******************************************
      * SEQUENCE 1 HAS NO NUMERICS
           IF OLT-SEQNO = 1
              GO TO VERIFY-FILE-LT-A-EXIT.

           IF OLT-SEQNO = 3
              GO TO VERIFY-FILE-LT-A-3.

           IF OLT-A-PHONE NOT NUMERIC 
              MOVE "LT-A-PHONE" TO D-FIELD
              MOVE OLT-A-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-A-EFF-DATE NOT NUMERIC OR
               (OLT-A-EFF-DATE = 202020)
              MOVE "LT-A-EFF-DATE" TO D-FIELD
              MOVE OLT-A-EFF-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-A-EFF-DATE NOT = 0
                 MOVE OLT-A-EFF-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-A-EFF-DATE" TO D-FIELD
                    MOVE OLT-A-EFF-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLT-A-EXP-DATE NOT NUMERIC OR
               (OLT-A-EXP-DATE = 202020)
              MOVE "LT-A-EXP-DATE" TO D-FIELD
              MOVE OLT-A-EXP-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-A-EXP-DATE NOT = 0
                 MOVE OLT-A-EXP-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-A-EXP-DATE" TO D-FIELD
                    MOVE OLT-A-EXP-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLT-A-CAN-DATE NOT NUMERIC OR
               (OLT-A-CAN-DATE = 202020)
              MOVE "LT-A-CAN-DATE" TO D-FIELD
              MOVE OLT-A-CAN-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-A-CAN-DATE NOT = 0
                 MOVE OLT-A-CAN-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-A-CAN-DATE" TO D-FIELD
                    MOVE OLT-A-CAN-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLT-A-CONF-DATE NOT NUMERIC OR
               (OLT-A-CONF-DATE = 202020)
              MOVE "LT-A-CONF-DATE" TO D-FIELD
              MOVE OLT-A-CONF-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-A-CONF-DATE NOT = 0
                 MOVE OLT-A-CONF-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-A-CONF-DATE" TO D-FIELD
                    MOVE OLT-A-CONF-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLT-A-LAST-FORM NOT NUMERIC 
              MOVE "LT-A-LAST-FORM" TO D-FIELD
              MOVE OLT-A-LAST-FORM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO VERIFY-FILE-LT-A-EXIT.

       VERIFY-FILE-LT-A-3.
           IF OLT-A-BIL-COV NOT NUMERIC 
              MOVE "LT-A-BIL-COV" TO D-FIELD
              MOVE OLT-A-BIL-COV  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-A-PDL-COV NOT NUMERIC 
              MOVE "LT-A-PDL-COV" TO D-FIELD
              MOVE OLT-A-PDL-COV  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-A-PIP-COV NOT NUMERIC 
              MOVE "LT-A-PIP-COV" TO D-FIELD
              MOVE OLT-A-PIP-COV  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-A-UMB-COV NOT NUMERIC 
              MOVE "LT-A-UMB-COV" TO D-FIELD
              MOVE OLT-A-UMB-COV  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-A-COL-COV NOT NUMERIC 
              MOVE "LT-A-COL-COV" TO D-FIELD
              MOVE OLT-A-COL-COV  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-A-COM-COV NOT NUMERIC 
              MOVE "LT-A-COM-COV" TO D-FIELD
              MOVE OLT-A-COM-COV  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

       VERIFY-FILE-LT-A-EXIT.
           EXIT.

      ******************************************
       VERIFY-FILE-LT-B SECTION.
      ******************************************
           IF OLT-SEQNO NOT = 1
              GO TO VERIFY-FILE-LT-B-NEXT-SEQ.

           IF OLT-B-FILEDATE NOT NUMERIC OR
               (OLT-B-FILEDATE = 202020)
              MOVE "LT-B-FILEDATE" TO D-FIELD
              MOVE OLT-B-FILEDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-B-FILEDATE NOT = 0
                 MOVE OLT-B-FILEDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-B-FILEDATE" TO D-FIELD
                    MOVE OLT-B-FILEDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLT-B-CHAPTER NOT NUMERIC 
              MOVE "LT-B-CHAPTER" TO D-FIELD
              MOVE OLT-B-CHAPTER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-B-BDDATE NOT NUMERIC OR
               (OLT-B-BDDATE = 202020)
              MOVE "LT-B-BDDATE" TO D-FIELD
              MOVE OLT-B-BDDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-B-BDDATE NOT = 0
                 MOVE OLT-B-BDDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-B-BDDATE" TO D-FIELD
                    MOVE OLT-B-BDDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLT-B-CLAIM-AMT NOT NUMERIC 
              MOVE "LT-B-CLAIM-AMT" TO D-FIELD
              MOVE OLT-B-CLAIM-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-B-INT-LC-DUE NOT NUMERIC 
              MOVE "LT-B-INT-LC-DUE" TO D-FIELD
              MOVE OLT-B-INT-LC-DUE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-B-OTH-DUE NOT NUMERIC 
              MOVE "LT-B-OTH-DUE" TO D-FIELD
              MOVE OLT-B-OTH-DUE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-B-TDUEAMT NOT NUMERIC 
              MOVE "LT-B-TDUEAMT" TO D-FIELD
              MOVE OLT-B-TDUEAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO VERIFY-FILE-LT-B-EXIT.

       VERIFY-FILE-LT-B-NEXT-SEQ.
           IF NOT (OLT-SEQNO = 2 OR 3 OR 4)
              GO TO VERIFY-FILE-LT-B-SEQ-5.
 
           IF OLT-B-PHONE NOT NUMERIC 
              MOVE "LT-B-PHONE" TO D-FIELD
              MOVE OLT-B-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO VERIFY-FILE-LT-B-EXIT.

       VERIFY-FILE-LT-B-SEQ-5.

           IF OLT-B-REAFF-DATE NOT NUMERIC OR
               (OLT-B-REAFF-DATE = 202020)
              MOVE "LT-B-REAFF-DATE" TO D-FIELD
              MOVE OLT-B-REAFF-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-B-REAFF-DATE NOT = 0
                 MOVE OLT-B-REAFF-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-B-REAFF-DATE" TO D-FIELD
                    MOVE OLT-B-REAFF-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLT-B-REAFF-SYSDATE NOT NUMERIC OR
               (OLT-B-REAFF-SYSDATE = 202020)
              MOVE "LT-B-REAFF-SYSDATE" TO D-FIELD
              MOVE OLT-B-REAFF-SYSDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-B-REAFF-SYSDATE NOT = 0
                 MOVE OLT-B-REAFF-SYSDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-B-REAFF-SYSDATE" TO D-FIELD
                    MOVE OLT-B-REAFF-SYSDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLT-B-REAFF-BALANCE NOT NUMERIC 
              MOVE "LT-B-REAFF-BALANCE" TO D-FIELD
              MOVE OLT-B-REAFF-BALANCE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-B-REAFF-PAYAMT NOT NUMERIC 
              MOVE "LT-B-REAFF-PAYAMT" TO D-FIELD
              MOVE OLT-B-REAFF-PAYAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-B-REAFF-NO-PAYS NOT NUMERIC 
              MOVE "LT-B-REAFF-NO-PAYS" TO D-FIELD
              MOVE OLT-B-REAFF-NO-PAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-B-INTRATE NOT NUMERIC 
              MOVE "LT-B-INTRATE" TO D-FIELD
              MOVE OLT-B-INTRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-B-RESCIND-DATE NOT NUMERIC OR
               (OLT-B-RESCIND-DATE = 202020)
              MOVE "LT-B-RESCIND-DATE" TO D-FIELD
              MOVE OLT-B-RESCIND-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-B-RESCIND-DATE NOT = 0
                 MOVE OLT-B-RESCIND-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-B-RESCIND-DATE" TO D-FIELD
                    MOVE OLT-B-RESCIND-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLT-B-DISCHARGE-DATE NOT NUMERIC OR
               (OLT-B-DISCHARGE-DATE = 202020)
              MOVE "LT-B-DISCHARGE-DATE" TO D-FIELD
              MOVE OLT-B-DISCHARGE-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-B-DISCHARGE-DATE NOT = 0
                 MOVE OLT-B-DISCHARGE-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-B-DISCHARGE-DATE" TO D-FIELD
                    MOVE OLT-B-DISCHARGE-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLT-B-DISCHARGE-SYSDATE NOT NUMERIC OR
               (OLT-B-DISCHARGE-SYSDATE = 202020)
              MOVE "LT-B-DISCHARGE-SYSDATE" TO D-FIELD
              MOVE OLT-B-DISCHARGE-SYSDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-B-DISCHARGE-SYSDATE NOT = 0
                 MOVE OLT-B-DISCHARGE-SYSDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-B-DISCHARGE-SYSDATE" TO D-FIELD
                    MOVE OLT-B-DISCHARGE-SYSDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

       VERIFY-FILE-LT-B-EXIT.
           EXIT.
          
      ******************************************
       VERIFY-FILE-LT-C SECTION.
      ******************************************
      * SEQUENCE 3,4 AND 5 HAVE NO NUMERICS
           IF OLT-SEQNO > 2
              GO TO VERIFY-FILE-LT-C-EXIT.

           IF OLT-MILEAGE NOT NUMERIC 
              MOVE "LT-MILEAGE" TO D-FIELD
              MOVE OLT-MILEAGE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-VALUE NOT NUMERIC 
              MOVE "LT-VALUE" TO D-FIELD
              MOVE OLT-VALUE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-SELLPR NOT NUMERIC 
              MOVE "LT-SELLPR" TO D-FIELD
              MOVE OLT-SELLPR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-MISC-AMT NOT NUMERIC 
              MOVE "LT-MISC-AMT" TO D-FIELD
              MOVE OLT-MISC-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-VIN-CHG-DATE NOT NUMERIC OR
               (OLT-VIN-CHG-DATE = 202020)
              MOVE "LT-VIN-CHG-DATE" TO D-FIELD
              MOVE OLT-VIN-CHG-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-VIN-CHG-DATE NOT = 0
                 MOVE OLT-VIN-CHG-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-VIN-CHG-DATE" TO D-FIELD
                    MOVE OLT-VIN-CHG-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

       VERIFY-FILE-LT-C-EXIT.
           EXIT.

      ******************************************
       VERIFY-FILE-LT-D SECTION.
      ******************************************
     
           IF OLT-D-DEBIT-CARD-NUMBER NOT NUMERIC 
              MOVE "LT-D-DEBIT-CARD-NUMBER" TO D-FIELD
              MOVE OLT-D-DEBIT-CARD-NUMBER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-D-DEBIT-CARD-AMT NOT NUMERIC 
              MOVE "LT-D-DEBIT-CARD-AMT" TO D-FIELD
              MOVE OLT-D-DEBIT-CARD-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-D-LOANDATE NOT NUMERIC OR
               (OLT-D-LOANDATE = 202020)
              MOVE "LT-D-LOANDATE" TO D-FIELD
              MOVE OLT-D-LOANDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-D-LOANDATE NOT = 0
                 MOVE OLT-D-LOANDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-D-LOANDATE" TO D-FIELD
                    MOVE OLT-D-LOANDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLT-D-VOID-DATE NOT NUMERIC OR
               (OLT-D-VOID-DATE = 202020)
              MOVE "LT-D-VOID-DATE" TO D-FIELD
              MOVE OLT-D-VOID-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-D-VOID-DATE NOT = 0
                 MOVE OLT-D-VOID-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-D-VOID-DATE" TO D-FIELD
                    MOVE OLT-D-VOID-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLT-D-CLEARED-DATE NOT NUMERIC OR
               (OLT-D-CLEARED-DATE = 202020)
              MOVE "LT-D-CLEARED-DATE" TO D-FIELD
              MOVE OLT-D-CLEARED-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-D-CLEARED-DATE NOT = 0
                 MOVE OLT-D-CLEARED-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-D-CLEARED-DATE" TO D-FIELD
                    MOVE OLT-D-CLEARED-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLT-D-CLEARED-AMT NOT NUMERIC 
              MOVE "LT-D-CLEARED-AMT" TO D-FIELD
              MOVE OLT-D-CLEARED-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

       VERIFY-FILE-LT-D-EXIT.
           EXIT.

      ******************************************
       VERIFY-FILE-LT-F SECTION.
      ******************************************
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF OLT-FEECODE-PAYABLE(SUB) NOT NUMERIC 
                 MOVE "LT-FEECODE-PAYABLE(SUB)" TO D-FIELD
                 MOVE OLT-FEECODE-PAYABLE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

       VERIFY-FILE-LT-F-EXIT.
           EXIT.

      ******************************************
       VERIFY-FILE-LT-I SECTION.
      ******************************************

           IF OLT-ORIG-1STPAY NOT NUMERIC OR
               (OLT-ORIG-1STPAY = 202020)
              MOVE "LT-ORIG-1STPAY" TO D-FIELD
              MOVE OLT-ORIG-1STPAY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-ORIG-1STPAY NOT = 0
                 MOVE OLT-ORIG-1STPAY TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-ORIG-1STPAY" TO D-FIELD
                    MOVE OLT-ORIG-1STPAY  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLT-PRIOR-NODEF NOT NUMERIC 
              MOVE "LT-PRIOR-NODEF" TO D-FIELD
              MOVE OLT-PRIOR-NODEF  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
              IF OLT-INSFEE(SUB) NOT NUMERIC 
                 MOVE "LT-INSFEE(SUB)" TO D-FIELD
                 MOVE OLT-INSFEE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OLT-INSCOMP NOT NUMERIC 
              MOVE "LT-INSCOMP" TO D-FIELD
              MOVE OLT-INSCOMP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-INSEFF NOT NUMERIC OR
               (OLT-INSEFF = 202020)
              MOVE "LT-INSEFF" TO D-FIELD
              MOVE OLT-INSEFF  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-INSEFF NOT = 0
                 MOVE OLT-INSEFF TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-INSEFF" TO D-FIELD
                    MOVE OLT-INSEFF  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLT-INSEXP NOT NUMERIC OR
               (OLT-INSEXP = 202020)
              MOVE "LT-INSEXP" TO D-FIELD
              MOVE OLT-INSEXP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-INSEXP NOT = 0
                 MOVE OLT-INSEXP TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-INSEXP" TO D-FIELD
                    MOVE OLT-INSEXP  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLT-INSPREM NOT NUMERIC 
              MOVE "LT-INSPREM" TO D-FIELD
              MOVE OLT-INSPREM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-INSCOVR NOT NUMERIC 
              MOVE "LT-INSCOVR" TO D-FIELD
              MOVE OLT-INSCOVR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-INSCOMM NOT NUMERIC 
              MOVE "LT-INSCOMM" TO D-FIELD
              MOVE OLT-INSCOMM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-ANTICTERM NOT NUMERIC 
              MOVE "LT-ANTICTERM" TO D-FIELD
              MOVE OLT-ANTICTERM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OLT-ANTICERN(SUB) NOT NUMERIC 
                 MOVE "LT-ANTICERN(SUB)" TO D-FIELD
                 MOVE OLT-ANTICERN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLT-ANTICADJ(SUB) NOT NUMERIC 
                 MOVE "LT-ANTICADJ(SUB)" TO D-FIELD
                 MOVE OLT-ANTICADJ(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLT-UFERN(SUB) NOT NUMERIC 
                 MOVE "LT-UFERN(SUB)" TO D-FIELD
                 MOVE OLT-UFERN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLT-ACCERN(SUB) NOT NUMERIC 
                 MOVE "LT-ACCERN(SUB)" TO D-FIELD
                 MOVE OLT-ACCERN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OLT-INS-PAYMNT-INCR NOT NUMERIC 
              MOVE "LT-INS-PAYMNT-INCR" TO D-FIELD
              MOVE OLT-INS-PAYMNT-INCR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-INS-TOT-ADDON NOT NUMERIC 
              MOVE "LT-INS-TOT-ADDON" TO D-FIELD
              MOVE OLT-INS-TOT-ADDON  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-INT-LAST-EARNED NOT NUMERIC 
              MOVE "LT-INT-LAST-EARNED" TO D-FIELD
              MOVE OLT-INT-LAST-EARNED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-RB-WORKER NOT NUMERIC 
              MOVE "LT-RB-WORKER" TO D-FIELD
              MOVE OLT-RB-WORKER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-ANTIC-START-DATE NOT NUMERIC OR
               (OLT-ANTIC-START-DATE = 202020)
              MOVE "LT-ANTIC-START-DATE" TO D-FIELD
              MOVE OLT-ANTIC-START-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-ANTIC-START-DATE NOT = 0
                 MOVE OLT-ANTIC-START-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-ANTIC-START-DATE" TO D-FIELD
                    MOVE OLT-ANTIC-START-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

       VERIFY-FILE-LT-I-EXIT.
           EXIT.

      ******************************************
       VERIFY-FILE-LT-L SECTION.
      ******************************************

           IF NOT (OLT-SEQNO = 1 OR 2 OR 3)
              GO TO VERIFY-FILE-LT-L-5.
             
           IF OLT-L-LIEN-TYPE NOT NUMERIC 
              MOVE "LT-L-LIEN-TYPE" TO D-FIELD
              MOVE OLT-L-LIEN-TYPE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLT-L-LIEN-DATE NOT NUMERIC OR
               (OLT-L-LIEN-DATE = 202020)
              MOVE "LT-L-LIEN-DATE" TO D-FIELD
              MOVE OLT-L-LIEN-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-L-LIEN-DATE NOT = 0
                 MOVE OLT-L-LIEN-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-L-LIEN-DATE" TO D-FIELD
                    MOVE OLT-L-LIEN-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLT-L-TITLE-RCV-DATE NOT NUMERIC OR
               (OLT-L-TITLE-RCV-DATE = 202020)
              MOVE "LT-L-TITLE-RCV-DATE" TO D-FIELD
              MOVE OLT-L-TITLE-RCV-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-L-TITLE-RCV-DATE NOT = 0
                 MOVE OLT-L-TITLE-RCV-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-L-TITLE-RCV-DATE" TO D-FIELD
                    MOVE OLT-L-TITLE-RCV-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLT-L-TITLE-LIEN-DATE NOT NUMERIC OR
               (OLT-L-TITLE-LIEN-DATE = 202020)
              MOVE "LT-L-TITLE-LIEN-DATE" TO D-FIELD
              MOVE OLT-L-TITLE-LIEN-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-L-TITLE-LIEN-DATE NOT = 0
                 MOVE OLT-L-TITLE-LIEN-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-L-TITLE-LIEN-DATE" TO D-FIELD
                    MOVE OLT-L-TITLE-LIEN-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
 
           GO TO VERIFY-FILE-LT-L-EXIT.

      * SEQ 4 HAS NOT NUMERICS, JUMP TO 5
       VERIFY-FILE-LT-L-5.
           IF OLT-SEQNO NOT = 5
              GO TO VERIFY-FILE-LT-L-EXIT.

           IF OLT-L-TITLE-DATE NOT NUMERIC OR
               (OLT-L-TITLE-DATE = 202020)
              MOVE "LT-L-TITLE-DATE" TO D-FIELD
              MOVE OLT-L-TITLE-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLT-L-TITLE-DATE NOT = 0
                 MOVE OLT-L-TITLE-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LT-L-TITLE-DATE" TO D-FIELD
                    MOVE OLT-L-TITLE-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLT-L-TITLE-SEND-FMNO NOT NUMERIC 
              MOVE "LT-L-TITLE-SEND-FMNO" TO D-FIELD
              MOVE OLT-L-TITLE-SEND-FMNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

       VERIFY-FILE-LT-L-EXIT.
           EXIT.

      ******************************************
       VERIFY-FILE-LT-P SECTION.
      ******************************************
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 17
              IF OLT-SCHLD-TERM(SUB) NOT NUMERIC 
                 MOVE "LT-SCHLD-TERM(SUB)" TO D-FIELD
                 MOVE OLT-SCHLD-TERM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLT-SCHLD-PAYMNT(SUB) NOT NUMERIC 
                 MOVE "LT-SCHLD-PAYMNT(SUB)" TO D-FIELD
                 MOVE OLT-SCHLD-PAYMNT(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

       VERIFY-FILE-LT-P-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - L X
      *
      ******************************************************************
       PROCESS-FILE-LX SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-LX = "N" )
              GO TO PROCESS-FILE-LX-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO LX-PATH-OVERRIDE
                                              LXE-PATH-OVERRIDE
                                              LXG-PATH-OVERRIDE.

           MOVE "LX"                       TO UP1534-CURRENT-FILE.
           MOVE "GB/LXFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

            PERFORM UP1534-VPI-START.
      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OLX-PATH.

           MOVE SPACES                     TO OLX1-KEY.
           MOVE ZEROES                     TO OLX-ACCTNO
                                              OLX-SEQNO.

           PERFORM OPEN-OLX1-FILE.
           PERFORM START-OLX1-FILE.

       PROCESS-FILE-LX-NEXT.
           PERFORM READ-OLX1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-LX-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.

           PERFORM UP1534-VPI-DISPLAY.

           IF ( OLX-CODE = "G" ) PERFORM VERIFY-FILE-LX-G.
           IF ( OLX-CODE = "E" ) PERFORM VERIFY-FILE-LX-E.
           IF ( OLX-CODE = "A" ) PERFORM VERIFY-FILE-LX-A.
           IF ( OLX-CODE = "2" ) PERFORM VERIFY-FILE-LX-2.
           IF ( OLX-CODE = "M" ) PERFORM VERIFY-FILE-LX-M.
           IF ( OLX-CODE = "Z" ) PERFORM VERIFY-FILE-LX-Z.
           IF ( OLX-CODE = "P" OR "T") PERFORM VERIFY-FILE-LX-P-AND-T.
           IF ( OLX-CODE = "R" ) PERFORM VERIFY-FILE-LX-R.

           GO TO PROCESS-FILE-LX-NEXT.

       PROCESS-FILE-LX-END.

           PERFORM CLOSE-OLX1-FILE.

       PROCESS-FILE-LX-EXIT.
           EXIT.

      ******************************************************************
       VERIFY-FILE-LX-G SECTION.
      ******************************************************************
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
             IF OLXG-GLNO(SUB) NOT NUMERIC 
                MOVE "LXG-GLNO(SUB)" TO D-FIELD
                MOVE OLXG-GLNO(SUB)  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OLXG-GLAMT(SUB) NOT NUMERIC 
                MOVE "LXG-GLAMT(SUB)" TO D-FIELD
                MOVE OLXG-GLAMT(SUB)  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
           END-PERFORM.

      ******************************************************************
       VERIFY-FILE-LX-E SECTION.
      ******************************************************************
            PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OLXE-EARN(SUB) NOT NUMERIC 
                 MOVE "LXE-EARN(SUB)" TO D-FIELD
                 MOVE OLXE-EARN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
            END-PERFORM.
           IF OLXE-WORKER NOT NUMERIC 
              MOVE "LXE-WORKER" TO D-FIELD
              MOVE OLXE-WORKER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLXE-WORKER2 NOT NUMERIC 
              MOVE "LXE-WORKER2" TO D-FIELD
              MOVE OLXE-WORKER2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       VERIFY-FILE-LX-A SECTION.
      ******************************************************************
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OLXA-START-PMT(SUB) NOT NUMERIC 
                 MOVE "LXA-START-PMT(SUB)" TO D-FIELD
                 MOVE OLXA-START-PMT(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLXA-END-PMT(SUB) NOT NUMERIC 
                 MOVE "LXA-END-PMT(SUB)" TO D-FIELD
                 MOVE OLXA-END-PMT(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLXA-NET-CHANGE(SUB) NOT NUMERIC 
                 MOVE "LXA-NET-CHANGE(SUB)" TO D-FIELD
                 MOVE OLXA-NET-CHANGE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OLXA-LN-INSCOMP NOT NUMERIC 
              MOVE "LXA-LN-INSCOMP" TO D-FIELD
              MOVE OLXA-LN-INSCOMP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLXA-LN-INSEFF NOT NUMERIC OR
               (OLXA-LN-INSEFF = 202020)
              MOVE "LXA-LN-INSEFF" TO D-FIELD
              MOVE OLXA-LN-INSEFF  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLXA-LN-INSEFF NOT = 0
                 MOVE OLXA-LN-INSEFF TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LXA-LN-INSEFF" TO D-FIELD
                    MOVE OLXA-LN-INSEFF  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLXA-LN-INSEXP NOT NUMERIC OR
               (OLXA-LN-INSEXP = 202020)
              MOVE "LXA-LN-INSEXP" TO D-FIELD
              MOVE OLXA-LN-INSEXP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLXA-LN-INSEXP NOT = 0
                 MOVE OLXA-LN-INSEXP TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LXA-LN-INSEXP" TO D-FIELD
                    MOVE OLXA-LN-INSEXP  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLXA-LN-INSPREM NOT NUMERIC 
              MOVE "LXA-LN-INSPREM" TO D-FIELD
              MOVE OLXA-LN-INSPREM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLXA-LN-INSCOVR NOT NUMERIC 
              MOVE "LXA-LN-INSCOVR" TO D-FIELD
              MOVE OLXA-LN-INSCOVR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLXA-LN-INSCOMM NOT NUMERIC 
              MOVE "LXA-LN-INSCOMM" TO D-FIELD
              MOVE OLXA-LN-INSCOMM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       VERIFY-FILE-LX-2 SECTION.
      ******************************************************************
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OLX2-START-PMT(SUB) NOT NUMERIC 
                 MOVE "LX2-START-PMT(SUB)" TO D-FIELD
                 MOVE OLX2-START-PMT(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLX2-END-PMT(SUB) NOT NUMERIC 
                 MOVE "LX2-END-PMT(SUB)" TO D-FIELD
                 MOVE OLX2-END-PMT(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OLX2-NET-CHANGE(SUB) NOT NUMERIC 
                 MOVE "LX2-NET-CHANGE(SUB)" TO D-FIELD
                 MOVE OLX2-NET-CHANGE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

      ******************************************************************
       VERIFY-FILE-LX-M SECTION.
      ******************************************************************
           IF OLXM-MIP-CLPAID NOT NUMERIC 
              MOVE "LXM-MIP-CLPAID" TO D-FIELD
              MOVE OLXM-MIP-CLPAID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLXM-MIP-CLPDTH NOT NUMERIC OR
               (OLXM-MIP-CLPDTH = 202020)
              MOVE "LXM-MIP-CLPDTH" TO D-FIELD
              MOVE OLXM-MIP-CLPDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLXM-MIP-CLPDTH NOT = 0
                 MOVE OLXM-MIP-CLPDTH TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LXM-MIP-CLPDTH" TO D-FIELD
                    MOVE OLXM-MIP-CLPDTH  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLXM-MIP-AHPAID NOT NUMERIC 
              MOVE "LXM-MIP-AHPAID" TO D-FIELD
              MOVE OLXM-MIP-AHPAID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLXM-MIP-AHPDTH NOT NUMERIC OR
               (OLXM-MIP-AHPDTH = 202020)
              MOVE "LXM-MIP-AHPDTH" TO D-FIELD
              MOVE OLXM-MIP-AHPDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLXM-MIP-AHPDTH NOT = 0
                 MOVE OLXM-MIP-AHPDTH TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LXM-MIP-AHPDTH" TO D-FIELD
                    MOVE OLXM-MIP-AHPDTH  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       VERIFY-FILE-LX-Z SECTION.
      ******************************************************************

           IF OLXZ-POST-REGPY NOT NUMERIC 
              MOVE "LXZ-POST-REGPY" TO D-FIELD
              MOVE OLXZ-POST-REGPY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLXZ-POST-EXTRA-PRIN NOT NUMERIC 
              MOVE "LXZ-POST-EXTRA-PRIN" TO D-FIELD
              MOVE OLXZ-POST-EXTRA-PRIN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLXZ-POST-OTH NOT NUMERIC 
              MOVE "LXZ-POST-OTH" TO D-FIELD
              MOVE OLXZ-POST-OTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLXZ-POST-LCHG NOT NUMERIC 
              MOVE "LXZ-POST-LCHG" TO D-FIELD
              MOVE OLXZ-POST-LCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLXZ-PDUE-APINT NOT NUMERIC 
              MOVE "LXZ-PDUE-APINT" TO D-FIELD
              MOVE OLXZ-PDUE-APINT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLXZ-PDUE-APPRIN NOT NUMERIC 
              MOVE "LXZ-PDUE-APPRIN" TO D-FIELD
              MOVE OLXZ-PDUE-APPRIN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLXZ-CUR-APINT NOT NUMERIC 
              MOVE "LXZ-CUR-APINT" TO D-FIELD
              MOVE OLXZ-CUR-APINT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLXZ-CUR-APPRIN NOT NUMERIC 
              MOVE "LXZ-CUR-APPRIN" TO D-FIELD
              MOVE OLXZ-CUR-APPRIN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       VERIFY-FILE-LX-P-AND-T SECTION.
      ******************************************************************
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
              IF OLXT-EARN(SUB) NOT NUMERIC 
                 MOVE "LXT-EARN(SUB)" TO D-FIELD
                 MOVE OLXT-EARN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

      ******************************************************************
       VERIFY-FILE-LX-R SECTION.
      ******************************************************************
           IF OLXR-REB-AMT NOT NUMERIC 
              MOVE "LXR-REB-AMT" TO D-FIELD
              MOVE OLXR-REB-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - P A
      *
      ******************************************************************
      *PROCESS-FILE-PA SECTION.

      *    MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

      *    IF ( FILE-OPTION-PA = "N" )
      *       GO TO PROCESS-FILE-PA-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
      *    MOVE "Y"                        TO PA-PATH-OVERRIDE.
      *    MOVE "PA"                       TO UP1534-CURRENT-FILE.
      *    MOVE "LP/PAFILE"                TO UP1534-FILE-INPUT.
      *    MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
      *    MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

      *    PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
      *    MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
      *    PERFORM UP1534-LOAD-FILE-PATH.
      *    MOVE UP1534-FILE-PATH           TO OPA-PATH.

      *    MOVE SPACES                     TO OPA1-KEY.
      *    MOVE ZEROES                     TO OPA-NUMBER.
      *    PERFORM OPEN-OPA1-FILE.
      *    PERFORM START-OPA1-FILE.

      *PROCESS-FILE-PA-NEXT.
      *    PERFORM READ-OPA1-FILE-NEXT.
      *    IF ( IO-BAD )
      *       GO TO PROCESS-FILE-PA-END.

      *    ADD  01  TO UP1534-RECORD-COUNTER-READ.
      *    MOVE "Y" TO RECORDS-FOUND.
      *    PERFORM UP1534-VPI-DISPLAY.

      *    IF OPA-NUMBER NOT NUMERIC 
      *       MOVE "PA-NUMBER" TO D-FIELD
      *       MOVE OPA-NUMBER  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OPA-LCHECK NOT NUMERIC 
      *       MOVE "PA-LCHECK" TO D-FIELD
      *       MOVE OPA-LCHECK  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OPA-LCDATE NOT NUMERIC OR
      *        (OPA-LCDATE = 202020)
      *       MOVE "PA-LCDATE" TO D-FIELD
      *       MOVE OPA-LCDATE  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE
      *    ELSE
      *       IF OPA-LCDATE NOT = 0
      *          MOVE OPA-LCDATE TO DATE-MMDDYY
      *          PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
      *          MOVE DATE-YYYYMMDD TO NUM-DATE
      *          PERFORM TSTDAT
      *          IF NUM-DATE = 0
      *             MOVE "PA-LCDATE" TO D-FIELD
      *             MOVE OPA-LCDATE  TO D-VALUE
      *             PERFORM WRITE-DETAIL-LINE.
      *    IF OPA-LAMT NOT NUMERIC 
      *       MOVE "PA-LAMT" TO D-FIELD
      *       MOVE OPA-LAMT  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OPA-YTD-PAID NOT NUMERIC 
      *       MOVE "PA-YTD-PAID" TO D-FIELD
      *       MOVE OPA-YTD-PAID  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OPA-LYTD-PAID NOT NUMERIC 
      *       MOVE "PA-LYTD-PAID" TO D-FIELD
      *       MOVE OPA-LYTD-PAID  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.

      *    IF OPA-PHONE NOT NUMERIC 
      *       MOVE "PA-PHONE" TO D-FIELD
      *       MOVE OPA-PHONE  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OPA-FAX NOT NUMERIC 
      *       MOVE "PA-FAX" TO D-FIELD
      *       MOVE OPA-FAX  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OPA-VAT-PERC NOT NUMERIC 
      *       MOVE "PA-VAT-PERC" TO D-FIELD
      *       MOVE OPA-VAT-PERC  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    
      *    GO TO PROCESS-FILE-PA-NEXT.

      *PROCESS-FILE-PA-END.

      *    PERFORM CLOSE-OPA1-FILE.

      *PROCESS-FILE-PA-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - T Y
      *
      ******************************************************************
       PROCESS-FILE-TY SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-TY = "N" )
              GO TO PROCESS-FILE-TY-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO TY-PATH-OVERRIDE.
           MOVE "TY"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/TYFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OTY-PATH.


           MOVE ZEROES                     TO OTY-BRNO
                                              OTY-DATE
                                              OTY-CLASS.
           PERFORM OPEN-OTY1-FILE.
           PERFORM START-OTY1-FILE.

      *=================================================================
       PROCESS-FILE-TY-NEXT.

           PERFORM READ-OTY1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-TY-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.      

           IF OTY-BRNO NOT NUMERIC 
              MOVE "TY-BRNO" TO D-FIELD
              MOVE OTY-BRNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OTY-DATE NOT NUMERIC 
              MOVE "TY-DATE" TO D-FIELD
              MOVE OTY-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTY-DATE NOT = 0
                 MOVE OTY-DATE TO DATE-YYYMMDD
                 PERFORM CONVERT-YYYMMDD-TO-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TY-DATE" TO D-FIELD
                    MOVE OTY-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OTY-CLASS NOT NUMERIC 
              MOVE "TY-CLASS" TO D-FIELD
              MOVE OTY-CLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VERIFY-TY-ACTIVITY-TBL VARYING
              TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 11
                AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2.

           PERFORM VERIFY-TY-REPO-ACTIVITY-TBL VARYING
               TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 5
                  AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2.

           PERFORM VERIFY-TY-PL-ACTIVITY-TBL VARYING
               TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 10
                  AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2.

           PERFORM VERIFY-TY-DLR-ACTIVITY-TBL VARYING
               TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 3
                  AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              IF OTY-PL-PRINCOL(SUB) NOT NUMERIC 
                 MOVE "TY-PL-PRINCOL(SUB)" TO D-FIELD
                 MOVE OTY-PL-PRINCOL(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTY-PL-INTCOL(SUB) NOT NUMERIC 
                 MOVE "TY-PL-INTCOL(SUB)" TO D-FIELD
                 MOVE OTY-PL-INTCOL(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTY-PL-LATECOL(SUB) NOT NUMERIC 
                 MOVE "TY-PL-LATECOL(SUB)" TO D-FIELD
                 MOVE OTY-PL-LATECOL(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTY-PL-O2COL(SUB) NOT NUMERIC 
                 MOVE "TY-PL-O2COL(SUB)" TO D-FIELD
                 MOVE OTY-PL-O2COL(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OTY-AMT-SOLD NOT NUMERIC 
              MOVE "TY-AMT-SOLD" TO D-FIELD
              MOVE OTY-AMT-SOLD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-AMT-TRANS NOT NUMERIC 
              MOVE "TY-AMT-TRANS" TO D-FIELD
              MOVE OTY-AMT-TRANS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-AMT-PURCH NOT NUMERIC 
              MOVE "TY-AMT-PURCH" TO D-FIELD
              MOVE OTY-AMT-PURCH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-NO-SOLD NOT NUMERIC 
              MOVE "TY-NO-SOLD" TO D-FIELD
              MOVE OTY-NO-SOLD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-NO-TRANS NOT NUMERIC 
              MOVE "TY-NO-TRANS" TO D-FIELD
              MOVE OTY-NO-TRANS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-NO-PURCH NOT NUMERIC 
              MOVE "TY-NO-PURCH" TO D-FIELD
              MOVE OTY-NO-PURCH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-OTHCOL NOT NUMERIC 
              MOVE "TY-OTHCOL" TO D-FIELD
              MOVE OTY-OTHCOL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-OT2COL NOT NUMERIC 
              MOVE "TY-OT2COL" TO D-FIELD
              MOVE OTY-OT2COL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-INTCOL NOT NUMERIC 
              MOVE "TY-INTCOL" TO D-FIELD
              MOVE OTY-INTCOL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-LATECOL NOT NUMERIC 
              MOVE "TY-LATECOL" TO D-FIELD
              MOVE OTY-LATECOL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-PRINCOL NOT NUMERIC 
              MOVE "TY-PRINCOL" TO D-FIELD
              MOVE OTY-PRINCOL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-DEFCOL NOT NUMERIC 
              MOVE "TY-DEFCOL" TO D-FIELD
              MOVE OTY-DEFCOL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-RNPRIN NOT NUMERIC 
              MOVE "TY-RNPRIN" TO D-FIELD
              MOVE OTY-RNPRIN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-INTREB NOT NUMERIC 
              MOVE "TY-INTREB" TO D-FIELD
              MOVE OTY-INTREB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-CLREB1 NOT NUMERIC 
              MOVE "TY-CLREB1" TO D-FIELD
              MOVE OTY-CLREB1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-CLREB2 NOT NUMERIC 
              MOVE "TY-CLREB2" TO D-FIELD
              MOVE OTY-CLREB2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-CLREB3 NOT NUMERIC 
              MOVE "TY-CLREB3" TO D-FIELD
              MOVE OTY-CLREB3  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-CLREB4 NOT NUMERIC 
              MOVE "TY-CLREB4" TO D-FIELD
              MOVE OTY-CLREB4  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-AHREB NOT NUMERIC 
              MOVE "TY-AHREB" TO D-FIELD
              MOVE OTY-AHREB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-PPREB NOT NUMERIC 
              MOVE "TY-PPREB" TO D-FIELD
              MOVE OTY-PPREB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-O1REB NOT NUMERIC 
              MOVE "TY-O1REB" TO D-FIELD
              MOVE OTY-O1REB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-O2REB NOT NUMERIC 
              MOVE "TY-O2REB" TO D-FIELD
              MOVE OTY-O2REB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-O3REB NOT NUMERIC 
              MOVE "TY-O3REB" TO D-FIELD
              MOVE OTY-O3REB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-O4REB NOT NUMERIC 
              MOVE "TY-O4REB" TO D-FIELD
              MOVE OTY-O4REB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-O5REB NOT NUMERIC 
              MOVE "TY-O5REB" TO D-FIELD
              MOVE OTY-O5REB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-O6REB NOT NUMERIC 
              MOVE "TY-O6REB" TO D-FIELD
              MOVE OTY-O6REB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-O7REB NOT NUMERIC 
              MOVE "TY-O7REB" TO D-FIELD
              MOVE OTY-O7REB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-SCREB NOT NUMERIC 
              MOVE "TY-SCREB" TO D-FIELD
              MOVE OTY-SCREB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-MFREB NOT NUMERIC 
              MOVE "TY-MFREB" TO D-FIELD
              MOVE OTY-MFREB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-DFREB NOT NUMERIC 
              MOVE "TY-DFREB" TO D-FIELD
              MOVE OTY-DFREB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-PART-CHGOFF NOT NUMERIC 
              MOVE "TY-PART-CHGOFF" TO D-FIELD
              MOVE OTY-PART-CHGOFF  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.


           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF OTY-INS-NO-ELIG(SUB) NOT NUMERIC 
                 MOVE "TY-INS-NO-ELIG(SUB)" TO D-FIELD
                 MOVE OTY-INS-NO-ELIG(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTY-INS-NO-WROTE(SUB) NOT NUMERIC 
                 MOVE "TY-INS-NO-WROTE(SUB)" TO D-FIELD
                 MOVE OTY-INS-NO-WROTE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTY-INS-NO-NOTWROTE(SUB) NOT NUMERIC 
                 MOVE "TY-INS-NO-NOTWROTE(SUB)" TO D-FIELD
                 MOVE OTY-INS-NO-NOTWROTE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTY-INS-AMT-WROTE(SUB) NOT NUMERIC 
                 MOVE "TY-INS-AMT-WROTE(SUB)" TO D-FIELD
                 MOVE OTY-INS-AMT-WROTE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OTY-OTH-NO-COL NOT NUMERIC 
              MOVE "TY-OTH-NO-COL" TO D-FIELD
              MOVE OTY-OTH-NO-COL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-OT2-NO-COL NOT NUMERIC 
              MOVE "TY-OT2-NO-COL" TO D-FIELD
              MOVE OTY-OT2-NO-COL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-INT-NO-COL NOT NUMERIC 
              MOVE "TY-INT-NO-COL" TO D-FIELD
              MOVE OTY-INT-NO-COL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-LATE-NO-COL NOT NUMERIC 
              MOVE "TY-LATE-NO-COL" TO D-FIELD
              MOVE OTY-LATE-NO-COL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-OTH-NO-WAIVED NOT NUMERIC 
              MOVE "TY-OTH-NO-WAIVED" TO D-FIELD
              MOVE OTY-OTH-NO-WAIVED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-OT2-NO-WAIVED NOT NUMERIC 
              MOVE "TY-OT2-NO-WAIVED" TO D-FIELD
              MOVE OTY-OT2-NO-WAIVED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-INT-NO-WAIVED NOT NUMERIC 
              MOVE "TY-INT-NO-WAIVED" TO D-FIELD
              MOVE OTY-INT-NO-WAIVED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-LATE-NO-WAIVED NOT NUMERIC 
              MOVE "TY-LATE-NO-WAIVED" TO D-FIELD
              MOVE OTY-LATE-NO-WAIVED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-OTH-AMT-WAIVED NOT NUMERIC 
              MOVE "TY-OTH-AMT-WAIVED" TO D-FIELD
              MOVE OTY-OTH-AMT-WAIVED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-OT2-AMT-WAIVED NOT NUMERIC 
              MOVE "TY-OT2-AMT-WAIVED" TO D-FIELD
              MOVE OTY-OT2-AMT-WAIVED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-INT-AMT-WAIVED NOT NUMERIC 
              MOVE "TY-INT-AMT-WAIVED" TO D-FIELD
              MOVE OTY-INT-AMT-WAIVED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-LATE-AMT-WAIVED NOT NUMERIC 
              MOVE "TY-LATE-AMT-WAIVED" TO D-FIELD
              MOVE OTY-LATE-AMT-WAIVED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.


           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              IF OTY-CKWRITTEN(SUB) NOT NUMERIC 
                 MOVE "TY-CKWRITTEN(SUB)" TO D-FIELD
                 MOVE OTY-CKWRITTEN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           PERFORM VERIFY-TY-DEL-BUCKETS VARYING
              TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 10
                AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2.

           IF OTY-SUMWGTTM NOT NUMERIC 
              MOVE "TY-SUMWGTTM" TO D-FIELD
              MOVE OTY-SUMWGTTM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-SUMWGTAV NOT NUMERIC 
              MOVE "TY-SUMWGTAV" TO D-FIELD
              MOVE OTY-SUMWGTAV  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-SUMWGTTA NOT NUMERIC 
              MOVE "TY-SUMWGTTA" TO D-FIELD
              MOVE OTY-SUMWGTTA  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-ALLOTMENTS NOT NUMERIC 
              MOVE "TY-ALLOTMENTS" TO D-FIELD
              MOVE OTY-ALLOTMENTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OTY-DLDISC NOT NUMERIC 
              MOVE "TY-DLDISC" TO D-FIELD
              MOVE OTY-DLDISC  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 10
                     AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2
              IF OTY-UNEARNED-AMT(TYSUB1 TYSUB2) NOT NUMERIC 
                 MOVE "TY-UNEARNED-AMT(TYSUB1 TYSUB2)" TO D-FIELD
                 MOVE OTY-UNEARNED-AMT(TYSUB1 TYSUB2)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OTY-PROCEEDS NOT NUMERIC 
              MOVE "TY-PROCEEDS" TO D-FIELD
              MOVE OTY-PROCEEDS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-LOCKBOX NOT NUMERIC 
              MOVE "TY-LOCKBOX" TO D-FIELD
              MOVE OTY-LOCKBOX  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-TY-NEXT.

       PROCESS-FILE-TY-END.

           PERFORM CLOSE-OTY1-FILE.

       PROCESS-FILE-TY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - A L
      *
      ******************************************************************
        PROCESS-FILE-AL SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-AL = "N" )
              GO TO PROCESS-FILE-AL-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO AL-PATH-OVERRIDE.
           MOVE "AL"                       TO UP1534-CURRENT-FILE.
           MOVE "GL/ALFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OAL-PATH.

           MOVE SPACES                     TO OAL1-KEY.
           PERFORM OPEN-OAL1-FILE.
           PERFORM START-OAL1-FILE.

       PROCESS-FILE-AL-NEXT.
           PERFORM READ-OAL1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-AL-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OAL-NO-ACCTS NOT NUMERIC 
              MOVE "AL-NO-ACCTS" TO D-FIELD
              MOVE OAL-NO-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF OAL-ACCT(SUB) NOT NUMERIC 
                 MOVE "AL-ACCT(SUB)" TO D-FIELD
                 MOVE OAL-ACCT(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OAL-PCT(SUB) NOT NUMERIC 
                 MOVE "AL-PCT(SUB)" TO D-FIELD
                 MOVE OAL-PCT(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           GO TO PROCESS-FILE-AL-NEXT.

       PROCESS-FILE-AL-END.

           PERFORM CLOSE-OAL1-FILE.

       PROCESS-FILE-AL-EXIT.
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

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OBY-PATH.

           MOVE ZEROES                     TO OBY-DATE
                                              OBY-BRNO
                                              OBY-CLASS.
           PERFORM OPEN-OBY1-FILE.
           PERFORM START-OBY1-FILE.

       PROCESS-FILE-BY-NEXT.
           PERFORM READ-OBY1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-BY-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OBY-DATE NOT NUMERIC 
              MOVE "BY-DATE" TO D-FIELD
              MOVE OBY-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-BRNO NOT NUMERIC 
              MOVE "BY-BRNO" TO D-FIELD
              MOVE OBY-BRNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-CLASS NOT NUMERIC 
              MOVE "BY-CLASS" TO D-FIELD
              MOVE OBY-CLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VERIFY-BY-PERIOD-TBL VARYING
               BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 11
                  AFTER BYSUB2 FROM 1 BY 1 UNTIL BYSUB2 > 2
                    AFTER BYSUB1 FROM 1 BY 1 UNTIL BYSUB1 > 3.

           PERFORM VERIFY-BY-REPO-ACTIVITY-TBL VARYING
               BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 5
                  AFTER BYSUB2 FROM 1 BY 1 UNTIL BYSUB2 > 2
                    AFTER BYSUB1 FROM 1 BY 1 UNTIL BYSUB1 > 3.

           PERFORM VERIFY-BY-PL-ACTIVITY-TBL VARYING
               BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 10
                  AFTER BYSUB2 FROM 1 BY 1 UNTIL BYSUB2 > 2
                    AFTER BYSUB1 FROM 1 BY 1 UNTIL BYSUB1 > 3.

           PERFORM VERIFY-BY-DLR-ACTIVITY-TBL VARYING
               BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 3
                  AFTER BYSUB2 FROM 1 BY 1 UNTIL BYSUB2 > 2
                    AFTER BYSUB1 FROM 1 BY 1 UNTIL BYSUB1 > 3.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 2
               PERFORM VARYING S2 FROM 1 BY 1 UNTIL S2 > 05
                 IF OBY-PL-PRINCOL(S1 S2) NOT NUMERIC 
                    MOVE "BY-PL-PRINCOL(S1 S2)" TO D-FIELD
                    MOVE OBY-PL-PRINCOL(S1 S2)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE
                 END-IF
                 IF OBY-PL-INTCOL(S1 S2) NOT NUMERIC 
                    MOVE "BY-PL-INTCOL(S1 S2)" TO D-FIELD
                    MOVE OBY-PL-INTCOL(S1 S2)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE
                 END-IF
                 IF OBY-PL-LATECOL(S1 S2) NOT NUMERIC 
                    MOVE "BY-PL-LATECOL(S1 S2)" TO D-FIELD
                    MOVE OBY-PL-LATECOL(S1 S2)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE
                 END-IF
                 IF OBY-PL-O2COL(S1 S2) NOT NUMERIC 
                    MOVE "BY-PL-O2COL(S1 S2)" TO D-FIELD
                    MOVE OBY-PL-O2COL(S1 S2)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE
                 END-IF
               END-PERFORM
               IF OBY-AMT-SOLD(S1) NOT NUMERIC 
                  MOVE "BY-AMT-SOLD(S1)" TO D-FIELD
                  MOVE OBY-AMT-SOLD(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-AMT-TRANS(S1) NOT NUMERIC 
                  MOVE "BY-AMT-TRANS(S1)" TO D-FIELD
                  MOVE OBY-AMT-TRANS(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-AMT-PURCH(S1) NOT NUMERIC 
                  MOVE "BY-AMT-PURCH(S1)" TO D-FIELD
                  MOVE OBY-AMT-PURCH(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-NO-SOLD(S1) NOT NUMERIC 
                  MOVE "BY-NO-SOLD(S1)" TO D-FIELD
                  MOVE OBY-NO-SOLD(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-NO-TRANS(S1) NOT NUMERIC 
                  MOVE "BY-NO-TRANS(S1)" TO D-FIELD
                  MOVE OBY-NO-TRANS(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-NO-PURCH(S1) NOT NUMERIC 
                  MOVE "BY-NO-PURCH(S1)" TO D-FIELD
                  MOVE OBY-NO-PURCH(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-OTHCOL(S1) NOT NUMERIC 
                  MOVE "BY-OTHCOL(S1)" TO D-FIELD
                  MOVE OBY-OTHCOL(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-OT2COL(S1) NOT NUMERIC 
                  MOVE "BY-OT2COL(S1)" TO D-FIELD
                  MOVE OBY-OT2COL(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-INTCOL(S1) NOT NUMERIC 
                  MOVE "BY-INTCOL(S1)" TO D-FIELD
                  MOVE OBY-INTCOL(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-LATECOL(S1) NOT NUMERIC 
                  MOVE "BY-LATECOL(S1)" TO D-FIELD
                  MOVE OBY-LATECOL(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-PRINCOL(S1) NOT NUMERIC 
                  MOVE "BY-PRINCOL(S1)" TO D-FIELD
                  MOVE OBY-PRINCOL(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-DEFCOL(S1) NOT NUMERIC 
                  MOVE "BY-DEFCOL(S1)" TO D-FIELD
                  MOVE OBY-DEFCOL(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-RNPRIN(S1) NOT NUMERIC 
                  MOVE "BY-RNPRIN(S1)" TO D-FIELD
                  MOVE OBY-RNPRIN(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-INTREB(S1) NOT NUMERIC 
                  MOVE "BY-INTREB(S1)" TO D-FIELD
                  MOVE OBY-INTREB(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-CLREB1(S1) NOT NUMERIC 
                  MOVE "BY-CLREB1(S1)" TO D-FIELD
                  MOVE OBY-CLREB1(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-CLREB2(S1) NOT NUMERIC 
                  MOVE "BY-CLREB2(S1)" TO D-FIELD
                  MOVE OBY-CLREB2(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-CLREB3(S1) NOT NUMERIC 
                  MOVE "BY-CLREB3(S1)" TO D-FIELD
                  MOVE OBY-CLREB3(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-CLREB4(S1) NOT NUMERIC 
                  MOVE "BY-CLREB4(S1)" TO D-FIELD
                  MOVE OBY-CLREB4(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-AHREB(S1) NOT NUMERIC 
                  MOVE "BY-AHREB(S1)" TO D-FIELD
                  MOVE OBY-AHREB(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-PPREB(S1) NOT NUMERIC 
                  MOVE "BY-PPREB(S1)" TO D-FIELD
                  MOVE OBY-PPREB(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-O1REB(S1) NOT NUMERIC 
                  MOVE "BY-O1REB(S1)" TO D-FIELD
                  MOVE OBY-O1REB(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-O2REB(S1) NOT NUMERIC 
                  MOVE "BY-O2REB(S1)" TO D-FIELD
                  MOVE OBY-O2REB(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-O3REB(S1) NOT NUMERIC 
                  MOVE "BY-O3REB(S1)" TO D-FIELD
                  MOVE OBY-O3REB(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-O4REB(S1) NOT NUMERIC 
                  MOVE "BY-O4REB(S1)" TO D-FIELD
                  MOVE OBY-O4REB(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-O5REB(S1) NOT NUMERIC 
                  MOVE "BY-O5REB(S1)" TO D-FIELD
                  MOVE OBY-O5REB(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-O6REB(S1) NOT NUMERIC 
                  MOVE "BY-O6REB(S1)" TO D-FIELD
                  MOVE OBY-O6REB(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-O7REB(S1) NOT NUMERIC 
                  MOVE "BY-O7REB(S1)" TO D-FIELD
                  MOVE OBY-O7REB(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-SCREB(S1) NOT NUMERIC 
                  MOVE "BY-SCREB(S1)" TO D-FIELD
                  MOVE OBY-SCREB(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-MFREB(S1) NOT NUMERIC 
                  MOVE "BY-MFREB(S1)" TO D-FIELD
                  MOVE OBY-MFREB(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-DFREB(S1) NOT NUMERIC 
                  MOVE "BY-DFREB(S1)" TO D-FIELD
                  MOVE OBY-DFREB(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-PART-CHGOFF(S1) NOT NUMERIC 
                  MOVE "BY-PART-CHGOFF(S1)" TO D-FIELD
                  MOVE OBY-PART-CHGOFF(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-ALLOTMENTS(S1) NOT NUMERIC 
                  MOVE "BY-ALLOTMENTS(S1)" TO D-FIELD
                  MOVE OBY-ALLOTMENTS(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               PERFORM VARYING S2 FROM 1 BY 1 UNTIL S2 > 10
                  IF OBY-INS-NO-ELIG(S1 S2) NOT NUMERIC 
                     MOVE "BY-INS-NO-ELIG(S1 S2)" TO D-FIELD
                     MOVE OBY-INS-NO-ELIG(S1 S2)  TO D-VALUE
                     PERFORM WRITE-DETAIL-LINE
                  END-IF
                  IF OBY-INS-NO-WROTE(S1 S2) NOT NUMERIC 
                     MOVE "BY-INS-NO-WROTE(S1 S2)" TO D-FIELD
                     MOVE OBY-INS-NO-WROTE(S1 S2)  TO D-VALUE
                     PERFORM WRITE-DETAIL-LINE
                  END-IF
                  IF OBY-INS-NO-NOTWROTE(S1 S2) NOT NUMERIC 
                     MOVE "BY-INS-NO-NOTWROTE(S1 S2)" TO D-FIELD
                     MOVE OBY-INS-NO-NOTWROTE(S1 S2)  TO D-VALUE
                     PERFORM WRITE-DETAIL-LINE
                  END-IF
                  IF OBY-INS-AMT-WROTE(S1 S2) NOT NUMERIC 
                     MOVE "BY-INS-AMT-WROTE(S1 S2)" TO D-FIELD
                     MOVE OBY-INS-AMT-WROTE(S1 S2)  TO D-VALUE
                     PERFORM WRITE-DETAIL-LINE
                  END-IF
               END-PERFORM
               IF OBY-OTH-NO-COL(S1) NOT NUMERIC 
                  MOVE "BY-OTH-NO-COL(S1)" TO D-FIELD
                  MOVE OBY-OTH-NO-COL(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-OT2-NO-COL(S1) NOT NUMERIC 
                  MOVE "BY-OT2-NO-COL(S1)" TO D-FIELD
                  MOVE OBY-OT2-NO-COL(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-INT-NO-COL(S1) NOT NUMERIC 
                  MOVE "BY-INT-NO-COL(S1)" TO D-FIELD
                  MOVE OBY-INT-NO-COL(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-LATE-NO-COL(S1) NOT NUMERIC 
                  MOVE "BY-LATE-NO-COL(S1)" TO D-FIELD
                  MOVE OBY-LATE-NO-COL(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-OTH-NO-WAIVED(S1) NOT NUMERIC 
                  MOVE "BY-OTH-NO-WAIVED(S1)" TO D-FIELD
                  MOVE OBY-OTH-NO-WAIVED(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-OT2-NO-WAIVED(S1) NOT NUMERIC 
                  MOVE "BY-OT2-NO-WAIVED(S1)" TO D-FIELD
                  MOVE OBY-OT2-NO-WAIVED(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-INT-NO-WAIVED(S1) NOT NUMERIC 
                  MOVE "BY-INT-NO-WAIVED(S1)" TO D-FIELD
                  MOVE OBY-INT-NO-WAIVED(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-LATE-NO-WAIVED(S1) NOT NUMERIC 
                  MOVE "BY-LATE-NO-WAIVED(S1)" TO D-FIELD
                  MOVE OBY-LATE-NO-WAIVED(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-OTH-AMT-WAIVED(S1) NOT NUMERIC 
                  MOVE "BY-OTH-AMT-WAIVED(S1)" TO D-FIELD
                  MOVE OBY-OTH-AMT-WAIVED(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-OT2-AMT-WAIVED(S1) NOT NUMERIC 
                  MOVE "BY-OT2-AMT-WAIVED(S1)" TO D-FIELD
                  MOVE OBY-OT2-AMT-WAIVED(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-INT-AMT-WAIVED(S1) NOT NUMERIC 
                  MOVE "BY-INT-AMT-WAIVED(S1)" TO D-FIELD
                  MOVE OBY-INT-AMT-WAIVED(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OBY-LATE-AMT-WAIVED(S1) NOT NUMERIC 
                  MOVE "BY-LATE-AMT-WAIVED(S1)" TO D-FIELD
                  MOVE OBY-LATE-AMT-WAIVED(S1)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
           END-PERFORM.

           PERFORM VERIFY-BY-DEL-BUCKETS VARYING
                     BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 10
                AFTER BYSUB2 FROM 1 BY 1 UNTIL BYSUB2 > 2
                 AFTER BYSUB1 FROM 1 BY 1 UNTIL BYSUB1 > 3.

           IF OBY-SUMWGTTM NOT NUMERIC 
              MOVE "BY-SUMWGTTM" TO D-FIELD
              MOVE OBY-SUMWGTTM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-SUMWGTAV NOT NUMERIC 
              MOVE "BY-SUMWGTAV" TO D-FIELD
              MOVE OBY-SUMWGTAV  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-SUMWGTTA NOT NUMERIC 
              MOVE "BY-SUMWGTTA" TO D-FIELD
              MOVE OBY-SUMWGTTA  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-IB-ACCRUED NOT NUMERIC 
              MOVE "BY-IB-ACCRUED" TO D-FIELD
              MOVE OBY-IB-ACCRUED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
              IF OBY-DLDISC(S1) NOT NUMERIC 
                 MOVE "BY-DLDISC(S1)" TO D-FIELD
                 MOVE OBY-DLDISC(S1)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OBY-STD-POS NOT NUMERIC 
              MOVE "BY-STD-POS" TO D-FIELD
              MOVE OBY-STD-POS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-30-DAY-MOVED NOT NUMERIC 
              MOVE "BY-30-DAY-MOVED" TO D-FIELD
              MOVE OBY-30-DAY-MOVED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-PROJ-INCREASE NOT NUMERIC 
              MOVE "BY-PROJ-INCREASE" TO D-FIELD
              MOVE OBY-PROJ-INCREASE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-PROJ-PROFIT NOT NUMERIC 
              MOVE "BY-PROJ-PROFIT" TO D-FIELD
              MOVE OBY-PROJ-PROFIT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                     AFTER S2 FROM 1 BY 1 UNTIL S2 > 2
                     AFTER S3 FROM 1 BY 1 UNTIL S3 > 6
              IF OBY-UNEARNED-AMT(S1 S2 S3) NOT NUMERIC 
                 MOVE "BY-UNEARNED-AMT(S1 S2 S3)" TO D-FIELD
                 MOVE OBY-UNEARNED-AMT(S1 S2 S3)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 2
              IF OBY-PROCEEDS(S1) NOT NUMERIC 
                 MOVE "BY-PROCEEDS(S1)" TO D-FIELD
                 MOVE OBY-PROCEEDS(S1)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OBY-LOCKBOX(S1) NOT NUMERIC 
                 MOVE "BY-LOCKBOX(S1)" TO D-FIELD
                 MOVE OBY-LOCKBOX(S1)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.


           GO TO PROCESS-FILE-BY-NEXT.

       PROCESS-FILE-BY-END.

           PERFORM CLOSE-OBY1-FILE.

       PROCESS-FILE-BY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - C D
      *
      ******************************************************************
       PROCESS-FILE-CD SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-CD = "N" )
              GO TO PROCESS-FILE-CD-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO CD-PATH-OVERRIDE
                                              CDB-PATH-OVERRIDE.
           MOVE "CD"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/CDFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OCD-PATH.

           MOVE SPACES                     TO OCD1-KEY.
           PERFORM OPEN-OCD1-FILE.
           PERFORM START-OCD1-FILE.

       PROCESS-FILE-CD-NEXT.
           PERFORM READ-OCD1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-CD-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

      * ADVERTISING SOURCE CODE
           IF ( OCD-AS-TYPE = "AS" )
              GO TO PROCESS-FILE-CD-NEXT.
      
      * DORMANT REASON
           IF ( OCD-DR-TYPE = "DR" )
              GO TO PROCESS-FILE-CD-NEXT.

      * TRIAD SCORING
           IF ( OCD-LR-TYPE = "LR" )
              GO TO PROCESS-FILE-CD-NEXT.

      * PRODUCTS CODES (REVOLVING)
           IF ( OCD-PR-TYPE = "PR" )
              GO TO PROCESS-FILE-CD-NEXT.

      * TURN DOWN (REFUSAL CODES/LOAN APPLICATION STATUS)
           IF ( OCD-RF-TYPE = "RF" )
              GO TO PROCESS-FILE-CD-NEXT.

      * REVOLVING OTHER CHARGE CODES
           IF ( OCD-RO-TYPE = "RO" )
              GO TO PROCESS-FILE-CD-NEXT.

           IF ( OCD-CN-TYPE = "CN" )
              IF OCD-CN-BRANCH NOT NUMERIC 
                 MOVE "CD-CN-BRANCH" TO D-FIELD
                 MOVE OCD-CN-BRANCH  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OCD-CN-PHONE NOT NUMERIC 
                 MOVE "CD-CN-PHONE" TO D-FIELD
                 MOVE OCD-CN-PHONE  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OCD-CN-PHONE-EXT NOT NUMERIC 
                 MOVE "CD-CN-PHONE-EXT" TO D-FIELD
                 MOVE OCD-CN-PHONE-EXT  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              GO TO PROCESS-FILE-CD-NEXT.
     

           IF ( OCD-BR-TYPE = "GC" )
             IF OCD-GLFEENO NOT NUMERIC 
                MOVE "CD-GLFEENO" TO D-FIELD
                MOVE OCD-GLFEENO  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OCD-FEE-PRODNO NOT NUMERIC 
                MOVE "CD-FEE-PRODNO" TO D-FIELD
                MOVE OCD-FEE-PRODNO  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OCD-FEE-PRICE NOT NUMERIC 
                MOVE "CD-FEE-PRICE" TO D-FIELD
                MOVE OCD-FEE-PRICE  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OCD-NEW-PRODNO NOT NUMERIC 
                MOVE "CD-NEW-PRODNO" TO D-FIELD
                MOVE OCD-NEW-PRODNO  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OCD-FEE-MAXAMT NOT NUMERIC 
                MOVE "CD-FEE-MAXAMT" TO D-FIELD
                MOVE OCD-FEE-MAXAMT  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OCD-ALT-GLFEENO NOT NUMERIC 
                MOVE "CD-ALT-GLFEENO" TO D-FIELD
                MOVE OCD-ALT-GLFEENO  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OCD-COMM-GLFEENO NOT NUMERIC 
                MOVE "CD-COMM-GLFEENO" TO D-FIELD
                MOVE OCD-COMM-GLFEENO  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OCD-COMM-PERCENT NOT NUMERIC 
                MOVE "CD-COMM-PERCENT" TO D-FIELD
                MOVE OCD-COMM-PERCENT  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OCD-BRANCH-COMM-AMOUNT NOT NUMERIC 
                MOVE "CD-BRANCH-COMM-AMOUNT" TO D-FIELD
                MOVE OCD-BRANCH-COMM-AMOUNT  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             GO TO PROCESS-FILE-CD-NEXT.

           IF ( OCD-BR-TYPE = "BR" )
              IF OCD-BR-GL-BRANCH NOT NUMERIC
                 MOVE "CD-BR-GL-BRANCH" TO D-FIELD
                 MOVE OCD-BR-GL-BRANCH  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OCD-BR-GL-ACCT NOT NUMERIC
                 MOVE "CD-BR-GL-ACCT" TO D-FIELD
                 MOVE OCD-BR-GL-ACCT  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF

              IF OCD-BR-OT-GL-BRANCH NOT NUMERIC
                 MOVE "CD-BR-OT-GL-BRANCH" TO D-FIELD
                 MOVE OCD-BR-OT-GL-BRANCH  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OCD-BR-OT-GL-ACCT NOT NUMERIC
                 MOVE "CD-BR-OT-GL-ACCT" TO D-FIELD
                 MOVE OCD-BR-OT-GL-ACCT  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              GO TO PROCESS-FILE-CD-NEXT.
              
           IF ( OCD-CT-TYPE = "CT" )
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
                 IF OCD-CT-INSTAXAMT(SUB) NOT NUMERIC 
                    MOVE "CD-CT-INSTAXAMT(SUB)" TO D-FIELD
                    MOVE OCD-CT-INSTAXAMT(SUB)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE
                 END-IF
              END-PERFORM
              GO TO PROCESS-FILE-CD-NEXT.


           IF ( OCD-CY-TYPE = "CY" )
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
                 IF OCD-CY-INSTAXAMT(SUB) NOT NUMERIC 
                    MOVE "CD-CY-INSTAXAMT(SUB)" TO D-FIELD
                    MOVE OCD-CY-INSTAXAMT(SUB)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE
                 END-IF
              END-PERFORM
              GO TO PROCESS-FILE-CD-NEXT.

           IF ( OCD-EL-TYPE = "EL" )
              IF OCD-EL-CODE NOT NUMERIC 
                 MOVE "CD-EL-CODE" TO D-FIELD
                 MOVE OCD-EL-CODE  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OCD-ELGRACE NOT NUMERIC 
                 MOVE "CD-ELGRACE" TO D-FIELD
                 MOVE OCD-ELGRACE  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OCD-ELRATE NOT NUMERIC 
                 MOVE "CD-ELRATE" TO D-FIELD
                 MOVE OCD-ELRATE  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OCD-ELMINCHG NOT NUMERIC 
                 MOVE "CD-ELMINCHG" TO D-FIELD
                 MOVE OCD-ELMINCHG  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OCD-ELMAXAMT NOT NUMERIC 
                 MOVE "CD-ELMAXAMT" TO D-FIELD
                 MOVE OCD-ELMAXAMT  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OCD-ELDELFAC NOT NUMERIC 
                 MOVE "CD-ELDELFAC" TO D-FIELD
                 MOVE OCD-ELDELFAC  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OCD-ELYRTYPE NOT NUMERIC 
                 MOVE "CD-ELYRTYPE" TO D-FIELD
                 MOVE OCD-ELYRTYPE  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              GO TO PROCESS-FILE-CD-NEXT.

           IF ( OCD-PO-TYPE = "PO" )
              IF OCD-TITLE-HOLD-DAYS NOT NUMERIC 
                 MOVE "CD-TITLE-HOLD-DAYS" TO D-FIELD
                 MOVE OCD-TITLE-HOLD-DAYS  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              GO TO PROCESS-FILE-CD-NEXT.

           IF ( OCD-PO-TYPE = "RE" )
              IF OCD-RE-RESCHEDULE-TIME NOT NUMERIC 
                 MOVE "CD-RE-RESCHEDULE-TIME" TO D-FIELD
                 MOVE OCD-RE-RESCHEDULE-TIME  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              GO TO PROCESS-FILE-CD-NEXT.

           IF ( OCD-PO-TYPE = "FC" )
              IF OCD-FCFEEAMT NOT NUMERIC
                 MOVE "CD-FCFEEAMT" TO D-FIELD
                 MOVE OCD-FCFEEAMT  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              GO TO PROCESS-FILE-CD-NEXT.

           GO TO PROCESS-FILE-CD-NEXT.

       PROCESS-FILE-CD-END.

           PERFORM CLOSE-OCD1-FILE.

       PROCESS-FILE-CD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - C L
      *
      ******************************************************************
       PROCESS-FILE-CL SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-CL = "N" )
              GO TO PROCESS-FILE-CL-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO CL-PATH-OVERRIDE.
           MOVE "CL"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/CLFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.
      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OCL-PATH.

           MOVE SPACES                     TO OCL1-KEY.
           MOVE ZEROES                     TO OCL-GROUP-LOC.
           PERFORM OPEN-OCL1-FILE.
           PERFORM START-OCL1-FILE.

       PROCESS-FILE-CL-NEXT.
           PERFORM READ-OCL1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-CL-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           MOVE OCL-PD-REC       TO OPD-REC.

           PERFORM VERIFY-PD-REC.

           GO TO PROCESS-FILE-CL-NEXT.

       PROCESS-FILE-CL-END.

           PERFORM CLOSE-OCL1-FILE.

       PROCESS-FILE-CL-EXIT.
           EXIT.

      ******************************************************************
       VERIFY-PD-REC SECTION.
      ******************************************************************
          
           IF OPD-OWNBR NOT NUMERIC 
              MOVE "PD-OWNBR" TO D-FIELD
              MOVE OPD-OWNBR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-CLASS NOT NUMERIC 
              MOVE "PD-CLASS" TO D-FIELD
              MOVE OPD-CLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-ACCTNO NOT NUMERIC 
              MOVE "PD-ACCTNO" TO D-FIELD
              MOVE OPD-ACCTNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-SUBCLASS NOT NUMERIC 
              MOVE "PD-SUBCLASS" TO D-FIELD
              MOVE OPD-SUBCLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-LAWCODE NOT NUMERIC 
              MOVE "PD-LAWCODE" TO D-FIELD
              MOVE OPD-LAWCODE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              IF OPD-SSNO(SUB) NOT NUMERIC 
                 MOVE "PD-SSNO(SUB)" TO D-FIELD
                 MOVE OPD-SSNO(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OPD-ORGBR NOT NUMERIC 
              MOVE "PD-ORGBR" TO D-FIELD
              MOVE OPD-ORGBR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-SPRCLASS NOT NUMERIC 
              MOVE "PD-SPRCLASS" TO D-FIELD
              MOVE OPD-SPRCLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-LC-CODE NOT NUMERIC 
              MOVE "PD-LC-CODE" TO D-FIELD
              MOVE OPD-LC-CODE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-ENTDATE NOT NUMERIC OR
               (OPD-ENTDATE = 202020)
              MOVE "PD-ENTDATE" TO D-FIELD
              MOVE OPD-ENTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-ENTDATE NOT = 0
                 MOVE OPD-ENTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-ENTDATE" TO D-FIELD
                    MOVE OPD-ENTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-LOANDATE NOT NUMERIC OR
               (OPD-LOANDATE = 202020)
              MOVE "PD-LOANDATE" TO D-FIELD
              MOVE OPD-LOANDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-LOANDATE NOT = 0
                 MOVE OPD-LOANDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-LOANDATE" TO D-FIELD
                    MOVE OPD-LOANDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-PURDATE NOT NUMERIC OR
               (OPD-PURDATE = 202020)
              MOVE "PD-PURDATE" TO D-FIELD
              MOVE OPD-PURDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-PURDATE NOT = 0
                 MOVE OPD-PURDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-PURDATE" TO D-FIELD
                    MOVE OPD-PURDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-INTDATE NOT NUMERIC OR
               (OPD-INTDATE = 202020)
              MOVE "PD-INTDATE" TO D-FIELD
              MOVE OPD-INTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-INTDATE NOT = 0
                 MOVE OPD-INTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-INTDATE" TO D-FIELD
                    MOVE OPD-INTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-CASHDATE NOT NUMERIC OR
               (OPD-CASHDATE = 202020)
              MOVE "PD-CASHDATE" TO D-FIELD
              MOVE OPD-CASHDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-CASHDATE NOT = 0
                 MOVE OPD-CASHDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-CASHDATE" TO D-FIELD
                    MOVE OPD-CASHDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-FILEDATE NOT NUMERIC OR
               (OPD-FILEDATE = 202020)
              MOVE "PD-FILEDATE" TO D-FIELD
              MOVE OPD-FILEDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-FILEDATE NOT = 0
                 MOVE OPD-FILEDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-FILEDATE" TO D-FIELD
                    MOVE OPD-FILEDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-ORIG-1STPAY NOT NUMERIC OR
               (OPD-ORIG-1STPAY = 202020)
              MOVE "PD-ORIG-1STPAY" TO D-FIELD
              MOVE OPD-ORIG-1STPAY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-ORIG-1STPAY NOT = 0
                 MOVE OPD-ORIG-1STPAY TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-ORIG-1STPAY" TO D-FIELD
                    MOVE OPD-ORIG-1STPAY  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-1STPYDATE NOT NUMERIC OR
               (OPD-1STPYDATE = 202020)
              MOVE "PD-1STPYDATE" TO D-FIELD
              MOVE OPD-1STPYDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-1STPYDATE NOT = 0
                 MOVE OPD-1STPYDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-1STPYDATE" TO D-FIELD
                    MOVE OPD-1STPYDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-JDDATE NOT NUMERIC OR
               (OPD-JDDATE = 202020)
              MOVE "PD-JDDATE" TO D-FIELD
              MOVE OPD-JDDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-JDDATE NOT = 0
                 MOVE OPD-JDDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-JDDATE" TO D-FIELD
                    MOVE OPD-JDDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-PLDATE NOT NUMERIC OR
               (OPD-PLDATE = 202020)
              MOVE "PD-PLDATE" TO D-FIELD
              MOVE OPD-PLDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-PLDATE NOT = 0
                 MOVE OPD-PLDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-PLDATE" TO D-FIELD
                    MOVE OPD-PLDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OPD-ACCELDATE NOT NUMERIC OR
               (OPD-ACCELDATE = 202020)
              MOVE "PD-ACCELDATE" TO D-FIELD
              MOVE OPD-ACCELDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-ACCELDATE NOT = 0
                 MOVE OPD-ACCELDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-ACCELDATE" TO D-FIELD
                    MOVE OPD-ACCELDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-REDUDATE NOT NUMERIC OR
               (OPD-REDUDATE = 202020)
              MOVE "PD-REDUDATE" TO D-FIELD
              MOVE OPD-REDUDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-REDUDATE NOT = 0
                 MOVE OPD-REDUDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-REDUDATE" TO D-FIELD
                    MOVE OPD-REDUDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-POFFDATE NOT NUMERIC OR
               (OPD-POFFDATE = 202020)
              MOVE "PD-POFFDATE" TO D-FIELD
              MOVE OPD-POFFDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-POFFDATE NOT = 0
                 MOVE OPD-POFFDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-POFFDATE" TO D-FIELD
                    MOVE OPD-POFFDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-LEGALDATE NOT NUMERIC OR
               (OPD-LEGALDATE = 202020)
              MOVE "PD-LEGALDATE" TO D-FIELD
              MOVE OPD-LEGALDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-LEGALDATE NOT = 0
                 MOVE OPD-LEGALDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-LEGALDATE" TO D-FIELD
                    MOVE OPD-LEGALDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-BNKRPTDATE NOT NUMERIC OR
               (OPD-BNKRPTDATE = 202020)
              MOVE "PD-BNKRPTDATE" TO D-FIELD
              MOVE OPD-BNKRPTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-BNKRPTDATE NOT = 0
                 MOVE OPD-BNKRPTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-BNKRPTDATE" TO D-FIELD
                    MOVE OPD-BNKRPTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-DEBTAIDATE NOT NUMERIC OR
               (OPD-DEBTAIDATE = 202020)
              MOVE "PD-DEBTAIDATE" TO D-FIELD
              MOVE OPD-DEBTAIDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-DEBTAIDATE NOT = 0
                 MOVE OPD-DEBTAIDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-DEBTAIDATE" TO D-FIELD
                    MOVE OPD-DEBTAIDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OPD-REPODATE NOT NUMERIC OR
               (OPD-REPODATE = 202020)
              MOVE "PD-REPODATE" TO D-FIELD
              MOVE OPD-REPODATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-REPODATE NOT = 0
                 MOVE OPD-REPODATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-REPODATE" TO D-FIELD
                    MOVE OPD-REPODATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-REDEEMDATE NOT NUMERIC OR
               (OPD-REDEEMDATE = 202020)
              MOVE "PD-REDEEMDATE" TO D-FIELD
              MOVE OPD-REDEEMDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-REDEEMDATE NOT = 0
                 MOVE OPD-REDEEMDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-REDEEMDATE" TO D-FIELD
                    MOVE OPD-REDEEMDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-SOLDDATE NOT NUMERIC OR
               (OPD-SOLDDATE = 202020)
              MOVE "PD-SOLDDATE" TO D-FIELD
              MOVE OPD-SOLDDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-SOLDDATE NOT = 0
                 MOVE OPD-SOLDDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-SOLDDATE" TO D-FIELD
                    MOVE OPD-SOLDDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-INS-CONFIRMATION NOT NUMERIC OR
               (OPD-INS-CONFIRMATION = 202020)
              MOVE "PD-INS-CONFIRMATION" TO D-FIELD
              MOVE OPD-INS-CONFIRMATION  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-INS-CONFIRMATION NOT = 0
                 MOVE OPD-INS-CONFIRMATION TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-INS-CONFIRMATION" TO D-FIELD
                    MOVE OPD-INS-CONFIRMATION  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OPD-APRATE NOT NUMERIC 
              MOVE "PD-APRATE" TO D-FIELD
              MOVE OPD-APRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-EFFRATE NOT NUMERIC 
              MOVE "PD-EFFRATE" TO D-FIELD
              MOVE OPD-EFFRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-SMPRATE NOT NUMERIC 
              MOVE "PD-SMPRATE" TO D-FIELD
              MOVE OPD-SMPRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-YLDRATE NOT NUMERIC 
              MOVE "PD-YLDRATE" TO D-FIELD
              MOVE OPD-YLDRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-OVRDRATE NOT NUMERIC 
              MOVE "PD-OVRDRATE" TO D-FIELD
              MOVE OPD-OVRDRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-ENDORATE NOT NUMERIC 
              MOVE "PD-ENDORATE" TO D-FIELD
              MOVE OPD-ENDORATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-OVRDRATE NOT NUMERIC 
              MOVE "PD-OVRDRATE" TO D-FIELD
              MOVE OPD-OVRDRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-JDRATE NOT NUMERIC 
              MOVE "PD-JDRATE" TO D-FIELD
              MOVE OPD-JDRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-ACCRATE NOT NUMERIC 
              MOVE "PD-ACCRATE" TO D-FIELD
              MOVE OPD-ACCRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-REDURATE NOT NUMERIC 
              MOVE "PD-REDURATE" TO D-FIELD
              MOVE OPD-REDURATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-SERVPER NOT NUMERIC 
              MOVE "PD-SERVPER" TO D-FIELD
              MOVE OPD-SERVPER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-SELLPR NOT NUMERIC 
              MOVE "PD-SELLPR" TO D-FIELD
              MOVE OPD-SELLPR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-DOWNPY NOT NUMERIC 
              MOVE "PD-DOWNPY" TO D-FIELD
              MOVE OPD-DOWNPY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-TRADEIN NOT NUMERIC 
              MOVE "PD-TRADEIN" TO D-FIELD
              MOVE OPD-TRADEIN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-LNAMT NOT NUMERIC 
              MOVE "PD-LNAMT" TO D-FIELD
              MOVE OPD-LNAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-FINCHG NOT NUMERIC 
              MOVE "PD-FINCHG" TO D-FIELD
              MOVE OPD-FINCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-INTCHG NOT NUMERIC 
              MOVE "PD-INTCHG" TO D-FIELD
              MOVE OPD-INTCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-SERCHG NOT NUMERIC 
              MOVE "PD-SERCHG" TO D-FIELD
              MOVE OPD-SERCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-EXTCHG NOT NUMERIC 
              MOVE "PD-EXTCHG" TO D-FIELD
              MOVE OPD-EXTCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-MAINTFEE NOT NUMERIC 
              MOVE "PD-MAINTFEE" TO D-FIELD
              MOVE OPD-MAINTFEE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-POINTS NOT NUMERIC 
              MOVE "PD-POINTS" TO D-FIELD
              MOVE OPD-POINTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PROCEEDS NOT NUMERIC 
              MOVE "PD-PROCEEDS" TO D-FIELD
              MOVE OPD-PROCEEDS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-ORGTERM NOT NUMERIC 
              MOVE "PD-ORGTERM" TO D-FIELD
              MOVE OPD-ORGTERM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-1STPYAMT NOT NUMERIC 
              MOVE "PD-1STPYAMT" TO D-FIELD
              MOVE OPD-1STPYAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-NOREGPY NOT NUMERIC 
              MOVE "PD-NOREGPY" TO D-FIELD
              MOVE OPD-NOREGPY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-REGPYAMT NOT NUMERIC 
              MOVE "PD-REGPYAMT" TO D-FIELD
              MOVE OPD-REGPYAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-LASTPYAMT NOT NUMERIC 
              MOVE "PD-LASTPYAMT" TO D-FIELD
              MOVE OPD-LASTPYAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PROCASH NOT NUMERIC 
              MOVE "PD-PROCASH" TO D-FIELD
              MOVE OPD-PROCASH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-STATE-WORKER NOT NUMERIC 
              MOVE "PD-STATE-WORKER" TO D-FIELD
              MOVE OPD-STATE-WORKER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-STATE-WORKER2 NOT NUMERIC 
              MOVE "PD-STATE-WORKER2" TO D-FIELD
              MOVE OPD-STATE-WORKER2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.


           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF OPD-FEEAMT(SUB) NOT NUMERIC 
                 MOVE "PD-FEEAMT(SUB)" TO D-FIELD
                 MOVE OPD-FEEAMT(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              IF OPD-INSCOMP(SUB) NOT NUMERIC 
                 MOVE "PD-INSCOMP(SUB)" TO D-FIELD
                 MOVE OPD-INSCOMP(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OPD-INSEFF(SUB) NOT NUMERIC 
                 MOVE "PD-INSEFF(SUB)" TO D-FIELD
                 MOVE OPD-INSEFF(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OPD-INSEXP(SUB) NOT NUMERIC 
                 MOVE "PD-INSEXP(SUB)" TO D-FIELD
                 MOVE OPD-INSEXP(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OPD-INSPREM(SUB) NOT NUMERIC 
                 MOVE "PD-INSPREM(SUB)" TO D-FIELD
                 MOVE OPD-INSPREM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OPD-INSCOVR(SUB) NOT NUMERIC 
                 MOVE "PD-INSCOVR(SUB)" TO D-FIELD
                 MOVE OPD-INSCOVR(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OPD-INSCOMM(SUB) NOT NUMERIC 
                 MOVE "PD-INSCOMM(SUB)" TO D-FIELD
                 MOVE OPD-INSCOMM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OPD-INSANTICERN(SUB) NOT NUMERIC 
                 MOVE "PD-INSANTICERN(SUB)" TO D-FIELD
                 MOVE OPD-INSANTICERN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OPD-INSANTICADJ(SUB) NOT NUMERIC 
                 MOVE "PD-INSANTICADJ(SUB)" TO D-FIELD
                 MOVE OPD-INSANTICADJ(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OPD-INSUFERN(SUB) NOT NUMERIC 
                 MOVE "PD-INSUFERN(SUB)" TO D-FIELD
                 MOVE OPD-INSUFERN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OPD-INSACCERN(SUB) NOT NUMERIC 
                 MOVE "PD-INSACCERN(SUB)" TO D-FIELD
                 MOVE OPD-INSACCERN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OPD-INSEARN-ORDER(SUB) NOT NUMERIC 
                 MOVE "PD-INSEARN-ORDER(SUB)" TO D-FIELD
                 MOVE OPD-INSEARN-ORDER(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OPD-PYMMYY-RECDEL NOT NUMERIC 
              MOVE "PD-PYMMYY-RECDEL" TO D-FIELD
              MOVE OPD-PYMMYY-RECDEL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-MTD-PAYS-RECDEL NOT NUMERIC 
              MOVE "PD-MTD-PAYS-RECDEL" TO D-FIELD
              MOVE OPD-MTD-PAYS-RECDEL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-AD-D-INS-COVR NOT NUMERIC 
              MOVE "PD-AD-D-INS-COVR" TO D-FIELD
              MOVE OPD-AD-D-INS-COVR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-TOT-PARTIAL-CHGOFF NOT NUMERIC 
              MOVE "PD-TOT-PARTIAL-CHGOFF" TO D-FIELD
              MOVE OPD-TOT-PARTIAL-CHGOFF  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PRIOR-RN-FOR10 NOT NUMERIC OR
               (OPD-PRIOR-RN-FOR10 = 202020)
              MOVE "PD-PRIOR-RN-FOR10" TO D-FIELD
              MOVE OPD-PRIOR-RN-FOR10  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-PRIOR-RN-FOR10 NOT = 0
                 MOVE OPD-PRIOR-RN-FOR10 TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-PRIOR-RN-FOR10" TO D-FIELD
                    MOVE OPD-PRIOR-RN-FOR10  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-LAST-RN-FOR10 NOT NUMERIC OR
               (OPD-LAST-RN-FOR10 = 202020)
              MOVE "PD-LAST-RN-FOR10" TO D-FIELD
              MOVE OPD-LAST-RN-FOR10  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-LAST-RN-FOR10 NOT = 0
                 MOVE OPD-LAST-RN-FOR10 TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-LAST-RN-FOR10" TO D-FIELD
                    MOVE OPD-LAST-RN-FOR10  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-WCBC-CREDITLIM NOT NUMERIC 
              MOVE "PD-WCBC-CREDITLIM" TO D-FIELD
              MOVE OPD-WCBC-CREDITLIM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-MIP-MTD-CLAHPAID NOT NUMERIC 
              MOVE "PD-MIP-MTD-CLAHPAID" TO D-FIELD
              MOVE OPD-MIP-MTD-CLAHPAID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-MIP-CLPDTH NOT NUMERIC OR
               (OPD-MIP-CLPDTH = 202020)
              MOVE "PD-MIP-CLPDTH" TO D-FIELD
              MOVE OPD-MIP-CLPDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-MIP-CLPDTH NOT = 0
                 MOVE OPD-MIP-CLPDTH TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-MIP-CLPDTH" TO D-FIELD
                    MOVE OPD-MIP-CLPDTH  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-MIP-AHPDTH NOT NUMERIC OR
               (OPD-MIP-AHPDTH = 202020)
              MOVE "PD-MIP-AHPDTH" TO D-FIELD
              MOVE OPD-MIP-AHPDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-MIP-AHPDTH NOT = 0
                 MOVE OPD-MIP-AHPDTH TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-MIP-AHPDTH" TO D-FIELD
                    MOVE OPD-MIP-AHPDTH  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-MIP-TOT-CLPAID NOT NUMERIC 
              MOVE "PD-MIP-TOT-CLPAID" TO D-FIELD
              MOVE OPD-MIP-TOT-CLPAID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-MIP-TOT-AHPAID NOT NUMERIC 
              MOVE "PD-MIP-TOT-AHPAID" TO D-FIELD
              MOVE OPD-MIP-TOT-AHPAID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-DLNO NOT NUMERIC 
              MOVE "PD-DLNO" TO D-FIELD
              MOVE OPD-DLNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-DLPOOL NOT NUMERIC 
              MOVE "PD-DLPOOL" TO D-FIELD
              MOVE OPD-DLPOOL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-DLPARTRCAMT NOT NUMERIC 
              MOVE "PD-DLPARTRCAMT" TO D-FIELD
              MOVE OPD-DLPARTRCAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-ORIGPURAMT NOT NUMERIC 
              MOVE "PD-ORIGPURAMT" TO D-FIELD
              MOVE OPD-ORIGPURAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PURBAL NOT NUMERIC 
              MOVE "PD-PURBAL" TO D-FIELD
              MOVE OPD-PURBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.


           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OPD-DLRVPRIN(SUB) NOT NUMERIC 
                 MOVE "PD-DLRVPRIN(SUB)" TO D-FIELD
                 MOVE OPD-DLRVPRIN(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OPD-DLRVCHGS(SUB) NOT NUMERIC 
                 MOVE "PD-DLRVCHGS(SUB)" TO D-FIELD
                 MOVE OPD-DLRVCHGS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OPD-DLPARVHELD NOT NUMERIC 
              MOVE "PD-DLPARVHELD" TO D-FIELD
              MOVE OPD-DLPARVHELD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-DLPARVPAID NOT NUMERIC 
              MOVE "PD-DLPARVPAID" TO D-FIELD
              MOVE OPD-DLPARVPAID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-DLDISC NOT NUMERIC 
              MOVE "PD-DLDISC" TO D-FIELD
              MOVE OPD-DLDISC  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-LCPDTH NOT NUMERIC OR
               (OPD-LCPDTH = 202020)
              MOVE "PD-LCPDTH" TO D-FIELD
              MOVE OPD-LCPDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-LCPDTH NOT = 0
                 MOVE OPD-LCPDTH TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-LCPDTH" TO D-FIELD
                    MOVE OPD-LCPDTH  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-INTPDTH NOT NUMERIC OR
               (OPD-INTPDTH = 202020)
              MOVE "PD-INTPDTH" TO D-FIELD
              MOVE OPD-INTPDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-INTPDTH NOT = 0
                 MOVE OPD-INTPDTH TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-INTPDTH" TO D-FIELD
                    MOVE OPD-INTPDTH  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OPD-CURBAL NOT NUMERIC 
              MOVE "PD-CURBAL" TO D-FIELD
              MOVE OPD-CURBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-INTBAL NOT NUMERIC 
              MOVE "PD-INTBAL" TO D-FIELD
              MOVE OPD-INTBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-LCBAL NOT NUMERIC 
              MOVE "PD-LCBAL" TO D-FIELD
              MOVE OPD-LCBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-OTHBAL NOT NUMERIC 
              MOVE "PD-OTHBAL" TO D-FIELD
              MOVE OPD-OTHBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-OT2BAL NOT NUMERIC 
              MOVE "PD-OT2BAL" TO D-FIELD
              MOVE OPD-OT2BAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-LCPAID NOT NUMERIC 
              MOVE "PD-LCPAID" TO D-FIELD
              MOVE OPD-LCPAID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-LCPARTIALS NOT NUMERIC 
              MOVE "PD-LCPARTIALS" TO D-FIELD
              MOVE OPD-LCPARTIALS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-STMT-COUNTER NOT NUMERIC 
              MOVE "PD-STMT-COUNTER" TO D-FIELD
              MOVE OPD-STMT-COUNTER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-LEDGERCNT NOT NUMERIC 
              MOVE "PD-LEDGERCNT" TO D-FIELD
              MOVE OPD-LEDGERCNT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-LPCNT NOT NUMERIC 
              MOVE "PD-LPCNT" TO D-FIELD
              MOVE OPD-LPCNT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-MONTHEND-BAL NOT NUMERIC 
              MOVE "PD-MONTHEND-BAL" TO D-FIELD
              MOVE OPD-MONTHEND-BAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-ANTICTERM NOT NUMERIC 
              MOVE "PD-ANTICTERM" TO D-FIELD
              MOVE OPD-ANTICTERM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               IF OPD-ANTICERN(SUB) NOT NUMERIC 
                  MOVE "PD-ANTICERN(SUB)" TO D-FIELD
                  MOVE OPD-ANTICERN(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OPD-ANTICADJ(SUB) NOT NUMERIC 
                  MOVE "PD-ANTICADJ(SUB)" TO D-FIELD
                  MOVE OPD-ANTICADJ(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OPD-UFERN(SUB) NOT NUMERIC 
                  MOVE "PD-UFERN(SUB)" TO D-FIELD
                  MOVE OPD-UFERN(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OPD-ACCERN(SUB) NOT NUMERIC 
                  MOVE "PD-ACCERN(SUB)" TO D-FIELD
                  MOVE OPD-ACCERN(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OPD-EARN-ORDER(SUB) NOT NUMERIC 
                  MOVE "PD-EARN-ORDER(SUB)" TO D-FIELD
                  MOVE OPD-EARN-ORDER(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
           END-PERFORM.

           IF OPD-LMO-PRIOR-UNEARNED NOT NUMERIC 
              MOVE "PD-LMO-PRIOR-UNEARNED" TO D-FIELD
              MOVE OPD-LMO-PRIOR-UNEARNED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PRIOR-UNEARNED NOT NUMERIC 
              MOVE "PD-PRIOR-UNEARNED" TO D-FIELD
              MOVE OPD-PRIOR-UNEARNED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-NONACRERN NOT NUMERIC 
              MOVE "PD-NONACRERN" TO D-FIELD
              MOVE OPD-NONACRERN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PRIOR-NONACRERN NOT NUMERIC 
              MOVE "PD-PRIOR-NONACRERN" TO D-FIELD
              MOVE OPD-PRIOR-NONACRERN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-ERN-IB-PDTH NOT NUMERIC OR
               (OPD-ERN-IB-PDTH = 202020)
              MOVE "PD-ERN-IB-PDTH" TO D-FIELD
              MOVE OPD-ERN-IB-PDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-ERN-IB-PDTH NOT = 0
                 MOVE OPD-ERN-IB-PDTH TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-ERN-IB-PDTH" TO D-FIELD
                    MOVE OPD-ERN-IB-PDTH  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-ERN-IB-PRINBAL NOT NUMERIC 
              MOVE "PD-ERN-IB-PRINBAL" TO D-FIELD
              MOVE OPD-ERN-IB-PRINBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-ERN-IB-INTOWE NOT NUMERIC 
              MOVE "PD-ERN-IB-INTOWE" TO D-FIELD
              MOVE OPD-ERN-IB-INTOWE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-TOTERN NOT NUMERIC 
              MOVE "PD-TOTERN" TO D-FIELD
              MOVE OPD-TOTERN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-TOTOTH NOT NUMERIC 
              MOVE "PD-TOTOTH" TO D-FIELD
              MOVE OPD-TOTOTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-TOTINT NOT NUMERIC 
              MOVE "PD-TOTINT" TO D-FIELD
              MOVE OPD-TOTINT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-TOTLCHG NOT NUMERIC 
              MOVE "PD-TOTLCHG" TO D-FIELD
              MOVE OPD-TOTLCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-TOTDEF NOT NUMERIC 
              MOVE "PD-TOTDEF" TO D-FIELD
              MOVE OPD-TOTDEF  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-TOTSERACCT NOT NUMERIC 
              MOVE "PD-TOTSERACCT" TO D-FIELD
              MOVE OPD-TOTSERACCT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-LCWAIVERS NOT NUMERIC 
              MOVE "PD-LCWAIVERS" TO D-FIELD
              MOVE OPD-LCWAIVERS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-INTWAIVERS NOT NUMERIC 
              MOVE "PD-INTWAIVERS" TO D-FIELD
              MOVE OPD-INTWAIVERS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-TOTPAYMNTD NOT NUMERIC 
              MOVE "PD-TOTPAYMNTD" TO D-FIELD
              MOVE OPD-TOTPAYMNTD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-TOTEXCPAYMNTD NOT NUMERIC 
              MOVE "PD-TOTEXCPAYMNTD" TO D-FIELD
              MOVE OPD-TOTEXCPAYMNTD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-TOTNODEF NOT NUMERIC 
              MOVE "PD-TOTNODEF" TO D-FIELD
              MOVE OPD-TOTNODEF  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-YTDNODEF NOT NUMERIC 
              MOVE "PD-YTDNODEF" TO D-FIELD
              MOVE OPD-YTDNODEF  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-TOTNOALDEL NOT NUMERIC 
              MOVE "PD-TOTNOALDEL" TO D-FIELD
              MOVE OPD-TOTNOALDEL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-YTDINTPD NOT NUMERIC 
              MOVE "PD-YTDINTPD" TO D-FIELD
              MOVE OPD-YTDINTPD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-LYRINTPD NOT NUMERIC 
              MOVE "PD-LYRINTPD" TO D-FIELD
              MOVE OPD-LYRINTPD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-TOTINTPD NOT NUMERIC 
              MOVE "PD-TOTINTPD" TO D-FIELD
              MOVE OPD-TOTINTPD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-RECASTINT NOT NUMERIC 
              MOVE "PD-RECASTINT" TO D-FIELD
              MOVE OPD-RECASTINT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-RECASTPRIN NOT NUMERIC 
              MOVE "PD-RECASTPRIN" TO D-FIELD
              MOVE OPD-RECASTPRIN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-TRW-DOFD NOT NUMERIC OR
               (OPD-TRW-DOFD = 202020)
              MOVE "PD-TRW-DOFD" TO D-FIELD
              MOVE OPD-TRW-DOFD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-TRW-DOFD NOT = 0
                 MOVE OPD-TRW-DOFD TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-TRW-DOFD" TO D-FIELD
                    MOVE OPD-TRW-DOFD  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-LAST-PYPA-DATE NOT NUMERIC OR
               (OPD-LAST-PYPA-DATE = 202020)
              MOVE "PD-LAST-PYPA-DATE" TO D-FIELD
              MOVE OPD-LAST-PYPA-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-LAST-PYPA-DATE NOT = 0
                 MOVE OPD-LAST-PYPA-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-LAST-PYPA-DATE" TO D-FIELD
                    MOVE OPD-LAST-PYPA-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-PROMDAYS-PASTDUE NOT NUMERIC 
              MOVE "PD-PROMDAYS-PASTDUE" TO D-FIELD
              MOVE OPD-PROMDAYS-PASTDUE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PROMDATE NOT NUMERIC OR
               (OPD-PROMDATE = 202020)
              MOVE "PD-PROMDATE" TO D-FIELD
              MOVE OPD-PROMDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-PROMDATE NOT = 0
                 MOVE OPD-PROMDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-PROMDATE" TO D-FIELD
                    MOVE OPD-PROMDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OPD-PROMAMT NOT NUMERIC 
              MOVE "PD-PROMAMT" TO D-FIELD
              MOVE OPD-PROMAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OPD-DATE-PAID-LAST NOT NUMERIC OR
               (OPD-DATE-PAID-LAST = 202020)
              MOVE "PD-DATE-PAID-LAST" TO D-FIELD
              MOVE OPD-DATE-PAID-LAST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-DATE-PAID-LAST NOT = 0
                 MOVE OPD-DATE-PAID-LAST TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-DATE-PAID-LAST" TO D-FIELD
                    MOVE OPD-DATE-PAID-LAST  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OPD-AMT-PAID-LAST NOT NUMERIC 
              MOVE "PD-AMT-PAID-LAST" TO D-FIELD
              MOVE OPD-AMT-PAID-LAST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-LASTCFORMNO NOT NUMERIC 
              MOVE "PD-LASTCFORMNO" TO D-FIELD
              MOVE OPD-LASTCFORMNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-LASTCFORMDATE NOT NUMERIC OR
               (OPD-LASTCFORMDATE = 202020)
              MOVE "PD-LASTCFORMDATE" TO D-FIELD
              MOVE OPD-LASTCFORMDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-LASTCFORMDATE NOT = 0
                 MOVE OPD-LASTCFORMDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-LASTCFORMDATE" TO D-FIELD
                    MOVE OPD-LASTCFORMDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-DELCNT NOT NUMERIC 
              MOVE "PD-DELCNT" TO D-FIELD
              MOVE OPD-DELCNT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-DELCNT2 NOT NUMERIC 
              MOVE "PD-DELCNT2" TO D-FIELD
              MOVE OPD-DELCNT2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OPD-CREDITLIM NOT NUMERIC 
              MOVE "PD-CREDITLIM" TO D-FIELD
              MOVE OPD-CREDITLIM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-CRSCORE NOT NUMERIC 
              MOVE "PD-CRSCORE" TO D-FIELD
              MOVE OPD-CRSCORE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-LASTSFORMNO NOT NUMERIC 
              MOVE "PD-LASTSFORMNO" TO D-FIELD
              MOVE OPD-LASTSFORMNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-LASTSFORMDATE NOT NUMERIC OR
               (OPD-LASTSFORMDATE = 202020)
              MOVE "PD-LASTSFORMDATE" TO D-FIELD
              MOVE OPD-LASTSFORMDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-LASTSFORMDATE NOT = 0
                 MOVE OPD-LASTSFORMDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-LASTSFORMDATE" TO D-FIELD
                    MOVE OPD-LASTSFORMDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OPD-SOLCNT NOT NUMERIC 
              MOVE "PD-SOLCNT" TO D-FIELD
              MOVE OPD-SOLCNT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PROMDATE2 NOT NUMERIC OR
               (OPD-PROMDATE2 = 202020)
              MOVE "PD-PROMDATE2" TO D-FIELD
              MOVE OPD-PROMDATE2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-PROMDATE2 NOT = 0
                 MOVE OPD-PROMDATE2 TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-PROMDATE2" TO D-FIELD
                    MOVE OPD-PROMDATE2  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OPD-OLDACCTNO NOT NUMERIC 
              MOVE "PD-OLDACCTNO" TO D-FIELD
              MOVE OPD-OLDACCTNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PRLNNO NOT NUMERIC 
              MOVE "PD-PRLNNO" TO D-FIELD
              MOVE OPD-PRLNNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PRBAL NOT NUMERIC 
              MOVE "PD-PRBAL" TO D-FIELD
              MOVE OPD-PRBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OPD-PRINTDUE NOT NUMERIC 
              MOVE "PD-PRINTDUE" TO D-FIELD
              MOVE OPD-PRINTDUE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PRFINCHGRB NOT NUMERIC 
              MOVE "PD-PRFINCHGRB" TO D-FIELD
              MOVE OPD-PRFINCHGRB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PRINSRB NOT NUMERIC 
              MOVE "PD-PRINSRB" TO D-FIELD
              MOVE OPD-PRINSRB  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PRPODATE NOT NUMERIC OR
               (OPD-PRPODATE = 202020)
              MOVE "PD-PRPODATE" TO D-FIELD
              MOVE OPD-PRPODATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-PRPODATE NOT = 0
                 MOVE OPD-PRPODATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-PRPODATE" TO D-FIELD
                    MOVE OPD-PRPODATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OPD-PRCONPROFILE NOT NUMERIC 
              MOVE "PD-PRCONPROFILE" TO D-FIELD
              MOVE OPD-PRCONPROFILE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PRDELINQAMT NOT NUMERIC 
              MOVE "PD-PRDELINQAMT" TO D-FIELD
              MOVE OPD-PRDELINQAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-SUM-ORIGPURAMT NOT NUMERIC 
              MOVE "PD-SUM-ORIGPURAMT" TO D-FIELD
              MOVE OPD-SUM-ORIGPURAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-SUM-LNAMT NOT NUMERIC 
              MOVE "PD-SUM-LNAMT" TO D-FIELD
              MOVE OPD-SUM-LNAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-SUM-FINCHG NOT NUMERIC 
              MOVE "PD-SUM-FINCHG" TO D-FIELD
              MOVE OPD-SUM-FINCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-SUM-INTCHG NOT NUMERIC 
              MOVE "PD-SUM-INTCHG" TO D-FIELD
              MOVE OPD-SUM-INTCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-SUM-ORGTERM NOT NUMERIC 
              MOVE "PD-SUM-ORGTERM" TO D-FIELD
              MOVE OPD-SUM-ORGTERM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-SUM-REGPYAMT NOT NUMERIC 
              MOVE "PD-SUM-REGPYAMT" TO D-FIELD
              MOVE OPD-SUM-REGPYAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-SUM-EXTCHG NOT NUMERIC 
              MOVE "PD-SUM-EXTCHG" TO D-FIELD
              MOVE OPD-SUM-EXTCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-SUM-SERCHG NOT NUMERIC 
              MOVE "PD-SUM-SERCHG" TO D-FIELD
              MOVE OPD-SUM-SERCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-SUM-MAINTFEE NOT NUMERIC 
              MOVE "PD-SUM-MAINTFEE" TO D-FIELD
              MOVE OPD-SUM-MAINTFEE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-UNITPER-FREQ NOT NUMERIC 
              MOVE "PD-UNITPER-FREQ" TO D-FIELD
              MOVE OPD-UNITPER-FREQ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-DAYS-IN-UNITPER NOT NUMERIC 
              MOVE "PD-DAYS-IN-UNITPER" TO D-FIELD
              MOVE OPD-DAYS-IN-UNITPER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-UNITPER-PER-YEAR NOT NUMERIC 
              MOVE "PD-UNITPER-PER-YEAR" TO D-FIELD
              MOVE OPD-UNITPER-PER-YEAR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-O3INS-DLPROC NOT NUMERIC 
              MOVE "PD-O3INS-DLPROC" TO D-FIELD
              MOVE OPD-O3INS-DLPROC  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-INSPREM-DLADJ NOT NUMERIC 
              MOVE "PD-INSPREM-DLADJ" TO D-FIELD
              MOVE OPD-INSPREM-DLADJ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PRLNAMT NOT NUMERIC 
              MOVE "PD-PRLNAMT" TO D-FIELD
              MOVE OPD-PRLNAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PRPDUE-DAYS NOT NUMERIC 
              MOVE "PD-PRPDUE-DAYS" TO D-FIELD
              MOVE OPD-PRPDUE-DAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PLAMT NOT NUMERIC 
              MOVE "PD-PLAMT" TO D-FIELD
              MOVE OPD-PLAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-MTD-PRIN NOT NUMERIC 
              MOVE "PD-MTD-PRIN" TO D-FIELD
              MOVE OPD-MTD-PRIN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-MTD-LC NOT NUMERIC 
              MOVE "PD-MTD-LC" TO D-FIELD
              MOVE OPD-MTD-LC  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-MTD-INT NOT NUMERIC 
              MOVE "PD-MTD-INT" TO D-FIELD
              MOVE OPD-MTD-INT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-MTD-OT NOT NUMERIC 
              MOVE "PD-MTD-OT" TO D-FIELD
              MOVE OPD-MTD-OT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-MTD-EX NOT NUMERIC 
              MOVE "PD-MTD-EX" TO D-FIELD
              MOVE OPD-MTD-EX  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-BNKRPT-CHAPTER NOT NUMERIC 
              MOVE "PD-BNKRPT-CHAPTER" TO D-FIELD
              MOVE OPD-BNKRPT-CHAPTER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-APPNO NOT NUMERIC 
              MOVE "PD-APPNO" TO D-FIELD
              MOVE OPD-APPNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OPD-GIFTNET-OUR-PO NOT NUMERIC 
              MOVE "PD-GIFTNET-OUR-PO" TO D-FIELD
              MOVE OPD-GIFTNET-OUR-PO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-NOODDPY NOT NUMERIC 
              MOVE "PD-NOODDPY" TO D-FIELD
              MOVE OPD-NOODDPY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
    
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               IF OPD-CONPROFILE(SUB) NOT NUMERIC 
                  MOVE "PD-CONPROFILE(SUB)" TO D-FIELD
                  MOVE OPD-CONPROFILE(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               IF OPD-RECPROFILE(SUB) NOT NUMERIC 
                  MOVE "PD-RECPROFILE(SUB)" TO D-FIELD
                  MOVE OPD-RECPROFILE(SUB)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
           END-PERFORM.
           IF OPD-FOLLOWUP-DATE NOT NUMERIC OR
               (OPD-FOLLOWUP-DATE = 202020)
              MOVE "PD-FOLLOWUP-DATE" TO D-FIELD
              MOVE OPD-FOLLOWUP-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-FOLLOWUP-DATE NOT = 0
                 MOVE OPD-FOLLOWUP-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-FOLLOWUP-DATE" TO D-FIELD
                    MOVE OPD-FOLLOWUP-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OPD-PRDLDISC-UNEARN NOT NUMERIC 
              MOVE "PD-PRDLDISC-UNEARN" TO D-FIELD
              MOVE OPD-PRDLDISC-UNEARN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-YTDNOALDEL NOT NUMERIC 
              MOVE "PD-YTDNOALDEL" TO D-FIELD
              MOVE OPD-YTDNOALDEL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OPD-BNKRPT-DISMISSAL NOT NUMERIC OR
               (OPD-BNKRPT-DISMISSAL = 202020)
              MOVE "PD-BNKRPT-DISMISSAL" TO D-FIELD
              MOVE OPD-BNKRPT-DISMISSAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-BNKRPT-DISMISSAL NOT = 0
                 MOVE OPD-BNKRPT-DISMISSAL TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-BNKRPT-DISMISSAL" TO D-FIELD
                    MOVE OPD-BNKRPT-DISMISSAL  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OPD-LAST-ACCRUAL-DATE NOT NUMERIC OR
               (OPD-LAST-ACCRUAL-DATE = 202020)
              MOVE "PD-LAST-ACCRUAL-DATE" TO D-FIELD
              MOVE OPD-LAST-ACCRUAL-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OPD-LAST-ACCRUAL-DATE NOT = 0
                 MOVE OPD-LAST-ACCRUAL-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PD-LAST-ACCRUAL-DATE" TO D-FIELD
                    MOVE OPD-LAST-ACCRUAL-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OPD-TOTNODEFACT NOT NUMERIC 
              MOVE "PD-TOTNODEFACT" TO D-FIELD
              MOVE OPD-TOTNODEFACT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-YTDNODEFACT NOT NUMERIC 
              MOVE "PD-YTDNODEFACT" TO D-FIELD
              MOVE OPD-YTDNODEFACT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-TOTNOLCHG NOT NUMERIC 
              MOVE "PD-TOTNOLCHG" TO D-FIELD
              MOVE OPD-TOTNOLCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-PRIOR-STMT-COUNTER NOT NUMERIC 
              MOVE "PD-PRIOR-STMT-COUNTER" TO D-FIELD
              MOVE OPD-PRIOR-STMT-COUNTER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-CONRATE NOT NUMERIC 
              MOVE "PD-CONRATE" TO D-FIELD
              MOVE OPD-CONRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-EXTRA-PRIN-LFP NOT NUMERIC 
              MOVE "PD-EXTRA-PRIN-LFP" TO D-FIELD
              MOVE OPD-EXTRA-PRIN-LFP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-REMAIN-INT-SINCE-LFP NOT NUMERIC 
              MOVE "PD-REMAIN-INT-SINCE-LFP" TO D-FIELD
              MOVE OPD-REMAIN-INT-SINCE-LFP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-REMAIN-PRIN-SINCE-LFP NOT NUMERIC 
              MOVE "PD-REMAIN-PRIN-SINCE-LFP" TO D-FIELD
              MOVE OPD-REMAIN-PRIN-SINCE-LFP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-DLCOMM-O3-CMSI NOT NUMERIC 
              MOVE "PD-DLCOMM-O3-CMSI" TO D-FIELD
              MOVE OPD-DLCOMM-O3-CMSI  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OPD-DLCOMM-PP-CMSI NOT NUMERIC 
              MOVE "PD-DLCOMM-PP-CMSI" TO D-FIELD
              MOVE OPD-DLCOMM-PP-CMSI  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.


       VERIFY-PD-REC-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - G L O B A L - C P
      *
      ******************************************************************
       PROCESS-FILE-GLOBAL-CP SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-GLOBAL-CP = "N" )
              GO TO PROCESS-FILE-GLO-CP-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NOTE: CP-PATH-OVERRIDE NEEDS TO BE "G" NOT "Y" HERE!!!
      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "G"                        TO CP-PATH-OVERRIDE.
           MOVE "CP"                       TO UP1534-CURRENT-FILE.
           MOVE "SP/CPFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OCP-PATH.

           MOVE SPACES                     TO OCP1-KEY.
           PERFORM OPEN-OCP1-FILE.
           PERFORM START-OCP1-FILE.


       PROCESS-FILE-GLO-CP-NEXT.
           PERFORM READ-OCP1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-GLO-CP-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OCP-TOTAL-ACCOUNTS NOT NUMERIC 
              MOVE "CP-TOTAL-ACCOUNTS" TO D-FIELD
              MOVE OCP-TOTAL-ACCOUNTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCP-COLLECTOR-COUNT NOT NUMERIC 
              MOVE "CP-COLLECTOR-COUNT" TO D-FIELD
              MOVE OCP-COLLECTOR-COUNT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCP-LAST-SUBSCRIPT NOT NUMERIC 
              MOVE "CP-LAST-SUBSCRIPT" TO D-FIELD
              MOVE OCP-LAST-SUBSCRIPT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-GLO-CP-NEXT.

       PROCESS-FILE-GLO-CP-END.

           PERFORM CLOSE-OCP1-FILE.

       PROCESS-FILE-GLO-CP-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - E A
      *
      ******************************************************************
       PROCESS-FILE-EA SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-EA = "N" )
              GO TO PROCESS-FILE-EA-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO EA-PATH-OVERRIDE.
           MOVE "EA"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/EAFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OEA-PATH.

           MOVE SPACES                     TO OEA1-KEY.
           MOVE 0                          TO OEA-BRANCH
                                              OEA-CLASS
                                              OEA-SEQNO.
           PERFORM OPEN-OEA1-FILE.
           PERFORM START-OEA1-FILE.


       PROCESS-FILE-EA-NEXT.
           PERFORM READ-OEA1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-EA-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

       
           IF OEA-BRANCH NOT NUMERIC 
              MOVE "EA-BRANCH" TO D-FIELD
              MOVE OEA-BRANCH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-CLASS NOT NUMERIC 
              MOVE "EA-CLASS" TO D-FIELD
              MOVE OEA-CLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-SEQNO NOT NUMERIC 
              MOVE "EA-SEQNO" TO D-FIELD
              MOVE OEA-SEQNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-PASTDUE-DAY1 NOT NUMERIC 
              MOVE "EA-PASTDUE-DAY1" TO D-FIELD
              MOVE OEA-PASTDUE-DAY1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-PASTDUE-DAY2 NOT NUMERIC 
              MOVE "EA-PASTDUE-DAY2" TO D-FIELD
              MOVE OEA-PASTDUE-DAY2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-PASTDUE-AMT1 NOT NUMERIC 
              MOVE "EA-PASTDUE-AMT1" TO D-FIELD
              MOVE OEA-PASTDUE-AMT1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-PASTDUE-AMT2 NOT NUMERIC 
              MOVE "EA-PASTDUE-AMT2" TO D-FIELD
              MOVE OEA-PASTDUE-AMT2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-ACCT-BAL1 NOT NUMERIC 
              MOVE "EA-ACCT-BAL1" TO D-FIELD
              MOVE OEA-ACCT-BAL1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-ACCT-BAL2 NOT NUMERIC 
              MOVE "EA-ACCT-BAL2" TO D-FIELD
              MOVE OEA-ACCT-BAL2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-LSTFM-DATE1 NOT NUMERIC OR
               (OEA-LSTFM-DATE1 = 202020)
              MOVE "EA-LSTFM-DATE1" TO D-FIELD
              MOVE OEA-LSTFM-DATE1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OEA-LSTFM-DATE1 NOT = 0
                 MOVE OEA-LSTFM-DATE1 TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "EA-LSTFM-DATE1" TO D-FIELD
                    MOVE OEA-LSTFM-DATE1  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OEA-LSTFM-DATE2 NOT NUMERIC OR
               (OEA-LSTFM-DATE2 = 202020)
              MOVE "EA-LSTFM-DATE2" TO D-FIELD
              MOVE OEA-LSTFM-DATE2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OEA-LSTFM-DATE2 NOT = 0
                 MOVE OEA-LSTFM-DATE2 TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "EA-LSTFM-DATE2" TO D-FIELD
                    MOVE OEA-LSTFM-DATE2  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OEA-LSTFM-NO1 NOT NUMERIC 
              MOVE "EA-LSTFM-NO1" TO D-FIELD
              MOVE OEA-LSTFM-NO1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-LSTFM-NO2 NOT NUMERIC 
              MOVE "EA-LSTFM-NO2" TO D-FIELD
              MOVE OEA-LSTFM-NO2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OEA-PASTPROM-DAY1 NOT NUMERIC 
              MOVE "EA-PASTPROM-DAY1" TO D-FIELD
              MOVE OEA-PASTPROM-DAY1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-PASTPROM-DAY2 NOT NUMERIC 
              MOVE "EA-PASTPROM-DAY2" TO D-FIELD
              MOVE OEA-PASTPROM-DAY2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-RECENCY-DAY1 NOT NUMERIC 
              MOVE "EA-RECENCY-DAY1" TO D-FIELD
              MOVE OEA-RECENCY-DAY1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-RECENCY-DAY2 NOT NUMERIC 
              MOVE "EA-RECENCY-DAY2" TO D-FIELD
              MOVE OEA-RECENCY-DAY2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-DLNO1 NOT NUMERIC 
              MOVE "EA-DLNO1" TO D-FIELD
              MOVE OEA-DLNO1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-DLNO2 NOT NUMERIC 
              MOVE "EA-DLNO2" TO D-FIELD
              MOVE OEA-DLNO2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-CONTRACTUAL-DAY1 NOT NUMERIC 
              MOVE "EA-CONTRACTUAL-DAY1" TO D-FIELD
              MOVE OEA-CONTRACTUAL-DAY1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-CONTRACTUAL-DAY2 NOT NUMERIC 
              MOVE "EA-CONTRACTUAL-DAY2" TO D-FIELD
              MOVE OEA-CONTRACTUAL-DAY2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
 
           IF OEA-SETFM-NO NOT NUMERIC 
              MOVE "EA-SETFM-NO" TO D-FIELD
              MOVE OEA-SETFM-NO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-INC-PROMISED NOT NUMERIC 
              MOVE "EA-INC-PROMISED" TO D-FIELD
              MOVE OEA-INC-PROMISED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OEA-SETFM(SUB) NOT NUMERIC 
                 MOVE "EA-SETFM(SUB)" TO D-FIELD
                 MOVE OEA-SETFM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OEA-PROM-BREAK1 NOT NUMERIC 
              MOVE "EA-PROM-BREAK1" TO D-FIELD
              MOVE OEA-PROM-BREAK1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-PROM-BREAK2 NOT NUMERIC 
              MOVE "EA-PROM-BREAK2" TO D-FIELD
              MOVE OEA-PROM-BREAK2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-PL-BEG-DATE NOT NUMERIC OR
               (OEA-PL-BEG-DATE = 202020)
              MOVE "EA-PL-BEG-DATE" TO D-FIELD
              MOVE OEA-PL-BEG-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OEA-PL-BEG-DATE NOT = 0
                 MOVE OEA-PL-BEG-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "PL-BEG-DATE" TO D-FIELD
                    MOVE OEA-PL-BEG-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OEA-PL-END-DATE NOT NUMERIC OR
               (OEA-PL-END-DATE = 202020)
              MOVE "EA-PL-END-DATE" TO D-FIELD
              MOVE OEA-PL-END-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OEA-PL-END-DATE NOT = 0
                 MOVE OEA-PL-END-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "EA-PL-END-DATE" TO D-FIELD
                    MOVE OEA-PL-END-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OEA-LNDATE-DAYS1 NOT NUMERIC 
              MOVE "EA-LNDATE-DAYS1" TO D-FIELD
              MOVE OEA-LNDATE-DAYS1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-LNDATE-DAYS2 NOT NUMERIC 
              MOVE "EA-LNDATE-DAYS2" TO D-FIELD
              MOVE OEA-LNDATE-DAYS2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-MATURE-DAYS1 NOT NUMERIC 
              MOVE "EA-MATURE-DAYS1" TO D-FIELD
              MOVE OEA-MATURE-DAYS1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-MATURE-DAYS2 NOT NUMERIC 
              MOVE "EA-MATURE-DAYS2" TO D-FIELD
              MOVE OEA-MATURE-DAYS2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-DAY-OF-MO-DUE1 NOT NUMERIC 
              MOVE "EA-DAY-OF-MO-DUE1" TO D-FIELD
              MOVE OEA-DAY-OF-MO-DUE1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OEA-DAY-OF-MO-DUE2 NOT NUMERIC 
              MOVE "EA-DAY-OF-MO-DUE2" TO D-FIELD
              MOVE OEA-DAY-OF-MO-DUE2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-EA-NEXT.

       PROCESS-FILE-EA-END.

           PERFORM CLOSE-OEA1-FILE.

       PROCESS-FILE-EA-EXIT.
           EXIT.


      ******************************************************************
      *
      *    P R O C E S S - F I L E - F S
      *
      ******************************************************************
      *PROCESS-FILE-FS SECTION.

      *    MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

      *    IF ( FILE-OPTION-FS = "N" )
      *       GO TO PROCESS-FILE-FS-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
      *    MOVE "Y"                        TO FS-PATH-OVERRIDE.
      *    MOVE "FS"                       TO UP1534-CURRENT-FILE.
      *    MOVE "GL/FSFILE"                TO UP1534-FILE-INPUT.
      *    MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
      *    MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

      *    PERFORM UP1534-VPI-START.
      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
      *    MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
      *    PERFORM UP1534-LOAD-FILE-PATH.
      *    MOVE UP1534-FILE-PATH           TO OFS-PATH.

      *    MOVE SPACES                     TO OFS1-KEY.
      *    MOVE ZEROES                     TO OFS-REPORT
      *                                       OFS-LINENO.

      *    PERFORM OPEN-OFS1-FILE.
      *    PERFORM START-OFS1-FILE.

      *PROCESS-FILE-FS-NEXT.
      *    PERFORM READ-OFS1-FILE-NEXT.
      *    IF ( IO-BAD )
      *       GO TO PROCESS-FILE-FS-END.

      *    ADD  01  TO UP1534-RECORD-COUNTER-READ.
      *    MOVE "Y" TO RECORDS-FOUND.
      *    PERFORM UP1534-VPI-DISPLAY.

      *    IF OFS-REPORT NOT NUMERIC 
      *       MOVE "FS-REPORT" TO D-FIELD
      *       MOVE OFS-REPORT  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OFS-LINENO NOT NUMERIC 
      *       MOVE "FS-LINENO" TO D-FIELD
      *       MOVE OFS-LINENO  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OFS-LEVEL NOT NUMERIC 
      *       MOVE "FS-LEVEL" TO D-FIELD
      *       MOVE OFS-LEVEL  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OFS-CONTROL NOT NUMERIC 
      *       MOVE "FS-CONTROL" TO D-FIELD
      *       MOVE OFS-CONTROL  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OFS-BRANCH NOT NUMERIC 
      *       MOVE "FS-BRANCH" TO D-FIELD
      *       MOVE OFS-BRANCH  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OFS-ACCTNO NOT NUMERIC 
      *       MOVE "FS-ACCTNO" TO D-FIELD
      *       MOVE OFS-ACCTNO  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OFS-CMONTH NOT NUMERIC 
      *       MOVE "FS-CMONTH" TO D-FIELD
      *       MOVE OFS-CMONTH  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OFS-LYMONTH NOT NUMERIC 
      *       MOVE "FS-LYMONTH" TO D-FIELD
      *       MOVE OFS-LYMONTH  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OFS-CYTD NOT NUMERIC 
      *       MOVE "FS-CYTD" TO D-FIELD
      *       MOVE OFS-CYTD  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OFS-LYTD NOT NUMERIC 
      *       MOVE "FS-LYTD" TO D-FIELD
      *       MOVE OFS-LYTD  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OFS-CMBUD NOT NUMERIC 
      *       MOVE "FS-CMBUD" TO D-FIELD
      *       MOVE OFS-CMBUD  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OFS-YTDBUD NOT NUMERIC 
      *       MOVE "FS-YTDBUD" TO D-FIELD
      *       MOVE OFS-YTDBUD  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OFS-ANBUD NOT NUMERIC 
      *       MOVE "FS-ANBUD" TO D-FIELD
      *       MOVE OFS-ANBUD  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OFS-REFNO NOT NUMERIC 
      *       MOVE "FS-REFNO" TO D-FIELD
      *       MOVE OFS-REFNO  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OFS-PERIOD NOT NUMERIC 
      *       MOVE "FS-PERIOD" TO D-FIELD
      *       MOVE OFS-PERIOD  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OFS-BR NOT NUMERIC 
      *       MOVE "FS-BR" TO D-FIELD
      *       MOVE OFS-BR  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.
      *    IF OFS-BR2 NOT NUMERIC 
      *       MOVE "FS-BR2" TO D-FIELD
      *       MOVE OFS-BR2  TO D-VALUE
      *       PERFORM WRITE-DETAIL-LINE.

      *    GO TO PROCESS-FILE-FS-NEXT.

      *PROCESS-FILE-FS-END.

      *    PERFORM CLOSE-OFS1-FILE.

      *PROCESS-FILE-FS-EXIT.
      *    EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - G I
      *
      ******************************************************************
       PROCESS-FILE-GI SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-GI = "N" )
              GO TO PROCESS-FILE-GI-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO GI-PATH-OVERRIDE.
           MOVE "GI"                       TO UP1534-CURRENT-FILE.
           MOVE "GL/GIFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OGI-PATH.

           MOVE SPACES                     TO OGI1-KEY.
           MOVE 0                          TO OGI-BRANCH
                                              OGI-CLASS.

           PERFORM OPEN-OGI1-FILE.
           PERFORM START-OGI1-FILE.

      *=================================================================
       PROCESS-FILE-GI-NEXT.

           PERFORM READ-OGI1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-GI-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.


        
           IF OGI-BRANCH NOT NUMERIC 
              MOVE "GI-BRANCH" TO D-FIELD
              MOVE OGI-BRANCH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGI-CLASS NOT NUMERIC 
              MOVE "GI-CLASS" TO D-FIELD
              MOVE OGI-CLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 123
              IF OGI-ACCT(SUB) NOT NUMERIC 
                 STRING "GI-ACCT",SUB DELIMITED BY " " INTO D-FIELD
                 MOVE OGI-ACCT(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OGI-LN-APPROVAL-LIMIT NOT NUMERIC 
              MOVE "GI-LN-APPROVAL-LIMIT" TO D-FIELD
              MOVE OGI-LN-APPROVAL-LIMIT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-GI-NEXT.

       PROCESS-FILE-GI-END.

           PERFORM CLOSE-OGI1-FILE.

       PROCESS-FILE-GI-EXIT.
           EXIT.


      ******************************************************************
      *
      *    P R O C E S S - F I L E - G L
      *
      ******************************************************************
       PROCESS-FILE-GL SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-GL = "N" )
              GO TO PROCESS-FILE-GL-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO GL-PATH-OVERRIDE.
           MOVE "GL"                       TO UP1534-CURRENT-FILE.
           MOVE "GL/GLFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OGL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - -{ OPEN & START FILES }
           MOVE SPACES                     TO OGL1-KEY.
           MOVE ZEROES                     TO OGL-BRANCH
                                              OGL-ACCOUNT.

           PERFORM OPEN-OGL1-FILE.
           PERFORM START-OGL1-FILE.

       PROCESS-FILE-GL-NEXT.
           PERFORM READ-OGL1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-GL-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OGL-BRANCH NOT NUMERIC 
              MOVE "GL-BRANCH" TO D-FIELD
              MOVE OGL-BRANCH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGL-ACCOUNT NOT NUMERIC 
              MOVE "GL-ACCOUNT" TO D-FIELD
              MOVE OGL-ACCOUNT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGL-CONSOL NOT NUMERIC 
              MOVE "GL-CONSOL" TO D-FIELD
              MOVE OGL-CONSOL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGL-OPENBAL NOT NUMERIC 
              MOVE "GL-OPENBAL" TO D-FIELD
              MOVE OGL-OPENBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
              IF OGL-CYBAL(SUB) NOT NUMERIC 
                 STRING "GL-CYBAL",SUB DELIMITED BY " " 
                                           INTO D-FIELD
                 MOVE OGL-CYBAL(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OGL-LYBAL(SUB) NOT NUMERIC 
                 STRING "GL-LYBAL",SUB DELIMITED BY " " 
                                           INTO D-FIELD
                 MOVE OGL-LYBAL(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OGL-CYBUD(SUB) NOT NUMERIC 
                 STRING "GL-CYBUD",SUB DELIMITED BY " " 
                                           INTO D-FIELD
                 MOVE OGL-CYBUD(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OGL-LYBUD(SUB) NOT NUMERIC 
                 STRING "GL-LYBUD",SUB DELIMITED BY " " 
                                           INTO D-FIELD
                 MOVE OGL-LYBUD(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OGL-NYBUD(SUB) NOT NUMERIC 
                 STRING "GL-NYBUD",SUB DELIMITED BY " " 
                                           INTO D-FIELD
                 MOVE OGL-NYBUD(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OGL-NUMBER1 NOT NUMERIC 
              MOVE "GL-NUMBER1" TO D-FIELD
              MOVE OGL-NUMBER1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGL-NUMBER2 NOT NUMERIC 
              MOVE "GL-NUMBER2" TO D-FIELD
              MOVE OGL-NUMBER2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGL-LYOPENBAL NOT NUMERIC 
              MOVE "GL-LYOPENBAL" TO D-FIELD
              MOVE OGL-LYOPENBAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-GL-NEXT.

       PROCESS-FILE-GL-END.

           PERFORM CLOSE-OGL1-FILE.

       PROCESS-FILE-GL-EXIT.
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

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OGT-PATH.

           MOVE SPACES                     TO OGT1-KEY.
           MOVE 0                          TO OGT-SOURCE
                                              OGT-DATE2
                                              OGT-BRANCH
                                              OGT-ACCTNO
                                              OGT-DATE3
                                              OGT-LINE.

           PERFORM OPEN-OGT1-FILE.
           PERFORM START-OGT1-FILE.

       PROCESS-FILE-GT-NEXT.
           PERFORM READ-OGT1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-GT-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

      * GT DATES ARE STORED YYMMDD
           IF OGT-DATE1 NOT NUMERIC OR
               (OGT-DATE1 = 202020)
              MOVE "GT-DATE1" TO D-FIELD
              MOVE OGT-DATE1  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OGT-DATE1 NOT = 0
                 MOVE OGT-DATE1 TO DATE-YYMMDD
                 PERFORM CONVERT-YYMMDD-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "GT-DATE1" TO D-FIELD
                    MOVE OGT-DATE1  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OGT-SOURCE NOT NUMERIC 
              MOVE "GT-SOURCE" TO D-FIELD
              MOVE OGT-SOURCE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGT-DATE2 NOT NUMERIC OR
               (OGT-DATE2 = 202020)
              MOVE "GT-DATE2" TO D-FIELD
              MOVE OGT-DATE2  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OGT-DATE2 NOT = 0
                 MOVE OGT-DATE2 TO DATE-YYMMDD
                 PERFORM CONVERT-YYMMDD-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "GT-DATE2" TO D-FIELD
                    MOVE OGT-DATE2  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OGT-BRANCH NOT NUMERIC 
              MOVE "GT-BRANCH" TO D-FIELD
              MOVE OGT-BRANCH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGT-ACCTNO NOT NUMERIC 
              MOVE "GT-ACCTNO" TO D-FIELD
              MOVE OGT-ACCTNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGT-DATE3 NOT NUMERIC OR
               (OGT-DATE3 = 202020)
              MOVE "GT-DATE3" TO D-FIELD
              MOVE OGT-DATE3  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OGT-DATE3 NOT = 0
                 MOVE OGT-DATE3 TO DATE-YYMMDD
                 PERFORM CONVERT-YYMMDD-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "GT-DATE3" TO D-FIELD
                    MOVE OGT-DATE3  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OGT-LINE NOT NUMERIC 
              MOVE "GT-LINE" TO D-FIELD
              MOVE OGT-LINE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGT-AMT NOT NUMERIC 
              MOVE "GT-AMT" TO D-FIELD
              MOVE OGT-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGT-PERIOD NOT NUMERIC 
              MOVE "GT-PERIOD" TO D-FIELD
              MOVE OGT-PERIOD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OGT-POST-DATE NOT NUMERIC OR
               (OGT-POST-DATE = 202020)
              MOVE "GT-POST-DATE" TO D-FIELD
              MOVE OGT-POST-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OGT-POST-DATE NOT = 0
                 MOVE OGT-POST-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "GT-POST-DATE" TO D-FIELD
                    MOVE OGT-POST-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OGT-POST-TIME NOT NUMERIC 
              MOVE "GT-POST-TIME" TO D-FIELD
              MOVE OGT-POST-TIME  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-GT-NEXT.

       PROCESS-FILE-GT-END.

           PERFORM CLOSE-OGT1-FILE.

       PROCESS-FILE-GT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - I I
      *
      ******************************************************************
       PROCESS-FILE-II SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-II = "N" )
              GO TO PROCESS-FILE-II-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO II-PATH-OVERRIDE.
           MOVE "II"                       TO UP1534-CURRENT-FILE.
           MOVE "GB/IIFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OII-PATH.

      *- - - - - - - - - - - - - - - - - - - - - -{ OPEN & START FILES }
           MOVE 0       TO OII-KEY.
           PERFORM OPEN-OII-FILE.
           PERFORM START-OII-FILE.

       PROCESS-FILE-II-NEXT.
           PERFORM READ-OII-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-II-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OII-EMPLOYER-ID NOT NUMERIC 
              MOVE "II-EMPLOYER-ID" TO D-FIELD
              MOVE OII-EMPLOYER-ID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OII-PAYER-PHONE NOT NUMERIC 
              MOVE "II-PAYER-PHONE" TO D-FIELD
              MOVE OII-PAYER-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OII-TAX-YEAR NOT NUMERIC 
              MOVE "II-TAX-YEAR" TO D-FIELD
              MOVE OII-TAX-YEAR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-II-NEXT.

       PROCESS-FILE-II-END.

           PERFORM CLOSE-OII-FILE.

       PROCESS-FILE-II-EXIT.
           EXIT.


      ******************************************************************
      *
      *    P R O C E S S - F I L E - I R
      *
      ******************************************************************
       PROCESS-FILE-IR SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-IR = "N" )
              GO TO PROCESS-FILE-IR-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO IR-PATH-OVERRIDE.
           MOVE "IR"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/IRFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OIR-PATH.

           MOVE 0       TO OIR-KEY.
           PERFORM OPEN-OIR-FILE.
           PERFORM START-OIR-FILE.

       PROCESS-FILE-IR-NEXT.
           PERFORM READ-OIR-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-IR-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.
 
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 120
              IF OIR-INSRATE(SUB) NOT NUMERIC 
                 STRING "IR-INSRATE", SUB DELIMITED BY "  "
                                INTO D-FIELD
                 MOVE OIR-INSRATE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           GO TO PROCESS-FILE-IR-NEXT.

       PROCESS-FILE-IR-END.

           PERFORM CLOSE-OIR-FILE.

       PROCESS-FILE-IR-EXIT.
           EXIT.


      ******************************************************************
      *
      *    P R O C E S S - F I L E - L C
      *
      ******************************************************************
       PROCESS-FILE-LC SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-LC = "N" )
              GO TO PROCESS-FILE-LC-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO LC-PATH-OVERRIDE.
           MOVE "LC"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/LCFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.
      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OLC-PATH.

           MOVE 0                     TO OLC-CODE.
           PERFORM OPEN-OLC1-FILE.
           PERFORM START-OLC1-FILE.

       PROCESS-FILE-LC-NEXT.
           PERFORM READ-OLC1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-LC-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.


           IF OLC-CODE NOT NUMERIC 
              MOVE "LC-CODE" TO D-FIELD
              MOVE OLC-CODE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLC-POOLID NOT NUMERIC 
              MOVE "LC-POOLID" TO D-FIELD
              MOVE OLC-POOLID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OLC-PROGRP-CLASS NOT NUMERIC 
              MOVE "LC-PROGRP-CLASS" TO D-FIELD
              MOVE OLC-PROGRP-CLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-LC-NEXT.

       PROCESS-FILE-LC-END.

           PERFORM CLOSE-OLC1-FILE.

       PROCESS-FILE-LC-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - P E
      *
      ******************************************************************
       PROCESS-FILE-PE SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-PE = "N" )
              GO TO PROCESS-FILE-PE-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO PE-PATH-OVERRIDE.

           MOVE "PE"                       TO UP1534-CURRENT-FILE.
           MOVE "GB/PEFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OPE-PATH.

           MOVE SPACES                     TO OPE1-KEY.
           MOVE ZEROES                     TO OPE-SEQ.
           PERFORM OPEN-OPE1-FILE.
           PERFORM START-OPE1-FILE.

       PROCESS-FILE-PE-NEXT.
           PERFORM READ-OPE1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-PE-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OPE-SEQ NOT NUMERIC 
              MOVE "PE-SEQ" TO D-FIELD
              MOVE OPE-SEQ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-PE-NEXT.

       PROCESS-FILE-PE-END.

           PERFORM CLOSE-OPE1-FILE.

       PROCESS-FILE-PE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - P W
      *
      ******************************************************************
       PROCESS-FILE-PW SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-PW = "N" )
              GO TO PROCESS-FILE-PW-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO PW-PATH-OVERRIDE.

           MOVE "PW"                       TO UP1534-CURRENT-FILE.
           MOVE "GB/PWFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OPW-PATH.

           MOVE SPACES                     TO OPW1-KEY.
           MOVE ZEROES                     TO OPW-SEQ.
           PERFORM OPEN-OPW1-FILE.
           PERFORM START-OPW1-FILE.

       PROCESS-FILE-PW-NEXT.
           PERFORM READ-OPW1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-PW-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OPW-SEQ NOT NUMERIC 
              MOVE "PW-SEQ" TO D-FIELD
              MOVE OPW-SEQ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-PW-NEXT.

       PROCESS-FILE-PW-END.

           PERFORM CLOSE-OPW1-FILE.

       PROCESS-FILE-PW-EXIT.
           EXIT.


      ******************************************************************
      *
      *    P R O C E S S - F I L E - R C
      *
      ******************************************************************
       PROCESS-FILE-RC SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-RC = "N" )
              GO TO PROCESS-FILE-RC-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO RC-PATH-OVERRIDE.
           MOVE "RC"                       TO UP1534-CURRENT-FILE.
           MOVE "GB/RCFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO ORC-PATH.

           MOVE SPACES                     TO ORC1-KEY.
           MOVE 0                          TO ORC-BRNO
                                              ORC-TRANS-DATE.
           PERFORM OPEN-ORC1-FILE.
           PERFORM START-ORC1-FILE.

       PROCESS-FILE-RC-NEXT.
           PERFORM READ-ORC1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-RC-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.


           IF ORC-BRNO NOT NUMERIC 
              MOVE "RC-BRNO" TO D-FIELD
              MOVE ORC-BRNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

      * RC-TRANS-DATE IS YYYMMDD
           IF ORC-TRANS-DATE NOT NUMERIC 
              MOVE "RC-TRANS-DATE" TO D-FIELD
              MOVE ORC-TRANS-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF ORC-TRANS-DATE NOT = 0
                 MOVE ORC-TRANS-DATE TO DATE-YYYMMDD
                 PERFORM CONVERT-YYYMMDD-TO-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "RC-TRANS-DATE" TO D-FIELD
                    MOVE ORC-TRANS-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF ORC-DEPOSIT NOT NUMERIC 
              MOVE "RC-DEPOSIT" TO D-FIELD
              MOVE ORC-DEPOSIT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ORC-OVER-SHORT NOT NUMERIC 
              MOVE "RC-OVER-SHORT" TO D-FIELD
              MOVE ORC-OVER-SHORT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ORC-CHECKS NOT NUMERIC 
              MOVE "RC-CHECKS" TO D-FIELD
              MOVE ORC-CHECKS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ORC-OPENED-DATE NOT NUMERIC OR
               (ORC-OPENED-DATE = 202020)
              MOVE "RC-OPENED-DATE" TO D-FIELD
              MOVE ORC-OPENED-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF ORC-OPENED-DATE NOT = 0
                 MOVE ORC-OPENED-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "RC-OPENED-DATE" TO D-FIELD
                    MOVE ORC-OPENED-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF ORC-OPENED-TIME NOT NUMERIC 
              MOVE "RC-OPENED-TIME" TO D-FIELD
              MOVE ORC-OPENED-TIME  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ORC-CLOSED-DATE NOT NUMERIC OR
               (ORC-CLOSED-DATE = 202020)
              MOVE "RC-CLOSED-DATE" TO D-FIELD
              MOVE ORC-CLOSED-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF ORC-CLOSED-DATE NOT = 0
                 MOVE ORC-CLOSED-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "RC-CLOSED-DATE" TO D-FIELD
                    MOVE ORC-CLOSED-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF ORC-CLOSED-TIME NOT NUMERIC 
              MOVE "RC-CLOSED-TIME" TO D-FIELD
              MOVE ORC-CLOSED-TIME  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.


           IF ORC-UPDATED-DATE NOT NUMERIC OR
               (ORC-UPDATED-DATE = 202020)
              MOVE "RC-UPDATED-DATE" TO D-FIELD
              MOVE ORC-UPDATED-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF ORC-UPDATED-DATE NOT = 0
                 MOVE ORC-UPDATED-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "RC-UPDATED-DATE" TO D-FIELD
                    MOVE ORC-UPDATED-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF ORC-UPDATED-TIME NOT NUMERIC 
              MOVE "RC-UPDATED-TIME" TO D-FIELD
              MOVE ORC-UPDATED-TIME  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ORC-EOD-BATCH-DATE NOT NUMERIC OR
               (ORC-EOD-BATCH-DATE = 202020)
              MOVE "RC-EOD-BATCH-DATE" TO D-FIELD
              MOVE ORC-EOD-BATCH-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF ORC-EOD-BATCH-DATE NOT = 0
                 MOVE ORC-EOD-BATCH-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "RC-EOD-BATCH-DATE" TO D-FIELD
                    MOVE ORC-EOD-BATCH-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF ORC-EOD-BATCH-TIME NOT NUMERIC 
              MOVE "RC-EOD-BATCH-TIME" TO D-FIELD
              MOVE ORC-EOD-BATCH-TIME  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ORC-FUNDS-DATE NOT NUMERIC OR
               (ORC-FUNDS-DATE = 202020)
              MOVE "RC-FUNDS-DATE" TO D-FIELD
              MOVE ORC-FUNDS-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF ORC-FUNDS-DATE NOT = 0
                 MOVE ORC-FUNDS-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "RC-FUNDS-DATE" TO D-FIELD
                    MOVE ORC-FUNDS-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF ORC-FUNDS-TIME NOT NUMERIC 
              MOVE "RC-FUNDS-TIME" TO D-FIELD
              MOVE ORC-FUNDS-TIME  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF ORC-EARNINGS-DATE NOT NUMERIC OR
               (ORC-EARNINGS-DATE = 202020)
              MOVE "RC-EARNINGS-DATE" TO D-FIELD
              MOVE ORC-EARNINGS-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF ORC-EARNINGS-DATE NOT = 0
                 MOVE ORC-EARNINGS-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "RC-EARNINGS-DATE" TO D-FIELD
                    MOVE ORC-EARNINGS-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF ORC-EARNINGS-TIME NOT NUMERIC 
              MOVE "RC-EARNINGS-TIME" TO D-FIELD
              MOVE ORC-EARNINGS-TIME  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ORC-EOM-UPDATED-DATE NOT NUMERIC OR
               (ORC-EOM-UPDATED-DATE = 202020)
              MOVE "RC-EOM-UPDATED-DATE" TO D-FIELD
              MOVE ORC-EOM-UPDATED-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF ORC-EOM-UPDATED-DATE NOT = 0
                 MOVE ORC-EOM-UPDATED-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "RC-EOM-UPDATED-DATE" TO D-FIELD
                    MOVE ORC-EOM-UPDATED-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF ORC-EOM-UPDATED-TIME NOT NUMERIC 
              MOVE "RC-EOM-UPDATED-TIME" TO D-FIELD
              MOVE ORC-EOM-UPDATED-TIME  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ORC-EOM2-BATCH-DATE NOT NUMERIC OR
               (ORC-EOM2-BATCH-DATE = 202020)
              MOVE "RC-EOM2-BATCH-DATE" TO D-FIELD
              MOVE ORC-EOM2-BATCH-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF ORC-EOM2-BATCH-DATE NOT = 0
                 MOVE ORC-EOM2-BATCH-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "RC-EOM2-BATCH-DATE" TO D-FIELD
                    MOVE ORC-EOM2-BATCH-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF ORC-EOM2-BATCH-TIME NOT NUMERIC 
              MOVE "RC-EOM2-BATCH-TIME" TO D-FIELD
              MOVE ORC-EOM2-BATCH-TIME  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ORC-CHECK-DATE NOT NUMERIC OR
               (ORC-CHECK-DATE = 202020)
              MOVE "RC-CHECK-DATE" TO D-FIELD
              MOVE ORC-CHECK-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF ORC-CHECK-DATE NOT = 0
                 MOVE ORC-CHECK-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "RC-CHECK-DATE" TO D-FIELD
                    MOVE ORC-CHECK-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF ORC-CHECK-TIME NOT NUMERIC 
              MOVE "RC-CHECK-TIME" TO D-FIELD
              MOVE ORC-CHECK-TIME  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.


           IF ORC-LAST-ACCRUE NOT NUMERIC 
              MOVE "RC-LAST-ACCRUE" TO D-FIELD
              MOVE ORC-LAST-ACCRUE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ORC-BANK-BAL NOT NUMERIC 
              MOVE "RC-BANK-BAL" TO D-FIELD
              MOVE ORC-BANK-BAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF ORC-CHECK-DEPOSIT NOT NUMERIC 
              MOVE "RC-CHECK-DEPOSIT" TO D-FIELD
              MOVE ORC-CHECK-DEPOSIT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-RC-NEXT.

       PROCESS-FILE-RC-END.

           PERFORM CLOSE-ORC1-FILE.

       PROCESS-FILE-RC-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - S E
      *
      ******************************************************************
       PROCESS-FILE-SE SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-SE = "N" )
              GO TO PROCESS-FILE-SE-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO SE-PATH-OVERRIDE.
           MOVE "SE"                       TO UP1534-CURRENT-FILE.
           MOVE "GB/SEFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OSE-PATH.

           MOVE SPACES                     TO OSE1-KEY.
           PERFORM OPEN-OSE1-FILE.
           PERFORM START-OSE1-FILE.

       PROCESS-FILE-SE-NEXT.
           PERFORM READ-OSE1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-SE-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.


           IF OSE-EXP-DATE NOT NUMERIC OR
               (OSE-EXP-DATE = 202020)
              MOVE "SE-EXP-DATE" TO D-FIELD
              MOVE OSE-EXP-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OSE-EXP-DATE NOT = 0
                 MOVE OSE-EXP-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "SE-EXP-DATE" TO D-FIELD
                    MOVE OSE-EXP-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-SE-NEXT.

       PROCESS-FILE-SE-END.

           PERFORM CLOSE-OSE1-FILE.

       PROCESS-FILE-SE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - S M
      *
      ******************************************************************
       PROCESS-FILE-SM SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-SM = "N" )
              GO TO PROCESS-FILE-SM-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO SM-PATH-OVERRIDE.
           MOVE "SM"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/SMFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OSM-PATH.


           MOVE SPACES                     TO OSM1-KEY.
           MOVE 0                          TO OSM-SPRCLASS
                                              OSM-SUBCLASS
                                              OSM-LAWCODE
                                              OSM-DATE
                                              OSM-SEQ.

           PERFORM OPEN-OSM1-FILE.
           PERFORM START-OSM1-FILE.

       PROCESS-FILE-SM-NEXT.
           PERFORM READ-OSM1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-SM-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OSM-SPRCLASS NOT NUMERIC 
              MOVE "SM-SPRCLASS" TO D-FIELD
              MOVE OSM-SPRCLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSM-SUBCLASS NOT NUMERIC 
              MOVE "SM-SUBCLASS" TO D-FIELD
              MOVE OSM-SUBCLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSM-LAWCODE NOT NUMERIC 
              MOVE "SM-LAWCODE" TO D-FIELD
              MOVE OSM-LAWCODE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSM-DATE NOT NUMERIC 
              MOVE "SM-DATE" TO D-FIELD
              MOVE OSM-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-SM-NEXT.

       PROCESS-FILE-SM-END.

           PERFORM CLOSE-OSM1-FILE.

       PROCESS-FILE-SM-EXIT.
           EXIT.


      ******************************************************************
      *
      *    P R O C E S S - F I L E - S N
      *
      ******************************************************************
       PROCESS-FILE-SN SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-SN = "N" )
              GO TO PROCESS-FILE-SN-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO SN-PATH-OVERRIDE.
           MOVE "SN"                       TO UP1534-CURRENT-FILE.
           MOVE "GB/SNFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OSN-PATH.

           MOVE SPACES                     TO OSN1-KEY.
           PERFORM OPEN-OSN1-FILE.
           PERFORM START-OSN1-FILE.

       PROCESS-FILE-SN-NEXT.
           PERFORM READ-OSN1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-SN-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OSN-SEQ NOT NUMERIC 
              MOVE "SN-SEQ" TO D-FIELD
              MOVE OSN-SEQ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-SN-NEXT.

       PROCESS-FILE-SN-END.

           PERFORM CLOSE-OSN1-FILE.

       PROCESS-FILE-SN-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - S P
      *
      ******************************************************************
       PROCESS-FILE-SP SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-SP = "N" )
              GO TO PROCESS-FILE-SP-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO SP-PATH-OVERRIDE.
           MOVE "SP"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/SPFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OSP-PATH.

           MOVE SPACES                     TO OSP1-KEY.
           MOVE ZEROES                     TO OSP-SPRCLASS
                                              OSP-SUBCLASS
                                              OSP-LAWCODE.

           PERFORM OPEN-OSP1-FILE.
           PERFORM START-OSP1-FILE.

       PROCESS-FILE-SP-NEXT.
           PERFORM READ-OSP1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-SP-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OSP-SPRCLASS NOT NUMERIC 
              MOVE "SP-SPRCLASS" TO D-FIELD
              MOVE OSP-SPRCLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-SUBCLASS NOT NUMERIC 
              MOVE "SP-SUBCLASS" TO D-FIELD
              MOVE OSP-SUBCLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-LAWCODE NOT NUMERIC 
              MOVE "SP-LAWCODE" TO D-FIELD
              MOVE OSP-LAWCODE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OSP-EFFDATE NOT NUMERIC OR
               (OSP-EFFDATE = 202020)
              MOVE "SP-EFFDATE" TO D-FIELD
              MOVE OSP-EFFDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OSP-EFFDATE NOT = 0
                 MOVE OSP-EFFDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "SP-EFFDATE" TO D-FIELD
                    MOVE OSP-EFFDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OSP-EXPDATE NOT NUMERIC OR
               (OSP-EXPDATE = 202020)
              MOVE "SP-EXPDATE" TO D-FIELD
              MOVE OSP-EXPDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OSP-EXPDATE NOT = 0
                 MOVE OSP-EXPDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "SP-EXPDATE" TO D-FIELD
                    MOVE OSP-EXPDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OSP-EXPD-MONTHS NOT NUMERIC 
              MOVE "SP-EXPD-MONTHS" TO D-FIELD
              MOVE OSP-EXPD-MONTHS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-EXPD-DAYS NOT NUMERIC 
              MOVE "SP-EXPD-DAYS" TO D-FIELD
              MOVE OSP-EXPD-DAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              IF OSP-EXPR-RATE(SUB) NOT NUMERIC 
                 MOVE "SP-EXPR-RATE(SUB)" TO D-FIELD
                 MOVE OSP-EXPR-RATE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-EXPR-BREAK(SUB) NOT NUMERIC 
                 MOVE "SP-EXPR-BREAK(SUB)" TO D-FIELD
                 MOVE OSP-EXPR-BREAK(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-EXPR-YRTYPE(SUB) NOT NUMERIC 
                 MOVE "SP-EXPR-YRTYPE(SUB)" TO D-FIELD
                 MOVE OSP-EXPR-YRTYPE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OSP-JUDGRATE NOT NUMERIC 
              MOVE "SP-JUDGRATE" TO D-FIELD
              MOVE OSP-JUDGRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-JUDGYRTYPE NOT NUMERIC 
              MOVE "SP-JUDGYRTYPE" TO D-FIELD
              MOVE OSP-JUDGYRTYPE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-ACC-YRTYPE NOT NUMERIC 
              MOVE "SP-ACC-YRTYPE" TO D-FIELD
              MOVE OSP-ACC-YRTYPE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              IF OSP-ACC-RATE(SUB) NOT NUMERIC 
                 MOVE "SP-ACC-RATE(SUB)" TO D-FIELD
                 MOVE OSP-ACC-RATE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-ACC-BREAK(SUB) NOT NUMERIC 
                 MOVE "SP-ACC-BREAK(SUB)" TO D-FIELD
                 MOVE OSP-ACC-BREAK(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OSP-CONTPDTH NOT NUMERIC 
              MOVE "SP-CONTPDTH" TO D-FIELD
              MOVE OSP-CONTPDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-FINCHG NOT NUMERIC 
              MOVE "SP-FINCHG" TO D-FIELD
              MOVE OSP-FINCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-TRWCONTPDTH NOT NUMERIC 
              MOVE "SP-TRWCONTPDTH" TO D-FIELD
              MOVE OSP-TRWCONTPDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-RECPDTH NOT NUMERIC 
              MOVE "SP-RECPDTH" TO D-FIELD
              MOVE OSP-RECPDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OSP-ERNFRMLA(SUB) NOT NUMERIC 
                 MOVE "SP-ERNFRMLA(SUB)" TO D-FIELD
                 MOVE OSP-ERNFRMLA(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-ERNAMT(SUB) NOT NUMERIC 
                 MOVE "SP-ERNAMT(SUB)" TO D-FIELD
                 MOVE OSP-ERNAMT(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-ERNPERCT(SUB) NOT NUMERIC 
                 MOVE "SP-ERNPERCT(SUB)" TO D-FIELD
                 MOVE OSP-ERNPERCT(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-ERN-DAYS(SUB) NOT NUMERIC 
                 MOVE "SP-ERN-DAYS(SUB)" TO D-FIELD
                 MOVE OSP-ERN-DAYS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-ERN-TERM(SUB) NOT NUMERIC 
                 MOVE "SP-ERN-TERM(SUB)" TO D-FIELD
                 MOVE OSP-ERN-TERM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OSP-ANTICIPATED NOT NUMERIC 
              MOVE "SP-ANTICIPATED" TO D-FIELD
              MOVE OSP-ANTICIPATED  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OSP-EARN-ORDER(SUB) NOT NUMERIC 
                 MOVE "SP-EARN-ORDER(SUB)" TO D-FIELD
                 MOVE OSP-EARN-ORDER(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OSP-ERNDELQ-START NOT NUMERIC 
              MOVE "SP-ERNDELQ-START" TO D-FIELD
              MOVE OSP-ERNDELQ-START  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-ERNDELQ-STOP NOT NUMERIC 
              MOVE "SP-ERNDELQ-STOP" TO D-FIELD
              MOVE OSP-ERNDELQ-STOP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-MAX-IB-ACR-DAYS NOT NUMERIC 
              MOVE "SP-MAX-IB-ACR-DAYS" TO D-FIELD
              MOVE OSP-MAX-IB-ACR-DAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-LCGRACE NOT NUMERIC 
              MOVE "SP-LCGRACE" TO D-FIELD
              MOVE OSP-LCGRACE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-LCRATE NOT NUMERIC 
              MOVE "SP-LCRATE" TO D-FIELD
              MOVE OSP-LCRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-LCMINCHG NOT NUMERIC 
              MOVE "SP-LCMINCHG" TO D-FIELD
              MOVE OSP-LCMINCHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-LCMAXAMT NOT NUMERIC 
              MOVE "SP-LCMAXAMT" TO D-FIELD
              MOVE OSP-LCMAXAMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-LCDELFAC NOT NUMERIC 
              MOVE "SP-LCDELFAC" TO D-FIELD
              MOVE OSP-LCDELFAC  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-LCYRTYPE NOT NUMERIC 
              MOVE "SP-LCYRTYPE" TO D-FIELD
              MOVE OSP-LCYRTYPE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-LC-CODE NOT NUMERIC 
              MOVE "SP-LC-CODE" TO D-FIELD
              MOVE OSP-LC-CODE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-DEFFRMLA NOT NUMERIC 
              MOVE "SP-DEFFRMLA" TO D-FIELD
              MOVE OSP-DEFFRMLA  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-DEFWORKER NOT NUMERIC 
              MOVE "SP-DEFWORKER" TO D-FIELD
              MOVE OSP-DEFWORKER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-DEFPERCD NOT NUMERIC 
              MOVE "SP-DEFPERCD" TO D-FIELD
              MOVE OSP-DEFPERCD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-DEFBASE NOT NUMERIC 
              MOVE "SP-DEFBASE" TO D-FIELD
              MOVE OSP-DEFBASE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OSP-DEFPOLMX(SUB) NOT NUMERIC 
                 MOVE "SP-DEFPOLMX(SUB)" TO D-FIELD
                 MOVE OSP-DEFPOLMX(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OSP-DEFMIN NOT NUMERIC 
              MOVE "SP-DEFMIN" TO D-FIELD
              MOVE OSP-DEFMIN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-DEFMAX NOT NUMERIC 
              MOVE "SP-DEFMAX" TO D-FIELD
              MOVE OSP-DEFMAX  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
              IF OSP-PRFORM(SUB) NOT NUMERIC 
                 MOVE "SP-PRFORM(SUB)" TO D-FIELD
                 MOVE OSP-PRFORM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OSP-SCFRMLA NOT NUMERIC 
              MOVE "SP-SCFRMLA" TO D-FIELD
              MOVE OSP-SCFRMLA  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-SCRATE NOT NUMERIC 
              MOVE "SP-SCRATE" TO D-FIELD
              MOVE OSP-SCRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-SCMIN NOT NUMERIC 
              MOVE "SP-SCMIN" TO D-FIELD
              MOVE OSP-SCMIN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-SCMAX NOT NUMERIC 
              MOVE "SP-SCMAX" TO D-FIELD
              MOVE OSP-SCMAX  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-MFFRMLA NOT NUMERIC 
              MOVE "SP-MFFRMLA" TO D-FIELD
              MOVE OSP-MFFRMLA  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-MFRATE NOT NUMERIC 
              MOVE "SP-MFRATE" TO D-FIELD
              MOVE OSP-MFRATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-MFMIN NOT NUMERIC 
              MOVE "SP-MFMIN" TO D-FIELD
              MOVE OSP-MFMIN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-MFMAX NOT NUMERIC 
              MOVE "SP-MFMAX" TO D-FIELD
              MOVE OSP-MFMAX  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-YEARTYPE NOT NUMERIC 
              MOVE "SP-YEARTYPE" TO D-FIELD
              MOVE OSP-YEARTYPE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-PYEXFRMLA NOT NUMERIC 
              MOVE "SP-PYEXFRMLA" TO D-FIELD
              MOVE OSP-PYEXFRMLA  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-PYEXSPCODE NOT NUMERIC 
              MOVE "SP-PYEXSPCODE" TO D-FIELD
              MOVE OSP-PYEXSPCODE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-PYEXYEARTP NOT NUMERIC 
              MOVE "SP-PYEXYEARTP" TO D-FIELD
              MOVE OSP-PYEXYEARTP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-PYEXBASE NOT NUMERIC 
              MOVE "SP-PYEXBASE" TO D-FIELD
              MOVE OSP-PYEXBASE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-PYEXDAYS NOT NUMERIC 
              MOVE "SP-PYEXDAYS" TO D-FIELD
              MOVE OSP-PYEXDAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-CAL-SPFRMLA NOT NUMERIC 
              MOVE "SP-CAL-SPFRMLA" TO D-FIELD
              MOVE OSP-CAL-SPFRMLA  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING OCTBL FROM 1 BY 1 UNTIL OCTBL > 2
              IF OSP-CAL-NORATES(OCTBL) NOT NUMERIC 
                 MOVE "SP-CAL-NORATES(OCTBL)" TO D-FIELD
                 MOVE OSP-CAL-NORATES(OCTBL)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-CAL-TBFRMLA(OCTBL) NOT NUMERIC 
                 MOVE "SP-CAL-TBFRMLA(OCTBL)" TO D-FIELD
                 MOVE OSP-CAL-TBFRMLA(OCTBL)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
             PERFORM VARYING OCSTP FROM 1 BY 1 UNTIL OCSTP > 5
                 IF OSP-CAL-RATE(OCTBL OCSTP) NOT NUMERIC 
                    MOVE "SP-CAL-RATE(OCTBL OCSTP)" TO D-FIELD
                    MOVE OSP-CAL-RATE(OCTBL OCSTP)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE
                 END-IF
                 IF OSP-CAL-BREAK(OCTBL OCSTP) NOT NUMERIC 
                    MOVE "SP-CAL-BREAK(OCTBL OCSTP)" TO D-FIELD
                    MOVE OSP-CAL-BREAK(OCTBL OCSTP)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE
                 END-IF
             END-PERFORM
           END-PERFORM.


           IF OSP-MINSF-FRMLA NOT NUMERIC 
              MOVE "SP-MINSF-FRMLA" TO D-FIELD
              MOVE OSP-MINSF-FRMLA  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-MINSF-CHG NOT NUMERIC 
              MOVE "SP-MINSF-CHG" TO D-FIELD
              MOVE OSP-MINSF-CHG  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-MAXAPR-FRMLA NOT NUMERIC 
              MOVE "SP-MAXAPR-FRMLA" TO D-FIELD
              MOVE OSP-MAXAPR-FRMLA  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING OETBL FROM 1 BY 1 UNTIL OETBL > 2
              IF OSP-EDT-NOEDITS(OETBL) NOT NUMERIC 
                 MOVE "SP-EDT-NOEDITS(OETBL)" TO D-FIELD
                 MOVE OSP-EDT-NOEDITS(OETBL)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
             PERFORM VARYING OESTP FROM 1 BY 1 UNTIL OESTP > 6
                IF OSP-EDT-RATE(OETBL OESTP) NOT NUMERIC 
                   MOVE "SP-EDT-RATE(OETBL OESTP)" TO D-FIELD
                   MOVE OSP-EDT-RATE(OETBL OESTP)  TO D-VALUE
                   PERFORM WRITE-DETAIL-LINE
                END-IF
                IF OSP-EDT-AMT(OETBL OESTP) NOT NUMERIC 
                   MOVE "SP-EDT-AMT(OETBL OESTP)" TO D-FIELD
                   MOVE OSP-EDT-AMT(OETBL OESTP)  TO D-VALUE
                   PERFORM WRITE-DETAIL-LINE
                END-IF
                IF OSP-EDT-TERM(OETBL OESTP) NOT NUMERIC 
                   MOVE "SP-EDT-TERM(OETBL OESTP)" TO D-FIELD
                   MOVE OSP-EDT-TERM(OETBL OESTP)  TO D-VALUE
                   PERFORM WRITE-DETAIL-LINE
                END-IF
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING OILEV FROM 1 BY 1 UNTIL OILEV > 12
              IF OSP-INSCOM(OILEV) NOT NUMERIC 
                 MOVE "SP-INSCOM(OILEV)" TO D-FIELD
                 MOVE OSP-INSCOM(OILEV)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-INSRATECD(OILEV) NOT NUMERIC 
                 MOVE "SP-INSRATECD(OILEV)" TO D-FIELD
                 MOVE OSP-INSRATECD(OILEV)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-INSJFAC(OILEV) NOT NUMERIC 
                 MOVE "SP-INSJFAC(OILEV)" TO D-FIELD
                 MOVE OSP-INSJFAC(OILEV)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-INSDISCNT(OILEV) NOT NUMERIC 
                 MOVE "SP-INSDISCNT(OILEV)" TO D-FIELD
                 MOVE OSP-INSDISCNT(OILEV)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF

             PERFORM VARYING OISTP FROM 1 BY 1 UNTIL OISTP > 20
                 IF OSP-INSTERM(OILEV OISTP) NOT NUMERIC 
                    MOVE "SP-INSTERM(OILEV OISTP)" TO D-FIELD
                    MOVE OSP-INSTERM(OILEV OISTP)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE
                 END-IF
                 IF OSP-INSRATE(OILEV OISTP) NOT NUMERIC 
                    MOVE "SP-INSRATE(OILEV OISTP)" TO D-FIELD
                    MOVE OSP-INSRATE(OILEV OISTP)  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE
                 END-IF
             END-PERFORM
              IF OSP-INSMINPREM(OILEV) NOT NUMERIC 
                 MOVE "SP-INSMINPREM(OILEV)" TO D-FIELD
                 MOVE OSP-INSMINPREM(OILEV)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-INSMINCHGABL(OILEV) NOT NUMERIC 
                 MOVE "SP-INSMINCHGABL(OILEV)" TO D-FIELD
                 MOVE OSP-INSMINCHGABL(OILEV)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-INSMAXCHGABL(OILEV) NOT NUMERIC 
                 MOVE "SP-INSMAXCHGABL(OILEV)" TO D-FIELD
                 MOVE OSP-INSMAXCHGABL(OILEV)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-INSMAXBENIF(OILEV) NOT NUMERIC 
                 MOVE "SP-INSMAXBENIF(OILEV)" TO D-FIELD
                 MOVE OSP-INSMAXBENIF(OILEV)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-INSMAXTERM(OILEV) NOT NUMERIC 
                 MOVE "SP-INSMAXTERM(OILEV)" TO D-FIELD
                 MOVE OSP-INSMAXTERM(OILEV)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-INSEXCEPT(OILEV) NOT NUMERIC 
                 MOVE "SP-INSEXCEPT(OILEV)" TO D-FIELD
                 MOVE OSP-INSEXCEPT(OILEV)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-INSNOVER(OILEV) NOT NUMERIC 
                 MOVE "SP-INSNOVER(OILEV)" TO D-FIELD
                 MOVE OSP-INSNOVER(OILEV)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
             PERFORM VARYING OIVER FROM 1 BY 1 UNTIL OIVER > 14
              IF OSP-INSMINAGE(OILEV OIVER) NOT NUMERIC 
                 MOVE "SP-INSMINAGE(OILEV OIVER)" TO D-FIELD
                 MOVE OSP-INSMINAGE(OILEV OIVER)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-INSMAXAGE(OILEV OIVER) NOT NUMERIC 
                 MOVE "SP-INSMAXAGE(OILEV OIVER)" TO D-FIELD
                 MOVE OSP-INSMAXAGE(OILEV OIVER)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-INSMAXCOV(OILEV OIVER) NOT NUMERIC 
                 MOVE "SP-INSMAXCOV(OILEV OIVER)" TO D-FIELD
                 MOVE OSP-INSMAXCOV(OILEV OIVER)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OSP-INSMAXTRM(OILEV OIVER) NOT NUMERIC 
                 MOVE "SP-INSMAXTRM(OILEV OIVER)" TO D-FIELD
                 MOVE OSP-INSMAXTRM(OILEV OIVER)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 16
              PERFORM VARYING S2 FROM 1 BY 1 UNTIL S2 > 2
                IF OSP-RBDAYS(S1 S2) NOT NUMERIC 
                   MOVE "SP-RBDAYS(S1 S2)" TO D-FIELD
                   MOVE OSP-RBDAYS(S1 S2)  TO D-VALUE
                   PERFORM WRITE-DETAIL-LINE
                END-IF
                IF OSP-RBYRTYPE(S1 S2) NOT NUMERIC 
                   MOVE "SP-RBYRTYPE(S1 S2)" TO D-FIELD
                   MOVE OSP-RBYRTYPE(S1 S2)  TO D-VALUE
                   PERFORM WRITE-DETAIL-LINE
                END-IF
              END-PERFORM
             IF OSP-RBMINRET(S1) NOT NUMERIC 
                MOVE "SP-RBMINRET(S1)" TO D-FIELD
                MOVE OSP-RBMINRET(S1)  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OSP-RBNOMON(S1) NOT NUMERIC 
                MOVE "SP-RBNOMON(S1)" TO D-FIELD
                MOVE OSP-RBNOMON(S1)  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OSP-RBDEDCHG(S1) NOT NUMERIC 
                MOVE "SP-RBDEDCHG(S1)" TO D-FIELD
                MOVE OSP-RBDEDCHG(S1)  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OSP-RBRECAST(S1) NOT NUMERIC 
                MOVE "SP-RBRECAST(S1)" TO D-FIELD
                MOVE OSP-RBRECAST(S1)  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OSP-RBEARLY(S1) NOT NUMERIC 
                MOVE "SP-RBEARLY(S1)" TO D-FIELD
                MOVE OSP-RBEARLY(S1)  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OSP-RBDEFER(S1) NOT NUMERIC 
                MOVE "SP-RBDEFER(S1)" TO D-FIELD
                MOVE OSP-RBDEFER(S1)  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OSP-RBPCT(S1) NOT NUMERIC 
                MOVE "SP-RBPCT(S1)" TO D-FIELD
                MOVE OSP-RBPCT(S1)  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OSP-RBREDUC(S1) NOT NUMERIC 
                MOVE "SP-RBREDUC(S1)" TO D-FIELD
                MOVE OSP-RBREDUC(S1)  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OSP-RBMIN(S1) NOT NUMERIC 
                MOVE "SP-RBMIN(S1)" TO D-FIELD
                MOVE OSP-RBMIN(S1)  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OSP-RBSPOPT1(S1) NOT NUMERIC 
                MOVE "SP-RBSPOPT1(S1)" TO D-FIELD
                MOVE OSP-RBSPOPT1(S1)  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OSP-RBSPOPT2(S1) NOT NUMERIC 
                MOVE "SP-RBSPOPT2(S1)" TO D-FIELD
                MOVE OSP-RBSPOPT2(S1)  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
             IF OSP-RBMANAGER(S1) NOT NUMERIC 
                MOVE "SP-RBMANAGER(S1)" TO D-FIELD
                MOVE OSP-RBMANAGER(S1)  TO D-VALUE
                PERFORM WRITE-DETAIL-LINE
             END-IF
           END-PERFORM.
           IF OSP-1STPY-ADDON-DAYS NOT NUMERIC 
              MOVE "SP-1STPY-ADDON-DAYS" TO D-FIELD
              MOVE OSP-1STPY-ADDON-DAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-1STPY-ADDON-YRTYPE NOT NUMERIC 
              MOVE "SP-1STPY-ADDON-YRTYPE" TO D-FIELD
              MOVE OSP-1STPY-ADDON-YRTYPE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-ADDON-RATE NOT NUMERIC 
              MOVE "SP-ADDON-RATE" TO D-FIELD
              MOVE OSP-ADDON-RATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-CONTR-CREDIT NOT NUMERIC 
              MOVE "SP-CONTR-CREDIT" TO D-FIELD
              MOVE OSP-CONTR-CREDIT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-CONTR-CREDIT-MIN-DEL-PER NOT NUMERIC 
              MOVE "SP-CONTR-CREDIT-MIN-DEL-PER" TO D-FIELD
              MOVE OSP-CONTR-CREDIT-MIN-DEL-PER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-PBDEL-DAYS NOT NUMERIC 
              MOVE "SP-PBDEL-DAYS" TO D-FIELD
              MOVE OSP-PBDEL-DAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-PBINCR-PERCENT NOT NUMERIC 
              MOVE "SP-PBINCR-PERCENT" TO D-FIELD
              MOVE OSP-PBINCR-PERCENT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-MAX-ALLOW-DELINQ NOT NUMERIC 
              MOVE "SP-MAX-ALLOW-DELINQ" TO D-FIELD
              MOVE OSP-MAX-ALLOW-DELINQ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-MIN-CREDIT-SCORE NOT NUMERIC 
              MOVE "SP-MIN-CREDIT-SCORE" TO D-FIELD
              MOVE OSP-MIN-CREDIT-SCORE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-WAGE-ASSIGN-DAYS NOT NUMERIC 
              MOVE "SP-WAGE-ASSIGN-DAYS" TO D-FIELD
              MOVE OSP-WAGE-ASSIGN-DAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-YTD-MAX-ALLOW-DELINQ NOT NUMERIC 
              MOVE "SP-YTD-MAX-ALLOW-DELINQ" TO D-FIELD
              MOVE OSP-YTD-MAX-ALLOW-DELINQ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-LNCOST-AMT NOT NUMERIC 
              MOVE "SP-LNCOST-AMT" TO D-FIELD
              MOVE OSP-LNCOST-AMT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-DLDISC-RVPRIN1-PCNT NOT NUMERIC 
              MOVE "SP-DLDISC-RVPRIN1-PCNT" TO D-FIELD
              MOVE OSP-DLDISC-RVPRIN1-PCNT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-RENEWABLE-PAYS NOT NUMERIC 
              MOVE "SP-RENEWABLE-PAYS" TO D-FIELD
              MOVE OSP-RENEWABLE-PAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-RENEWABLE-DAYS NOT NUMERIC 
              MOVE "SP-RENEWABLE-DAYS" TO D-FIELD
              MOVE OSP-RENEWABLE-DAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OSP-DLDISC-RVPRIN1-AF-PCNT NOT NUMERIC 
              MOVE "SP-DLDISC-RVPRIN1-AF-PCNT" TO D-FIELD
              MOVE OSP-DLDISC-RVPRIN1-AF-PCNT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
             PERFORM VARYING S2 FROM 1 BY 1 UNTIL S2 > 4
              IF OSP-REPO-FMNO(S1 S2) NOT NUMERIC 
                 MOVE "SP-REPO-FMNO(S1 S2)" TO D-FIELD
                 MOVE OSP-REPO-FMNO(S1 S2)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
             END-PERFORM
           END-PERFORM.

           IF OSP-DEFAULT-O2-INS-COMP NOT NUMERIC 
              MOVE "SP-DEFAULT-O2-INS-COMP" TO D-FIELD
              MOVE OSP-DEFAULT-O2-INS-COMP  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-DELQ-IB-AGE NOT NUMERIC 
              MOVE "SP-DELQ-IB-AGE" TO D-FIELD
              MOVE OSP-DELQ-IB-AGE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-DELQ-IB-MAX-DAYS NOT NUMERIC 
              MOVE "SP-DELQ-IB-MAX-DAYS" TO D-FIELD
              MOVE OSP-DELQ-IB-MAX-DAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-MAX-CASH-PROCEEDS NOT NUMERIC 
              MOVE "SP-MAX-CASH-PROCEEDS" TO D-FIELD
              MOVE OSP-MAX-CASH-PROCEEDS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-BANKRULE-RATE NOT NUMERIC 
              MOVE "SP-BANKRULE-RATE" TO D-FIELD
              MOVE OSP-BANKRULE-RATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 2
             PERFORM VARYING S2 FROM 1 BY 1 UNTIL S2 > 4
               IF OSP-AUTO-SEND-FMNO (S1 S2) NOT NUMERIC 
                  MOVE "SP-AUTO-SEND-FMNO (S1 S2)" TO D-FIELD
                  MOVE OSP-AUTO-SEND-FMNO (S1 S2)  TO D-VALUE
                  PERFORM WRITE-DETAIL-LINE
               END-IF
             END-PERFORM
           END-PERFORM.

           IF OSP-DLDISC-CONV-BF-PCNT NOT NUMERIC 
              MOVE "SP-DLDISC-CONV-BF-PCNT" TO D-FIELD
              MOVE OSP-DLDISC-CONV-BF-PCNT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-CONTRACTUAL-SET-NONACCRUAL NOT NUMERIC 
              MOVE "SP-CONTRACTUAL-SET-NONACCRUAL" TO D-FIELD
              MOVE OSP-CONTRACTUAL-SET-NONACCRUAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-CONTRACTUAL-RESET-ACCRUAL NOT NUMERIC 
              MOVE "SP-CONTRACTUAL-RESET-ACCRUAL" TO D-FIELD
              MOVE OSP-CONTRACTUAL-RESET-ACCRUAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-MONTHS-PAY-NAPP-COUNTER NOT NUMERIC 
              MOVE "SP-MONTHS-PAY-NAPP-COUNTER" TO D-FIELD
              MOVE OSP-MONTHS-PAY-NAPP-COUNTER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-SHORT-PAY-THRESHOLD NOT NUMERIC 
              MOVE "SP-SHORT-PAY-THRESHOLD" TO D-FIELD
              MOVE OSP-SHORT-PAY-THRESHOLD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-CASH-AVAIL-PERCENT NOT NUMERIC 
              MOVE "SP-CASH-AVAIL-PERCENT" TO D-FIELD
              MOVE OSP-CASH-AVAIL-PERCENT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-NET-BAL-REQ NOT NUMERIC 
              MOVE "SP-NET-BAL-REQ" TO D-FIELD
              MOVE OSP-NET-BAL-REQ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-AMT-PROCEEDS-REQ NOT NUMERIC 
              MOVE "SP-AMT-PROCEEDS-REQ" TO D-FIELD
              MOVE OSP-AMT-PROCEEDS-REQ  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-ALT-PREPAY-MONTHS NOT NUMERIC 
              MOVE "SP-ALT-PREPAY-MONTHS" TO D-FIELD
              MOVE OSP-ALT-PREPAY-MONTHS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-PERCENT-REGPAY-AVAIL NOT NUMERIC 
              MOVE "SP-PERCENT-REGPAY-AVAIL" TO D-FIELD
              MOVE OSP-PERCENT-REGPAY-AVAIL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-ALT-DEFFRMLA NOT NUMERIC 
              MOVE "SP-ALT-DEFFRMLA" TO D-FIELD
              MOVE OSP-ALT-DEFFRMLA  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-ALT-DEFMIN NOT NUMERIC 
              MOVE "SP-ALT-DEFMIN" TO D-FIELD
              MOVE OSP-ALT-DEFMIN  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSP-ALT-DEFMAX NOT NUMERIC 
              MOVE "SP-ALT-DEFMAX" TO D-FIELD
              MOVE OSP-ALT-DEFMAX  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-SP-NEXT.

       PROCESS-FILE-SP-END.

           PERFORM CLOSE-OSP1-FILE.

       PROCESS-FILE-SP-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - S S
      *
      ******************************************************************
       PROCESS-FILE-SS SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-SS = "N" )
              GO TO PROCESS-FILE-SS-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO SS-PATH-OVERRIDE.
           MOVE "SS"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/SSFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OSS-PATH.

      * GET DATE 6 YEARS AGO, SKIP SS RECORDS OLDER THAN 6 YEARS
           MOVE TODAYS-DATE TO NDTE-DATE.
           MOVE -6          TO NDTE-HOLD.
           PERFORM INCREMENT-YEARS.
           MOVE NDTE-DATE   TO COMPARE-DATE.

           MOVE SPACES                     TO OSS1-KEY.
           MOVE 0                          TO OSS-DATE
                                              OSS-BRNO
                                              OSS-CLASS.

           PERFORM OPEN-OSS1-FILE.
           PERFORM START-OSS1-FILE.

       PROCESS-FILE-SS-NEXT.
           PERFORM READ-OSS1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-SS-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           MOVE OSS-DATE  TO DATE-YYYYMM.
      * COMPARE DATE HOLDS TODAYS DATE - 6 YEARS
           IF DATE-YYYYMM-YYYY < COMPARE-DATE-YYYY
              ADD 1        TO UP1534-RECORD-COUNTER-SKIP
              GO TO PROCESS-FILE-SS-NEXT.

      * YYYYMM
           IF OSS-DATE NOT NUMERIC 
              MOVE "SS-DATE" TO D-FIELD
              MOVE OSS-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSS-BRNO NOT NUMERIC 
              MOVE "SS-BRNO" TO D-FIELD
              MOVE OSS-BRNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OSS-CLASS NOT NUMERIC 
              MOVE "SS-CLASS" TO D-FIELD
              MOVE OSS-CLASS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING S3 FROM 1 BY 1 UNTIL S3 > 5
             AFTER S2 FROM 1 BY 1 UNTIL S2 > 2
              AFTER S1 FROM 1 BY 1 UNTIL S1 > 8
                IF OSS-UNITS(S1 S2 S3) NOT NUMERIC 
                   MOVE "SS-UNITS(S1 S2 S3)" TO D-FIELD
                   MOVE OSS-UNITS(S1 S2 S3)  TO D-VALUE
                   PERFORM WRITE-DETAIL-LINE
                END-IF
                IF OSS-OUTBAL(S1 S2 S3) NOT NUMERIC 
                   MOVE "SS-OUTBAL(S1 S2 S3)" TO D-FIELD
                   MOVE OSS-OUTBAL(S1 S2 S3)  TO D-VALUE
                   PERFORM WRITE-DETAIL-LINE
                END-IF
                IF S3 < 3
                    IF OSS-NO-PL(S1 S2 S3) NOT NUMERIC 
                       MOVE "SS-NO-PL(S1 S2 S3)" TO D-FIELD
                       MOVE OSS-NO-PL(S1 S2 S3)  TO D-VALUE
                       PERFORM WRITE-DETAIL-LINE
                    END-IF
                    IF OSS-AMT-PL(S1 S2 S3) NOT NUMERIC 
                       MOVE "SS-AMT-PL(S1 S2 S3)" TO D-FIELD
                       MOVE OSS-AMT-PL(S1 S2 S3)  TO D-VALUE
                       PERFORM WRITE-DETAIL-LINE
                    END-IF
                END-IF
           END-PERFORM.


           GO TO PROCESS-FILE-SS-NEXT.

       PROCESS-FILE-SS-END.

           PERFORM CLOSE-OSS1-FILE.

       PROCESS-FILE-SS-EXIT.
           EXIT.


      ******************************************************************
      *
      *    P R O C E S S - F I L E - T R W I N F O
      *
      ******************************************************************
       PROCESS-FILE-TRWINFO SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-TRWINFO = "N" )
              GO TO PROCESS-FILE-TRWINFO-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO TRW-INFO-PATH-OVERRIDE.
           MOVE "TRWINFO"                  TO UP1534-CURRENT-FILE.
           MOVE "GB/TRWINFO"               TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OTRW-INFO-PATH.


           MOVE 0                     TO OTRW-INFO-ID.
           PERFORM OPEN-OTRW-INFO-FILE.
           PERFORM START-OTRW-INFO-FILE.

       PROCESS-FILE-TRWINFO-NEXT.
           PERFORM READ-OTRW-INFO-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-TRWINFO-END.


           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OTRW-INFO-ID NOT NUMERIC 
              MOVE "TRW-INFO-ID" TO D-FIELD
              MOVE OTRW-INFO-ID  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
       
           IF OTRW-INFO-REPORTER-PHONE NOT NUMERIC 
              MOVE "TRW-INFO-REPORTER-PHONE" TO D-FIELD
              MOVE OTRW-INFO-REPORTER-PHONE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTRW-YEARS-STOP-REPORTING NOT NUMERIC 
              MOVE "TRW-YEARS-STOP-REPORTING" TO D-FIELD
              MOVE OTRW-YEARS-STOP-REPORTING  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.


           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           GO TO PROCESS-FILE-TRWINFO-NEXT.

       PROCESS-FILE-TRWINFO-END.

           PERFORM CLOSE-OTRW-INFO-FILE.

       PROCESS-FILE-TRWINFO-EXIT.
           EXIT.



      ******************************************************************
      *
      *    P R O C E S S - F I L E - T Z
      *
      ******************************************************************
       PROCESS-FILE-TZ SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-TZ = "N" )
              GO TO PROCESS-FILE-TZ-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO TZ-PATH-OVERRIDE.
           MOVE "TZ"                       TO UP1534-CURRENT-FILE.
           MOVE "LP/TZFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.
      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OTZ-PATH.

           MOVE SPACES                     TO OTZ1-KEY.
           MOVE ZEROES                     TO OTZ-CODE
                                              OTZ-PREFIX.

           PERFORM OPEN-OTZ1-FILE.
           PERFORM START-OTZ1-FILE.

       PROCESS-FILE-TZ-NEXT.
           PERFORM READ-OTZ1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-TZ-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OTZ-CODE NOT NUMERIC 
              MOVE "TZ-CODE" TO D-FIELD
              MOVE OTZ-CODE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTZ-PREFIX NOT NUMERIC 
              MOVE "TZ-PREFIX" TO D-FIELD
              MOVE OTZ-PREFIX  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTZ-FACTOR NOT NUMERIC 
              MOVE "TZ-FACTOR" TO D-FIELD
              MOVE OTZ-FACTOR  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-FILE-TZ-NEXT.

       PROCESS-FILE-TZ-END.

           PERFORM CLOSE-OTZ1-FILE.

       PROCESS-FILE-TZ-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - W R
      *
      ******************************************************************
       PROCESS-FILE-WR SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-WR = "N" )
              GO TO PROCESS-FILE-WR-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO WR-PATH-OVERRIDE.
           MOVE "WR"                       TO UP1534-CURRENT-FILE.
           MOVE "SP/WRFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.
      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO OWR-PATH.

           MOVE SPACES                     TO OWR1-KEY.
           MOVE ZEROES                     TO OWR-YYYYMM
                                              OWR-BRNO.

           PERFORM OPEN-OWR1-FILE.
           PERFORM START-OWR1-FILE.

       PROCESS-FILE-WR-NEXT.
           PERFORM READ-OWR1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-WR-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

           IF OWR-YYYYMM NOT NUMERIC 
              MOVE "WR-YYYYMM" TO D-FIELD
              MOVE OWR-YYYYMM  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-BRNO NOT NUMERIC 
              MOVE "WR-BRNO" TO D-FIELD
              MOVE OWR-BRNO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-CUR-NO-CUST NOT NUMERIC 
              MOVE "WR-CUR-NO-CUST" TO D-FIELD
              MOVE OWR-CUR-NO-CUST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-ELM-NO-CUST NOT NUMERIC 
              MOVE "WR-ELM-NO-CUST" TO D-FIELD
              MOVE OWR-ELM-NO-CUST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-ELY-NO-CUST NOT NUMERIC 
              MOVE "WR-ELY-NO-CUST" TO D-FIELD
              MOVE OWR-ELY-NO-CUST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-OUTS-BALANCE NOT NUMERIC 
              MOVE "WR-OUTS-BALANCE" TO D-FIELD
              MOVE OWR-OUTS-BALANCE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-ELM-OUTS-BALANCE NOT NUMERIC 
              MOVE "WR-ELM-OUTS-BALANCE" TO D-FIELD
              MOVE OWR-ELM-OUTS-BALANCE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-ELY-OUTS-BALANCE NOT NUMERIC 
              MOVE "WR-ELY-OUTS-BALANCE" TO D-FIELD
              MOVE OWR-ELY-OUTS-BALANCE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
              IF OWR-NO-NB(SUB) NOT NUMERIC 
                 MOVE "WR-NO-NB(SUB)" TO D-FIELD
                 MOVE OWR-NO-NB(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-FB(SUB) NOT NUMERIC 
                 MOVE "WR-NO-FB(SUB)" TO D-FIELD
                 MOVE OWR-NO-FB(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-PB(SUB) NOT NUMERIC 
                 MOVE "WR-NO-PB(SUB)" TO D-FIELD
                 MOVE OWR-NO-PB(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-GROSS-VOLUME(SUB) NOT NUMERIC 
                 MOVE "WR-GROSS-VOLUME(SUB)" TO D-FIELD
                 MOVE OWR-GROSS-VOLUME(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-BSR-BONUS(SUB) NOT NUMERIC 
                 MOVE "WR-BSR-BONUS(SUB)" TO D-FIELD
                 MOVE OWR-BSR-BONUS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-INTEREST-ALLOCATION(SUB) NOT NUMERIC 
                 MOVE "WR-INTEREST-ALLOCATION(SUB)" TO D-FIELD
                 MOVE OWR-INTEREST-ALLOCATION(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-OLD-SUPERVISION(SUB) NOT NUMERIC 
                 MOVE "WR-OLD-SUPERVISION(SUB)" TO D-FIELD
                 MOVE OWR-OLD-SUPERVISION(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-GEN-RESERVE-ADJ(SUB) NOT NUMERIC 
                 MOVE "WR-GEN-RESERVE-ADJ(SUB)" TO D-FIELD
                 MOVE OWR-GEN-RESERVE-ADJ(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-INTANGIBLE-AMORT(SUB) NOT NUMERIC 
                 MOVE "WR-INTANGIBLE-AMORT(SUB)" TO D-FIELD
                 MOVE OWR-INTANGIBLE-AMORT(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-ROBBERIES(SUB) NOT NUMERIC 
                 MOVE "WR-ROBBERIES(SUB)" TO D-FIELD
                 MOVE OWR-ROBBERIES(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-RATING-SHEET-INCOME(SUB) NOT NUMERIC 
                 MOVE "WR-RATING-SHEET-INCOME(SUB)" TO D-FIELD
                 MOVE OWR-RATING-SHEET-INCOME(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-INT-FEE-INCOME(SUB) NOT NUMERIC 
                 MOVE "WR-INT-FEE-INCOME(SUB)" TO D-FIELD
                 MOVE OWR-INT-FEE-INCOME(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-INSURANCE-INCOME(SUB) NOT NUMERIC 
                 MOVE "WR-INSURANCE-INCOME(SUB)" TO D-FIELD
                 MOVE OWR-INSURANCE-INCOME(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-RATING-SHEET-EXPENSES(SUB) NOT NUMERIC 
                 MOVE "WR-RATING-SHEET-EXPENSES(SUB)" TO D-FIELD
                 MOVE OWR-RATING-SHEET-EXPENSES(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-PERSONNEL-EXPENSE(SUB) NOT NUMERIC 
                 MOVE "WR-PERSONNEL-EXPENSE(SUB)" TO D-FIELD
                 MOVE OWR-PERSONNEL-EXPENSE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-CHARGEOFF-EXPENSE(SUB) NOT NUMERIC 
                 MOVE "WR-CHARGEOFF-EXPENSE(SUB)" TO D-FIELD
                 MOVE OWR-CHARGEOFF-EXPENSE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-CHARGEOFFS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-CHARGEOFFS(SUB)" TO D-FIELD
                 MOVE OWR-NO-CHARGEOFFS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-RECOVERY-INCOME(SUB) NOT NUMERIC 
                 MOVE "WR-RECOVERY-INCOME(SUB)" TO D-FIELD
                 MOVE OWR-RECOVERY-INCOME(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-BAD-DEBT-INCREASE(SUB) NOT NUMERIC 
                 MOVE "WR-BAD-DEBT-INCREASE(SUB)" TO D-FIELD
                 MOVE OWR-BAD-DEBT-INCREASE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OWR-NO-30-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-NO-30-DAY-ACCTS" TO D-FIELD
              MOVE OWR-NO-30-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-AMT-30-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-AMT-30-DAY-ACCTS" TO D-FIELD
              MOVE OWR-AMT-30-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-NO-60-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-NO-60-DAY-ACCTS" TO D-FIELD
              MOVE OWR-NO-60-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-AMT-60-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-AMT-60-DAY-ACCTS" TO D-FIELD
              MOVE OWR-AMT-60-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-NO-90-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-NO-90-DAY-ACCTS" TO D-FIELD
              MOVE OWR-NO-90-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-AMT-90-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-AMT-90-DAY-ACCTS" TO D-FIELD
              MOVE OWR-AMT-90-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-ELM-AMT-90-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-ELM-AMT-90-DAY-ACCTS" TO D-FIELD
              MOVE OWR-ELM-AMT-90-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-ELY-AMT-90-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-ELY-AMT-90-DAY-ACCTS" TO D-FIELD
              MOVE OWR-ELY-AMT-90-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OWR-YTD-PCT-TURNOVER NOT NUMERIC 
              MOVE "WR-YTD-PCT-TURNOVER" TO D-FIELD
              MOVE OWR-YTD-PCT-TURNOVER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-YTD-PCT-RATE-SHT-PROFIT NOT NUMERIC 
              MOVE "WR-YTD-PCT-RATE-SHT-PROFIT" TO D-FIELD
              MOVE OWR-YTD-PCT-RATE-SHT-PROFIT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-YTD-PCT-RS-INC-LEDGER NOT NUMERIC 
              MOVE "WR-YTD-PCT-RS-INC-LEDGER" TO D-FIELD
              MOVE OWR-YTD-PCT-RS-INC-LEDGER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-YTD-PCT-INT-FEE-LEDGER NOT NUMERIC 
              MOVE "WR-YTD-PCT-INT-FEE-LEDGER" TO D-FIELD
              MOVE OWR-YTD-PCT-INT-FEE-LEDGER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-YTD-PCT-INS-INC-LEDGER NOT NUMERIC 
              MOVE "WR-YTD-PCT-INS-INC-LEDGER" TO D-FIELD
              MOVE OWR-YTD-PCT-INS-INC-LEDGER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-YTD-PCT-EXPENSE-LEDGER NOT NUMERIC 
              MOVE "WR-YTD-PCT-EXPENSE-LEDGER" TO D-FIELD
              MOVE OWR-YTD-PCT-EXPENSE-LEDGER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-YTD-PCT-BAD-DEBT-LEDGER NOT NUMERIC 
              MOVE "WR-YTD-PCT-BAD-DEBT-LEDGER" TO D-FIELD
              MOVE OWR-YTD-PCT-BAD-DEBT-LEDGER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-YTD-PCT-NF-BNK-LEDGER NOT NUMERIC 
              MOVE "WR-YTD-PCT-NF-BNK-LEDGER" TO D-FIELD
              MOVE OWR-YTD-PCT-NF-BNK-LEDGER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-YTD-PCT-PRETAX-LEDGER NOT NUMERIC 
              MOVE "WR-YTD-PCT-PRETAX-LEDGER" TO D-FIELD
              MOVE OWR-YTD-PCT-PRETAX-LEDGER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-YTD-PCT-DEBT-NF-LEDGER NOT NUMERIC 
              MOVE "WR-YTD-PCT-DEBT-NF-LEDGER" TO D-FIELD
              MOVE OWR-YTD-PCT-DEBT-NF-LEDGER  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-CUR-NO-CUST NOT NUMERIC 
              MOVE "WR-WCBC-CUR-NO-CUST" TO D-FIELD
              MOVE OWR-WCBC-CUR-NO-CUST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-ELM-NO-CUST NOT NUMERIC 
              MOVE "WR-WCBC-ELM-NO-CUST" TO D-FIELD
              MOVE OWR-WCBC-ELM-NO-CUST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-ELY-NO-CUST NOT NUMERIC 
              MOVE "WR-WCBC-ELY-NO-CUST" TO D-FIELD
              MOVE OWR-WCBC-ELY-NO-CUST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-OUTS-BALANCE NOT NUMERIC 
              MOVE "WR-WCBC-OUTS-BALANCE" TO D-FIELD
              MOVE OWR-WCBC-OUTS-BALANCE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-ELM-OUTS-BAL NOT NUMERIC 
              MOVE "WR-WCBC-ELM-OUTS-BAL" TO D-FIELD
              MOVE OWR-WCBC-ELM-OUTS-BAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-ELY-OUTS-BAL NOT NUMERIC 
              MOVE "WR-WCBC-ELY-OUTS-BAL" TO D-FIELD
              MOVE OWR-WCBC-ELY-OUTS-BAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.


           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
              IF OWR-WCBC-NO-NB(SUB) NOT NUMERIC 
                 MOVE "WR-WCBC-NO-NB(SUB)" TO D-FIELD
                 MOVE OWR-WCBC-NO-NB(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-WCBC-NO-FB(SUB) NOT NUMERIC 
                 MOVE "WR-WCBC-NO-FB(SUB)" TO D-FIELD
                 MOVE OWR-WCBC-NO-FB(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-WCBC-NO-PB(SUB) NOT NUMERIC 
                 MOVE "WR-WCBC-NO-PB(SUB)" TO D-FIELD
                 MOVE OWR-WCBC-NO-PB(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-WCBC-GROSS-VOLUME(SUB) NOT NUMERIC 
                 MOVE "WR-WCBC-GROSS-VOLUME(SUB)" TO D-FIELD
                 MOVE OWR-WCBC-GROSS-VOLUME(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-WCBC-LATE-CHGS(SUB) NOT NUMERIC 
                 MOVE "WR-WCBC-LATE-CHGS(SUB)" TO D-FIELD
                 MOVE OWR-WCBC-LATE-CHGS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-WCBC-NET-FIN-CHGS(SUB) NOT NUMERIC 
                 MOVE "WR-WCBC-NET-FIN-CHGS(SUB)" TO D-FIELD
                 MOVE OWR-WCBC-NET-FIN-CHGS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-WCBC-SALES(SUB) NOT NUMERIC 
                 MOVE "WR-WCBC-SALES(SUB)" TO D-FIELD
                 MOVE OWR-WCBC-SALES(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-WCBC-COST-OF-SALES(SUB) NOT NUMERIC 
                 MOVE "WR-WCBC-COST-OF-SALES(SUB)" TO D-FIELD
                 MOVE OWR-WCBC-COST-OF-SALES(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-WCBC-EXPENSES(SUB) NOT NUMERIC 
                 MOVE "WR-WCBC-EXPENSES(SUB)" TO D-FIELD
                 MOVE OWR-WCBC-EXPENSES(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-WCBC-RECOVERY-INCOME(SUB) NOT NUMERIC 
                 MOVE "WR-WCBC-RECOVERY-INCOME(SUB)" TO D-FIELD
                 MOVE OWR-WCBC-RECOVERY-INCOME(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-WCBC-CHARGEOFF-EXPENSE(SUB) NOT NUMERIC 
                 MOVE "WR-WCBC-CHARGEOFF-EXPENSE(SUB)" TO D-FIELD
                 MOVE OWR-WCBC-CHARGEOFF-EXPENSE(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-WCBC-BAD-DEBT-BNKRPCY(SUB) NOT NUMERIC 
                 MOVE "WR-WCBC-BAD-DEBT-BNKRPCY(SUB)" TO D-FIELD
                 MOVE OWR-WCBC-BAD-DEBT-BNKRPCY(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OWR-WCBC-NO-30-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-WCBC-NO-30-DAY-ACCTS" TO D-FIELD
              MOVE OWR-WCBC-NO-30-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-AMT-30-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-WCBC-AMT-30-DAY-ACCTS" TO D-FIELD
              MOVE OWR-WCBC-AMT-30-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-NO-60-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-WCBC-NO-60-DAY-ACCTS" TO D-FIELD
              MOVE OWR-WCBC-NO-60-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-AMT-60-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-WCBC-AMT-60-DAY-ACCTS" TO D-FIELD
              MOVE OWR-WCBC-AMT-60-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-NO-90-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-WCBC-NO-90-DAY-ACCTS" TO D-FIELD
              MOVE OWR-WCBC-NO-90-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-AMT-90-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-WCBC-AMT-90-DAY-ACCTS" TO D-FIELD
              MOVE OWR-WCBC-AMT-90-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-ELM-AMT-90-DAYS NOT NUMERIC 
              MOVE "WR-WCBC-ELM-AMT-90-DAYS" TO D-FIELD
              MOVE OWR-WCBC-ELM-AMT-90-DAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-ELY-AMT-90-DAYS NOT NUMERIC 
              MOVE "WR-WCBC-ELY-AMT-90-DAYS" TO D-FIELD
              MOVE OWR-WCBC-ELY-AMT-90-DAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OWR-WCBC-PCT-VOLUME NOT NUMERIC 
              MOVE "WR-WCBC-PCT-VOLUME" TO D-FIELD
              MOVE OWR-WCBC-PCT-VOLUME  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-GROSS-PROF-PCT NOT NUMERIC 
              MOVE "WR-WCBC-GROSS-PROF-PCT" TO D-FIELD
              MOVE OWR-WCBC-GROSS-PROF-PCT  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OWR-LARGE-CUR-NO-CUST NOT NUMERIC 
              MOVE "WR-LARGE-CUR-NO-CUST" TO D-FIELD
              MOVE OWR-LARGE-CUR-NO-CUST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-LARGE-ELM-NO-CUST NOT NUMERIC 
              MOVE "WR-LARGE-ELM-NO-CUST" TO D-FIELD
              MOVE OWR-LARGE-ELM-NO-CUST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-LARGE-ELY-NO-CUST NOT NUMERIC 
              MOVE "WR-LARGE-ELY-NO-CUST" TO D-FIELD
              MOVE OWR-LARGE-ELY-NO-CUST  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-LARGE-OUTS-BALANCE NOT NUMERIC 
              MOVE "WR-LARGE-OUTS-BALANCE" TO D-FIELD
              MOVE OWR-LARGE-OUTS-BALANCE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-LARGE-ELM-OUTS-BAL NOT NUMERIC 
              MOVE "WR-LARGE-ELM-OUTS-BAL" TO D-FIELD
              MOVE OWR-LARGE-ELM-OUTS-BAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-LARGE-ELY-OUTS-BAL NOT NUMERIC 
              MOVE "WR-LARGE-ELY-OUTS-BAL" TO D-FIELD
              MOVE OWR-LARGE-ELY-OUTS-BAL  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
              IF OWR-LARGE-NO-NB(SUB) NOT NUMERIC 
                 MOVE "WR-LARGE-NO-NB(SUB)" TO D-FIELD
                 MOVE OWR-LARGE-NO-NB(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-LARGE-NO-FB(SUB) NOT NUMERIC 
                 MOVE "WR-LARGE-NO-FB(SUB)" TO D-FIELD
                 MOVE OWR-LARGE-NO-FB(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-LARGE-NO-PB(SUB) NOT NUMERIC 
                 MOVE "WR-LARGE-NO-PB(SUB)" TO D-FIELD
                 MOVE OWR-LARGE-NO-PB(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-LARGE-GROSS-VOLUME(SUB) NOT NUMERIC 
                 MOVE "WR-LARGE-GROSS-VOLUME(SUB)" TO D-FIELD
                 MOVE OWR-LARGE-GROSS-VOLUME(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OWR-LARGE-NO-30-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-LARGE-NO-30-DAY-ACCTS" TO D-FIELD
              MOVE OWR-LARGE-NO-30-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-LARGE-AMT-30-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-LARGE-AMT-30-DAY-ACCTS" TO D-FIELD
              MOVE OWR-LARGE-AMT-30-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-LARGE-NO-60-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-LARGE-NO-60-DAY-ACCTS" TO D-FIELD
              MOVE OWR-LARGE-NO-60-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-LARGE-AMT-60-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-LARGE-AMT-60-DAY-ACCTS" TO D-FIELD
              MOVE OWR-LARGE-AMT-60-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-LARGE-NO-90-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-LARGE-NO-90-DAY-ACCTS" TO D-FIELD
              MOVE OWR-LARGE-NO-90-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-LARGE-AMT-90-DAY-ACCTS NOT NUMERIC 
              MOVE "WR-LARGE-AMT-90-DAY-ACCTS" TO D-FIELD
              MOVE OWR-LARGE-AMT-90-DAY-ACCTS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-LARGE-ELM-AMT-90-DAYS NOT NUMERIC 
              MOVE "WR-LARGE-ELM-AMT-90-DAYS" TO D-FIELD
              MOVE OWR-LARGE-ELM-AMT-90-DAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-LARGE-ELY-AMT-90-DAYS NOT NUMERIC 
              MOVE "WR-LARGE-ELY-AMT-90-DAYS" TO D-FIELD
              MOVE OWR-LARGE-ELY-AMT-90-DAYS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
              IF OWR-LIFE-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-LIFE-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-LIFE-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-AH-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-AH-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-AH-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-FIRE-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-FIRE-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-FIRE-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-AUTO-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-AUTO-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-AUTO-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-UNEMP-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-UNEMP-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-UNEMP-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-AD-D-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-AD-D-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-AD-D-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-L-PROT-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-L-PROT-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-L-PROT-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NF-NON-BK-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-NF-NON-BK-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-NF-NON-BK-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NF-BK-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-NF-BK-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-NF-BK-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-ROAD-GARD-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-ROAD-GARD-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-ROAD-GARD-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-LIFE-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-LIFE-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-LIFE-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-AH-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-AH-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-AH-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-FIRE-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-FIRE-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-FIRE-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-AUTO-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-AUTO-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-AUTO-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-UNEMP-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-UNEMP-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-UNEMP-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-AD-D-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-AD-D-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-AD-D-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-L-PROT-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-L-PROT-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-L-PROT-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-NF-NON-BK-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-NF-NON-BK-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-NF-NON-BK-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-NF-BK-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-NF-BK-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-NF-BK-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-ROAD-GARD-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-ROAD-GARD-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-ROAD-GARD-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-LIFE-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-LIFE-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-LIFE-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-AH-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-AH-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-AH-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-FIRE-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-FIRE-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-FIRE-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-AUTO-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-AUTO-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-AUTO-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-UNEMP-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-UNEMP-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-UNEMP-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-AD-D-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-AD-D-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-AD-D-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-L-PROT-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-L-PROT-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-L-PROT-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NON-FILE-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-NON-FILE-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-NON-FILE-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-ROAD-GARD-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-ROAD-GARD-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-ROAD-GARD-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-LIFE-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-LIFE-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-LIFE-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-AH-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-AH-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-AH-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-FIRE-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-FIRE-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-FIRE-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-AUTO-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-AUTO-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-AUTO-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-UNEMP-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-UNEMP-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-UNEMP-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-AD-D-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-AD-D-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-AD-D-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-L-PROT-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-L-PROT-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-L-PROT-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-NON-FILE-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-NON-FILE-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-NON-FILE-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-ROAD-GARD-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-ROAD-GARD-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-NO-ROAD-GARD-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-LIFE-REFS(SUB) NOT NUMERIC 
                 MOVE "WR-LIFE-REFS(SUB)" TO D-FIELD
                 MOVE OWR-LIFE-REFS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-AH-REFS(SUB) NOT NUMERIC 
                 MOVE "WR-AH-REFS(SUB)" TO D-FIELD
                 MOVE OWR-AH-REFS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-FIRE-REFS(SUB) NOT NUMERIC 
                 MOVE "WR-FIRE-REFS(SUB)" TO D-FIELD
                 MOVE OWR-FIRE-REFS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-AUTO-REFS(SUB) NOT NUMERIC 
                 MOVE "WR-AUTO-REFS(SUB)" TO D-FIELD
                 MOVE OWR-AUTO-REFS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-UNEMP-REFS(SUB) NOT NUMERIC 
                 MOVE "WR-UNEMP-REFS(SUB)" TO D-FIELD
                 MOVE OWR-UNEMP-REFS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-AD-D-REFS(SUB) NOT NUMERIC 
                 MOVE "WR-AD-D-REFS(SUB)" TO D-FIELD
                 MOVE OWR-AD-D-REFS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-L-PROT-REFS(SUB) NOT NUMERIC 
                 MOVE "WR-L-PROT-REFS(SUB)" TO D-FIELD
                 MOVE OWR-L-PROT-REFS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NON-FILE-REFS(SUB) NOT NUMERIC 
                 MOVE "WR-NON-FILE-REFS(SUB)" TO D-FIELD
                 MOVE OWR-NON-FILE-REFS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-ROAD-GARD-REFS(SUB) NOT NUMERIC 
                 MOVE "WR-ROAD-GARD-REFS(SUB)" TO D-FIELD
                 MOVE OWR-ROAD-GARD-REFS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NF-RECOVERIES(SUB) NOT NUMERIC 
                 MOVE "WR-NF-RECOVERIES(SUB)" TO D-FIELD
                 MOVE OWR-NF-RECOVERIES(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OWR-NONFILE-ENDING-BALANCE NOT NUMERIC 
              MOVE "WR-NONFILE-ENDING-BALANCE" TO D-FIELD
              MOVE OWR-NONFILE-ENDING-BALANCE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-NO-CUST-1-YR-AGO NOT NUMERIC 
              MOVE "WR-NO-CUST-1-YR-AGO" TO D-FIELD
              MOVE OWR-NO-CUST-1-YR-AGO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-OUTS-BAL-1-YR-AGO NOT NUMERIC 
              MOVE "WR-OUTS-BAL-1-YR-AGO" TO D-FIELD
              MOVE OWR-OUTS-BAL-1-YR-AGO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-NO-CUST-1-YR-AGO NOT NUMERIC 
              MOVE "WR-WCBC-NO-CUST-1-YR-AGO" TO D-FIELD
              MOVE OWR-WCBC-NO-CUST-1-YR-AGO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-OUTS-BAL-1-YR-AGO NOT NUMERIC 
              MOVE "WR-WCBC-OUTS-BAL-1-YR-AGO" TO D-FIELD
              MOVE OWR-WCBC-OUTS-BAL-1-YR-AGO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-LARGE-NO-CUST-1-YR-AGO NOT NUMERIC 
              MOVE "WR-LARGE-NO-CUST-1-YR-AGO" TO D-FIELD
              MOVE OWR-LARGE-NO-CUST-1-YR-AGO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-LARGE-OUTS-BAL-1-YR-AGO NOT NUMERIC 
              MOVE "WR-LARGE-OUTS-BAL-1-YR-AGO" TO D-FIELD
              MOVE OWR-LARGE-OUTS-BAL-1-YR-AGO  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-CUR-NO-CONT-SLOW NOT NUMERIC 
              MOVE "WR-CUR-NO-CONT-SLOW" TO D-FIELD
              MOVE OWR-CUR-NO-CONT-SLOW  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-WCBC-CUR-NO-CONT-SLOW NOT NUMERIC 
              MOVE "WR-WCBC-CUR-NO-CONT-SLOW" TO D-FIELD
              MOVE OWR-WCBC-CUR-NO-CONT-SLOW  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-LARGE-CUR-NO-CONT-SLOW NOT NUMERIC 
              MOVE "WR-LARGE-CUR-NO-CONT-SLOW" TO D-FIELD
              MOVE OWR-LARGE-CUR-NO-CONT-SLOW  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-CASHIER-BONUS NOT NUMERIC 
              MOVE "WR-CASHIER-BONUS" TO D-FIELD
              MOVE OWR-CASHIER-BONUS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-ASST-MGR-BONUS NOT NUMERIC 
              MOVE "WR-ASST-MGR-BONUS" TO D-FIELD
              MOVE OWR-ASST-MGR-BONUS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OWR-MGR-BONUS NOT NUMERIC 
              MOVE "WR-MGR-BONUS" TO D-FIELD
              MOVE OWR-MGR-BONUS  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
              IF OWR-LOTS-NF-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-LOTS-NF-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-LOTS-NF-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-LOTS-NO-NF-CLAIMS(SUB) NOT NUMERIC 
                 MOVE "WR-LOTS-NO-NF-CLAIMS(SUB)" TO D-FIELD
                 MOVE OWR-LOTS-NO-NF-CLAIMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-LOTS-NON-FILE-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-LOTS-NON-FILE-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-LOTS-NON-FILE-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-LOTS-NO-NON-FILE-PREMS(SUB) NOT NUMERIC 
                 MOVE "WR-LOTS-NO-NON-FILE-PREMS(SUB)" TO D-FIELD
                 MOVE OWR-LOTS-NO-NON-FILE-PREMS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-LOTS-NF-RECOVERIES(SUB) NOT NUMERIC 
                 MOVE "WR-LOTS-NF-RECOVERIES(SUB)" TO D-FIELD
                 MOVE OWR-LOTS-NF-RECOVERIES(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-PRETAX-PROFIT(SUB) NOT NUMERIC 
                 MOVE "WR-PRETAX-PROFIT(SUB)" TO D-FIELD
                 MOVE OWR-PRETAX-PROFIT(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-SUPERVISION(SUB) NOT NUMERIC 
                 MOVE "WR-SUPERVISION(SUB)" TO D-FIELD
                 MOVE OWR-SUPERVISION(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OWR-NO-TAX-RETURNS(SUB) NOT NUMERIC 
                 MOVE "WR-NO-TAX-RETURNS(SUB)" TO D-FIELD
                 MOVE OWR-NO-TAX-RETURNS(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           GO TO PROCESS-FILE-WR-NEXT.

       PROCESS-FILE-WR-END.

           PERFORM CLOSE-OWR1-FILE.

       PROCESS-FILE-WR-EXIT.
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

           IF UP1534-CURRENT-FILE NOT = "00"
              MOVE SOURCE-MACHINE TO D-MACHINE.

           IF UP1534-CURRENT-FILE = "BR"
              MOVE OBR-NO TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "BM"
              MOVE OBM1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "BW"
              MOVE OBW1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "CK"
              MOVE OCK-BRNO TO HOLD-CK-BRNO
              MOVE OCK-NO   TO HOLD-CK-NO
              MOVE HOLD-CK1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "CA"
              MOVE OCA1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "CI"
              MOVE OCI1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "CJ"
              MOVE OCJ-BRNO       TO HOLD-CJ-BRNO
              MOVE OCJ-COLLID     TO HOLD-CJ-COLLID
              MOVE OCJ-DATE       TO HOLD-CJ-DATE
              MOVE OCJ-START-TIME TO HOLD-CJ-START-TIME
              MOVE OCJ-SEQNO      TO HOLD-CJ-SEQNO
              MOVE HOLD-CJ1-KEY   TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "CP"
              MOVE OCP1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "DL"
              MOVE ODL1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "DT"
              MOVE ODT1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "GB"
              MOVE OGB-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "IC"
              MOVE OIC1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "IX"
              MOVE OIX-LT-ID-ACCTNO TO HOLD-IX-LT-ID-ACCTNO
              MOVE OIX-LT-ID-SEQNO TO HOLD-IX-LT-ID-SEQNO
              MOVE OIX-LT-ID-TYPE TO HOLD-IX-LT-ID-TYPE
              MOVE OIX-LT-ID-FILLER TO HOLD-IX-LT-ID-FILLER
              MOVE HOLD-IX1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "LE"
              MOVE OLE-ACCTNO   TO HOLD-LE-ACCTNO
              MOVE OLE-TYPE     TO HOLD-LE-TYPE
              MOVE OLE-SSNO     TO HOLD-LE-SSNO
              MOVE OLE-OSACCTNO TO HOLD-LE-OSACCTNO
              MOVE HOLD-LE1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "LH"
              MOVE OLH1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "LM"
              MOVE OLM1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "LN"
              MOVE OLN1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "LP"
              MOVE OLP1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "LS"
              MOVE OLS1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "LT"
              MOVE OLT-ACCTNO    TO HOLD-LT-ACCTNO
              MOVE OLT-CODE      TO HOLD-LT-CODE
              MOVE OLT-SEQNO     TO HOLD-LT-SEQNO
              MOVE HOLD-LT1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "LX"
              MOVE OLX1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "TY"
              MOVE OTY1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "AL"
              MOVE OAL1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "BY"
              MOVE OBY1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "CD"
              MOVE OCD1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "PD" OR "CL"
              MOVE OPD1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "CP"
              MOVE OCP1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "EA"
              MOVE OEA1-KEY TO D-KEY
           ELSE
      *    IF UP1534-CURRENT-FILE = "FS"
      *       MOVE OFS1-KEY TO D-KEY
      *    ELSE
           IF UP1534-CURRENT-FILE = "GI"
              MOVE OGI1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "GL"
              MOVE OGL1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "GT"
              MOVE OGT1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "II"
              MOVE OII-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "IR"
              MOVE OIR-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "LC"
              MOVE OLC1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "PE"
              MOVE OPE1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "PW"
              MOVE OPW1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "RC"
              MOVE ORC1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "SE"
              MOVE OSE1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "SM"
              MOVE OSM1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "SN"
              MOVE OSN1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "SP"
              MOVE OSP1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "SS"
              MOVE OSS1-KEY TO D-KEY
           ELSE
           IF UP1534-CURRENT-FILE = "TRWINFO"
              MOVE OTRW-INFO-KEY TO D-KEY.
 
      * PRINT RANGES
           IF UP1534-CURRENT-FILE NOT = "00"
              MOVE UP1534-CURRENT-FILE TO D-FILE
              MOVE BR-NO               TO D-BRNO.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED                     TO LN-COUNT.
           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.

      ******************************************************************
      *
      *    P R I N T - R A N G E S
      *
      ******************************************************************
       PRINT-RANGES SECTION.

           MOVE 99 TO LN-COUNT.

           MOVE "00" TO UP1534-CURRENT-FILE.
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

           MOVE "AL-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-AL                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BY-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-BY                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CD-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-CD                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CL-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-CL                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CP-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-GLOBAL-CP                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EA-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-EA                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FS-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-FS                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "GI-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-GI                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "GL-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-GL                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "GT-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-GT                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "II-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-II                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "IR-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-IR                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LC-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-LC                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PE-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-PE                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PW-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-PW                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RC-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-RC                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SE-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-SE                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SN-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-SN                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SM-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-SM                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SP-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-SP                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SS-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-SS                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ST-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-ST                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TRW-INFO-FILE (Y/N)              :" TO R1-DESC.
           MOVE FILE-OPTION-TRWINFO                  TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TZ-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-TZ                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "WR-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-WR                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BR-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-BR                       TO R1-FIELD.
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

           MOVE "BW-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-BW                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CA-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-CA                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CI-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-CI                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CJ-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-CJ                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CK-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-CK                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CP-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-CP                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CX-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-CX                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DL-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-DL                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DT-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-DT                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "GB-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-GB                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "IC-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-IC                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "IX-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-IX                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LE-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-LE                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LH-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-LH                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LM-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-LM                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LN-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-LN                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LP-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-LP                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LS-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-LS                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LT-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-LT                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LX-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-LX                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *
      *    MOVE "PA-FILE (Y/N)                    :" TO R1-DESC.
      *    MOVE FILE-OPTION-PA                       TO R1-FIELD.
      *    MOVE 1                                    TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

           MOVE "RX-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-RX                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TY-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-TY                       TO R1-FIELD.
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
       FORM-RESET SECTION.
      ******************************************************************
     
           MOVE EXT-CONAME-SYSDATE         TO UP15VY-SYSDATE.
           DISPLAY UP15VY-SCREEN        UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE UP15VY-FORM-FIELDS         TO WR-BUF.
           MOVE UP15VY-FORM-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.
     
       FORM-RESET-EXIT.
           EXIT.
     
      ******************************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************************
     
           MOVE UP15VY-HELP--FILE          TO HELP-LINK-FILE.
           MOVE UP15VY-HELP--DIR           TO HELP-LINK-DIR.
     
       SETUP-LINK-INFO-EXIT.
           EXIT.
     
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************************
     
           MOVE STAT---GOOD                TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.
     
           EVALUATE TRUE
              COPY "LIBUP/UP15VY_EVA.CPY".
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
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPTBNO.CPY".             *> NEEDED FOR LIBLP/LPSP1IN
      *COPY "LIBUP/UP1534.CPY".
      * COPYMEMBER: LIBUP/UP1534.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBUP/UP1534.CPY
      *
      * DESCRIPTION   : ROUTINES FOR UPGRADE A15 TO A32
      *
      *                 UP1534-CREATE-DETAIL-LINE
      *                 UP1534-CREATE-BRANCH-HEAD
      *                 UP1534-CREATE-MACHINE-HEAD
      *                 UP1534-CREATE-FILE-ERROR
      *                 UP1534-CREATE-FILE-TOTALS
      *                 UP1534-LOAD-FILE-PATH
      *                 UP1534-VPI-DISPLAY
      *                 UP1534-VPI-START
      *


      ******************************************************************
      *
      *    U P 1 5 3 4 - L O A D - F I L E - P A T H
      *
      *=================================================================
      *    IN  : BR-REC
      *          UP1534-FILE-INPUT  [EXP.  XX/XXXXXX]
      *          UP1534-FILE-ORDER  [SOURCE/TARGET]
      *          UP1534-FILE-TYPE   [GLOBAL/LOCAL]
      *          UP1534-SOURCE-PATH
      *          UP1534-TARGET-PATH
      *
      *    OUT : UP1534-FILE-PATH
      *          - GLOBAL: /XXX/XXXX/R#/XX/XXXXXX
      *          - LOCAL : /XXX/XXXX/R#/XX####/XX/XXXXXX
      *
      *    DESC: LOAD FILE PATH BASED ON SOURCE/TARGET & GLOBAL/LOCAL.
      ******************************************************************
       UP1534-LOAD-FILE-PATH SECTION.

           MOVE SPACES                     TO UP1534-FILE-PATH.

           IF ( UP1534-FILE-INPUT           = SPACES ) OR
              ( UP1534-FILE-INPUT-DIR       = SPACES ) OR
              ( UP1534-FILE-INPUT-SLASH NOT = "/"    ) OR
              ( UP1534-FILE-INPUT-NAME      = SPACES )
              GO TO UP1534-LOAD-FILE-PATH-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD SOURCE GLOBAL --- /XXX/XXXX/R#/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP1534-FILE-ORDER-SOURCE ) AND
              ( UP1534-FILE-TYPE-GLOBAL  )
              MOVE SOURCE-PATH         TO UP1534-FILE-PATH
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-3
              MOVE SOURCE-MACHINE      TO UP1534-FILE-PATH-MACHINE
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-4
              MOVE UP1534-FILE-INPUT   TO UP1534-FILE-PATH-GLOBAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD SOURCE LOCAL --- /XXX/XXXX/R#/XX####/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP1534-FILE-ORDER-SOURCE ) AND
              ( UP1534-FILE-TYPE-LOCAL   )
              MOVE SOURCE-PATH         TO UP1534-FILE-PATH
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-3
              MOVE SOURCE-MACHINE      TO UP1534-FILE-PATH-MACHINE
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-4
              MOVE BR-DATA-PATH        TO UP1534-FILE-PATH-LOCAL-DATA
              MOVE "/"                 TO UP1534-FILE-PATH-LOCAL-DATA-SL
              MOVE UP1534-FILE-INPUT   TO UP1534-FILE-PATH-LOCAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD TARGET GLOBAL --- /XXX/XXXX/R#/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP1534-FILE-ORDER-TARGET ) AND
              ( UP1534-FILE-TYPE-GLOBAL  )
            MOVE TARGET-PATH         TO UP1534-FILE-PATH
            MOVE "/"                 TO UP1534-FILE-PATH-SLASH-3
            MOVE TARGET-MACHINE      TO UP1534-FILE-PATH-MACHINE
            MOVE "/"                 TO UP1534-FILE-PATH-SLASH-4
            MOVE UP1534-FILE-INPUT   TO UP1534-FILE-PATH-GLOBAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD TARGET LOCAL --- /XXX/XXXX/R#/XX####/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP1534-FILE-ORDER-TARGET ) AND
              ( UP1534-FILE-TYPE-LOCAL   )
              MOVE TARGET-PATH         TO UP1534-FILE-PATH
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-3
              MOVE TARGET-MACHINE      TO UP1534-FILE-PATH-MACHINE
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-4
              MOVE BR-DATA-PATH        TO UP1534-FILE-PATH-LOCAL-DATA
              MOVE "/"                 TO UP1534-FILE-PATH-LOCAL-DATA-SL
              MOVE UP1534-FILE-INPUT   TO UP1534-FILE-PATH-LOCAL-PATH.

       UP1534-LOAD-FILE-PATH-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - L O A D - T I M E - E L A P S E D
      *
      *=================================================================
      *    IN  : UP1534-TIME-BEG
      *          UP1534-TIME-END
      *
      *    OUT : UP1534-TIME-ELAPSED
      *
      *    DESC: USED TO GET THE ELAPSED TIME BETWEEN BEGIN AND END
      *
      *    NOTE: IF THE TIME CROSSED OVER MIDNIGHT, NEED TO ADD 24 HOURS
      *          TO THE END TIME SO TO CALCULATE THE ELAPSED TIME.
      *
      *=================================================================
      *    CALCULATION IS BASED ON CONVERTING THE TIME INTO SECONDS:
      *      1 HOUR   = 3600 SECONDS
      *      1 MINUTE = 60 SECONDS
      *      1 SECOND = 1 SECOND
      *
      *      02:34:56  = 9296
      *      02 * 3600 = 7200
      *      34 * 60   = 2040
      *      56 * 1    = 56
      *
      *      9296 = 02:34:56
      *      02 = 9296 / 3600
      *      34 = (9296 - (3600 * (9296/3600))) / 60
      *      56 = (9296 - (3600 * (9296/3600)) -
      *           ( 60 * ((9296 - (3600 * (9296/3600)))
      *
      *      9296 = 02:34:56
      *      02 = 9296 / 3600
      *      34 = (9296 - (3600 * [2]) / 60
      *      56 = (9296 - (3600 * [2]) - (60 * [34])
      *            9296 - (  7200    ) - (  2040   )
      *
      ******************************************************************
       UP1534-LOAD-TIME-ELAPSED SECTION.

           IF ( UP1534-TIME-BEG-HH > UP1534-TIME-END-HH )
              ADD 24 TO UP1534-TIME-END-HH.

           COMPUTE UP1534-TIME-SECONDS = (( UP1534-TIME-END-HH * 3600 )
                                       +  ( UP1534-TIME-END-MM * 60   )
                                       +    UP1534-TIME-END-SS         )
                                       - (( UP1534-TIME-BEG-HH * 3600 )
                                       +  ( UP1534-TIME-BEG-MM * 60   )
                                       +    UP1534-TIME-BEG-SS        ).

           COMPUTE UP1534-TIME-ELAPSED-HH = 
                   UP1534-TIME-SECONDS / 3600.

           COMPUTE UP1534-TIME-ELAPSED-MM =
                   ( UP1534-TIME-SECONDS - 
                    (3600 * UP1534-TIME-ELAPSED-HH))
                   / 60.

           COMPUTE UP1534-TIME-ELAPSED-SS =
                   UP1534-TIME-SECONDS -
                   (3600 * UP1534-TIME-ELAPSED-HH) -
                   (60 * UP1534-TIME-ELAPSED-MM).

      *=================================================================
       UP1534-LOAD-TIME-ELAP-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - V P I - D I S P L A Y 
      *
      *=================================================================
      *   IN:  UP1534-VPI-BRNO
      *        UP1534-VPI-FILE-PREFIX
      *        UP1534-VPI-FILE-TYPE
      *        UP1534-VPI-PRU-LP
      *        UP1534-VPI-ROUTE-BUF
      *        UP1534-VPI-SLEEP-COUNTER
      *        UP1534-VPI-SLEEP-COUNTER-MAX
      *        UP1534-VPI-TICKER
      *        UP1534-VPI-TIME-EDIT
      *
      *  OUT:  UP1534-VPI-SLEEP-COUNTER
      *        UP1534-VPI-TICKER
      *
      * DESC:  UP1534 VIDEO PROGRESS INDICATOR (VPI)
      *        DISPLAYS TO THE SCREEN THE BRANCH NUMBER & FILE THE
      *        PROGRAM IS CURRENTLY CONVERTING.
      *
      ******************************************************************
       UP1534-VPI-DISPLAY SECTION.

           IF ( UP1534-VPI-SLEEP-COUNTER 
              < UP1534-VPI-SLEEP-COUNTER-MAX )
              ADD 1 TO UP1534-VPI-SLEEP-COUNTER
              GO TO UP1534-VPI-DISPLAY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    BEGIN TO CREATE UP1534-VPI-DISPLAY-BUF
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES                     TO UP1534-VPI-SLEEP-COUNTER.
           MOVE SPACES                     TO UP1534-VPI-DISPLAY-BUF.

           MOVE "("                        TO UP1534-VPI-DISPLAY-LEFT.
           MOVE ")"                        TO UP1534-VPI-DISPLAY-RIGHT.

           IF ( UP1534-VPI-TICKER = "-" )
              MOVE "\"                     TO UP1534-VPI-DISPLAY-TICKER
                                              UP1534-VPI-TICKER
           ELSE
           IF ( UP1534-VPI-TICKER = "\" )
              MOVE "|"                     TO UP1534-VPI-DISPLAY-TICKER
                                              UP1534-VPI-TICKER
           ELSE
           IF ( UP1534-VPI-TICKER = "|" )
              MOVE "/"                     TO UP1534-VPI-DISPLAY-TICKER
                                              UP1534-VPI-TICKER
           ELSE
              MOVE "-"                     TO UP1534-VPI-DISPLAY-TICKER
                                              UP1534-VPI-TICKER.

           IF ( UP1534-VPI-FILE-TYPE-LOCAL )
              MOVE UP1534-VPI-BRNO         TO UP1534-VPI-DISPLAY-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SET THE FILE NAME IN THE DISPLAY
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "***FILE NOT FOUND***"     TO UP1534-VPI-DISPLAY-FILE

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ GLOBAL }
           IF ( UP1534-VPI-FILE-AL )
              MOVE "G/L ALLOCATION (AL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-BR )
              MOVE "BRANCH MASTER (BR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-BY )
              MOVE "BRANCH STATISTICS (BY)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CD )
              MOVE "CODE DESCRIPTION (CD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CL )
              MOVE "CAN LOAN (CL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-EA )
              MOVE "EXTRACTION (EA)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-EP )
              MOVE "ELECTRONIC PAYMENT (EP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-EW )
              MOVE "ELECTRONIC PAY WORK (EW)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-FD )
              MOVE "FORMS DATABASE (FD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-FS )
              MOVE "FINANCIAL STATEMENT (FS)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-GI )
              MOVE "G/L INTERFACE (GI)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-GL )
              MOVE "G/L MASTER (GL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-GT )
              MOVE "G/L TRANSACTIONS (GT)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-GQ )
              MOVE "GLOBAL QUEUE (GQ)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-II )
              MOVE "IRS IDENTIFICATION (II)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-IR )
              MOVE "INSURANCE RATE TABLE (IR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-JD )
              MOVE "G/L JOURNAL DETAIL (JD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-JH )
              MOVE "G/L JOURNAL HISTORY (JH)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LC )
              MOVE "LOAN CLASS (LC)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-PE )
              MOVE "PASSWORD EXCEPTIONS (PE)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-RB )
              MOVE "REBATE/CANCEL (RB)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-RC )
              MOVE "DAILY STATUS (RC)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-SE )
              MOVE "USER SECURITY (SE)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-SN )
              MOVE "SECURITY NAMES (SN)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-SM )
              MOVE "STATE PARAM MEMOS (SM)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-SP )
              MOVE "STATE PARAMETERS (SP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-SS )
              MOVE "CREDIT SCORE STATS (SS)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-ST )
              MOVE "STATE FILE (ST)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-TRWINFO )
              MOVE "TRW INFO FILE (TRWINFO)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-TZ )
              MOVE "TIME ZONE (TZ)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-WR )
              MOVE "WORLD RATING SHEET (WR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ LOCAL  }
           IF ( UP1534-VPI-FILE-BM )
              MOVE "BORROWER MEMO (BM)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-BW )
              MOVE "BORROWER MASTER (BW)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CA )
              MOVE "CASH DRAWER (CA)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CI )
              MOVE "CUSTOMER INFORMATION (CI)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CJ )
              MOVE "COLLECTOR JOURNAL (CJ)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CK )
              MOVE "LOAN CHECK MASTER (CK)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CP )
              MOVE "COLLECTOR POOL (CP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CQ )
              MOVE "COLLECTOR QUEUE (CQ)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-DL )
              MOVE "DEALER MASTER (DL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-DS )
              MOVE "DEALER STATISTICS (DS)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-DT )
              MOVE "DEALER TRANSACTION (DT)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-ES )
              MOVE "LOAN ESCROW (ES)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-GB )
              MOVE "GLOBAL (GB)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-IC )
              MOVE "INSURANCE COMPANY (IC)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-IR )
              MOVE "INSURANCE RATE (IR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-IX )
              MOVE "ALTERNATE INDEX (IX)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LE )
              MOVE "LOAN ENDORSER (LE)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LH )
              MOVE "LOAN HISTORY (LH)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LM )
              MOVE "LOAN MEMO (LM)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LN )
              MOVE "LOAN MASTER (LN)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LP )
              MOVE "LOAN PAYMENT (LP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LS )
              MOVE "LOAN STATUS TRAILER (LS)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSD )
              MOVE "LOAN STATUS - DEFICIENCY (LSD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSF )
              MOVE "LOAN STATUS - FOLLOW UP (LSF)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSG )
              MOVE "LOAN STATUS - GPS STARTER (LSG)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSJ )
              MOVE "LOAN STATUS - JUDGEMENT (LSJ)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSL )
              MOVE "LOAN STATUS - LEGEL (LSL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSM )
              MOVE "LOAN STATUS - MISC (LSM)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSP )
              MOVE "LOAN STATUS - PAID THRU (LSP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSR )
              MOVE "LOAN STATUS - RESCHEDULE (LSR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSX )
              MOVE "LOAN STATUS - REPO (LSX)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LT )
              MOVE "LOAN TRAILER (LT)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTA )
              MOVE "LOAN TRAILER - AUTO INS (LTA)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTB )
              MOVE "LOAN TRAILER - BANKRUPTCY (LTB)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTC )
              MOVE "LOAN TRAILER - COLLATERAL (LTC)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTD )
              MOVE "LOAN TRAILER - DEBIT CARD (LTD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTF )
              MOVE "LOAN TRAILER - FEE CODE (LTF)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTI )
              MOVE "LOAN TRAILER - INSURANCE (LTI)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTL )
              MOVE "LOAN TRAILER - LIENHOLDER (LTL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTP )
              MOVE "LOAN TRAILER - PYMT SCHEDULE (LTP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTR )
              MOVE "LOAN TRAILER - REVOLVING (LTR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LX )
              MOVE "LOAN PAYMENT ADDON (LX)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-MP )
              MOVE "LOAN MEMO PAYMENT (MP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-PA )
              MOVE "PAYEE (PA)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-PC )
              MOVE "LOAN SCRATCH COLLATERAL (PC)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-PD )
              MOVE "LOAN SCRATCH PAD (PD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-TY )
              MOVE "TODAY'S TOTAL STATISTICS (TY)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -{ MISC }
           IF ( UP1534-VPI-FILE-WK1 )
              MOVE "WORK FILE 1 (WK1)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-WK2 )
              MOVE "WORK FILE 2 (WK2)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-REPORT )
              MOVE "UP1534 REPORT (REPORT)"
                TO UP1534-VPI-DISPLAY-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE UP1534-VPI-TIME-EDIT       TO UP1534-VPI-DISPLAY-TIME.
           CALL "C$JUSTIFY" USING UP1534-VPI-DISPLAY-TEXT "L".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SENDING THE 'PROGRESS INDICTATOR' LINE DISPLAY TO THE SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP1534-VPI-ROUTE-MENU-DRIVEN     ) AND
              ( UP1534-VPI-QUEUE-PRINTER-OR-VIEW )
              MOVE UP1534-VPI-DISPLAY-BUF  TO WR-BUF
              MOVE UP1534-VPI-DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      *=================================================================
       UP1534-VPI-DISPLAY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - V P I - S T A R T
      *
      *=================================================================
      *   IN:  UP1534-CURRENT-FILE
      *        UP1534-FILE-TYPE
      *        UP1534-PRU-LP
      *
      *  OUT:  UP1534-VPI WORKERS
      *
      * DESC:  SETTING THE VIDEO PROGRESS INDICATOR (VPI) WORKERS FOR
      *        THE START OF FILE UPGRADE.
      *
      ******************************************************************
       UP1534-VPI-START SECTION.

           IF ( UP1534-FILE-TYPE-LOCAL )
              MOVE BR-NO                   TO UP1534-VPI-BRNO
           ELSE
              MOVE ZEROES                  TO UP1534-VPI-BRNO.

           MOVE UP1534-CURRENT-FILE        TO UP1534-VPI-FILE-PREFIX.
           MOVE UP1534-FILE-TYPE           TO UP1534-VPI-FILE-TYPE.
      *KC  MOVE UP1534-PRU-LP              TO UP1534-VPI-PRU-LP.
           MOVE PRU-LP                     TO UP1534-VPI-PRU-LP.
           MOVE EXT-ROUTE-BUF              TO UP1534-VPI-ROUTE-BUF.
           MOVE 9999                       TO UP1534-VPI-SLEEP-COUNTER.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO UP1534-VPI-TIME-EDIT.

      *=================================================================
       UP1534-VPI-START-EXIT.
           EXIT.


      ******************************************************************
      *
      *     M I S C E L L A N E O U S - P A R A G R A P H S  
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPH SECTION.
       CONVERT-YYYMMDD-TO-MMDDYY.
           MOVE DATE-YYYMMDD-MM  TO DATE-MMDDYY-MM.
           MOVE DATE-YYYMMDD-DD  TO DATE-MMDDYY-DD.
           MOVE DATE-YYYMMDD-YYY TO DATE-MMDDYY-YY.
       CONVERT-YYYMM-TO-MMDDYY.
           MOVE DATE-YYYMM-MM  TO DATE-MMDDYY-MM.
           MOVE ZEROES         TO DATE-MMDDYY-DD.
           MOVE DATE-YYYMM-YYY TO DATE-MMDDYY-YY.
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       VERIFY-TY-ACTIVITY-TBL.
           IF OTY-NO(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-NO" TO D-FIELD
              MOVE OTY-NO(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-AMT(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-AMT" TO D-FIELD
              MOVE OTY-AMT(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-O2(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-O2" TO D-FIELD
              MOVE OTY-O2(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-INT(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-INT" TO D-FIELD
              MOVE OTY-INT(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

       VERIFY-TY-REPO-ACTIVITY-TBL.
           IF OTY-NO-REPO(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-NO-REPO" TO D-FIELD
              MOVE OTY-NO-REPO(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-AMT-REPO(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-AMT-REPO" TO D-FIELD
              MOVE OTY-AMT-REPO(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-O2-REPO(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-O2-REPO" TO D-FIELD
              MOVE OTY-O2-REPO(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-INT-REPO(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-INT-REPO" TO D-FIELD
              MOVE OTY-INT-REPO(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

       VERIFY-TY-PL-ACTIVITY-TBL.
           IF OTY-NO-PL(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-NO-PL" TO D-FIELD
              MOVE OTY-NO-PL(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-AMT-PL(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-AMT-PL" TO D-FIELD
              MOVE OTY-AMT-PL(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-O2-PL(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-O2-PL" TO D-FIELD
              MOVE OTY-O2-PL(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-INT-PL(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-INT-PL" TO D-FIELD
              MOVE OTY-INT-PL(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

       VERIFY-TY-DLR-ACTIVITY-TBL.
           IF OTY-DLRVPRIN(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-DLRVPRIN" TO D-FIELD
              MOVE OTY-DLRVPRIN(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-DLRVCHGS(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-DLRVCHGS" TO D-FIELD
              MOVE OTY-DLRVCHGS(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-DLPARVHELD(TYSUB1) NOT NUMERIC 
              MOVE "TY-DLPARVHELD" TO D-FIELD
              MOVE OTY-DLPARVHELD(TYSUB1)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
       VERIFY-TY-DEL-BUCKETS.
           IF OTY-UNITS(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-UNITS(TYSUB1 TYSUB2)" TO D-FIELD
              MOVE OTY-UNITS(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-OUTBAL(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-OUTBAL(TYSUB1 TYSUB2)" TO D-FIELD
              MOVE OTY-OUTBAL(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-UNITS-BNKRPT(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-UNITS-BNKRPT(TYSUB1 TYSUB2)" TO D-FIELD
              MOVE OTY-UNITS-BNKRPT(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTY-OUTBAL-BNKRPT(TYSUB1 TYSUB2) NOT NUMERIC 
              MOVE "TY-OUTBAL-BNKRPT(TYSUB1 TYSUB2)" TO D-FIELD
              MOVE OTY-OUTBAL-BNKRPT(TYSUB1 TYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

       VERIFY-BY-PERIOD-TBL.
           IF OBY-NO(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-NO" TO D-FIELD
              MOVE OBY-NO(BYSUB1 BYSUB2 BYSUB3)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-AMT(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-AMT" TO D-FIELD
              MOVE OBY-AMT(BYSUB1 BYSUB2 BYSUB3)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-O2(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-O2" TO D-FIELD
              MOVE OBY-O2(BYSUB1 BYSUB2 BYSUB3)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-INT(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-INT" TO D-FIELD
              MOVE OBY-INT(BYSUB1 BYSUB2 BYSUB3)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
       VERIFY-BY-REPO-ACTIVITY-TBL.
           IF OBY-NO-REPO(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-NO-REPO" TO D-FIELD
              MOVE OBY-NO-REPO(BYSUB1 BYSUB2 BYSUB3)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-AMT-REPO(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-AMT-REPO" TO D-FIELD
              MOVE OBY-AMT-REPO(BYSUB1 BYSUB2 BYSUB3)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-O2-REPO(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-O2-REPO" TO D-FIELD
              MOVE OBY-O2-REPO(BYSUB1 BYSUB2 BYSUB3)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-INT-REPO(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-INT-REPO" TO D-FIELD
              MOVE OBY-INT-REPO(BYSUB1 BYSUB2 BYSUB3)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
       VERIFY-BY-PL-ACTIVITY-TBL.
           IF OBY-NO-PL(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-NO-PL" TO D-FIELD
              MOVE OBY-NO-PL(BYSUB1 BYSUB2 BYSUB3)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-AMT-PL(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-AMT-PL" TO D-FIELD
              MOVE OBY-AMT-PL(BYSUB1 BYSUB2 BYSUB3)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-O2-PL(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-O2-PL" TO D-FIELD
              MOVE OBY-O2-PL(BYSUB1 BYSUB2 BYSUB3)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-INT-PL(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-INT-PL" TO D-FIELD
              MOVE OBY-INT-PL(BYSUB1 BYSUB2 BYSUB3)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
       VERIFY-BY-DLR-ACTIVITY-TBL.
           IF OBY-DLRVPRIN(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-DLRVPRIN" TO D-FIELD
              MOVE OBY-DLRVPRIN(BYSUB1 BYSUB2 BYSUB3)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-DLRVCHGS(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-DLRVCHGS" TO D-FIELD
              MOVE OBY-DLRVCHGS(BYSUB1 BYSUB2 BYSUB3)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-DLPARVHELD(BYSUB1 BYSUB2) NOT NUMERIC 
              MOVE "BY-DLPARVHELD" TO D-FIELD
              MOVE OBY-DLPARVHELD(BYSUB1 BYSUB2)  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
       VERIFY-BY-DEL-BUCKETS.
           IF OBY-UNITS(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-UNITS" TO D-FIELD
              MOVE OBY-UNITS(BYSUB1 BYSUB2 BYSUB3) TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-OUTBAL(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-OUTBAL" TO D-FIELD
              MOVE OBY-OUTBAL(BYSUB1 BYSUB2 BYSUB3) TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-UNITS-BNKRPT(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-UNITS-BNKRPT" TO D-FIELD
              MOVE OBY-UNITS-BNKRPT(BYSUB1 BYSUB2 BYSUB3) TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OBY-OUTBAL-BNKRPT(BYSUB1 BYSUB2 BYSUB3) NOT NUMERIC 
              MOVE "BY-OUTBAL-BNKRPT" TO D-FIELD
              MOVE OBY-OUTBAL-BNKRPT(BYSUB1 BYSUB2 BYSUB3) TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------------

       CHECK-LE-R-DATES.
           IF OLE-LOANDATE NOT NUMERIC OR
               (OLE-LOANDATE = 202020)
              MOVE "LE-LOANDATE" TO D-FIELD
              MOVE OLE-LOANDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLE-LOANDATE NOT = 0
                 MOVE OLE-LOANDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LE-LOANDATE" TO D-FIELD
                    MOVE OLE-LOANDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OLE-POFFDATE NOT NUMERIC OR
               (OLE-POFFDATE = 202020)
              MOVE "LE-POFFDATE" TO D-FIELD
              MOVE OLE-POFFDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLE-POFFDATE NOT = 0
                 MOVE OLE-POFFDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LE-POFFDATE" TO D-FIELD
                    MOVE OLE-POFFDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
       CHECK-LE-DATES.
           IF OLE-OSDATE NOT NUMERIC OR
               (OLE-OSDATE = 202020)
              MOVE "LE-OSDATE" TO D-FIELD
              MOVE OLE-OSDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLE-OSDATE NOT = 0
                 MOVE OLE-OSDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LE-OSDATE" TO D-FIELD
                    MOVE OLE-OSDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OLE-DATE NOT NUMERIC OR
               (OLE-DATE = 202020)
              MOVE "LE-DATE" TO D-FIELD
              MOVE OLE-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OLE-DATE NOT = 0
                 MOVE OLE-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "LE-DATE" TO D-FIELD
                    MOVE OLE-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

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


      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".

       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       COPY "LIBUP/UP1534_IO.CPY".

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
       OPEN-OB21-FILE.
           PERFORM OPEN-IT.
           MOVE OB2-PATH TO E-FILE.
           OPEN I-O OB2-FILE.
           IF IO-FG = 8
             GO TO OPEN-OB21-FILE.
           IF IO-FG = 7
             CLOSE OB2-FILE
             GO TO OPEN-OB21-FILE.
       READ-OB21-FILE.
           PERFORM READ-IT.
           MOVE OB2-PATH TO E-FILE.
           MOVE OB21-KEY TO E-KEYX.
           READ OB2-FILE.
           IF IO-FG = 8
              GO TO READ-OB21-FILE.
       CLOSE-OB21-FILE.
           PERFORM CLOSE-IT.
           CLOSE OB2-FILE.


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

           IF EXIT-PRIOR-TO-EXECUTION
              GO TO EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

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
              DESTROY UP15VY-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
 
