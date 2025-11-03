       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TR15VY.
       DATE-WRITTEN.  06-11-2020.
      ******************************************************************
      *
      *  IDENTIFICATION: UP/TR15VY.CBL
      *
      *  DESCRIPTION   : VERIFY NUMERICS IN FILES, USES O A15 FILES
      *
      *    CH [LP01CH; LOCAL ]
      *    TR [LP01TR; LOCAL ]
      *
      *=================================================================
      * REV:
      *
      * BAH 20220921 ADDED TP-LCDUE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      *=============================================[ SOURCE FILE - CH ]
           SELECT OCH-FILE ASSIGN TO OCH-PATH
                  ORGANIZATION SEQUENTIAL
                  ACCESS SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

      *========================================[ SOURCE FILE - TR (LP) ]
           SELECT OTR-FILE ASSIGN TO OTR-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OTR1-KEY
                  FILE STATUS FILE-STAT.

      * USING FOR ORIGINAL MACHINE
           SELECT OBR-FILE ASSIGN TO OBR-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY OBR-KEY
                  FILE STATUS FILE-STAT.

      ******************************************************************
      ******************************************************************
      ******************************************************************
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      * USING FOR BR-MACHINE IF FIX WAS ALREADY RUN
       FD  OBR-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01BR.CPY" REPLACING LEADING "BR" BY "OBR".

      *=============================================[ SOURCE FILE - CH ]
       FD  OCH-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/LP01CH.CPY" REPLACING LEADING "CH" BY "OCH".

      *========================================[ SOURCE FILE - TR (LP) ]
       FD  OTR-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       78  OTR-SIZE                     VALUE 550.
       01  OTR-REC.
           03  OTR1-KEY.
               05  OTR1-DAY              PIC 9(2)      COMP.
               05  OTR1-BRNO             PIC 9(4)      COMP.
               05  OTR1-CODE             PIC X.
                   88  OTR1-VALID-CD-B   VALUE "B".
                   88  OTR1-VALID-CD-C   VALUE "C".
                   88  OTR1-VALID-CD-D   VALUE "D".
                   88  OTR1-VALID-CD-G   VALUE "G".
                   88  OTR1-VALID-CD-I   VALUE "I".
                   88  OTR1-VALID-CD-J   VALUE "J".
                   88  OTR1-VALID-CD-K   VALUE "K".
                   88  OTR1-VALID-CD-L   VALUE "L".
                   88  OTR1-VALID-CD-P   VALUE "P".
                   88  OTR1-VALID-CD-S   VALUE "S".
                   88  OTR1-VALID-CD-X   VALUE "X".
                   88  OTR1-VALID-CD-ALL VALUE "B" "C" "D" "G" "J"
                                               "K" "L" "P" "S" "X" "I".
               05  OTR1-CLASS            PIC 99        COMP.
               05  OTR1-SUBKEY           PIC X(9).
           03  FILLER                   PIC X(536).
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/LPFDTRB.CPY" REPLACING LEADING "TB" BY "OTB".
       COPY "LIBUP/A15SRC/LPFDTRC.CPY" REPLACING LEADING "TC" BY "OTC".
       COPY "LIBUP/A15SRC/LPFDTRD.CPY" REPLACING LEADING "TD" BY "OTD".
       COPY "LIBUP/A15SRC/LPFDTRF.CPY" REPLACING LEADING "TF" BY "OTF".
      *COPY "LIBUP/A15SRC/LPFDTRG.CPY" REPLACING LEADING "TG" BY "OTG".
       COPY "LIBUP/A15SRC/LPFDTRI.CPY" REPLACING LEADING "TI" BY "OTI".
       COPY "LIBUP/A15SRC/LPFDTRJ.CPY" REPLACING LEADING "TJ" BY "OTJ".
       COPY "LIBUP/A15SRC/LPFDTRK.CPY" REPLACING LEADING "TK" BY "OTK".
       COPY "LIBUP/A15SRC/LPFDTRL.CPY" REPLACING LEADING "TL" BY "OTL".
       COPY "LIBUP/A15SRC/LPFDTRP.CPY" REPLACING LEADING "TP" BY "OTP".
       COPY "LIBUP/A15SRC/LPFDTRS.CPY" REPLACING LEADING "TS" BY "OTS".
       COPY "LIBUP/A15SRC/LPFDTRW.CPY" REPLACING LEADING "TW" BY "OTW".
       COPY "LIBUP/A15SRC/LPFDTRX.CPY" REPLACING LEADING "TX" BY "OTX".


      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UP/TR15VY.CBL A32 06/12/20-09:34:21 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".

       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LPWSTRB.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LPWSTRD.CPY".
       COPY "LIBLP/LPWSTRI.CPY".
       COPY "LIBLP/LPWSTRJ.CPY".
       COPY "LIBLP/LPWSTRK.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LPWSTRW.CPY".
       COPY "LIBLP/LPWSTRX.CPY".

      *-------------------------------------------------{ SOURCE PATHS }
       01  OCH-PATH                    PIC X(40)     VALUE SPACES.
       01  OTR-PATH                    PIC X(40)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  PR-IFN                      PIC X(02)     VALUE "PR".
       01  OBR-KEY                     PIC 9(4).
       01  OBR-PATH                    PIC X(35)     VALUE SPACES.

       01  HOLD-CH-KEY                 PIC X(34) VALUE SPACES.
       01  HOLD-CH-SEQ                 PIC 9(3)  VALUE 0.

       01  TRFILE-MMYY                 PIC 9(04)     VALUE ZEROES.
       01  CHFILE-MMYY                 REDEFINES TRFILE-MMYY
                                       PIC 9(04).

       01  TRFILE-YYYYMM               PIC 9(06)     VALUE ZEROES.
       01  FILLER                      REDEFINES TRFILE-YYYYMM.
           03  TRFILE-YYYYMM-YYYY      PIC 9(04).
           03  TRFILE-YYYYMM-MM        PIC 9(02).
       01  CHFILE-YYYYMM               REDEFINES TRFILE-YYYYMM.
           03  CHFILE-YYYYMM-YYYY      PIC 9(04).
           03  CHFILE-YYYYMM-MM        PIC 9(02).

       01 HOLD-OTR1-KEY.
          03  HOLD-OTR1-DAY              PIC 9(2).
          03  HOLD-OTR1-BRNO             PIC 9(4).
          03  HOLD-OTR1-CODE             PIC X.
          03  HOLD-OTR1-CLASS            PIC 99.

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  CASE-BUF                    PIC X(04)     VALUE SPACES.

      *- - - - - - - - - - - - - - - - - -{ EDIT DISPLAY FORMAT - EDF }
       01  EDF-WORKERS.
           03  EDF-SORT-TIME-BEG       PIC 9.9(7).
           03  EDF-SORT-TIME-END       PIC 9.9(7).
           03  EDF-SORT-TIME-ELAPSED   PIC 9.9(7).

       01  ERRCD                       PIC X(01)     VALUE SPACES.
       01  ERRCD-LIST                  PIC X(06)     VALUE "ERRCD.".

       01  EXITPROG                    PIC X(09)     VALUE "UP/UPMENU".
       01  EXITPROG-LIST               PIC X(09)     VALUE "EXITPROG.".

      *-----------------------------------------------[ HEX CHARACTERS ]
       01  HEX-CARRIAGE-RETURN         PIC X(01)     VALUE X"0D".
       01  HEX-LINE-FEED               PIC X(01)     VALUE X"0A".
       01  HEX-TAB                     PIC X(01)     VALUE X"09".

      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED                 PIC 9(03)     VALUE ZEROES.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.

       01  ER-FEED                     PIC 9(01)     VALUE 1.
       01  PAGE-NUMBER                 PIC 9(04)     VALUE ZEROES.

       01  BRANCH-TABLE.
           03  BRANCH-MACHINE          PIC XX OCCURS 9999 TIMES.

       01  SUBSCRIPT-WORKERS.
           03  ELE                     PIC 9(04)     VALUE ZEROES.
           03  SUB                     PIC 9(04)     VALUE ZEROES.
           03  TBSUB     REDEFINES SUB PIC 9(04).
           03  TJSUB     REDEFINES SUB PIC 9(03).
           03  TKSUB     REDEFINES SUB PIC 9(03).
           03  S1                      PIC 9(03)     VALUE ZEROES.
           03  SUB1      REDEFINES S1  PIC 9(03).
           03  S2                      PIC 9(03)     VALUE ZEROES.
           03  SUB2      REDEFINES S2  PIC 9(03).
           03  S3                      PIC 9(03)     VALUE ZEROES.
           03  SUB3      REDEFINES S3  PIC 9(03).

       01  TEST-TRCD                   PIC X(02).
           88  ADDON-TRCD              VALUE "RT" "LP" "NP" "ET" "ST".
           88  DEFER-TRCD              VALUE "DF" "D2" "D3" "D4" "D5" 
                                             "D6" "D7" "D8" "D9".
           88  PMT-TRCD                VALUE "PY" "PA".
           88  REBATE-TRCD             VALUE "RC" "RA" "RP" "R1" "R2"
                                             "R3" "RI" "RS" "RM" "RD"
                                             "CC" "CA" "CP" "C1" "C2"
                                             "C3" "CD".
           88  RESERVE-TRCD            VALUE "PR" "PC" "PP".
           88  RESERVE-P4P5-TRCD       VALUE "P4" "P5".
                                     
       01  TEST-REFNO                  PIC X(05)     VALUE SPACES.
       01  TEST-REFNO-TRANSFER         REDEFINES TEST-REFNO.
           03  TEST-FROMCL             PIC X(02).
           03  FILLER                  PIC X(01).
           03  TEST-TOCL               PIC X(02).

       01  TEST-ACCTNO                 PIC 9(8).
       01  FILLER REDEFINES TEST-ACCTNO.
             03  TEST-NUMBER           PIC 9(6).
             03  TEST-SEQ              PIC 9(2).    

      *=================================================[ UPGRADE-STAT ]
       78  UPGRADE-STAT---GOOD                     VALUE "00".
       78  UPGRADE-STAT---BAD                      VALUE "XX".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       77  UPGRADE-STAT                PIC X(02)   VALUE SPACES.
           88  UPGRADE-STAT-GOOD                   VALUE "00".
           88  UPGRADE-STAT-BAD                    VALUE "  " THRU "//"
                                                         "01" THRU "~~".

      *=================================================================
       01  WSS-CH-TIME                 PIC 9(06)   VALUE ZEROES.
       01  FILLER            REDEFINES WSS-CH-TIME.
           03  WSS-CH-HR               PIC 9(02).
           03  WSS-CH-MIN              PIC 9(02).
           03  WSS-CH-SEC              PIC 9(02).

       01  WSS-DATE-YYYYSMM            PIC 9999/99.

      *-----------------------------------------------------------------
       01  WSS-MSEXCEL-TIME-INPUT      PIC 9(6)      VALUE ZEROES.
       01  FILLER            REDEFINES WSS-MSEXCEL-TIME-INPUT.
           03  WSS-MSEXCEL-TIME-INPUT-HH   PIC 9(2).
           03  WSS-MSEXCEL-TIME-INPUT-MM   PIC 9(2).
           03  WSS-MSEXCEL-TIME-INPUT-SS   PIC 9(2).

       01  WSS-MSEXCEL-TIME-OUTPUT     PIC 9V9(7)    VALUE ZEROES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-PRINTER-OPTIONS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER PRINT".
           03  FILLER PIC X(30) VALUE "ER OPTIONS                    ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-SOURCE-PATH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER SOURC".
           03  FILLER PIC X(30) VALUE "E PATH                        ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-TARGET-PATH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER TARGET PATH               ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, UP/DOWN, ENTER BEGINNING".
           03  FILLER PIC X(19) VALUE " BRANCH NUMBER     ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER ENDING BRANCH NUMBER      ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-BEG-DATE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER BEGINNING DATE (MMYY)     ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-END-DATE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER ENDING DATE (MMYY)        ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-OPTION-FILES.
           03  FILLER PIC X(30) VALUE "F1 THRU F7, F8-HELP, UP/DOWN, ".
           03  FILLER PIC X(30) VALUE "ENTER (Y)ES/(N)O/(C)HECK FOR U".
           03  FILLER PIC X(19) VALUE "PDATING THIS FILE  ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-ACCEPT.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER TO ACCEPT (YES/NO)        ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-BEGIN-PROCESSING.
           03  FILLER PIC X(30) VALUE "PROCESSING....................".
           03  FILLER PIC X(30) VALUE "..............................".
           03  FILLER PIC X(19) VALUE "...................".

      ******************************************************************
      *
      *    M E S S A G E - W O R K E R S
      *
      ******************************************************************
       01  MESSAGE-INVALID-YN.
           03  FILLER PIC X(30) VALUE "INVALID: (Y)ES OR (N)O ONLY!!!".
           03  FILLER PIC X(30) VALUE "                              ".

       01  MESSAGE-INVALID-YNC.
           03  FILLER PIC X(30) VALUE "INVALID: (Y)ES OR (N)O OR (A)S".
           03  FILLER PIC X(30) VALUE "CII OR (C)HECK FILE ONLY!!!   ".

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
               " VERIFY A15 TRANSACTON FILES/CHFILE".

      *=================================================================
       01  DETAIL-LINE                PIC X(80)   VALUE SPACES.
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

           03  FILLER                         PIC X(62).

      ******************************************************************
      *
      *    T R 1 5 3 0 - W O R K E R S
      *
      ******************************************************************
       01  TR15VY-RECORD-COUNTERS                    VALUE ZEROES.
           03  TR15VY-RECORD-COUNTER-READ            PIC 9(10).
           03  TR15VY-RECORD-COUNTER-WRITE           PIC 9(10).
           03  TR15VY-RECORD-COUNTER-SKIP            PIC 9(10).

       01  TR15VY-RECORDS-FOUND        PIC X(01).
           88  TR15VY-NO-RECORDS-FOUND               VALUE "N".
           88  TR15VY-EXIT-PRIOR-TO-EXECUTION        VALUE "X".

       01  TR15VY-UPDATE-KEY           PIC X(40)     VALUE SPACES.
       01  FILLER                      REDEFINES TR15VY-UPDATE-KEY.
           03  TR15VY-OCH-BRNO         PIC 9(04).
           03  TR15VY-OCH-DAY          PIC 9(02).
           03  TR15VY-OCH-CODE         PIC X(03).
           03  TR15VY-OCH-USERID       PIC X(10).
           03  TR15VY-OCH-TIME         PIC 9(06).
           03  TR15VY-OCH-SEQ          PIC 9(02).
       01  FILLER                      REDEFINES TR15VY-UPDATE-KEY.
           03  TR15VY-OLPTR1-DAY       PIC 9(02).
           03  TR15VY-OLPTR1-BRNO      PIC 9(04).
           03  TR15VY-OLPTR1-CODE      PIC X(01).
           03  TR15VY-OLPTR1-CLASS     PIC 9(02).
      *KC> 03  TR15VY-OLPTR1-SUBKEY    PIC X(09).

       01  TR15VY-UPDATE-STAT          PIC X(05)     VALUE SPACES.

       01  TR15VY-UPDATE-TYPE          PIC X(01)     VALUE SPACES.
           88  TR15VY-UPDATE-TYPE-ERROR              VALUE "E".
           88  TR15VY-UPDATE-TYPE-SKIP               VALUE SPACES.

      ******************************************************************
      *
      *    T R 1 5 3 0 - F I L E - W O R K E R S
      *
      ******************************************************************
       01  TR15VY-CURRENT-FILE                 PIC X(08)   VALUE SPACES.
       01  TR15VY-CURRENT-MMYY                 PIC 9(04)   VALUE ZEROES.

       01  TR15VY-FILE-BEG-TIME                PIC 9(06)   VALUE ZEROES.
       01  FILLER REDEFINES TR15VY-FILE-BEG-TIME.
           03  TR15VY-FILE-BEG-TIME-HH         PIC 9(02).
           03  TR15VY-FILE-BEG-TIME-MM         PIC 9(02).
           03  TR15VY-FILE-BEG-TIME-SS         PIC 9(02).

       01  TR15VY-FILE-INPUT                   PIC X(22)   VALUE SPACES.
       01  FILLER                    REDEFINES TR15VY-FILE-INPUT.
           03  TR15VY-FILE-INPUT-DIR           PIC X(02).
           03  TR15VY-FILE-INPUT-SLASH         PIC X(01).
           03  TR15VY-FILE-INPUT-NAME          PIC X(19).

      *-----------------------------------------------------------------
       78  TR15VY-FILE-ORDER---SOURCE                      VALUE "S".
       78  TR15VY-FILE-ORDER---TARGET                      VALUE "T".
       77  TR15VY-FILE-ORDER                   PIC X(01)   VALUE SPACES.
           88  TR15VY-FILE-ORDER-SOURCE                    VALUE "S".
           88  TR15VY-FILE-ORDER-TARGET                    VALUE "T".

      *-----------------------------------------------------------------
       78  TR15VY-FILE-TYPE---BASESH                       VALUE "B".
       78  TR15VY-FILE-TYPE---GLOBAL                       VALUE "G".
       78  TR15VY-FILE-TYPE---LOCAL                        VALUE "L".
       77  TR15VY-FILE-TYPE                    PIC X(01)   VALUE SPACES.
           88  TR15VY-FILE-TYPE-BASESH                     VALUE "B".
           88  TR15VY-FILE-TYPE-GLOBAL                     VALUE "G".
           88  TR15VY-FILE-TYPE-LOCAL                      VALUE "L".

      *=================================================================
       01  TR15VY-FILE-PATH                          PIC X(35).

       01  FILLER                          REDEFINES TR15VY-FILE-PATH.
           03  FILLER                                PIC X(01).
           03  TR15VY-FILE-PATH-FIL                  PIC X(34).

       01  FILLER                          REDEFINES TR15VY-FILE-PATH.
           03  TR15VY-FILE-PATH-SLASH-1              PIC X(01).
           03  TR15VY-FILE-PATH-DIR-1                PIC X(03).
           03  TR15VY-FILE-PATH-SLASH-2              PIC X(01).
           03  TR15VY-FILE-PATH-DIR-2                PIC X(04).
           03  TR15VY-FILE-PATH-SLASH-3              PIC X(01).
           03  TR15VY-FILE-PATH-MACHINE              PIC X(02).
           03  TR15VY-FILE-PATH-SLASH-4              PIC X(01).
           03  TR15VY-FILE-PATH-GLOBAL-LOCAL         PIC X(22).

       01  FILLER                          REDEFINES TR15VY-FILE-PATH.
           03  FILLER                                PIC X(13).
           03  TR15VY-FILE-PATH-GLOBAL-PATH.
               05  TR15VY-FILE-PATH-GLOBAL-DIR       PIC X(02).
               05  TR15VY-FILE-PATH-GLOBAL-SLASH     PIC X(01).
               05  TR15VY-FILE-PATH-GLOBAL-FILE      PIC X(19).

       01  FILLER                          REDEFINES TR15VY-FILE-PATH.
           03  TR15VY-FILLER                         PIC X(13).
           03  TR15VY-FILE-PATH-LOCAL-DATA           PIC X(06).
           03  TR15VY-FILE-PATH-LOCAL-DATA-SL        PIC X(01).
           03  TR15VY-FILE-PATH-LOCAL-PATH.
               05  TR15VY-FILE-PATH-LOCAL-DIR        PIC X(02).
               05  TR15VY-FILE-PATH-LOCAL-SLASH      PIC X(01).
               05  TR15VY-FILE-PATH-LOCAL-FILE       PIC X(12).

      *=================================================================
       01  TR15VY-TIME-BEG                     PIC 9(06)   VALUE ZEROES.
       01  TR15VY-TIME-BEGX          REDEFINES TR15VY-TIME-BEG
                                               PIC X(06).
       01  FILLER                    REDEFINES TR15VY-TIME-BEG.
           03  TR15VY-TIME-BEG-HH              PIC 9(02).
           03  TR15VY-TIME-BEG-MM              PIC 9(02).
           03  TR15VY-TIME-BEG-SS              PIC 9(02).

       01  TR15VY-TIME-END                     PIC 9(06)   VALUE ZEROES.
       01  TR15VY-TIME-ENDX          REDEFINES TR15VY-TIME-END
                                               PIC X(06).
       01  FILLER                    REDEFINES TR15VY-TIME-END.
           03  TR15VY-TIME-END-HH              PIC 9(02).
           03  TR15VY-TIME-END-MM              PIC 9(02).
           03  TR15VY-TIME-END-SS              PIC 9(02).

       01  TR15VY-TIME-ELAPSED                 PIC 9(06)   VALUE ZEROES.
       01  FILLER REDEFINES TR15VY-TIME-ELAPSED.
           03  TR15VY-TIME-ELAPSED-HH          PIC 9(02).
           03  TR15VY-TIME-ELAPSED-MM          PIC 9(02).
           03  TR15VY-TIME-ELAPSED-SS          PIC 9(02).

       01  TR15VY-TIME-SECONDS                 PIC 9(06)   VALUE ZEROES.

      ******************************************************************
      *
      *    T R 1 5 3 0 - D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  TR15VY-LN-FEED                     PIC 9(03)    VALUE ZEROES.

      *=================================================================
       01  TR15VY-DETAIL-LINE                 PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  TR15VY-DETAIL-LINE-1     REDEFINES TR15VY-DETAIL-LINE.
           03  TR15VY-D1-BRANCH-PROMPT        PIC X(08).
           03  TR15VY-D1-BRANCH               PIC ZZZ9.
           03  FILLER                         PIC X(02).
           03  TR15VY-D1-BRANCH-NAME          PIC X(66).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-2            REDEFINES TR15VY-DETAIL-LINE.
           03  TR15VY-D2-MACHINE-PROMPT       PIC X(09).
           03  TR15VY-D2-MACHINE              PIC X(02).
           03  FILLER                         PIC X(03).
           03  TR15VY-D2-MACHINE-NAME         PIC X(66).

      *-----------------------------------------------------------------
       01  TR15VY-FILE-LINE-1       REDEFINES TR15VY-DETAIL-LINE.
           03  TR15VY-F1-FILE-PROMPT          PIC X(06).
           03  TR15VY-F1-FILE                 PIC X(10).
           03  FILLER                         PIC X(02).
           03  TR15VY-F1-FILE-MESSAGE         PIC X(62).
           03  FILLER               REDEFINES TR15VY-F1-FILE-MESSAGE.
               05  TR15VY-F1-RECORD-READ-PROMPT   PIC X(06).
               05  TR15VY-F1-RECORD-READ          PIC Z,ZZZ,ZZZ,ZZ9.
               05  FILLER                         PIC X(02).
               05  TR15VY-F1-RECORD-WRITE-PROMPT  PIC X(07).
               05  TR15VY-F1-RECORD-WRITE         PIC Z,ZZZ,ZZZ,ZZ9.
               05  FILLER                         PIC X(02).
               05  TR15VY-F1-RECORD-SKIP-PROMPT   PIC X(06).
               05  TR15VY-F1-RECORD-SKIP          PIC Z,ZZZ,ZZZ,ZZ9.
           03  FILLER                         PIC X(02).
           03  TR15VY-F1-TIMES                PIC X(50).
           03  FILLER               REDEFINES TR15VY-F1-TIMES.
               05  FILLER                     PIC X(05).
               05  TR15VY-F1-BEG-PROMPT       PIC X(05).
               05  TR15VY-F1-BEG-TIME         PIC 99/99/99.
               05  FILLER                     PIC X(02).
               05  TR15VY-F1-END-PROMPT       PIC X(05).
               05  TR15VY-F1-END-TIME         PIC 99/99/99.
               05  FILLER                     PIC X(02).
               05  TR15VY-F1-TOT-PROMPT       PIC X(07).
               05  TR15VY-F1-TOT-TIME         PIC 99/99/99.

      ******************************************************************
      *
      *    T R 1 5 3 4 - M V 1 5 - F I L E - W O R K E R S
      *
      *=================================================================
      * DUE TO ACU4GL, THE A15 FILE NEEDS TO BE RENAMED (VIA MOVE; MV)
      * SO IT IDENTIFIES THE CORRECT FILE.
      * FOR EXAMPLE, USING THE STFILE, THE A15 FILE OF
      * /US1/DA15/R1/LP/STFILE IS SEEN AS THE A34 STFILE DATABASE TABLE.
      * WHEN TRYING TO OPEN THE A15 FILE IT WILL RESULT IN AN ACU ERROR
      * OF "9D,,2".  FOR THE UPGRADE PROGRAMS TO WORK, NEED TO:
      * 
      *  1. MOVE /US1/DA15/R1/LP/STFILE TO /US1/DA15/R1/LP/OSTFILE
      *  2. OPEN, READ-NEXT, CLOSE ON THE OSTFILE.
      *  3. MOVE /US1/DA15/R1/LP/OSTFILE TO /US1/DA15/R1/LP/STFILE
      * 
      * NEED TO MOVE THE FILE BACK TO ITS ORIGINAL NAME IN CASE THE
      * UPGRADE IS BEING EXECUTED MULTIPLE TIMES (QA TESTING) OR FOR
      * SIDE-BY-SIDE TESTING BETWEEN A15 & A34.
      *
      *=================================================================
      *  BASESH - /US1/DA15/SH/YYFILE
      *  GLOBAL - /US1/DA15/R1/XX/YYFILE
      *  LOCAL  - /US1/DA15/R1/SS####/XX/YYFILE
      *           ....|....1....|....2....|....3
      *
      ******************************************************************
       01  TR15VY-MV15-OLD-PATH                  PIC X(35).

       01  FILLER                      REDEFINES TR15VY-MV15-OLD-PATH.
           03  FILLER                            PIC X(13).
           03  TR15VY-MV15-OLD-BASESH-FILE       PIC X(22).

       01  FILLER                      REDEFINES TR15VY-MV15-OLD-PATH.
           03  FILLER                            PIC X(16).
           03  TR15VY-MV15-OLD-GLOBAL-FILE       PIC X(19).

       01  FILLER                      REDEFINES TR15VY-MV15-OLD-PATH.
           03  FILLER                            PIC X(23).
           03  TR15VY-MV15-OLD-LOCAL-FILE        PIC X(12).

      *-----------------------------------------------------------------
       01  TR15VY-MV15-NEW-PATH                  PIC X(35).

       01  FILLER                      REDEFINES TR15VY-MV15-NEW-PATH.
           03  FILLER                            PIC X(13).
           03  TR15VY-MV15-NEW-BASESH-O          PIC X(01).
           03  TR15VY-MV15-NEW-BASESH-FILE       PIC X(21).

       01  FILLER                      REDEFINES TR15VY-MV15-NEW-PATH.
           03  FILLER                            PIC X(16).
           03  TR15VY-MV15-NEW-GLOBAL-O          PIC X(01).
           03  TR15VY-MV15-NEW-GLOBAL-FILE       PIC X(18).

       01  FILLER                      REDEFINES TR15VY-MV15-NEW-PATH.
           03  FILLER                            PIC X(23).
           03  TR15VY-MV15-NEW-LOCAL-O           PIC X(01).
           03  TR15VY-MV15-NEW-LOCAL-FILE        PIC X(11).

      ******************************************************************
      *
      *    T R 1 5 3 0 - V P I - W O R K E R S 
      *
      *=================================================================
      *    TR15VY VIDEO PROGRESS INDICATOR (VPI)
      ******************************************************************
       01  TR15VY-VPI-BRNO                 PIC 9(04)   VALUE ZEROES.

       01  TR15VY-VPI-PRU-LP               PIC X(03)   VALUE "HLD".
           88  TR15VY-VPI-QUEUE-PIGGYBACK              VALUE "PRU"
                                                             "DEV".
           88  TR15VY-VPI-QUEUE-PRINTER                VALUE "   ".
           88  TR15VY-VPI-QUEUE-TERMINAL               VALUE "VDU".
           88  TR15VY-VPI-QUEUE-VIEW                   VALUE "HLD".
           88  TR15VY-VPI-QUEUE-PRINTER-OR-VIEW        VALUE "   "
                                                             "HLD".

       01  TR15VY-VPI-ROUTE-BUF            PIC X(02)   VALUE "00".
           88  TR15VY-VPI-ROUTE-MENU-DRIVEN            VALUE "00" "  ".

       01  TR15VY-VPI-SLEEP-COUNTER        PIC 9(04)   VALUE ZEROES.
       01  TR15VY-VPI-SLEEP-COUNTER-MAX    PIC 9(04)   VALUE 500.
       01  TR15VY-VPI-TICKER               PIC X(01)   VALUE "-".
       01  TR15VY-VPI-TIME-EDIT            PIC X(08)   VALUE SPACES.

      *=================================================================
      *
      *    T R 1 5 3 0 - V P I - D I S P L A Y  - W O R K E R S
      *
      *=================================================================
       01  TR15VY-VPI-DISPLAY-BUF               PIC X(80)  VALUE SPACES.
       01  FILLER                     REDEFINES TR15VY-VPI-DISPLAY-BUF.
           03  TR15VY-VPI-DISPLAY-LEFT          PIC X(01).
           03  TR15VY-VPI-DISPLAY-TICKER        PIC X(01).
           03  TR15VY-VPI-DISPLAY-RIGHT         PIC X(01).
           03  FILLER                           PIC X(01).
           03  TR15VY-VPI-DISPLAY-TEXT          PIC X(76).
           03  FILLER                 REDEFINES TR15VY-VPI-DISPLAY-TEXT.
               05  TR15VY-VPI-DISPLAY-BRNO      PIC 9(04).
               05  FILLER                       PIC X(02).
               05  TR15VY-VPI-DISPLAY-FILE      PIC X(60).
               05  FILLER                       PIC X(02).
               05  TR15VY-VPI-DISPLAY-TIME      PIC X(08).

       01  TR15VY-VPI-DISPLAY-LIST.
           03  FILLER PIC X(14) VALUE "LEGEND ACCEPT.".

      *=================================================================
      *
      *    T R 1 5 3 0 - V P I - F I L E - W O R K E R S
      *
      *=================================================================
       78  TR15VY-VPI-FILE-TYPE---GLOBAL                 VALUE "G".
       78  TR15VY-VPI-FILE-TYPE---LOCAL                  VALUE "L".
       77  TR15VY-VPI-FILE-TYPE            PIC X(01)     VALUE SPACES.
           88  TR15VY-VPI-FILE-TYPE-GLOBAL               VALUE "G".
           88  TR15VY-VPI-FILE-TYPE-LOCAL                VALUE "L".

      *-----------------------------------------------------------------
       01  TR15VY-VPI-FILE-PREFIX         PIC X(06)   VALUE SPACES.
           88  TR15VY-VPI-FILE-CH                     VALUE "CH".
           88  TR15VY-VPI-FILE-TR                     VALUE "TR".
      *- - - - - - - - - - -{ MISC. }- - - - - - - - - - -{ MISC. }- - -
           88  TR15VY-VPI-FILE-WK1                    VALUE "WK1".
           88  TR15VY-VPI-FILE-WK2                    VALUE "WK2".
           88  TR15VY-VPI-FILE-REPORT                 VALUE "REPORT".

      *-----------------------------------------------------------------
       01  TR15VY-VPI-FILE-MMYY           PIC 9(04)   VALUE ZEROES.

      ******************************************************************
      *
      *    S C R E E N - W O R K E R S
      *
      ******************************************************************
       01  DEFAULT-GLOBAL-OPTIONS      PIC X(01)     VALUE SPACES.
       01  DEFAULT-LOCAL-OPTIONS       PIC X(01)     VALUE SPACES.

       01  COPIES-LIST                 PIC X(07)     VALUE "COPIES.".
       01  EOBUF-COPIES                PIC 9(01).

       01  CUSTOMER-LINE-LIST          PIC X(11)    VALUE "START-TIME.".
       01  CUSTOMER-LINE-BUF.
           03  CUSTOMER-RIGHT-SIDE     PIC X(02)     VALUE SPACES.
           03  CUSTOMER-ID             PIC X(08)     VALUE SPACES.
           03  CUSTOMER-LEFT-SIDE      PIC X(02)     VALUE SPACES.

       01  FIL-LIST                    PIC X(11)    VALUE "DATA-SET:B.".
       01  FIL-BUF.
           03  FIL-PROMPT              PIC X(13)     VALUE SPACES.
           03  FIL-INFO                PIC X(35)     VALUE SPACES.

       01  MACHINE-LIST                PIC X(12)   VALUE "MMACHINE:BF.".
       01  MACHINE-BUF.
           03  MACHINE-PROMPT          PIC X(18)     VALUE SPACES.
           03  MACHINE-NAME            PIC X(02)     VALUE SPACES.

       01  PRU-BUF                     PIC 9(02).
       01  PRU-LIST                    PIC X(04)     VALUE "PRU.".

      *-----------------------------------------------------------------
       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".
       01  TARGET-PATH-LIST            PIC X(07)     VALUE "T-PATH.".

       01  STARTING-TIME-LIST          PIC X(11)    VALUE "START-TIME.".
       01  STARTING-TIME-BUF.
           03  STARTING-TIME-RIGHT     PIC X(02)     VALUE SPACES.
           03  STARTING-TIME           PIC X(08)     VALUE SPACES.
           03  STARTING-TIME-LEFT      PIC X(02)     VALUE SPACES.

      *=================================================================
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------

           03  TR15VY-MACHINE          PIC X(02).
           03  TR15VY-SOURCE-PATH      PIC X(35).
           03  TR15VY-TARGET-PATH      PIC X(35).

           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).

           03  GROUP-OPTION-WORKERS.
               05  GROUP-FLAG          PIC X(01).
               05  ENT-GROUP           PIC X(04).

           03  BEG-DATE-YYYYMM         PIC 9(06).
           03  END-DATE-YYYYMM         PIC 9(06).

           03  FILE-OPTION-CH             PIC X(01).
           03  FILE-OPTION-TR           PIC X(01).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).

       01  TR15VY-PRU-LP               PIC X(03).

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
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
      *------------------------------------------{ NEED TO BE TOGETHER }
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUP/TR15VY_DEF.CPY".
       COPY "LIBUP/TR15VY_WKS.CPY".
     
      ******************************************************************
      *
      *    L I N K A G E - S C R E E N - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
     
      *=================================================================
       SCREEN SECTION.
       COPY "LIBUP/TR15VY_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 OBR-FILE OCH-FILE OTR-FILE.

       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE OBR-FILE OCH-FILE OTR-FILE.

      *-----------------------------------------------------------------
       COPY "LIBGB/DECLAR2.CPY".
      *-----------------------------------------------------------------

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           PERFORM INITIALIZATION.
           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

      * LOAD A TABLE INDEXED BY BR-NO JUST TO KNOW THAT BR-MACHINE.
      * USING THE ORIGINAL A15 BRANCH FILE IN CASE YOU ALREADY CHANGED
      * THE NEW FILE TO ALL BE R1 BRANCHES!
      * BRANCH 9990 WILL BE R1
           PERFORM LOAD-BRANCH-TABLE.
           IF ( ERRCD NOT = SPACES )
              MOVE "NEED ORIGINAL BRANCH FILE FOR MACHINE" TO MESS
              PERFORM SEND-MESS
              GO TO MAIN-PROGRAM-EXIT.

      * LOCAL FILE UPGRADES

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
           IF ( IO-BAD ) OR ( BR-NO > END-BRANCH )
              GO TO MAIN-PROGRAM-EXIT.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-PROCESS-BRNO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           PERFORM PROCESS-FILE-TR.
           PERFORM PROCESS-FILE-CH.

           GO TO MAIN-PROGRAM-NEXT-GROUP.

       MAIN-PROGRAM-EXIT.
           PERFORM PRINT-RANGES.
           GO TO END-PROGRAM.

      *****************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.

           MOVE "X"                        TO TR15VY-RECORDS-FOUND.
           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "TR15VY"                   TO VDU-FORM-NAME.
           MOVE TR15VY-QUEUE-POS           TO Q-POS.
           MOVE TR15VY-COPIES-POS          TO C-POS.
           MOVE TR15VY-PRU-POS             TO P-POS.
           MOVE SPACES                     TO Q-BUF  C-BUF  P-BUF.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/EOINIT.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC     TO BEG-BRANCH
                                              END-BRANCH.

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

           PERFORM OPEN-PR-FILE.
           MOVE PRU-LP                     TO TR15VY-PRU-LP.
           MOVE "N"                        TO TR15VY-RECORDS-FOUND.
           PERFORM SEND-STARTING-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ SETUP-HEADER }
           MOVE EXT-CONAME-CONAME          TO H1-NAME.
           MOVE EXT-CONAME-SYSDATE         TO H1-DATE.
           MOVE EXT-CONAME-USER-ID         TO H2-USERID.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

       EXIT-INITIALIZATION.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.
       ENTER-PARAMETERS-BEGIN.

           PERFORM SEND-TITLE.
           PERFORM SEND-FIL.
           PERFORM SEND-MACHINE.
           PERFORM SEND-CUSTOMER-LINE.

           MOVE TR15VY-SOURCE-PATH         TO WR-BUF.
           MOVE SOURCE-PATH-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE TR15VY-TARGET-PATH         TO WR-BUF.
           MOVE TARGET-PATH-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP-KEY )
              GO TO ENTER-OPTION-CH.

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
           MOVE SPACES                     TO DIR-ACCESS-BUF.

           STRING VDUBUF, "/", TR15VY-MACHINE
                  DELIMITED BY " "       INTO DIR-ACCESS-BUF.

           PERFORM DIR-ACCESS-CALL.

           IF ( STAT-BAD )
              PERFORM INVALID-FILE-PATH-MESSAGE
              GO TO ENTER-SOURCE-PATH.

           MOVE VDUBUF                     TO TR15VY-SOURCE-PATH.

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

           STRING VDUBUF, "/", TR15VY-MACHINE
                  DELIMITED BY " "       INTO DIR-ACCESS-BUF.

           PERFORM DIR-ACCESS-CALL.

           IF ( STAT-BAD )
              PERFORM INVALID-FILE-PATH-MESSAGE
              GO TO ENTER-TARGET-PATH.

           MOVE VDUBUF                     TO TR15VY-TARGET-PATH.

           IF ( TR15VY-SOURCE-PATH = TR15VY-TARGET-PATH )
              PERFORM INVALID-SAME-PATHS-MESSAGE
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

      *=================================================================
       ENTER-BEG-DATE.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO ENTER-OPTION-TR.

           MOVE LEGEND-BEG-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-BEG-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO DATE-MMYY.

           IF ( DATE-MMYY-MM < 01 ) OR
              ( DATE-MMYY-MM > 12 )
              MOVE "ERROR: ENTRY NOT VALID (MMYY)" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BEG-DATE-YYYYMM   WSS-DATE-YYYYSMM.

           MOVE "S  A040BEG-YYYYMM." TO VDU.
           MOVE WSS-DATE-YYYYSMM     TO VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-END-DATE.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO ENTER-END-BRANCH.

           MOVE LEGEND-END-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-END-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO DATE-MMYY.

           IF ( DATE-MMYY-MM < 01 ) OR
              ( DATE-MMYY-MM > 12 )
              MOVE "ERROR: ENTRY NOT VALID (MMYY)" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO END-DATE-YYYYMM   WSS-DATE-YYYYSMM.

           MOVE "S  A040END-YYYYMM." TO VDU.
           MOVE WSS-DATE-YYYYSMM     TO VDUBUF.
           PERFORM SCREENIO.

           IF ( BEG-DATE-YYYYMM > END-DATE-YYYYMM )
              MOVE "ERROR: DATE RANGE INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

      *========================================{ SOURCE & TARGET FILES }
       ENTER-OPTION-TR.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-TR." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-TR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-TR.

           IF NOT ( FILE-OPTION-TR = "Y" OR "N" OR "C" OR "A" )
              PERFORM INVALID-YNC-MESSAGE
              GO TO ENTER-OPTION-TR.

      *-----------------------------------------------------------------
       ENTER-OPTION-CH.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-CH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-TR.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-CH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-CH.

           IF NOT ( FILE-OPTION-CH = "Y" OR "N" OR "C" OR "A" )
              PERFORM INVALID-YNC-MESSAGE
              GO TO ENTER-OPTION-CH.

      *- - - - - - - - - - - - - - - - - - - - -{ SKIP TO 'ACCEPT Y/N' }
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010OPTION-TR."  TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS  TO VDUBUF
                                          FILE-OPTION-TR.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-CH."    TO VDU.
           MOVE DEFAULT-LOCAL-OPTIONS  TO VDUBUF
                                          FILE-OPTION-CH.
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

           PERFORM SEND-MACHINE.

           MOVE "S  A090S-PATH."          TO VDU.
           MOVE TR15VY-SOURCE-PATH        TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A090T-PATH."          TO VDU.
           MOVE TR15VY-TARGET-PATH        TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010OPTION-TR."    TO VDU.
           MOVE FILE-OPTION-TR  TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-CH."      TO VDU.
           MOVE FILE-OPTION-CH    TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

           MOVE EXT-EOBUF-DATE1            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM                TO BEG-DATE-YYYYMM.

           MOVE EXT-EOBUF-DATE2            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM                TO END-DATE-YYYYMM.

      *=================================================================
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

           MOVE "BR"                       TO TR15VY-CURRENT-FILE.
           MOVE "GB/BRFILE"                TO TR15VY-FILE-INPUT.
           MOVE TR15VY-FILE-TYPE---GLOBAL  TO TR15VY-FILE-TYPE.

           MOVE TR15VY-FILE-ORDER---SOURCE TO TR15VY-FILE-ORDER.
           PERFORM TR15VY-LOAD-FILE-PATH.
           MOVE TR15VY-FILE-PATH           TO TR15VY-MV15-OLD-PATH.
           PERFORM TR15VY-MV15-OLD-TO-NEW.
           MOVE TR15VY-MV15-NEW-PATH       TO OBR-PATH.

           IF ( STAT-BAD )     *> MOVE OF OLD FILE TO NEW FILE WAS BAD
              MOVE "X" TO ERRCD
              GO TO LOAD-BRANCH-TABLE-EXIT.

           PERFORM OPEN-OBR-FILE.

           MOVE 1 TO OBR-KEY.
           PERFORM START-OBR-FILE.

        LOAD-NEXT-BRANCH.
           PERFORM READ-OBR-FILE-NEXT.
           IF IO-BAD
              MOVE SPACES   TO ERRCD
              MOVE OBR-PATH TO TR15VY-MV15-NEW-PATH
              PERFORM TR15VY-MV15-NEW-TO-OLD
              GO TO LOAD-BRANCH-TABLE-EXIT.

           MOVE OBR-MACHINE TO BRANCH-MACHINE(OBR-NO).

           GO TO LOAD-NEXT-BRANCH.

       LOAD-BRANCH-TABLE-EXIT.
           EXIT.

      *****************************************************************
      *
      *    S C A N - G R - F I L E
      *
      *****************************************************************
       SCAN-GR-FILE SECTION.
       
           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF GRSCAN-STATUS NOT = "00"
              MOVE 9 TO IO-FG
              GO TO EXIT-SCAN-GR-FILE.
       
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF GR1-KEY.
           PERFORM SCREENIO.
       
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "00"       TO VDUSTATUS.
           MOVE ZEROES     TO IO-FG.
           MOVE GRSCAN-KEY TO VDUBUF.
       
      *=================================================================
       EXIT-SCAN-GR-FILE.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - C H
      *
      *=================================================================
      *    IN  : BR-REC
      *          TR15VY-VPI WORKERS
      *          TR15VY-FILE-OPTION-WORKERS
      *
      *    OUT : TR15VY-VPI WORKERS
      *          UPGRADE-STAT   [BAD/GOOD]
      *
      *    DESC: PROCESS SOURCE TO TARGET OF GIVEN FILE.
      *
      ******************************************************************
       PROCESS-FILE-CH SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-CH = "N" )
              GO TO PROCESS-FILE-CH-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    START CHFILE DATE LOOP.
      *    CHFILE-YYYYMM NEEDS TO START FROM THE BEGINNING DATE MINUS
      *    ONE MONTH SO TO ALLOW THE ADD LOGIC TO WORK PROPERLY.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE BEG-DATE-YYYYMM            TO CHFILE-YYYYMM.

           IF ( CHFILE-YYYYMM-MM = 01 )
              SUBTRACT 1                 FROM CHFILE-YYYYMM-YYYY
              MOVE 12                      TO CHFILE-YYYYMM-MM
           ELSE
              SUBTRACT 1                 FROM CHFILE-YYYYMM-MM.

       PROCESS-FILE-CH-DATE.

           ADD 01                          TO CHFILE-YYYYMM-MM.

           IF ( CHFILE-YYYYMM-MM > 12 )
              ADD  01                      TO CHFILE-YYYYMM-YYYY
              MOVE 01                      TO CHFILE-YYYYMM-MM.

           IF ( CHFILE-YYYYMM > END-DATE-YYYYMM )
              GO TO PROCESS-FILE-CH-EXIT.

      *- - - - - - - - - - - - - - - -{ SET CHFILE DATE - CHFILE-MMYY  }
           MOVE CHFILE-YYYYMM              TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY                  TO CHFILE-MMYY.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO CH-PATH-OVERRIDE.

           MOVE "CH"                       TO TR15VY-CURRENT-FILE.
           MOVE CHFILE-MMYY                TO TR15VY-CURRENT-MMYY.

           MOVE TR15VY-FILE-TYPE---LOCAL   TO TR15VY-FILE-TYPE.
           MOVE ZEROES                     TO TR15VY-RECORD-COUNTERS.

           PERFORM TR15VY-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE TR15VY-FILE-ORDER---SOURCE TO TR15VY-FILE-ORDER.

           MOVE SPACES                     TO TR15VY-FILE-INPUT.
           STRING "ED/CH", CHFILE-MMYY
                  DELIMITED BY SIZE      INTO TR15VY-FILE-INPUT.

           PERFORM TR15VY-LOAD-FILE-PATH.
           MOVE TR15VY-FILE-PATH           TO ACCESS-BUF       OCH-PATH.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              GO TO PROCESS-FILE-CH-DATE.

      * IT IS A SEQUENTIAL FILE IN A15, NO START

           PERFORM OPEN-OCH-FILE.

      *=================================================================
       PROCESS-FILE-CH-NEXT.

           PERFORM READ-OCH-FILE.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-CH-END.

           ADD  01  TO TR15VY-RECORD-COUNTER-READ.
           MOVE "Y" TO TR15VY-RECORDS-FOUND.
           PERFORM TR15VY-VPI-DISPLAY.

           PERFORM VERIFY-FILE-CH.

           GO TO PROCESS-FILE-CH-NEXT.

       PROCESS-FILE-CH-END.

           PERFORM CLOSE-OCH-FILE.

           GO TO PROCESS-FILE-CH-DATE.

       PROCESS-FILE-CH-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - L P T R
      *
      *=================================================================
      *    IN  : BR-REC
      *          TR15VY-VPI WORKERS
      *          TR15VY-FILE-OPTION-WORKERS
      *
      *    OUT : TR15VY-VPI WORKERS
      *          UPGRADE-STAT   [BAD/GOOD]
      *
      *    DESC: PROCESS SOURCE TO TARGET OF GIVEN FILE.
      *
      ******************************************************************
       PROCESS-FILE-TR SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-TR = "N" )
              GO TO PROCESS-FILE-TR-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    START TRFILE DATE LOOP.
      *    TRFILE-YYYYMM NEEDS TO START FROM THE BEGINNING DATE MINUS
      *    ONE MONTH SO TO ALLOW THE ADD LOGIC TO WORK PROPERLY.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE BEG-DATE-YYYYMM            TO TRFILE-YYYYMM.

           IF ( TRFILE-YYYYMM-MM = 01 )
              SUBTRACT 1                 FROM TRFILE-YYYYMM-YYYY
              MOVE 12                      TO TRFILE-YYYYMM-MM
           ELSE
              SUBTRACT 1                 FROM TRFILE-YYYYMM-MM.

      *=================================================================
       PROCESS-FILE-TR-DATE.

           ADD 01                          TO TRFILE-YYYYMM-MM.

           IF ( TRFILE-YYYYMM-MM > 12 )
              ADD  01                      TO TRFILE-YYYYMM-YYYY
              MOVE 01                      TO TRFILE-YYYYMM-MM.

           IF ( TRFILE-YYYYMM > END-DATE-YYYYMM )
              GO TO PROCESS-FILE-TR-EXIT.

      *- - - - - - - - - - - - - - - -{ SET TRFILE DATE - TRFILE-MMYY  }
           MOVE TRFILE-YYYYMM              TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY                  TO TRFILE-MMYY.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }

           MOVE "Y"                        TO  TRB-PATH-OVERRIDE
                                               TRC-PATH-OVERRIDE
                                               TRD-PATH-OVERRIDE
                                               TRI-PATH-OVERRIDE
                                               TRJ-PATH-OVERRIDE
                                               TRK-PATH-OVERRIDE
                                               TRL-PATH-OVERRIDE
                                               TRP-PATH-OVERRIDE
                                               TRS-PATH-OVERRIDE
                                               TRW-PATH-OVERRIDE
                                               TRX-PATH-OVERRIDE.

           MOVE "TR"                       TO TR15VY-CURRENT-FILE.
           MOVE TRFILE-MMYY                TO TR15VY-CURRENT-MMYY.

           MOVE TR15VY-FILE-TYPE---LOCAL   TO TR15VY-FILE-TYPE.
           MOVE ZEROES                     TO TR15VY-RECORD-COUNTERS.

           PERFORM TR15VY-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE TR15VY-FILE-ORDER---SOURCE TO TR15VY-FILE-ORDER.

           MOVE SPACES                     TO TR15VY-FILE-INPUT.
           STRING "ED/TR", TRFILE-MMYY
                  DELIMITED BY SIZE      INTO TR15VY-FILE-INPUT.

           PERFORM TR15VY-LOAD-FILE-PATH.
           MOVE TR15VY-FILE-PATH           TO ACCESS-BUF       OTR-PATH.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              GO TO PROCESS-FILE-TR-DATE.

           MOVE "TR"                       TO TR15VY-CURRENT-FILE.

           MOVE LOW-VALUES                 TO OTR1-KEY.
           MOVE ZEROES                     TO OTR1-DAY
                                              OTR1-BRNO
                                              OTR1-CLASS.

           PERFORM OPEN-OTR1-FILE.
           PERFORM START-OTR1-FILE.

       PROCESS-FILE-TR-NEXT.

           PERFORM READ-OTR1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-TR-END.

           ADD  01  TO TR15VY-RECORD-COUNTER-READ.
           MOVE "Y" TO TR15VY-RECORDS-FOUND.
           PERFORM TR15VY-VPI-DISPLAY.

           IF ( OTR1-CODE = "E" )
               ADD 1 TO TR15VY-RECORD-COUNTER-SKIP
               GO TO PROCESS-FILE-TR-NEXT.
 
      *- - - - - - - - - - - - - - - - - - - - - - - - -{ VALID RECORD }
           IF ( OTR1-CODE = "B" )
              PERFORM VERIFY-FILE-TRB
           ELSE
           IF ( OTR1-CODE = "C" )
              PERFORM VERIFY-FILE-TRC
           ELSE
           IF ( OTR1-CODE = "D" )
              PERFORM VERIFY-FILE-TRD
           ELSE
           IF ( OTR1-CODE = "F" ) OR        *> PAYMENTS WITH POOLID
              ( OTR1-CODE = "G" ) OR        *> PAYMENTS FOR SET POOL(SP)
              ( OTR1-CODE = "P" )           *> STANDARD PAYMENTS
              PERFORM VERIFY-FILE-TRP
           ELSE
           IF ( OTR1-CODE = "I" )
              PERFORM VERIFY-FILE-TRI
           ELSE
           IF ( OTR1-CODE = "J" )
              PERFORM VERIFY-FILE-TRJ
           ELSE
           IF ( OTR1-CODE = "K" )
              PERFORM VERIFY-FILE-TRK
           ELSE
           IF ( OTR1-CODE = "L" )
              PERFORM VERIFY-FILE-TRL
           ELSE
           IF ( OTR1-CODE = "P" )
              PERFORM VERIFY-FILE-TRP
           ELSE
           IF ( OTR1-CODE = "S" )
              PERFORM VERIFY-FILE-TRS
           ELSE
           IF ( OTR1-CODE = "W" )
              PERFORM VERIFY-FILE-TRW
           ELSE
           IF ( OTR1-CODE = "X" )
              PERFORM VERIFY-FILE-TRX.

           GO TO PROCESS-FILE-TR-NEXT.

       PROCESS-FILE-TR-END.

           PERFORM CLOSE-OTR1-FILE.

           GO TO PROCESS-FILE-TR-DATE.

       PROCESS-FILE-TR-EXIT.
           EXIT.

      ******************************************************************
       VERIFY-FILE-TRB SECTION.
      ******************************************************************

           IF OTB-DAY NOT NUMERIC
              MOVE "OTB-DAY" TO D-FIELD
              MOVE OTB-DAY   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-BRNO NOT NUMERIC
              MOVE "OTB-BRNO" TO D-FIELD
              MOVE OTB-BRNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-CLASS NOT NUMERIC
              MOVE "OTB-CLASS" TO D-FIELD
              MOVE OTB-CLASS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-NUMBER NOT NUMERIC
              MOVE "OTB-NUMBER" TO D-FIELD
              MOVE OTB-NUMBER   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-SEQNO NOT NUMERIC
              MOVE "OTB-SEQNO" TO D-FIELD
              MOVE OTB-SEQNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-LAWCODE NOT NUMERIC
              MOVE "OTB-LAWCODE" TO D-FIELD
              MOVE OTB-LAWCODE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-LINE NOT NUMERIC
              MOVE "OTB-LINE" TO D-FIELD
              MOVE OTB-LINE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              IF OTB-SSNO(SUB) NOT NUMERIC
                 MOVE "OTB-SSNO(SUB)" TO D-FIELD
                 MOVE OTB-SSNO(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTB-REFBR NOT NUMERIC
              MOVE "OTB-REFBR" TO D-FIELD
              MOVE OTB-REFBR   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-LOANDATE NOT NUMERIC
              MOVE "TB-LOANDATE" TO D-FIELD
              MOVE OTB-LOANDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTB-LOANDATE NOT = 0
                 MOVE OTB-LOANDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TB-LOANDATE" TO D-FIELD
                    MOVE OTB-LOANDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTB-PROCEEDS NOT NUMERIC
              MOVE "OTB-PROCEEDS" TO D-FIELD
              MOVE OTB-PROCEEDS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-LNAMT NOT NUMERIC
              MOVE "OTB-LNAMT" TO D-FIELD
              MOVE OTB-LNAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-FINCHG NOT NUMERIC
              MOVE "OTB-FINCHG" TO D-FIELD
              MOVE OTB-FINCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-INTCHG NOT NUMERIC
              MOVE "OTB-INTCHG" TO D-FIELD
              MOVE OTB-INTCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-SERCHG NOT NUMERIC
              MOVE "OTB-SERCHG" TO D-FIELD
              MOVE OTB-SERCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-EXTCHG NOT NUMERIC
              MOVE "OTB-EXTCHG" TO D-FIELD
              MOVE OTB-EXTCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-MAINTFEE NOT NUMERIC
              MOVE "OTB-MAINTFEE" TO D-FIELD
              MOVE OTB-MAINTFEE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-ORGTERM NOT NUMERIC
              MOVE "OTB-ORGTERM" TO D-FIELD
              MOVE OTB-ORGTERM   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OTB-FEEAMT(SUB) NOT NUMERIC
                 MOVE "OTB-FEEAMT(SUB)" TO D-FIELD
                 MOVE OTB-FEEAMT(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
               END-IF
           END-PERFORM.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              IF OTB-INSCOMP(SUB) NOT NUMERIC
                 MOVE "OTB-INSCOMP(SUB)" TO D-FIELD
                 MOVE OTB-INSCOMP(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTB-INSEFF(SUB) NOT NUMERIC
                 MOVE "OTB-INSEFF(SUB)" TO D-FIELD
                 MOVE OTB-INSEFF(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTB-INSEXP(SUB) NOT NUMERIC
                 MOVE "OTB-INSEXP(SUB)" TO D-FIELD
                 MOVE OTB-INSEXP(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTB-INSPREM(SUB) NOT NUMERIC
                 MOVE "OTB-INSPREM(SUB)" TO D-FIELD
                 MOVE OTB-INSPREM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTB-INSCOMM(SUB) NOT NUMERIC
                 MOVE "OTB-INSCOMM(SUB)" TO D-FIELD
                 MOVE OTB-INSCOMM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTB-DLNO NOT NUMERIC
              MOVE "OTB-DLNO" TO D-FIELD
              MOVE OTB-DLNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-DLRV NOT NUMERIC
              MOVE "OTB-DLRV" TO D-FIELD
              MOVE OTB-DLRV   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-DLSPRV NOT NUMERIC
              MOVE "OTB-DLSPRV" TO D-FIELD
              MOVE OTB-DLSPRV   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-DLDISC NOT NUMERIC
              MOVE "OTB-DLDISC" TO D-FIELD
              MOVE OTB-DLDISC   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-DLPARV NOT NUMERIC
              MOVE "OTB-DLPARV" TO D-FIELD
              MOVE OTB-DLPARV   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-DLPARTRCAMT NOT NUMERIC
              MOVE "OTB-DLPARTRCAMT" TO D-FIELD
              MOVE OTB-DLPARTRCAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-PURBAL NOT NUMERIC
              MOVE "OTB-PURBAL" TO D-FIELD
              MOVE OTB-PURBAL   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.


           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OTB-UNEARN(SUB) NOT NUMERIC
                 MOVE "OTB-UNEARN(SUB)" TO D-FIELD
                 MOVE OTB-UNEARN(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTB-REF-ACCTNO NOT NUMERIC
              MOVE "OTB-REF-ACCTNO" TO D-FIELD
              MOVE OTB-REF-ACCTNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OTB-DLRVPRIN(SUB) NOT NUMERIC
                 MOVE "OTB-DLRVPRIN(SUB)" TO D-FIELD
                 MOVE OTB-DLRVPRIN(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTB-DLRVCHGS(SUB) NOT NUMERIC
                 MOVE "OTB-DLRVCHGS(SUB)" TO D-FIELD
                 MOVE OTB-DLRVCHGS(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTB-DLPARVPAID NOT NUMERIC
              MOVE "OTB-DLPARVPAID" TO D-FIELD
              MOVE OTB-DLPARVPAID   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-DLPARVHELD NOT NUMERIC
              MOVE "OTB-DLPARVHELD" TO D-FIELD
              MOVE OTB-DLPARVHELD   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTB-POSTDATE NOT NUMERIC
              MOVE "TB-POSTDATE" TO D-FIELD
              MOVE OTB-POSTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTB-POSTDATE NOT = 0
                 MOVE OTB-POSTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TB-POSTDATE" TO D-FIELD
                    MOVE OTB-POSTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OTB-POSTTIME NOT NUMERIC
              MOVE "OTB-POSTTIME" TO D-FIELD
              MOVE OTB-POSTTIME   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

       VERIFY-FILE-TRB-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - C H
      *
      *=================================================================
      *    IN  : OCH-REC
      *    OUT : CH-REC
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       VERIFY-FILE-CH SECTION.

           
           IF OCH-DAY NOT NUMERIC
              MOVE "OCH-DAY" TO D-FIELD
              MOVE OCH-DAY   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCH-BRNO NOT NUMERIC
              MOVE "OCH-BRNO" TO D-FIELD
              MOVE OCH-BRNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCH-HR NOT NUMERIC
              MOVE "OCH-HR" TO D-FIELD
              MOVE OCH-HR   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCH-MIN NOT NUMERIC
              MOVE "OCH-MIN" TO D-FIELD
              MOVE OCH-MIN   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OCH-SEC NOT NUMERIC
              MOVE "OCH-SEC" TO D-FIELD
              MOVE OCH-SEC   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

       UPGRADE-FILE-CH-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R C
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRC-REC   [ TRC-REC - CHECKS/CASH ]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       VERIFY-FILE-TRC SECTION.

           IF OTC-DAY NOT NUMERIC
              MOVE "OTC-DAY" TO D-FIELD
              MOVE OTC-DAY   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTC-BRNO NOT NUMERIC
              MOVE "OTC-BRNO" TO D-FIELD
              MOVE OTC-BRNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTC-CKNO NOT NUMERIC
              MOVE "OTC-CKNO" TO D-FIELD
              MOVE OTC-CKNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTC-CLASS NOT NUMERIC
              MOVE "OTC-CLASS" TO D-FIELD
              MOVE OTC-CLASS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTC-SEQ NOT NUMERIC
              MOVE "OTC-SEQ" TO D-FIELD
              MOVE OTC-SEQ   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTC-DATE NOT NUMERIC
              MOVE "TC-DATE" TO D-FIELD
              MOVE OTC-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTC-DATE NOT = 0
                 MOVE OTC-DATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TC-DATE" TO D-FIELD
                    MOVE OTC-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTC-AMT NOT NUMERIC
              MOVE "OTC-AMT" TO D-FIELD
              MOVE OTC-AMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTC-REF NOT NUMERIC
              MOVE "OTC-REF" TO D-FIELD
              MOVE OTC-REF   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              IF OTC-GLNO(SUB) NOT NUMERIC
                 MOVE "OTC-GLNO(SUB)" TO D-FIELD
                 MOVE OTC-GLNO(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTC-GLAMT(SUB) NOT NUMERIC
                 MOVE "OTC-GLAMT(SUB)" TO D-FIELD
                 MOVE OTC-GLAMT(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTC-ACCTNO NOT NUMERIC
              MOVE "OTC-ACCTNO" TO D-FIELD
              MOVE OTC-ACCTNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTC-DLRVNO NOT NUMERIC
              MOVE "OTC-DLRVNO" TO D-FIELD
              MOVE OTC-DLRVNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTC-SPRCLASS NOT NUMERIC
              MOVE "OTC-SPRCLASS" TO D-FIELD
              MOVE OTC-SPRCLASS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTC-DLNO NOT NUMERIC
              MOVE "OTC-DLNO" TO D-FIELD
              MOVE OTC-DLNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTC-POSTDATE NOT NUMERIC
              MOVE "TC-POSTDATE" TO D-FIELD
              MOVE OTC-POSTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTC-POSTDATE NOT = 0
                 MOVE OTC-POSTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TC-POSTDATE" TO D-FIELD
                    MOVE OTC-POSTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTC-POSTTIME NOT NUMERIC
              MOVE "OTC-POSTTIME" TO D-FIELD
              MOVE OTC-POSTTIME   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTC-PAYEE-NO NOT NUMERIC
              MOVE "OTC-PAYEE-NO" TO D-FIELD
              MOVE OTC-PAYEE-NO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

       VERIFY-FILE-TRC-EXIT.           
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R D
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRD-REC   [ TRD-REC - DEPOSIT ]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       VERIFY-FILE-TRD SECTION.

           IF OTD-DAY NOT NUMERIC
              MOVE "OTD-DAY" TO D-FIELD
              MOVE OTD-DAY   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTD-BRNO NOT NUMERIC
              MOVE "OTD-BRNO" TO D-FIELD
              MOVE OTD-BRNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTD-DEPOSIT NOT NUMERIC
              MOVE "OTD-DEPOSIT" TO D-FIELD
              MOVE OTD-DEPOSIT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTD-BANK NOT NUMERIC
              MOVE "OTD-BANK" TO D-FIELD
              MOVE OTD-BANK   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTD-CASH NOT NUMERIC
              MOVE "OTD-CASH" TO D-FIELD
              MOVE OTD-CASH   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTD-GLOVERSHORT NOT NUMERIC
              MOVE "OTD-GLOVERSHORT" TO D-FIELD
              MOVE OTD-GLOVERSHORT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTD-OVERSHORT NOT NUMERIC
              MOVE "OTD-OVERSHORT" TO D-FIELD
              MOVE OTD-OVERSHORT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTD-POSTDATE NOT NUMERIC
              MOVE "TD-POSTDATE" TO D-FIELD
              MOVE OTD-POSTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTD-POSTDATE NOT = 0
                 MOVE OTD-POSTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TD-POSTDATE" TO D-FIELD
                    MOVE OTD-POSTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTD-POSTTIME NOT NUMERIC
              MOVE "OTD-POSTTIME" TO D-FIELD
              MOVE OTD-POSTTIME   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OTD-CHECK-DEPOSIT NOT NUMERIC
              MOVE "OTD-CHECK-DEPOSIT" TO D-FIELD
              MOVE OTD-CHECK-DEPOSIT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

       VERIFY-FILE-TRD-EXIT.          
           EXIT.


      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R I
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRI-REC   [ TRI-REC - INSURANCE CLAIMS ]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       VERIFY-FILE-TRI SECTION.

           IF OTI-DAY NOT NUMERIC
              MOVE "OTI-DAY" TO D-FIELD
              MOVE OTI-DAY   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTI-BRNO NOT NUMERIC
              MOVE "OTI-BRNO" TO D-FIELD
              MOVE OTI-BRNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTI-SEQ NOT NUMERIC
              MOVE "OTI-SEQ" TO D-FIELD
              MOVE OTI-SEQ   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTI-TYPE NOT NUMERIC
              MOVE "OTI-TYPE" TO D-FIELD
              MOVE OTI-TYPE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTI-AMOUNT NOT NUMERIC
              MOVE "OTI-AMOUNT" TO D-FIELD
              MOVE OTI-AMOUNT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTI-DATE NOT NUMERIC
              MOVE "TI-DATE" TO D-FIELD
              MOVE OTI-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTI-DATE NOT = 0
                 MOVE OTI-DATE TO DATE-YYMMDD
                 PERFORM CONVERT-YYMMDD-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TI-DATE" TO D-FIELD
                    MOVE OTI-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTI-POSTTIME NOT NUMERIC
              MOVE "OTI-POSTTIME" TO D-FIELD
              MOVE OTI-POSTTIME   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTI-POSTDATE NOT NUMERIC
              MOVE "TI-POSTDATE" TO D-FIELD
              MOVE OTI-POSTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTI-POSTDATE NOT = 0
                 MOVE OTI-POSTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TI-POSTDATE" TO D-FIELD
                    MOVE OTI-POSTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTI-INSCOMP NOT NUMERIC
              MOVE "OTI-INSCOMP" TO D-FIELD
              MOVE OTI-INSCOMP   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTI-DAYS-PAID NOT NUMERIC
              MOVE "OTI-DAYS-PAID" TO D-FIELD
              MOVE OTI-DAYS-PAID   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTI-FROM-DATE NOT NUMERIC
              MOVE "TI-FROM-DATE" TO D-FIELD
              MOVE OTI-FROM-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTI-FROM-DATE NOT = 0
                 MOVE OTI-FROM-DATE TO DATE-YYMMDD
                 PERFORM CONVERT-YYMMDD-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TI-FROM-DATE" TO D-FIELD
                    MOVE OTI-FROM-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTI-TO-DATE NOT NUMERIC
              MOVE "TI-TO-DATE" TO D-FIELD
              MOVE OTI-TO-DATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTI-TO-DATE NOT = 0
                 MOVE OTI-TO-DATE TO DATE-YYMMDD
                 PERFORM CONVERT-YYMMDD-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TI-TO-DATE" TO D-FIELD
                    MOVE OTI-TO-DATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

       VERIFY-FILE-TRI-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R J
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRJ-REC   [ TRJ-REC - CLASS TRANSFER (WITHIN ONE BRANCH)]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       VERIFY-FILE-TRJ SECTION.

           IF OTJ-DAY NOT NUMERIC
              MOVE "OTJ-DAY" TO D-FIELD
              MOVE OTJ-DAY   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-BRNO NOT NUMERIC
              MOVE "OTJ-BRNO" TO D-FIELD
              MOVE OTJ-BRNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-CLASS NOT NUMERIC
              MOVE "OTJ-CLASS" TO D-FIELD
              MOVE OTJ-CLASS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-NUMBER NOT NUMERIC
              MOVE "OTJ-NUMBER" TO D-FIELD
              MOVE OTJ-NUMBER   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-SEQNO NOT NUMERIC
              MOVE "OTJ-SEQNO" TO D-FIELD
              MOVE OTJ-SEQNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-LAWCODE NOT NUMERIC
              MOVE "OTJ-LAWCODE" TO D-FIELD
              MOVE OTJ-LAWCODE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-LINE NOT NUMERIC
              MOVE "OTJ-LINE" TO D-FIELD
              MOVE OTJ-LINE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              IF OTJ-SSNO(SUB) NOT NUMERIC
                 MOVE "OTJ-SSNO(SUB)" TO D-FIELD
                 MOVE OTJ-SSNO(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OTJ-REF-FRLC NOT NUMERIC
              MOVE "OTJ-REF-FRLC" TO D-FIELD
              MOVE OTJ-REF-FRLC   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-REF-TOLC NOT NUMERIC
              MOVE "OTJ-REF-TOLC" TO D-FIELD
              MOVE OTJ-REF-TOLC   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-LOANDATE NOT NUMERIC
              MOVE "TJ-LOANDATE" TO D-FIELD
              MOVE OTJ-LOANDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTJ-LOANDATE NOT = 0
                 MOVE OTJ-LOANDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TJ-LOANDATE" TO D-FIELD
                    MOVE OTJ-LOANDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTJ-PROCEEDS NOT NUMERIC
              MOVE "OTJ-PROCEEDS" TO D-FIELD
              MOVE OTJ-PROCEEDS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-LNAMT NOT NUMERIC
              MOVE "OTJ-LNAMT" TO D-FIELD
              MOVE OTJ-LNAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-FINCHG NOT NUMERIC
              MOVE "OTJ-FINCHG" TO D-FIELD
              MOVE OTJ-FINCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-INTCHG NOT NUMERIC
              MOVE "OTJ-INTCHG" TO D-FIELD
              MOVE OTJ-INTCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-SERCHG NOT NUMERIC
              MOVE "OTJ-SERCHG" TO D-FIELD
              MOVE OTJ-SERCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-EXTCHG NOT NUMERIC
              MOVE "OTJ-EXTCHG" TO D-FIELD
              MOVE OTJ-EXTCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-MAINTFEE NOT NUMERIC
              MOVE "OTJ-MAINTFEE" TO D-FIELD
              MOVE OTJ-MAINTFEE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-ORGTERM NOT NUMERIC
              MOVE "OTJ-ORGTERM" TO D-FIELD
              MOVE OTJ-ORGTERM   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OTJ-FEEAMT(SUB) NOT NUMERIC
                 MOVE "OTJ-FEEAMT(SUB)" TO D-FIELD
                 MOVE OTJ-FEEAMT(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
               END-IF
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              IF OTJ-INSCOMP(SUB) NOT NUMERIC
                 MOVE "OTJ-INSCOMP(SUB)" TO D-FIELD
                 MOVE OTJ-INSCOMP(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTJ-INSEFF(SUB) NOT NUMERIC
                 MOVE "OTJ-INSEFF(SUB)" TO D-FIELD
                 MOVE OTJ-INSEFF(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTJ-INSEXP(SUB) NOT NUMERIC
                 MOVE "OTJ-INSEXP(SUB)" TO D-FIELD
                 MOVE OTJ-INSEXP(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTJ-INSPREM(SUB) NOT NUMERIC
                 MOVE "OTJ-INSPREM(SUB)" TO D-FIELD
                 MOVE OTJ-INSPREM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTJ-INSCOMM(SUB) NOT NUMERIC
                 MOVE "OTJ-INSCOMM(SUB)" TO D-FIELD
                 MOVE OTJ-INSCOMM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTJ-DLNO NOT NUMERIC
              MOVE "OTJ-DLNO" TO D-FIELD
              MOVE OTJ-DLNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-DLRV NOT NUMERIC
              MOVE "OTJ-DLRV" TO D-FIELD
              MOVE OTJ-DLRV   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-DLSPRV NOT NUMERIC
              MOVE "OTJ-DLSPRV" TO D-FIELD
              MOVE OTJ-DLSPRV   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-DLDISC NOT NUMERIC
              MOVE "OTJ-DLDISC" TO D-FIELD
              MOVE OTJ-DLDISC   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-DLPARV NOT NUMERIC
              MOVE "OTJ-DLPARV" TO D-FIELD
              MOVE OTJ-DLPARV   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-DLPARTRCAMT NOT NUMERIC
              MOVE "OTJ-DLPARTRCAMT" TO D-FIELD
              MOVE OTJ-DLPARTRCAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-PURBAL NOT NUMERIC
              MOVE "OTJ-PURBAL" TO D-FIELD
              MOVE OTJ-PURBAL   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OTJ-UNEARN(SUB) NOT NUMERIC
                 MOVE "OTJ-UNEARN(SUB)" TO D-FIELD
                 MOVE OTJ-UNEARN(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTJ-PRLNNO NOT NUMERIC
              MOVE "OTJ-PRLNNO" TO D-FIELD
              MOVE OTJ-PRLNNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OTJ-DLRVPRIN(SUB) NOT NUMERIC
                 MOVE "OTJ-DLRVPRIN(SUB)" TO D-FIELD
                 MOVE OTJ-DLRVPRIN(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTJ-DLRVCHGS(SUB) NOT NUMERIC
                 MOVE "OTJ-DLRVCHGS(SUB)" TO D-FIELD
                 MOVE OTJ-DLRVCHGS(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTJ-DLPARVPAID NOT NUMERIC
              MOVE "OTJ-DLPARVPAID" TO D-FIELD
              MOVE OTJ-DLPARVPAID   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-DLPARVHELD NOT NUMERIC
              MOVE "OTJ-DLPARVHELD" TO D-FIELD
              MOVE OTJ-DLPARVHELD   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTJ-POSTDATE NOT NUMERIC
              MOVE "TJ-POSTDATE" TO D-FIELD
              MOVE OTJ-POSTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTJ-POSTDATE NOT = 0
                 MOVE OTJ-POSTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TJ-POSTDATE" TO D-FIELD
                    MOVE OTJ-POSTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTJ-POSTTIME NOT NUMERIC
              MOVE "OTJ-POSTTIME" TO D-FIELD
              MOVE OTJ-POSTTIME   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

       VERIFY-FILE-TRJ-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R K
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRK-REC   [ TRK-REC - CONVERSIONS ]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       VERIFY-FILE-TRK SECTION.

           IF OTK-DAY NOT NUMERIC
              MOVE "OTK-DAY" TO D-FIELD
              MOVE OTK-DAY   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-BRNO NOT NUMERIC
              MOVE "OTK-BRNO" TO D-FIELD
              MOVE OTK-BRNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-CLASS NOT NUMERIC
              MOVE "OTK-CLASS" TO D-FIELD
              MOVE OTK-CLASS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-NUMBER NOT NUMERIC
              MOVE "OTK-NUMBER" TO D-FIELD
              MOVE OTK-NUMBER   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-SEQNO NOT NUMERIC
              MOVE "OTK-SEQNO" TO D-FIELD
              MOVE OTK-SEQNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OTK-LAWCODE NOT NUMERIC
              MOVE "OTK-LAWCODE" TO D-FIELD
              MOVE OTK-LAWCODE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-LINE NOT NUMERIC
              MOVE "OTK-LINE" TO D-FIELD
              MOVE OTK-LINE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              IF OTK-SSNO(SUB) NOT NUMERIC
                 MOVE "OTK-SSNO(SUB)" TO D-FIELD
                 MOVE OTK-SSNO(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTK-REFBR NOT NUMERIC
              MOVE "OTK-REFBR" TO D-FIELD
              MOVE OTK-REFBR   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-LOANDATE NOT NUMERIC
              MOVE "TK-LOANDATE" TO D-FIELD
              MOVE OTK-LOANDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTK-LOANDATE NOT = 0
                 MOVE OTK-LOANDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TK-LOANDATE" TO D-FIELD
                    MOVE OTK-LOANDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTK-PROCEEDS NOT NUMERIC
              MOVE "OTK-PROCEEDS" TO D-FIELD
              MOVE OTK-PROCEEDS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-LNAMT NOT NUMERIC
              MOVE "OTK-LNAMT" TO D-FIELD
              MOVE OTK-LNAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-FINCHG NOT NUMERIC
              MOVE "OTK-FINCHG" TO D-FIELD
              MOVE OTK-FINCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-INTCHG NOT NUMERIC
              MOVE "OTK-INTCHG" TO D-FIELD
              MOVE OTK-INTCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-SERCHG NOT NUMERIC
              MOVE "OTK-SERCHG" TO D-FIELD
              MOVE OTK-SERCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-EXTCHG NOT NUMERIC
              MOVE "OTK-EXTCHG" TO D-FIELD
              MOVE OTK-EXTCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-MAINTFEE NOT NUMERIC
              MOVE "OTK-MAINTFEE" TO D-FIELD
              MOVE OTK-MAINTFEE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-ORGTERM NOT NUMERIC
              MOVE "OTK-ORGTERM" TO D-FIELD
              MOVE OTK-ORGTERM   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OTK-FEEAMT(SUB) NOT NUMERIC
                 MOVE "OTK-FEEAMT(SUB)" TO D-FIELD
                 MOVE OTK-FEEAMT(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
               END-IF
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              IF OTK-INSCOMP(SUB) NOT NUMERIC
                 MOVE "OTK-INSCOMP(SUB)" TO D-FIELD
                 MOVE OTK-INSCOMP(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTK-INSEFF(SUB) NOT NUMERIC
                 MOVE "OTK-INSEFF(SUB)" TO D-FIELD
                 MOVE OTK-INSEFF(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTK-INSEXP(SUB) NOT NUMERIC
                 MOVE "OTK-INSEXP(SUB)" TO D-FIELD
                 MOVE OTK-INSEXP(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTK-INSPREM(SUB) NOT NUMERIC
                 MOVE "OTK-INSPREM(SUB)" TO D-FIELD
                 MOVE OTK-INSPREM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTK-INSCOMM(SUB) NOT NUMERIC
                 MOVE "OTK-INSCOMM(SUB)" TO D-FIELD
                 MOVE OTK-INSCOMM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTK-DLNO NOT NUMERIC
              MOVE "OTK-DLNO" TO D-FIELD
              MOVE OTK-DLNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-DLRV NOT NUMERIC
              MOVE "OTK-DLRV" TO D-FIELD
              MOVE OTK-DLRV   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-DLSPRV NOT NUMERIC
              MOVE "OTK-DLSPRV" TO D-FIELD
              MOVE OTK-DLSPRV   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-DLDISC NOT NUMERIC
              MOVE "OTK-DLDISC" TO D-FIELD
              MOVE OTK-DLDISC   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-DLPARV NOT NUMERIC
              MOVE "OTK-DLPARV" TO D-FIELD
              MOVE OTK-DLPARV   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-DLPARTRCAMT NOT NUMERIC
              MOVE "OTK-DLPARTRCAMT" TO D-FIELD
              MOVE OTK-DLPARTRCAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-PURBAL NOT NUMERIC
              MOVE "OTK-PURBAL" TO D-FIELD
              MOVE OTK-PURBAL   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OTK-UNEARN(SUB) NOT NUMERIC
                 MOVE "OTK-UNEARN(SUB)" TO D-FIELD
                 MOVE OTK-UNEARN(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTK-PRLNNO NOT NUMERIC
              MOVE "OTK-PRLNNO" TO D-FIELD
              MOVE OTK-PRLNNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OTK-DLRVPRIN(SUB) NOT NUMERIC
                 MOVE "OTK-DLRVPRIN(SUB)" TO D-FIELD
                 MOVE OTK-DLRVPRIN(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTK-DLRVCHGS(SUB) NOT NUMERIC
                 MOVE "OTK-DLRVCHGS(SUB)" TO D-FIELD
                 MOVE OTK-DLRVCHGS(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTK-EARNED(SUB) NOT NUMERIC
                 MOVE "OTK-EARNED(SUB)" TO D-FIELD
                 MOVE OTK-EARNED(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTK-DLPARVPAID NOT NUMERIC
              MOVE "OTK-DLPARVPAID" TO D-FIELD
              MOVE OTK-DLPARVPAID   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-DLPARVHELD NOT NUMERIC
              MOVE "OTK-DLPARVHELD" TO D-FIELD
              MOVE OTK-DLPARVHELD   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTK-POSTDATE NOT NUMERIC
              MOVE "TK-POSTDATE" TO D-FIELD
              MOVE OTK-POSTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTK-POSTDATE NOT = 0
                 MOVE OTK-POSTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TK-POSTDATE" TO D-FIELD
                    MOVE OTK-POSTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OTK-POSTTIME NOT NUMERIC
              MOVE "OTK-POSTTIME" TO D-FIELD
              MOVE OTK-POSTTIME   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

       VERIFY-FILE-TRK-EXIT.           
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R L
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRL-REC  [TRL-REC - DIRECT LOANS]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       VERIFY-FILE-TRL SECTION.

           IF OTL-DAY NOT NUMERIC
              MOVE "OTL-DAY" TO D-FIELD
              MOVE OTL-DAY   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-BRNO NOT NUMERIC
              MOVE "OTL-BRNO" TO D-FIELD
              MOVE OTL-BRNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-CLASS NOT NUMERIC
              MOVE "OTL-CLASS" TO D-FIELD
              MOVE OTL-CLASS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-NUMBER NOT NUMERIC
              MOVE "OTL-NUMBER" TO D-FIELD
              MOVE OTL-NUMBER   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-SEQNO NOT NUMERIC
              MOVE "OTL-SEQNO" TO D-FIELD
              MOVE OTL-SEQNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OTL-LAWCODE NOT NUMERIC
              MOVE "OTL-LAWCODE" TO D-FIELD
              MOVE OTL-LAWCODE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-LINE NOT NUMERIC
              MOVE "OTL-LINE" TO D-FIELD
              MOVE OTL-LINE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              IF OTL-SSNO(SUB) NOT NUMERIC
                 MOVE "OTL-SSNO(SUB)" TO D-FIELD
                 MOVE OTL-SSNO(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTL-LOANDATE NOT NUMERIC
              MOVE "TL-LOANDATE" TO D-FIELD
              MOVE OTL-LOANDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTL-LOANDATE NOT = 0
                 MOVE OTL-LOANDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TL-LOANDATE" TO D-FIELD
                    MOVE OTL-LOANDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTL-INTDATE NOT NUMERIC
              MOVE "TL-INTDATE" TO D-FIELD
              MOVE OTL-INTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTL-INTDATE NOT = 0
                 MOVE OTL-INTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TL-INTDATE" TO D-FIELD
                    MOVE OTL-INTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OTL-PROCEEDS NOT NUMERIC
              MOVE "OTL-PROCEEDS" TO D-FIELD
              MOVE OTL-PROCEEDS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-LNAMT NOT NUMERIC
              MOVE "OTL-LNAMT" TO D-FIELD
              MOVE OTL-LNAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-FINCHG NOT NUMERIC
              MOVE "OTL-FINCHG" TO D-FIELD
              MOVE OTL-FINCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-INTCHG NOT NUMERIC
              MOVE "OTL-INTCHG" TO D-FIELD
              MOVE OTL-INTCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-SERCHG NOT NUMERIC
              MOVE "OTL-SERCHG" TO D-FIELD
              MOVE OTL-SERCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-EXTCHG NOT NUMERIC
              MOVE "OTL-EXTCHG" TO D-FIELD
              MOVE OTL-EXTCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-MAINTFEE NOT NUMERIC
              MOVE "OTL-MAINTFEE" TO D-FIELD
              MOVE OTL-MAINTFEE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-ORGTERM NOT NUMERIC
              MOVE "OTL-ORGTERM" TO D-FIELD
              MOVE OTL-ORGTERM   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-1STPYAMT NOT NUMERIC
              MOVE "OTL-1STPYAMT" TO D-FIELD
              MOVE OTL-1STPYAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-NOREGPY NOT NUMERIC
              MOVE "OTL-NOREGPY" TO D-FIELD
              MOVE OTL-NOREGPY   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-REGPYAMT NOT NUMERIC
              MOVE "OTL-REGPYAMT" TO D-FIELD
              MOVE OTL-REGPYAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-LASTPYAMT NOT NUMERIC
              MOVE "OTL-LASTPYAMT" TO D-FIELD
              MOVE OTL-LASTPYAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-PROCASH NOT NUMERIC
              MOVE "OTL-PROCASH" TO D-FIELD
              MOVE OTL-PROCASH   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF OTL-FEEAMT(SUB) NOT NUMERIC
                 MOVE "OTL-FEEAMT(SUB)" TO D-FIELD
                 MOVE OTL-FEEAMT(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
               END-IF
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF OTL-INSCOMP(SUB) NOT NUMERIC
                 MOVE "OTL-INSCOMP(SUB)" TO D-FIELD
                 MOVE OTL-INSCOMP(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTL-INSEFF(SUB) NOT NUMERIC
                 MOVE "OTL-INSEFF(SUB)" TO D-FIELD
                 MOVE OTL-INSEFF(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTL-INSEXP(SUB) NOT NUMERIC
                 MOVE "OTL-INSEXP(SUB)" TO D-FIELD
                 MOVE OTL-INSEXP(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTL-INSPREM(SUB) NOT NUMERIC
                 MOVE "OTL-INSPREM(SUB)" TO D-FIELD
                 MOVE OTL-INSPREM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTL-INSCOMM(SUB) NOT NUMERIC
                 MOVE "OTL-INSCOMM(SUB)" TO D-FIELD
                 MOVE OTL-INSCOMM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTL-DLNO NOT NUMERIC
              MOVE "OTL-DLNO" TO D-FIELD
              MOVE OTL-DLNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OTL-EARNINGS(SUB) NOT NUMERIC
                 MOVE "OTL-EARNINGS(SUB)" TO D-FIELD
                 MOVE OTL-EARNINGS(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTL-ANTICERN(SUB) NOT NUMERIC
                 MOVE "OTL-ANTICERN(SUB)" TO D-FIELD
                 MOVE OTL-ANTICERN(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTL-PRLNNO NOT NUMERIC
              MOVE "OTL-PRLNNO" TO D-FIELD
              MOVE OTL-PRLNNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-PRBAL NOT NUMERIC
              MOVE "OTL-PRBAL" TO D-FIELD
              MOVE OTL-PRBAL   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-PRINTDUE NOT NUMERIC
              MOVE "OTL-PRINTDUE" TO D-FIELD
              MOVE OTL-PRINTDUE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-PRFINCHGRB NOT NUMERIC
              MOVE "OTL-PRFINCHGRB" TO D-FIELD
              MOVE OTL-PRFINCHGRB   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-PRINSRB NOT NUMERIC
              MOVE "OTL-PRINSRB" TO D-FIELD
              MOVE OTL-PRINSRB   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-STATE-WORKER NOT NUMERIC
              MOVE "OTL-STATE-WORKER" TO D-FIELD
              MOVE OTL-STATE-WORKER   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-RESTRICTD-ACCTNO NOT NUMERIC
              MOVE "OTL-RESTRICTD-ACCTNO" TO D-FIELD
              MOVE OTL-RESTRICTD-ACCTNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-SPRCLASS NOT NUMERIC
              MOVE "OTL-SPRCLASS" TO D-FIELD
              MOVE OTL-SPRCLASS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-POSTDATE NOT NUMERIC
              MOVE "TL-POSTDATE" TO D-FIELD
              MOVE OTL-POSTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTL-POSTDATE NOT = 0
                 MOVE OTL-POSTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TL-POSTDATE" TO D-FIELD
                    MOVE OTL-POSTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTL-POSTTIME NOT NUMERIC
              MOVE "OTL-POSTTIME" TO D-FIELD
              MOVE OTL-POSTTIME   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-APR NOT NUMERIC
              MOVE "OTL-APR" TO D-FIELD
              MOVE OTL-APR   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-PRDLDISC-UNEARN NOT NUMERIC
              MOVE "OTL-PRDLDISC-UNEARN" TO D-FIELD
              MOVE OTL-PRDLDISC-UNEARN   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-POINTS NOT NUMERIC
              MOVE "OTL-POINTS" TO D-FIELD
              MOVE OTL-POINTS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTL-DEBIT-CARD-AMT NOT NUMERIC
              MOVE "OTL-DEBIT-CARD-AMT" TO D-FIELD
              MOVE OTL-DEBIT-CARD-AMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           
       VERIFY-FILE-TRL-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R P
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRP-REC   [TRP-REC - PAYMENTS]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       VERIFY-FILE-TRP SECTION.

           IF OTP-DAY NOT NUMERIC
              MOVE "OTP-DAY" TO D-FIELD
              MOVE OTP-DAY   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-BRNO NOT NUMERIC
              MOVE "OTP-BRNO" TO D-FIELD
              MOVE OTP-BRNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OTP-CODE = "F" OR "G"
              IF OTPF-POOLID NOT NUMERIC
                 MOVE "OTPF-POOLID" TO D-FIELD
                 MOVE OTPF-POOLID   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTPF-CLASS NOT NUMERIC
                 MOVE "OTPF-CLASS" TO D-FIELD
                 MOVE OTPF-CLASS   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTPF-NUMBER NOT NUMERIC
                 MOVE "OTPF-NUMBER" TO D-FIELD
                 MOVE OTPF-NUMBER   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTPF-SEQNO NOT NUMERIC
                 MOVE "OTPF-SEQNO" TO D-FIELD
                 MOVE OTPF-SEQNO   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTPF-LINE NOT NUMERIC
                 MOVE "OTPF-LINE" TO D-FIELD
                 MOVE OTPF-LINE   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           ELSE
              IF OTP-CLASS NOT NUMERIC
                 MOVE "OTP-CLASS" TO D-FIELD
                 MOVE OTP-CLASS   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTP-NUMBER NOT NUMERIC
                 MOVE "OTP-NUMBER" TO D-FIELD
                 MOVE OTP-NUMBER   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTP-SEQNO NOT NUMERIC
                 MOVE "OTP-SEQNO" TO D-FIELD
                 MOVE OTP-SEQNO   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTP-LINE NOT NUMERIC
                 MOVE "OTP-LINE" TO D-FIELD
                 MOVE OTP-LINE   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-IF.

           IF OTP-LAWCODE NOT NUMERIC
              MOVE "OTP-LAWCODE" TO D-FIELD
              MOVE OTP-LAWCODE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-SSNO NOT NUMERIC
              MOVE "OTP-SSNO" TO D-FIELD
              MOVE OTP-SSNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OTP-PAYDATE NOT NUMERIC
              MOVE "TP-PAYDATE" TO D-FIELD
              MOVE OTP-PAYDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTP-PAYDATE NOT = 0
                 MOVE OTP-PAYDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TP-PAYDATE" TO D-FIELD
                    MOVE OTP-PAYDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTP-DUEDATE NOT NUMERIC
              MOVE "TP-DUEDATE" TO D-FIELD
              MOVE OTP-DUEDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTP-DUEDATE NOT = 0
                 MOVE OTP-DUEDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TP-DUEDATE" TO D-FIELD
                    MOVE OTP-DUEDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTP-TRAMT NOT NUMERIC
              MOVE "OTP-TRAMT" TO D-FIELD
              MOVE OTP-TRAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-INTDUE NOT NUMERIC
              MOVE "OTP-INTDUE" TO D-FIELD
              MOVE OTP-INTDUE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-LCDUE NOT NUMERIC
              MOVE "OTP-LCDUE" TO D-FIELD
              MOVE OTP-LCDUE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-APOTH NOT NUMERIC
              MOVE "OTP-APOTH" TO D-FIELD
              MOVE OTP-APOTH   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-APOT2 NOT NUMERIC
              MOVE "OTP-APOT2" TO D-FIELD
              MOVE OTP-APOT2   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-APLC NOT NUMERIC
              MOVE "OTP-APLC" TO D-FIELD
              MOVE OTP-APLC   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-APCUR NOT NUMERIC
              MOVE "OTP-APCUR" TO D-FIELD
              MOVE OTP-APCUR   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OTP-GLNO(SUB) NOT NUMERIC
                 MOVE "OTP-GLNO(SUB)" TO D-FIELD
                 MOVE OTP-GLNO(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTP-GLAMT(SUB) NOT NUMERIC
                 MOVE "OTP-GLAMT(SUB)" TO D-FIELD
                 MOVE OTP-GLAMT(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OTP-EARNINGS(SUB) NOT NUMERIC
                 MOVE "OTP-EARNINGS(SUB)" TO D-FIELD
                 MOVE OTP-EARNINGS(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTP-OTHBAL NOT NUMERIC
              MOVE "OTP-OTHBAL" TO D-FIELD
              MOVE OTP-OTHBAL   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-OT2BAL NOT NUMERIC
              MOVE "OTP-OT2BAL" TO D-FIELD
              MOVE OTP-OT2BAL   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-INTBAL NOT NUMERIC
              MOVE "OTP-INTBAL" TO D-FIELD
              MOVE OTP-INTBAL   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-LCBAL NOT NUMERIC
              MOVE "OTP-LCBAL" TO D-FIELD
              MOVE OTP-LCBAL   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-CURBAL NOT NUMERIC
              MOVE "OTP-CURBAL" TO D-FIELD
              MOVE OTP-CURBAL   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-PDTHRU NOT NUMERIC
              MOVE "TP-PDTHRU" TO D-FIELD
              MOVE OTP-PDTHRU  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTP-PDTHRU NOT = 0
                 MOVE OTP-PDTHRU TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TP-PDTHRU" TO D-FIELD
                    MOVE OTP-PDTHRU  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OTP-DLNO NOT NUMERIC
              MOVE "OTP-DLNO" TO D-FIELD
              MOVE OTP-DLNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-DLPROC NOT NUMERIC
              MOVE "OTP-DLPROC" TO D-FIELD
              MOVE OTP-DLPROC   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-LCPD NOT NUMERIC
              MOVE "TP-LCPD" TO D-FIELD
              MOVE OTP-LCPD  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTP-LCPD NOT = 0
                 MOVE OTP-LCPD TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TP-LCPD" TO D-FIELD
                    MOVE OTP-LCPD  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OTP-WORKAMT(SUB) NOT NUMERIC
                 MOVE "OTP-WORKAMT(SUB)" TO D-FIELD
                 MOVE OTP-WORKAMT(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OTP-POSTING-BR NOT NUMERIC
              MOVE "OTP-POSTING-BR" TO D-FIELD
              MOVE OTP-POSTING-BR   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-REVSEQNO NOT NUMERIC
              MOVE "OTP-REVSEQNO" TO D-FIELD
              MOVE OTP-REVSEQNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-INTPAID NOT NUMERIC
              MOVE "OTP-INTPAID" TO D-FIELD
              MOVE OTP-INTPAID   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-PRIOR-POOLID NOT NUMERIC
              MOVE "OTP-PRIOR-POOLID" TO D-FIELD
              MOVE OTP-PRIOR-POOLID   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OTP-DLRVCHGS-ADJ(SUB) NOT NUMERIC
                 MOVE "OTP-DLRVCHGS-ADJ(SUB)" TO D-FIELD
                 MOVE OTP-DLRVCHGS-ADJ(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTP-DLPARVHELD-ADJ NOT NUMERIC
              MOVE "OTP-DLPARVHELD-ADJ" TO D-FIELD
              MOVE OTP-DLPARVHELD-ADJ   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-DLPARVPAID-ADJ NOT NUMERIC
              MOVE "OTP-DLPARVPAID-ADJ" TO D-FIELD
              MOVE OTP-DLPARVPAID-ADJ   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-NEWSTAT-RATE NOT NUMERIC
              MOVE "OTP-NEWSTAT-RATE" TO D-FIELD
              MOVE OTP-NEWSTAT-RATE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-PODATE NOT NUMERIC
              MOVE "TP-PODATE" TO D-FIELD
              MOVE OTP-PODATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTP-PODATE NOT = 0
                 MOVE OTP-PODATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TP-PODATE" TO D-FIELD
                    MOVE OTP-PODATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTP-TOTNODEF NOT NUMERIC
              MOVE "OTP-TOTNODEF" TO D-FIELD
              MOVE OTP-TOTNODEF   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-LNAMT NOT NUMERIC
              MOVE "OTP-LNAMT" TO D-FIELD
              MOVE OTP-LNAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-ORIG-1STPAY NOT NUMERIC
              MOVE "TP-ORIG-1STPAY" TO D-FIELD
              MOVE OTP-ORIG-1STPAY  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTP-ORIG-1STPAY NOT = 0
                 MOVE OTP-ORIG-1STPAY TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TP-ORIG-1STPAY" TO D-FIELD
                    MOVE OTP-ORIG-1STPAY  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTP-OLD1ST-PYDATE NOT NUMERIC
              MOVE "TP-OLD1ST-PYDATE" TO D-FIELD
              MOVE OTP-OLD1ST-PYDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTP-OLD1ST-PYDATE NOT = 0
                 MOVE OTP-OLD1ST-PYDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TP-OLD1ST-PYDATE" TO D-FIELD
                    MOVE OTP-OLD1ST-PYDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTP-1STPYDATE NOT NUMERIC
              MOVE "TP-1STPYDATE" TO D-FIELD
              MOVE OTP-1STPYDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTP-1STPYDATE NOT = 0
                 MOVE OTP-1STPYDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TP-1STPYDATE" TO D-FIELD
                    MOVE OTP-1STPYDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTP-ORGTERM NOT NUMERIC
              MOVE "OTP-ORGTERM" TO D-FIELD
              MOVE OTP-ORGTERM   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-INSCOMM NOT NUMERIC
              MOVE "OTP-INSCOMM" TO D-FIELD
              MOVE OTP-INSCOMM   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-POSTTIME NOT NUMERIC
              MOVE "OTP-POSTTIME" TO D-FIELD
              MOVE OTP-POSTTIME   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-PL-OTH-AR NOT NUMERIC
              MOVE "OTP-PL-OTH-AR" TO D-FIELD
              MOVE OTP-PL-OTH-AR   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-LP-DLNO NOT NUMERIC
              MOVE "OTP-LP-DLNO" TO D-FIELD
              MOVE OTP-LP-DLNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-SPRCLASS NOT NUMERIC
              MOVE "OTP-SPRCLASS" TO D-FIELD
              MOVE OTP-SPRCLASS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-POSTDATE NOT NUMERIC
              MOVE "TP-POSTDATE" TO D-FIELD
              MOVE OTP-POSTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTP-POSTDATE NOT = 0
                 MOVE OTP-POSTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TP-POSTDATE" TO D-FIELD
                    MOVE OTP-POSTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTP-UNITPER-FREQ NOT NUMERIC
              MOVE "OTP-UNITPER-FREQ" TO D-FIELD
              MOVE OTP-UNITPER-FREQ   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-SP-DEFFRMLA NOT NUMERIC
              MOVE "OTP-SP-DEFFRMLA" TO D-FIELD
              MOVE OTP-SP-DEFFRMLA   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-PENALTY-INT NOT NUMERIC
              MOVE "OTP-PENALTY-INT" TO D-FIELD
              MOVE OTP-PENALTY-INT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-INSCOMP NOT NUMERIC
              MOVE "OTP-INSCOMP" TO D-FIELD
              MOVE OTP-INSCOMP   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-PROMDATE NOT NUMERIC
              MOVE "TP-PROMDATE" TO D-FIELD
              MOVE OTP-PROMDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTP-PROMDATE NOT = 0
                 MOVE OTP-PROMDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TP-PROMDATE" TO D-FIELD
                    MOVE OTP-PROMDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OTP-PROMAMT NOT NUMERIC
              MOVE "OTP-PROMAMT" TO D-FIELD
              MOVE OTP-PROMAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-INS-TAX-RB NOT NUMERIC
              MOVE "OTP-INS-TAX-RB" TO D-FIELD
              MOVE OTP-INS-TAX-RB   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-PROMDAYS-PASTDUE NOT NUMERIC
              MOVE "OTP-PROMDAYS-PASTDUE" TO D-FIELD
              MOVE OTP-PROMDAYS-PASTDUE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-MIP-CLPAID NOT NUMERIC
              MOVE "OTP-MIP-CLPAID" TO D-FIELD
              MOVE OTP-MIP-CLPAID   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-MIP-CLCOMM NOT NUMERIC
              MOVE "OTP-MIP-CLCOMM" TO D-FIELD
              MOVE OTP-MIP-CLCOMM   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-MIP-AHPAID NOT NUMERIC
              MOVE "OTP-MIP-AHPAID" TO D-FIELD
              MOVE OTP-MIP-AHPAID   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-MIP-AHCOMM NOT NUMERIC
              MOVE "OTP-MIP-AHCOMM" TO D-FIELD
              MOVE OTP-MIP-AHCOMM   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-MIP-CLPDTH NOT NUMERIC
              MOVE "TP-MIP-CLPDTH" TO D-FIELD
              MOVE OTP-MIP-CLPDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTP-MIP-CLPDTH NOT = 0
                 MOVE OTP-MIP-CLPDTH TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TP-MIP-CLPDTH" TO D-FIELD
                    MOVE OTP-MIP-CLPDTH  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTP-MIP-AHPDTH NOT NUMERIC
              MOVE "TP-MIP-AHPDTH" TO D-FIELD
              MOVE OTP-MIP-AHPDTH  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTP-MIP-AHPDTH NOT = 0
                 MOVE OTP-MIP-AHPDTH TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TP-MIP-AHPDTH" TO D-FIELD
                    MOVE OTP-MIP-AHPDTH  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

           IF OTP-APPLIED-TO-ESCROW NOT NUMERIC
              MOVE "OTP-APPLIED-TO-ESCROW" TO D-FIELD
              MOVE OTP-APPLIED-TO-ESCROW   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-ESCROW-AMT NOT NUMERIC
              MOVE "OTP-ESCROW-AMT" TO D-FIELD
              MOVE OTP-ESCROW-AMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-ESCROW-PREPAID NOT NUMERIC
              MOVE "OTP-ESCROW-PREPAID" TO D-FIELD
              MOVE OTP-ESCROW-PREPAID   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-ESCROW-DISBURSED NOT NUMERIC
              MOVE "OTP-ESCROW-DISBURSED" TO D-FIELD
              MOVE OTP-ESCROW-DISBURSED   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-LP-SEQNO NOT NUMERIC
              MOVE "OTP-LP-SEQNO" TO D-FIELD
              MOVE OTP-LP-SEQNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTP-PRIOR-DF-POSTDATE NOT NUMERIC
              MOVE "TP-PRIOR-DF-POSTDATE" TO D-FIELD
              MOVE OTP-PRIOR-DF-POSTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTP-PRIOR-DF-POSTDATE NOT = 0
                 MOVE OTP-PRIOR-DF-POSTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TP-PRIOR-DF-POSTDATE" TO D-FIELD
                    MOVE OTP-PRIOR-DF-POSTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.

       VERIFY-FILE-TRP-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R S
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRS-REC   [TRS-REC - SALES FINANCE NEW INPUT]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       VERIFY-FILE-TRS SECTION.

           IF OTS-DAY NOT NUMERIC
              MOVE "OTS-DAY" TO D-FIELD
              MOVE OTS-DAY   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-BRNO NOT NUMERIC
              MOVE "OTS-BRNO" TO D-FIELD
              MOVE OTS-BRNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-CLASS NOT NUMERIC
              MOVE "OTS-CLASS" TO D-FIELD
              MOVE OTS-CLASS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-NUMBER NOT NUMERIC
              MOVE "OTS-NUMBER" TO D-FIELD
              MOVE OTS-NUMBER   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-SEQNO NOT NUMERIC
              MOVE "OTS-SEQNO" TO D-FIELD
              MOVE OTS-SEQNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OTS-LAWCODE NOT NUMERIC
              MOVE "OTS-LAWCODE" TO D-FIELD
              MOVE OTS-LAWCODE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-LINE NOT NUMERIC
              MOVE "OTS-LINE" TO D-FIELD
              MOVE OTS-LINE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              IF OTS-SSNO(SUB) NOT NUMERIC
                 MOVE "OTS-SSNO(SUB)" TO D-FIELD
                 MOVE OTS-SSNO(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTS-LOANDATE NOT NUMERIC
              MOVE "TS-LOANDATE" TO D-FIELD
              MOVE OTS-LOANDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTS-LOANDATE NOT = 0
                 MOVE OTS-LOANDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TS-LOANDATE" TO D-FIELD
                    MOVE OTS-LOANDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTS-INTDATE NOT NUMERIC
              MOVE "TS-INTDATE" TO D-FIELD
              MOVE OTS-INTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTS-INTDATE NOT = 0
                 MOVE OTS-INTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TS-INTDATE" TO D-FIELD
                    MOVE OTS-INTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTS-PROCEEDS NOT NUMERIC
              MOVE "OTS-PROCEEDS" TO D-FIELD
              MOVE OTS-PROCEEDS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-LNAMT NOT NUMERIC
              MOVE "OTS-LNAMT" TO D-FIELD
              MOVE OTS-LNAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-FINCHG NOT NUMERIC
              MOVE "OTS-FINCHG" TO D-FIELD
              MOVE OTS-FINCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-INTCHG NOT NUMERIC
              MOVE "OTS-INTCHG" TO D-FIELD
              MOVE OTS-INTCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-SERCHG NOT NUMERIC
              MOVE "OTS-SERCHG" TO D-FIELD
              MOVE OTS-SERCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-EXTCHG NOT NUMERIC
              MOVE "OTS-EXTCHG" TO D-FIELD
              MOVE OTS-EXTCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-MAINTFEE NOT NUMERIC
              MOVE "OTS-MAINTFEE" TO D-FIELD
              MOVE OTS-MAINTFEE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-ORGTERM NOT NUMERIC
              MOVE "OTS-ORGTERM" TO D-FIELD
              MOVE OTS-ORGTERM   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-1STPYAMT NOT NUMERIC
              MOVE "OTS-1STPYAMT" TO D-FIELD
              MOVE OTS-1STPYAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-NOREGPY NOT NUMERIC
              MOVE "OTS-NOREGPY" TO D-FIELD
              MOVE OTS-NOREGPY   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OTS-REGPYAMT NOT NUMERIC
              MOVE "OTS-REGPYAMT" TO D-FIELD
              MOVE OTS-REGPYAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-LASTPYAMT NOT NUMERIC
              MOVE "OTS-LASTPYAMT" TO D-FIELD
              MOVE OTS-LASTPYAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF OTS-FEEAMT(SUB) NOT NUMERIC
                 MOVE "OTS-FEEAMT(SUB)" TO D-FIELD
                 MOVE OTS-FEEAMT(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
               END-IF
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              IF OTS-INSCOMP(SUB) NOT NUMERIC
                 MOVE "OTS-INSCOMP(SUB)" TO D-FIELD
                 MOVE OTS-INSCOMP(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTS-INSEFF(SUB) NOT NUMERIC
                 MOVE "OTS-INSEFF(SUB)" TO D-FIELD
                 MOVE OTS-INSEFF(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTS-INSEXP(SUB) NOT NUMERIC
                 MOVE "OTS-INSEXP(SUB)" TO D-FIELD
                 MOVE OTS-INSEXP(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTS-INSPREM(SUB) NOT NUMERIC
                 MOVE "OTS-INSPREM(SUB)" TO D-FIELD
                 MOVE OTS-INSPREM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTS-INSCOMM(SUB) NOT NUMERIC
                 MOVE "OTS-INSCOMM(SUB)" TO D-FIELD
                 MOVE OTS-INSCOMM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTS-DLCOMM-PP-CMSI NOT NUMERIC
              MOVE "OTS-DLCOMM-PP-CMSI" TO D-FIELD
              MOVE OTS-DLCOMM-PP-CMSI   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-DLCOMM-O3-CMSI NOT NUMERIC
              MOVE "OTS-DLCOMM-O3-CMSI" TO D-FIELD
              MOVE OTS-DLCOMM-O3-CMSI   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OTS-DLNO NOT NUMERIC
              MOVE "OTS-DLNO" TO D-FIELD
              MOVE OTS-DLNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-DLPARTRCAMT NOT NUMERIC
              MOVE "OTS-DLPARTRCAMT" TO D-FIELD
              MOVE OTS-DLPARTRCAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-PURBAL NOT NUMERIC
              MOVE "OTS-PURBAL" TO D-FIELD
              MOVE OTS-PURBAL   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OTS-DLRVPRIN(SUB) NOT NUMERIC
                 MOVE "OTS-DLRVPRIN(SUB)" TO D-FIELD
                 MOVE OTS-DLRVPRIN(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTS-DLRVCHGS(SUB) NOT NUMERIC
                 MOVE "OTS-DLRVCHGS(SUB)" TO D-FIELD
                 MOVE OTS-DLRVCHGS(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTS-DLPARVPAID NOT NUMERIC
              MOVE "OTS-DLPARVPAID" TO D-FIELD
              MOVE OTS-DLPARVPAID   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-DLPARVHELD NOT NUMERIC
              MOVE "OTS-DLPARVHELD" TO D-FIELD
              MOVE OTS-DLPARVHELD   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-DLDISC NOT NUMERIC
              MOVE "OTS-DLDISC" TO D-FIELD
              MOVE OTS-DLDISC   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-ANTICTERM NOT NUMERIC
              MOVE "OTS-ANTICTERM" TO D-FIELD
              MOVE OTS-ANTICTERM   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OTS-EARNINGS(SUB) NOT NUMERIC
                 MOVE "OTS-EARNINGS(SUB)" TO D-FIELD
                 MOVE OTS-EARNINGS(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTS-ANTICERN(SUB) NOT NUMERIC
                 MOVE "OTS-ANTICERN(SUB)" TO D-FIELD
                 MOVE OTS-ANTICERN(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTS-PRLNNO NOT NUMERIC
              MOVE "OTS-PRLNNO" TO D-FIELD
              MOVE OTS-PRLNNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-INSPREM-DLADJ NOT NUMERIC
              MOVE "OTS-INSPREM-DLADJ" TO D-FIELD
              MOVE OTS-INSPREM-DLADJ   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-PRDLDISC-UNEARN NOT NUMERIC
              MOVE "OTS-PRDLDISC-UNEARN" TO D-FIELD
              MOVE OTS-PRDLDISC-UNEARN   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-SPRCLASS NOT NUMERIC
              MOVE "OTS-SPRCLASS" TO D-FIELD
              MOVE OTS-SPRCLASS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-O3INS-DLPROC NOT NUMERIC
              MOVE "OTS-O3INS-DLPROC" TO D-FIELD
              MOVE OTS-O3INS-DLPROC   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTS-POSTDATE NOT NUMERIC
              MOVE "TS-POSTDATE" TO D-FIELD
              MOVE OTS-POSTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTS-POSTDATE NOT = 0
                 MOVE OTS-POSTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TS-POSTDATE" TO D-FIELD
                    MOVE OTS-POSTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTL-POSTTIME NOT NUMERIC
              MOVE "OTL-POSTTIME" TO D-FIELD
              MOVE OTL-POSTTIME   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

       VERIFY-FILE-TRS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R W
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRW-REC   [ TRW-REC - CASH BALANCING ]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       VERIFY-FILE-TRW SECTION.

           IF OTW-DAY NOT NUMERIC
              MOVE "OTW-DAY" TO D-FIELD
              MOVE OTW-DAY   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTW-BRNO NOT NUMERIC
              MOVE "OTW-BRNO" TO D-FIELD
              MOVE OTW-BRNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
              IF OTW-TIME(SUB) NOT NUMERIC
                 MOVE "OTW-TIME(SUB)" TO D-FIELD
                 MOVE OTW-TIME(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTW-AMOUNT(SUB) NOT NUMERIC
                 MOVE "OTW-AMOUNT(SUB)" TO D-FIELD
                 MOVE OTW-AMOUNT(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTW-OVERSHORT(SUB) NOT NUMERIC
                 MOVE "OTW-OVERSHORT(SUB)" TO D-FIELD
                 MOVE OTW-OVERSHORT(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

       VERIFY-FILE-TRW-EXIT.          
           EXIT.

      ******************************************************************
      *
      *    U P G R A D E - F I L E - T R X
      *
      *=================================================================
      *    IN  : OTR-REC
      *    OUT : TRX-REC   [ TRX-REC - CLASS TRANSFER ]
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      ******************************************************************
       VERIFY-FILE-TRX SECTION.

           IF OTX-DAY NOT NUMERIC
              MOVE "OTX-DAY" TO D-FIELD
              MOVE OTX-DAY   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-BRNO NOT NUMERIC
              MOVE "OTX-BRNO" TO D-FIELD
              MOVE OTX-BRNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-CLASS NOT NUMERIC
              MOVE "OTX-CLASS" TO D-FIELD
              MOVE OTX-CLASS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-NUMBER NOT NUMERIC
              MOVE "OTX-NUMBER" TO D-FIELD
              MOVE OTX-NUMBER   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-SEQNO NOT NUMERIC
              MOVE "OTX-SEQNO" TO D-FIELD
              MOVE OTX-SEQNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           IF OTX-LAWCODE NOT NUMERIC
              MOVE "OTX-LAWCODE" TO D-FIELD
              MOVE OTX-LAWCODE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-LINE NOT NUMERIC
              MOVE "OTX-LINE" TO D-FIELD
              MOVE OTX-LINE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              IF OTX-SSNO(SUB) NOT NUMERIC
                 MOVE "OTX-SSNO(SUB)" TO D-FIELD
                 MOVE OTX-SSNO(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTX-LOANDATE NOT NUMERIC
              MOVE "TX-LOANDATE" TO D-FIELD
              MOVE OTX-LOANDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTX-LOANDATE NOT = 0
                 MOVE OTX-LOANDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TX-LOANDATE" TO D-FIELD
                    MOVE OTX-LOANDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTX-PROCEEDS NOT NUMERIC
              MOVE "OTX-PROCEEDS" TO D-FIELD
              MOVE OTX-PROCEEDS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-LNAMT NOT NUMERIC
              MOVE "OTX-LNAMT" TO D-FIELD
              MOVE OTX-LNAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-FINCHG NOT NUMERIC
              MOVE "OTX-FINCHG" TO D-FIELD
              MOVE OTX-FINCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-INTCHG NOT NUMERIC
              MOVE "OTX-INTCHG" TO D-FIELD
              MOVE OTX-INTCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-SERCHG NOT NUMERIC
              MOVE "OTX-SERCHG" TO D-FIELD
              MOVE OTX-SERCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-EXTCHG NOT NUMERIC
              MOVE "OTX-EXTCHG" TO D-FIELD
              MOVE OTX-EXTCHG   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-MAINTFEE NOT NUMERIC
              MOVE "OTX-MAINTFEE" TO D-FIELD
              MOVE OTX-MAINTFEE   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-ORGTERM NOT NUMERIC
              MOVE "OTX-ORGTERM" TO D-FIELD
              MOVE OTX-ORGTERM   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF OTX-FEEAMT(SUB) NOT NUMERIC
                 MOVE "OTX-FEEAMT(SUB)" TO D-FIELD
                 MOVE OTB-FEEAMT(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
               END-IF
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF OTX-INSCOMP(SUB) NOT NUMERIC
                 MOVE "OTX-INSCOMP(SUB)" TO D-FIELD
                 MOVE OTX-INSCOMP(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTX-INSEFF(SUB) NOT NUMERIC
                 MOVE "OTX-INSEFF(SUB)" TO D-FIELD
                 MOVE OTX-INSEFF(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTX-INSEXP(SUB) NOT NUMERIC
                 MOVE "OTX-INSEXP(SUB)" TO D-FIELD
                 MOVE OTX-INSEXP(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTX-INSPREM(SUB) NOT NUMERIC
                 MOVE "OTX-INSPREM(SUB)" TO D-FIELD
                 MOVE OTX-INSPREM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTX-INSCOMM(SUB) NOT NUMERIC
                 MOVE "OTX-INSCOMM(SUB)" TO D-FIELD
                 MOVE OTX-INSCOMM(SUB)  TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTX-DLNO NOT NUMERIC
              MOVE "OTX-DLNO" TO D-FIELD
              MOVE OTX-DLNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-DLPARTRCAMT NOT NUMERIC
              MOVE "OTX-DLPARTRCAMT" TO D-FIELD
              MOVE OTX-DLPARTRCAMT   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-PURBAL NOT NUMERIC
              MOVE "OTX-PURBAL" TO D-FIELD
              MOVE OTX-PURBAL   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OTX-DLRVPRIN(SUB) NOT NUMERIC
                 MOVE "OTX-DLRVPRIN(SUB)" TO D-FIELD
                 MOVE OTX-DLRVPRIN(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTX-DLRVCHGS(SUB) NOT NUMERIC
                 MOVE "OTX-DLRVCHGS(SUB)" TO D-FIELD
                 MOVE OTX-DLRVCHGS(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTX-DLPARVHELD NOT NUMERIC
              MOVE "OTX-DLPARVHELD" TO D-FIELD
              MOVE OTX-DLPARVHELD   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-DLPARVPAID NOT NUMERIC
              MOVE "OTX-DLPARVPAID" TO D-FIELD
              MOVE OTX-DLPARVPAID   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-DLDISC NOT NUMERIC
              MOVE "OTX-DLDISC" TO D-FIELD
              MOVE OTX-DLDISC   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-ANTICTERM NOT NUMERIC
              MOVE "OTX-ANTICTERM" TO D-FIELD
              MOVE OTX-ANTICTERM   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OTX-EARNINGS(SUB) NOT NUMERIC
                 MOVE "OTX-EARNINGS(SUB)" TO D-FIELD
                 MOVE OTX-EARNINGS(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              IF OTX-ANTICERN(SUB) NOT NUMERIC
                 MOVE "OTX-ANTICERN(SUB)" TO D-FIELD
                 MOVE OTX-ANTICERN(SUB)   TO D-VALUE
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.
           IF OTX-PRLNNO NOT NUMERIC
              MOVE "OTX-PRLNNO" TO D-FIELD
              MOVE OTX-PRLNNO   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-PRBAL NOT NUMERIC
              MOVE "OTX-PRBAL" TO D-FIELD
              MOVE OTX-PRBAL   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-TFRBAL NOT NUMERIC
              MOVE "OTX-TFRBAL" TO D-FIELD
              MOVE OTX-TFRBAL   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-SPRCLASS NOT NUMERIC
              MOVE "OTX-SPRCLASS" TO D-FIELD
              MOVE OTX-SPRCLASS   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
           IF OTX-POSTDATE NOT NUMERIC
              MOVE "TX-POSTDATE" TO D-FIELD
              MOVE OTX-POSTDATE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF OTX-POSTDATE NOT = 0
                 MOVE OTX-POSTDATE TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "TX-POSTDATE" TO D-FIELD
                    MOVE OTX-POSTDATE  TO D-VALUE
                    PERFORM WRITE-DETAIL-LINE.
           IF OTX-POSTTIME NOT NUMERIC
              MOVE "OTX-POSTTIME" TO D-FIELD
              MOVE OTX-POSTTIME   TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.
       VERIFY-FILE-TRX-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R A N G E S
      *
      ******************************************************************
       PRINT-RANGES SECTION.

           MOVE 99 TO LN-COUNT.

           MOVE "00" TO TR15VY-CURRENT-FILE.

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
           MOVE TR15VY-SOURCE-PATH                   TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "TARGET PATH                      :" TO R1-DESC.
           MOVE TR15VY-TARGET-PATH                   TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "TR-FILE [LP] (Y/N)               :" TO R1-DESC.
           MOVE FILE-OPTION-TR              TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CH-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-CH                TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME OF THE UPGRADE     :" TO R1-DESC.
           MOVE STARTING-TIME                        TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING   TIME OF THE UPGRADE     :" TO R1-DESC.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                            TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-RANGES.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK >  60 )
              PERFORM WRITE-HEADER-LINES.

           IF TR15VY-CURRENT-FILE NOT = "00"
              MOVE BRANCH-MACHINE(BR-NO) TO D-MACHINE.

           IF TR15VY-CURRENT-FILE = "TR"
              MOVE OTR1-DAY TO HOLD-OTR1-DAY
              MOVE OTR1-BRNO TO HOLD-OTR1-BRNO
              MOVE OTR1-CODE TO HOLD-OTR1-CODE
              MOVE OTR1-CLASS TO HOLD-OTR1-CLASS
              MOVE HOLD-OTR1-KEY TO D-KEY.

      * PRINT RANGES
           IF TR15VY-CURRENT-FILE NOT = "00"
              MOVE TR15VY-CURRENT-FILE TO D-FILE
              MOVE BR-NO               TO D-BRNO.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED                     TO LN-COUNT.
           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.

       EXIT-WRITE-DETAIL-LINE.
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
      *    T R 1 5 3 0 - L O A D - F I L E - P A T H
      *
      *=================================================================
      *    IN  : BR-REC
      *          TR15VY-FILE-INPUT  [EXP.  XX/XXXXXX OR XX/TR####]
      *          TR15VY-FILE-ORDER  [SOURCE/TARGET]
      *          TR15VY-FILE-TYPE   [GLOBAL/LOCAL]
      *          TR15VY-SOURCE-PATH
      *          TR15VY-TARGET-PATH
      *
      *    OUT : TR15VY-FILE-PATH
      *          - GLOBAL: /XXX/XXXX/R#/XX/XXXXXX
      *          - LOCAL : /XXX/XXXX/R#/XX####/XX/XXXXXX
      *
      *    DESC: LOAD FILE PATH BASED ON SOURCE/TARGET & GLOBAL/LOCAL.
      ******************************************************************
       TR15VY-LOAD-FILE-PATH SECTION.

           MOVE SPACES                     TO TR15VY-FILE-PATH.

           IF ( TR15VY-FILE-INPUT           = SPACES ) OR
              ( TR15VY-FILE-INPUT-DIR       = SPACES ) OR
              ( TR15VY-FILE-INPUT-SLASH NOT = "/"    ) OR
              ( TR15VY-FILE-INPUT-NAME      = SPACES )
              GO TO TR15VY-LOAD-FILE-PATH-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD SOURCE GLOBAL --- /XXX/XXXX/R#/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( TR15VY-FILE-ORDER-SOURCE ) AND
              ( TR15VY-FILE-TYPE-GLOBAL  )
              MOVE TR15VY-SOURCE-PATH  TO TR15VY-FILE-PATH
              MOVE "/"                 TO TR15VY-FILE-PATH-SLASH-3
              MOVE TR15VY-MACHINE      TO TR15VY-FILE-PATH-MACHINE
              MOVE "/"                 TO TR15VY-FILE-PATH-SLASH-4
              MOVE TR15VY-FILE-INPUT   TO TR15VY-FILE-PATH-GLOBAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD SOURCE LOCAL --- /XXX/XXXX/R#/XX####/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( TR15VY-FILE-ORDER-SOURCE ) AND
              ( TR15VY-FILE-TYPE-LOCAL   )
              MOVE TR15VY-SOURCE-PATH    TO TR15VY-FILE-PATH
              MOVE "/"                   TO TR15VY-FILE-PATH-SLASH-3
      * NEED ORIGINAL OLD BRANCH, MIGHT HAVE RAN FIX
              MOVE BRANCH-MACHINE(BR-NO) TO TR15VY-FILE-PATH-MACHINE
              MOVE "/"                 TO TR15VY-FILE-PATH-SLASH-4
              MOVE BR-DATA-PATH        TO TR15VY-FILE-PATH-LOCAL-DATA
              MOVE "/"                 TO TR15VY-FILE-PATH-LOCAL-DATA-SL
              MOVE TR15VY-FILE-INPUT   TO TR15VY-FILE-PATH-LOCAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD TARGET GLOBAL --- /XXX/XXXX/R#/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( TR15VY-FILE-ORDER-TARGET ) AND
              ( TR15VY-FILE-TYPE-GLOBAL  )
              MOVE TR15VY-TARGET-PATH  TO TR15VY-FILE-PATH
              MOVE "/"                 TO TR15VY-FILE-PATH-SLASH-3
              MOVE TR15VY-MACHINE      TO TR15VY-FILE-PATH-MACHINE
              MOVE "/"                 TO TR15VY-FILE-PATH-SLASH-4
              MOVE TR15VY-FILE-INPUT   TO TR15VY-FILE-PATH-GLOBAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD TARGET LOCAL --- /XXX/XXXX/R#/XX####/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( TR15VY-FILE-ORDER-TARGET ) AND
              ( TR15VY-FILE-TYPE-LOCAL   )
              MOVE TR15VY-TARGET-PATH  TO TR15VY-FILE-PATH
              MOVE "/"                 TO TR15VY-FILE-PATH-SLASH-3
              MOVE TR15VY-MACHINE      TO TR15VY-FILE-PATH-MACHINE
              MOVE "/"                 TO TR15VY-FILE-PATH-SLASH-4
              MOVE BR-DATA-PATH        TO TR15VY-FILE-PATH-LOCAL-DATA
              MOVE "/"                 TO TR15VY-FILE-PATH-LOCAL-DATA-SL
              MOVE TR15VY-FILE-INPUT   TO TR15VY-FILE-PATH-LOCAL-PATH.

       TR15VY-LOAD-FILE-PATH-EXIT.
           EXIT.

      ******************************************************************
      * DUE TO ACU4GL, THE A15 FILE NEEDS TO BE RENAMED (VIA MOVE; MV)
      * SO IT IDENTIFIES THE CORRECT FILE.
      * FOR EXAMPLE, USING THE STFILE, THE A15 FILE OF
      * /US1/DA15/R1/LP/STFILE IS SEEN AS THE A34 STFILE DATABASE TABLE.
      * WHEN TRYING TO OPEN THE A15 FILE IT WILL RESULT IN AN ACU ERROR
      * OF "9D,,2".  FOR THE UPGRADE PROGRAMS TO WORK, NEED TO:
      * 
      *  1. MOVE /US1/DA15/R1/LP/STFILE TO /US1/DA15/R1/LP/OSTFILE
      *  2. OPEN, READ-NEXT, CLOSE ON THE OSTFILE.
      *  3. MOVE /US1/DA15/R1/LP/OSTFILE TO /US1/DA15/R1/LP/STFILE
      * 
      * NEED TO MOVE THE FILE BACK TO ITS ORIGINAL NAME IN CASE THE
      * UPGRADE IS BEING EXECUTED MULTIPLE TIMES (QA TESTING) OR FOR
      * SIDE-BY-SIDE TESTING BETWEEN A15 & A34.
      *
      *=================================================================
      *  BASESH - /US1/DA15/SH/YYFILE
      *  GLOBAL - /US1/DA15/R1/XX/YYFILE
      *  LOCAL  - /US1/DA15/R1/SS####/XX/YYFILE
      *           ....|....1....|....2....|....3
      *
      ******************************************************************
      *
      *    T R 1 5 3 4 - M V 1 5 - O L D - T O - N E W
      *
      *=================================================================
      * IN  : TR15VY-MV15-OLD-PATH
      *       TR15VY-FILE-TYPE  [B, G, L]
      * OUT : TR15VY-MV15-NEW-PATH
      * DESC: EXECUTE SYSTEM MOVE COMMAND TO MOVE OLD FILE TO NEW FILE.
      ******************************************************************
       TR15VY-MV15-OLD-TO-NEW SECTION.

           MOVE TR15VY-MV15-OLD-PATH TO TR15VY-MV15-NEW-PATH.

           IF ( TR15VY-FILE-TYPE-BASESH )
              MOVE "O" TO TR15VY-MV15-NEW-BASESH-O 
              MOVE TR15VY-MV15-OLD-BASESH-FILE TO
                   TR15VY-MV15-NEW-BASESH-FILE
           ELSE
           IF ( TR15VY-FILE-TYPE-GLOBAL )
              MOVE "O" TO TR15VY-MV15-NEW-GLOBAL-O 
              MOVE TR15VY-MV15-OLD-GLOBAL-FILE TO
                   TR15VY-MV15-NEW-GLOBAL-FILE
           ELSE
              MOVE "O" TO TR15VY-MV15-NEW-LOCAL-O 
              MOVE TR15VY-MV15-OLD-LOCAL-FILE TO
                   TR15VY-MV15-NEW-LOCAL-FILE.

           MOVE MV-CMD               TO SYSTEM-BUF.
           MOVE TR15VY-MV15-OLD-PATH TO MV-FILE1.
           MOVE TR15VY-MV15-NEW-PATH TO MV-FILE2.

           PERFORM SYSTEM-CALL.

      *=================================================================
       TR15VY-MV15-OLD-TO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T R 1 5 3 4 - M V 1 5 - N E W - T O - O L D
      *
      *=================================================================
      * IN  : TR15VY-MV15-NEW-PATH
      *       TR15VY-FILE-TYPE  [B, G, L]
      * OUT : TR15VY-MV15-OLD-PATH
      * DESC: EXECUTE SYSTEM MOVE COMMAND TO MOVE NEW FILE TO OLD FILE.
      ******************************************************************
       TR15VY-MV15-NEW-TO-OLD SECTION.

           MOVE TR15VY-MV15-NEW-PATH TO TR15VY-MV15-OLD-PATH.

           IF ( TR15VY-FILE-TYPE-BASESH )
              MOVE TR15VY-MV15-NEW-BASESH-FILE TO
                   TR15VY-MV15-OLD-BASESH-FILE
           ELSE
           IF ( TR15VY-FILE-TYPE-GLOBAL )
              MOVE TR15VY-MV15-NEW-GLOBAL-FILE TO
                   TR15VY-MV15-OLD-GLOBAL-FILE
           ELSE
              MOVE TR15VY-MV15-NEW-LOCAL-FILE TO
                   TR15VY-MV15-OLD-LOCAL-FILE.

           MOVE MV-CMD               TO SYSTEM-BUF.
           MOVE TR15VY-MV15-NEW-PATH TO MV-FILE1.
           MOVE TR15VY-MV15-OLD-PATH TO MV-FILE2.

           PERFORM SYSTEM-CALL.

      *=================================================================
       TR15VY-MV15-NEW-TO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T R 1 5 3 0 - V P I - D I S P L A Y 
      *
      *=================================================================
      *   IN:  TR15VY-VPI-BRNO
      *        TR15VY-VPI-FILE-PREFIX
      *        TR15VY-VPI-FILE-MMYY
      *        TR15VY-VPI-FILE-TYPE
      *        TR15VY-VPI-PRU-LP
      *        TR15VY-VPI-ROUTE-BUF
      *        TR15VY-VPI-SLEEP-COUNTER
      *        TR15VY-VPI-SLEEP-COUNTER-MAX
      *        TR15VY-VPI-TICKER
      *        TR15VY-VPI-TIME-EDIT
      *
      *  OUT:  TR15VY-VPI-SLEEP-COUNTER
      *        TR15VY-VPI-TICKER
      *
      * DESC:  TR15VY VIDEO PROGRESS INDICATOR (VPI)
      *        DISPLAYS TO THE SCREEN THE BRANCH NUMBER & FILE THE
      *        PROGRAM IS CURRENTLY CONVERTING.
      *
      ******************************************************************
       TR15VY-VPI-DISPLAY SECTION.

           IF ( TR15VY-VPI-SLEEP-COUNTER 
              < TR15VY-VPI-SLEEP-COUNTER-MAX )
              ADD 1 TO TR15VY-VPI-SLEEP-COUNTER
              GO TO TR15VY-VPI-DISPLAY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    BEGIN TO CREATE TR15VY-VPI-DISPLAY-BUF
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES                     TO TR15VY-VPI-SLEEP-COUNTER.
           MOVE SPACES                     TO TR15VY-VPI-DISPLAY-BUF.

           MOVE "("                        TO TR15VY-VPI-DISPLAY-LEFT.
           MOVE ")"                        TO TR15VY-VPI-DISPLAY-RIGHT.

           IF ( TR15VY-VPI-TICKER = "-" )
              MOVE "\"                     TO TR15VY-VPI-DISPLAY-TICKER
                                              TR15VY-VPI-TICKER
           ELSE
           IF ( TR15VY-VPI-TICKER = "\" )
              MOVE "|"                     TO TR15VY-VPI-DISPLAY-TICKER
                                              TR15VY-VPI-TICKER
           ELSE
           IF ( TR15VY-VPI-TICKER = "|" )
              MOVE "/"                     TO TR15VY-VPI-DISPLAY-TICKER
                                              TR15VY-VPI-TICKER
           ELSE
              MOVE "-"                     TO TR15VY-VPI-DISPLAY-TICKER
                                              TR15VY-VPI-TICKER.

           IF ( TR15VY-VPI-FILE-TYPE-LOCAL )
              MOVE TR15VY-VPI-BRNO         TO TR15VY-VPI-DISPLAY-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SET THE FILE NAME IN THE DISPLAY
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "***FILE NOT FOUND***"     TO TR15VY-VPI-DISPLAY-FILE

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ LOCAL  }
           IF ( TR15VY-VPI-FILE-CH )
              MOVE SPACES                  TO TR15VY-VPI-DISPLAY-FILE
              STRING "CHANGES (CH) ", 
                     TR15VY-VPI-FILE-MMYY
                     DELIMITED BY SIZE   INTO TR15VY-VPI-DISPLAY-FILE
           ELSE
           IF ( TR15VY-VPI-FILE-TR )
              MOVE SPACES                  TO TR15VY-VPI-DISPLAY-FILE
              STRING "TRANSACTION (LP) ", 
                     TR15VY-VPI-FILE-MMYY
                     DELIMITED BY SIZE   INTO TR15VY-VPI-DISPLAY-FILE
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -{ MISC }
           IF ( TR15VY-VPI-FILE-WK1 )
              MOVE "WORK FILE 1 (WK1)"
                TO TR15VY-VPI-DISPLAY-FILE
           ELSE
           IF ( TR15VY-VPI-FILE-WK2 )
              MOVE "WORK FILE 2 (WK2)"
                TO TR15VY-VPI-DISPLAY-FILE
           ELSE
           IF ( TR15VY-VPI-FILE-REPORT )
              MOVE "TR15VY REPORT (REPORT)"
                TO TR15VY-VPI-DISPLAY-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE TR15VY-VPI-TIME-EDIT       TO TR15VY-VPI-DISPLAY-TIME.
           CALL "C$JUSTIFY" USING TR15VY-VPI-DISPLAY-TEXT "L".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SENDING THE 'PROGRESS INDICTATOR' LINE DISPLAY TO THE SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( TR15VY-VPI-ROUTE-MENU-DRIVEN     ) AND
              ( TR15VY-VPI-QUEUE-PRINTER-OR-VIEW )
              MOVE TR15VY-VPI-DISPLAY-BUF  TO WR-BUF
              MOVE TR15VY-VPI-DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      *=================================================================
       TR15VY-VPI-DISPLAY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T R 1 5 3 0 - V P I - S T A R T
      *
      *=================================================================
      *   IN:  TR15VY-CURRENT-FILE
      *        TR15VY-CURRENT-MMYY
      *        TR15VY-FILE-TYPE
      *        TR15VY-PRU-LP
      *
      *  OUT:  TR15VY-VPI WORKERS
      *
      * DESC:  SETTING THE VIDEO PROGRESS INDICATOR (VPI) WORKERS FOR
      *        THE START OF FILE UPGRADE.
      *
      ******************************************************************
       TR15VY-VPI-START SECTION.

           IF ( TR15VY-FILE-TYPE-LOCAL )
              MOVE BR-NO                   TO TR15VY-VPI-BRNO
           ELSE
              MOVE ZEROES                  TO TR15VY-VPI-BRNO.

           MOVE TR15VY-CURRENT-FILE        TO TR15VY-VPI-FILE-PREFIX.
           MOVE TR15VY-CURRENT-MMYY        TO TR15VY-VPI-FILE-MMYY.
           MOVE TR15VY-FILE-TYPE           TO TR15VY-VPI-FILE-TYPE.
           MOVE TR15VY-PRU-LP              TO TR15VY-VPI-PRU-LP.
           MOVE EXT-ROUTE-BUF              TO TR15VY-VPI-ROUTE-BUF.
           MOVE 9999                       TO TR15VY-VPI-SLEEP-COUNTER.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO TR15VY-VPI-TIME-EDIT.

      *=================================================================
       TR15VY-VPI-START-EXIT.
           EXIT.


      ******************************************************************
      *
      *    U P 1 5 3 4 - L O A D - T I M E - E L A P S E D
      *
      *=================================================================
      *    IN  : TR15VY-TIME-BEG
      *          TR15VY-TIME-END
      *
      *    OUT : TR15VY-TIME-ELAPSED
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
       TR15VY-LOAD-TIME-ELAPSED SECTION.

           IF ( TR15VY-TIME-BEG-HH > TR15VY-TIME-END-HH )
              ADD 24 TO TR15VY-TIME-END-HH.

           COMPUTE TR15VY-TIME-SECONDS = (( TR15VY-TIME-END-HH * 3600 )
                                       +  ( TR15VY-TIME-END-MM * 60   )
                                       +    TR15VY-TIME-END-SS         )
                                       - (( TR15VY-TIME-BEG-HH * 3600 )
                                       +  ( TR15VY-TIME-BEG-MM * 60   )
                                       +    TR15VY-TIME-BEG-SS        ).

           COMPUTE TR15VY-TIME-ELAPSED-HH =
                   TR15VY-TIME-SECONDS / 3600.

           COMPUTE TR15VY-TIME-ELAPSED-MM =
                   ( TR15VY-TIME-SECONDS -
                    (3600 * TR15VY-TIME-ELAPSED-HH))
                   / 60.

           COMPUTE TR15VY-TIME-ELAPSED-SS =
                   TR15VY-TIME-SECONDS -
                   (3600 * TR15VY-TIME-ELAPSED-HH) -
                   (60 * TR15VY-TIME-ELAPSED-MM).

       TR15VY-LOAD-TIME-ELAP-EXIT.
           EXIT.


      ******************************************************************
      *
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.
     
      *    MOVE EXT-CONAME-CONAME          TO TR15VY-CONAME.
           MOVE EXT-CONAME-SYSDATE         TO TR15VY-SYSDATE.
      *    DISPLAY TR15VY-SCREEN         UPON TCLP-MAIN-WINDOW-HANDLE.
           DISPLAY TR15VY-SCREEN         UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TR15VY-FORM-FIELDS         TO WR-BUF.
           MOVE TR15VY-FORM-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.
     
      *=================================================================
       FORM-RESET-EXIT.
           EXIT.
     
      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.
     
           MOVE TR15VY-HELP--FILE          TO HELP-LINK-FILE.
           MOVE TR15VY-HELP--DIR           TO HELP-LINK-DIR.
     
      *=================================================================
       SETUP-LINK-INFO-EXIT.
           EXIT.
     
      ******************************************************************
      *
      *    V D U - C O N V E R T - F I E L D
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
     
           MOVE STAT---GOOD                TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.
     
           EVALUATE TRUE
              COPY "LIBUP/TR15VY_EVA.CPY".
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
      *
      *     M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPH SECTION.
      *=================================================================
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *=================================================================
       INVALID-FILE-PATH-MESSAGE.
           MOVE MESSAGE-INVALID-FILE-PATH TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       INVALID-SAME-PATHS-MESSAGE.
           MOVE MESSAGE-INVALID-SAME-PATHS TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       INVALID-YNC-MESSAGE.
           MOVE MESSAGE-INVALID-YNC TO MESS.
           PERFORM SEND-MESS.

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

      *=================================================================
       SEND-CUSTOMER-LINE.

           MOVE 1 TO GB-KEY.
      *KC> PERFORM OPEN-GB-FILE.
      *KC> PERFORM READ-GB-FILE.
      *KC> PERFORM CLOSE-GB-FILE.

      *KC> IF ( IO-BAD )
              MOVE "**N/A**" TO GB-UUCP-HOME.

           MOVE "[ "         TO CUSTOMER-RIGHT-SIDE.
           MOVE GB-UUCP-HOME TO CUSTOMER-ID.
           MOVE " ]"         TO CUSTOMER-LEFT-SIDE.

           MOVE CUSTOMER-LINE-BUF          TO WR-BUF.
           MOVE CUSTOMER-LINE-LIST         TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-FIL.

           MOVE "FIL"                      TO GETENV-BUF.
           PERFORM GETENV-CALL.

           MOVE SPACES                     TO TR15VY-FILE-PATH.
           STRING "/", GETENV-BUF
                  DELIMITED BY "  "      INTO TR15VY-FILE-PATH.
           MOVE TR15VY-FILE-PATH-MACHINE   TO TR15VY-MACHINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHECK TO SEE IF THE SOURCE & TARGET PATHS ARE SET FROM BATCH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( TR15VY-SOURCE-PATH = SPACES )
              MOVE TR15VY-FILE-PATH        TO TR15VY-SOURCE-PATH
           ELSE
              MOVE TR15VY-SOURCE-PATH      TO DIR-ACCESS-BUF
              PERFORM DIR-ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE TR15VY-FILE-PATH     TO TR15VY-SOURCE-PATH.
     
           IF ( TR15VY-TARGET-PATH = SPACES )
              MOVE TR15VY-FILE-PATH        TO TR15VY-TARGET-PATH
           ELSE
              MOVE TR15VY-TARGET-PATH      TO DIR-ACCESS-BUF
              PERFORM DIR-ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE TR15VY-FILE-PATH     TO TR15VY-TARGET-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CURRENT FIL: "            TO FIL-PROMPT.
           MOVE TR15VY-FILE-PATH           TO FIL-INFO.


           MOVE FIL-BUF                    TO WR-BUF.
           MOVE FIL-LIST                   TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND                     TO WR-BUF.
           MOVE LEGEND-LIST                TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-MACHINE.

           MOVE "ONLY FOR MACHINE: " TO MACHINE-PROMPT.
           MOVE TR15VY-MACHINE       TO MACHINE-NAME.

           MOVE MACHINE-BUF                TO WR-BUF.
           MOVE MACHINE-LIST               TO WR-LIST.
           PERFORM CALL-WRFORM.

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
       SEND-TITLE.
           MOVE "S  A300TITLE:FR."                       TO VDU.
           MOVE "[ A15 TO A30 --- TRANSACTION FILES   ]" TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

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
      *    SOURCE FILE - CH - NEEDED TO BE HARDCODED 
      *=================================================================
      * COPYMEMBER: LIBLP/LPCHR
       OPEN-OCH-FILE.
           PERFORM OPEN-IT.
           MOVE OCH-PATH TO E-FILE.
           OPEN INPUT OCH-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCH-FILE.
           IF IO-FG = 7
              CLOSE OCH-FILE
              GO TO OPEN-OCH-FILE.
       READ-OCH-FILE.
           PERFORM READ-IT.
           MOVE OCH-PATH TO E-FILE.
           READ OCH-FILE.
           IF IO-FG = 8
              GO TO READ-OCH-FILE.
       CLOSE-OCH-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCH-FILE.

      *=================================================================
      *    SOURCE FILE - TR (LP) - NEEDED TO BE HARDCODED 
      *=================================================================
      * COPYMEMBER: LIBLP/LPTR1RN
       OPEN-OTR1-FILE.
           PERFORM OPEN-IT.
           MOVE OTR-PATH TO E-FILE.
           OPEN INPUT OTR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OTR1-FILE.
           IF IO-FG = 7
              CLOSE OTR-FILE
              GO TO OPEN-OTR1-FILE.
       READ-OTR1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OTR-PATH TO E-FILE.
           MOVE OTR1-KEY TO E-KEYX.
           READ OTR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OTR1-FILE-NEXT.
       READ-OTR1-FILE.
           PERFORM READ-IT.
           MOVE OTR-PATH TO E-FILE.
           MOVE OTR1-KEY TO E-KEYX.
           READ OTR-FILE.
           IF IO-FG = 8
              GO TO READ-OTR1-FILE.
       START-OTR1-FILE.
           PERFORM START-IT.
           MOVE OTR-PATH TO E-FILE.
           MOVE OTR1-KEY TO E-KEYX.
           START OTR-FILE KEY NOT < OTR1-KEY.
       CLOSE-OTR1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OTR-FILE.


      *=================================================================
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
      D    STOP MESS.

           PERFORM CLOSE-FILES.

           IF ( TR15VY-EXIT-PRIOR-TO-EXECUTION )
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
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY TR15VY-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.

