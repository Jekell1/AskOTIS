       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FDMAIN.
       DATE-WRITTEN.    061284.
      ******************************************************************
      *
      *                 (LP)
      *  DESCRIPTION:   FORM DESCRIPTOR FILE MAINT.
      *
      *          1 - (LN) LOAN FILE
      *          2 - (BW) BORROWER FILE
      *          3 - (BR) BRANCH FILE
      *          4 - (NO) NOTICE FILE
      *          5 - (LP) LOAN PAYMENT FILE
      *          6 - (DL) DEALER FILE
      *          7 - (CL) COLLATERAL TRAILER FILE
      *          8 - (LT) INSURANCE TRAILER FILE
      *          9 - (GB) GLOBAL FILE
      *         10 - (LS) REPO TRAILER FILE
      *         11 - (LA) LOAN APP FILE
      *         12 - (BA) BORROWER APP FILE
      *         13 - (MA) MINI APP FILE
      *         14 - (ML) MAIL LIST FILE
      *         15 - (BK) BANKRUPT TRAILER FILE
      *         16 - (LN) LOAN APPENDAGE (MORE FIELDS)
      *         17 - (VN) A/P VENDOR FILE
      *         18 - (CD) POOL ID
      *         19 - (LI) LIEN/TITLE TRAILER
      *
      *         21 - (NT) NOTE MASTER FILE
      *         22 - (IV) INVESTOR FILE
      *         23 - (NC) NOTE CLASS FILE
      *
      *         30 - (PA) MISC CHECK PAYEE FILE
      *         31 - (ES) ESCROW FILE
      *
      *=================================================================
      * REV:
      * MJD 051895 ADDED F6 - FORM SCAN OPTION.
      * BAH 010397 ADDED FD-PROG-PREFIX TO ALLOW CUSTOM PRINT
      *            PROGRAMS. IF PREFIX = 0, THEN PROGRAM WILL BE
      *            PRT0(FD-PROG), OTHERWISE IT WILL STILL USE
      *            LONP(FD-PROG)
      * JTG 021797 ADDED LOGIC FOR ENTRY OF FD-FORM-CHG-CD
      * BAH 072897 ALLOW '1S' TO ASK FOR MEMO'S, RAC (KEVIN)
      * GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * MJD 112597 EXPANDED FD-REC TO 2828 BYTES (ADDED 30)
      * KEC 123197 ADDED FD-FXPRESS. USED TO KNOW IF TO USE FORMSXPRESS
      * BAH 021798 ALLOW 1STFILE 13 - MAFILE FOR APPLICATION FORMS
      * BAH 032398 ALLOW LONP83 TRITON TO ASK FOR MEMO'S
      * BAH 040998 ALLOW 1STFILE 14 - MLFILE MAIL SOLICITATION, WORLD
      * BAH 052898 ALLOW MEMO Y/N ON 1STFILE (13) APPS, TO CREATE APP MEMO
      * GLF 021199 ALLOW MEMO Y/N ON LONPCP(REG COUPON), TO CREATE MEMO
      * GLF 021199 ALLOW MEMO Y/N ON LONP8L(REG NOTICE), TO CREATE MEMO
      * GLF 032599 ALLOW MEMO Y/N ON LONPTC FOR 1STFILES 21 AND 22
      * BAH 990723 REMOVED ALL FORMS EXPRESS OPTIONS, AND MADE THEM
      *            SMART PORT, BF MOTOR CREDIT, PR# 917
      * MJD 000309 ADDED SMARTPORT OPTION "C" FOR CUSTOMER STATEMENT ASCII
      *            EXTRACTION.  ADDED VERIFY PASSWORD FOR "C" OPTION.
      *            (MERC/NEWS PR#????)
      * BAH 001013 ADDED FILE 16 (LN) APPENDAGE, REGENCY NEEDED REVOLVING
      *            FIELDS AND FILE 1 IS OUT OF SPACE !! PR# 1324
      * BAH 001108 ADDED FILE 17 (VN) AP VENDOR FILE FOR WORLDS 1099 PRINT
      *            ADDED FD-TYPE = "A" FOR A/P FORMS
      * BAH 020515 ADDED SMART PORT OPTION "E" FOR EXTRACTIONS, WILL WRITE
      *            FM RECORD IN GP-CORP-BRNO, TO THEN CREATE A FILE
      *            EXTRACTION , REGACC PR# 1800
      * KEC 021015 CHANGED "FD-PROG-PREFIX" TO ACCEPT "Z" (LASER).
      * BAH 040218 FORCE PRT01R AND PRT018 TO "C" COLLECTION TYPE FOR
      *            AUTO CREDIT EXPRESS
      * KEC 050801 CHANGED FD-PROG-PREFIX SO TO HAVE " " AND 0-9.
      * KEC 050809 CHANGED AGAIN. CHANGED IT FOR FD-OPTION (AKA FD-SMARTPORT)
      *            AND MULTIPLE 88-LEVELS VARIATIONS OF FD-PROG-PREFIX
      * BAH 050921 ADDED FD-VALID-BRANCH, LENDMARK PR# 2579
      * BAH 040114 ADDED FILE 18 (CDFILE) POOLID CENTRIX 2228
      * BAH 060712 ADDED FILE 19 (LTFILE) CODE "L" SEQ 1 - 5
      *            LIEN AND TITLE FIELDS FROM F9 COLLATERAL P2 AND P3
      *            REGACC PR# 136
      * BAH 061107 ADDED FILE 30 - (PA) MISC CHECK PAYEE FILE, MAS PR# 2868
      * KEC 071016 PR#03075 ADDED "LOAD-TEST-VARIABLE" SECTION.
      * BAH 090805 ADDED FD-MIN-FORM-SENT-DAYS FOR REGACC PR# 251
      *            MUST HAVE GP-USE-MIN-FORM-SENT-DAYS 'Y' TO STOP ON
      *            THIS NEW FIELD.
      * BAH 091001 ADDED FD-SKIP-BAD-ADR FOR REGACC PR# 256
      * BAH 100226 ADDED NEW EDIT TYPE OF 12 FOR MASKING THE FIRST 3 DIGITS
      *            OF THE ACCOUNT NUMBER WITH "XXX", REGENCY PR# 3545
      *              50112501 BECOMES XXX12501
      *  CS 110314 ADD VALUE OF 'M' TO FD-SKIP-BAD-ADR, IF 'M & ACCOUNT IN
      *            CEASE& DESIST DON'T GENERATE FORM MODERN PR#3720
      * KEC 111003 ADDED 'PST' (POSTSCRIPT) OPTION.
      * BAH 111216 ADDED FILE 31 - (ES) ESCROW FILE, REGENCY PR# 3930
      * BAH 130325 ADDED FD-TRAILER-REC FOR REGACC PR# 335
      * KEC 2016.0118 FIXED KEYS & RECS FOR FD-REC.
      *               ADDED NEW FDB-FILE; WAS IN FD-FILE.
      *               REMOVED FT-FILE; MERGED INTO FD-FILE.
      * KEC 2016.0211 FIXED KEYS & RECS FOR FD-REC.
      *               CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      * KEC 2016.0310 CHANGED CD-FC???? TO CD-FC-????
      * KEC 2016.0325 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-??-KEY (NEW).
      * KEC 2017.0119 CHANGED FD-SMARTPORT TO FD-OPTION [88 LEVELS].
      * BAH 190130 SPLIT CDFILE
      * BAH 190710 ADDED LPFDCLR AND USE CLEAR ROUTINE FOR NEW RECORD
      * BAH 190710 ADDED LPFDCLR AND USE CLEAR ROUTINE FOR NEW RECORD
      * JKC 230711 FIXED THE DISPLAY OF THE FORM NUMBER COMING BACK
      *            FROM FDSCAN; FD-FMNO USE TO BE 9(3) BUT IS NOT 9(4)
      *            WHICH THE DISPLAY USING ALPHANUMERIC WAS TRUNCATING
      *            THE LAST BYTE (612 WAS SHOWING AS 061).
      * BAH 2023.1110 ADDED FD-TYPE-OF-DOC FOR F3-ALL DOCS TO IDENTIFY IF
      *               CONTRACT,INSURANCE,OPT-IN, ETC #1630
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/FDMAIN.CBL S35 02/23/24-10:23:29 >".

      ******************************************************************
      *
      *    F I L E - R E C O R D - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FDB.CPY".
       COPY "LIBLP/LP01FDB_SQL.CPY".
       COPY "LIBLP/LPWSFDB.CPY".

      *=================================================================
       01  CHANGED-FD-REC              PIC X(FD-SIZE).
      *01  CHANGED-FDB-REC             PIC X(FDB-SIZE).
       01  ORIGINAL-FD-REC             PIC X(FD-SIZE).
      *01  ORIGINAL-FDB-REC            PIC X(FDB-SIZE).

      *=================================================================
       01  HOLD-FDB1-KEY.
           03  HOLD-FDB-DBNO           PIC 9(02).

       01  HOLD-FD1-KEY.
           03  HOLD-FD-FMNO            PIC 9(04).

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  SAVE-DTYPE      PIC 99   COMP.
       01  SUB             PIC 9(6) COMP.
       01  SUB1            PIC 9(6) COMP.
       01  F-F-SUB         PIC 9(6) COMP.
       01  S-F-SUB         PIC 9(6) COMP.
       01  D-B-SUB         PIC 9(6) COMP.
       01  D-F-SUB         PIC 9(6) COMP.
       01  SWITCH-FLAG     PIC X VALUE "N".

      * FD-FIELDINFO
       01  HOLD-FIELDINFO.
           03  HOLD-FD-FIELDNO      PIC 9(4) COMP-3.
           03  HOLD-FD-ROWCOL.
               05  HOLD-FD-FIELDROW PIC 9(2) COMP-3.
               05  HOLD-FD-FIELDCOL PIC 9(2) COMP-3.
           03  HOLD-FD-FIELDED      PIC 9(2) COMP-3.
           03  HOLD-FD-FIELDKEY     PIC 9(4) COMP-3.

      *  SEL-TYPE IS USED FOR FORM SCAN.
       01  SEL-TYPE        VALUE SPACES.
           03  SEL-FMNO    PIC 9(4).
           03  SEL-FMNOX   REDEFINES SEL-FMNO
                           PIC X(4).

       01  ROW             PIC 99.
       01  FORM-LINE       PIC 99.
       01  TOP-LINE        PIC 99.
       01  BOTTOM-LINE     PIC 99.
       01  MAX-ROW         PIC 99  VALUE 15.
       01  MAX-LINE        PIC 99  VALUE 30.
       01  LINE-WK         PIC S99 VALUE +0.
       01  SCROLL-AMT      PIC 99  VALUE 0.
       01  ALL-X           PIC X(40) VALUE ALL "X".
       01  RULER-EDIT.
           03  FILLER      PIC X(50) VALUE
           "....^....1....^....2....^....3....^....4....^....5".
           03  FILLER      PIC X(30) VALUE
           "....^....6....^....7....^....8".

       01  A-BUF           PIC X(40) VALUE SPACES.

      *    STORE VARIABLE FIELD DESCRIPTIONS FROM FD-REC, FD-INFO
       01  STR-INFO.
           03  STR-FIELDINFO                  OCCURS 50.
               05  STR-FIELDNO      PIC 9(4)  COMP-3.
               05  STR-ROWCOL.
                   07  STR-FIELDROW PIC 9(2)  COMP-3.
                   07  STR-FIELDCOL PIC 9(2)  COMP-3.
               05  STR-FIELDED      PIC 9(2)  COMP-3.
               05  STR-FIELDKEY     PIC 9(4)  COMP-3.

      *    SAVE ENTERED ROW, COL, FIELD, KEY, EDIT
       01  ROWCOL-NO.
           03  ROW-NO      PIC 9(2) VALUE 0.
           03  COL-NO      PIC 9(2) VALUE 0.
       01  FIELD-NO        PIC 9(4) VALUE 0.
       01  KEY-NO          PIC 9(4) VALUE 0.
       01  EDIT-NO         PIC 9(2) VALUE 0.

      *************************************
      *   E D I T   O P T I O N S         *
      *   USED TO SCAN AVAILABLE EDITS    *
      *************************************
       01  EDIT-ALP.
           03  FILLER      PIC X(15)  VALUE "    NO EDITING ".
           03  FILLER      PIC X(15)  VALUE "    0000009.99-".
           03  FILLER      PIC X(15)  VALUE "       9999.99-".
           03  FILLER      PIC X(15)  VALUE "      9,999.99-".
           03  FILLER      PIC X(15)  VALUE "     $9,999.99-".
           03  FILLER      PIC X(15)  VALUE "    ****999.99-".
           03  FILLER      PIC X(15)  VALUE "      99/99/99 ".
           03  FILLER      PIC X(15)  VALUE "(999) 999-9999 ".
           03  FILLER      PIC X(15)  VALUE "   999-99-9999 ".
           03  FILLER      PIC X(15)  VALUE "      SM/SD/SY ".
           03  FILLER      PIC X(15)  VALUE "    99-9999999 ".
           03  FILLER      PIC X(15)  VALUE "      XXX99999 ".
           03  FILLER      PIC X(15)  VALUE "        XXX999 ".
       01  EDIT-ALPX REDEFINES EDIT-ALP.
           03  EDIT-DESC   PIC X(15)  OCCURS 13.
       01  EDIT-MAX        PIC 99     VALUE 13.

       01  DATA-TYPE       PIC 99     COMP.
       01  DATA-DECIMAL    PIC 99     COMP.
       01  DATA-DIGITS     PIC 99     COMP.
       01  DATA-NUMOUTX.
           03  D-N-CHAR    PIC X OCCURS 18.
       01  DATA-NUMOUT     REDEFINES DATA-NUMOUTX
                           PIC S9(13)V9(5).

       01  ELE             PIC 9(6).
       01  ACTION          PIC X(6).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

       01  GROUP-FLAG      PIC X.
       01  ENT-GROUP       PIC X(4).
      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  D-000       PIC ZZ9.
           03  D-TYPE      PIC XX.
           03  D-010       PIC X(25).
           03  D-011       PIC X.
           03  D-PROG-PREFIX PIC X.
           03  D-020       PIC XX.
           03  D-030       PIC Z9.
           03  D-040       PIC Z9.
           03  D-050       PIC Z9.
           03  D-060       PIC Z9.
           03  D-070       PIC X.
           03  D-FD-OPTION PIC X.
           03  D-080       PIC X.
           03  D-090       PIC X.
           03  D-100       PIC X.
           03  D-110       PIC X(4).
           03  D-120       PIC X.
           03  D-130       PIC Z9.
           03  D-131       PIC X.
           03  D-132       PIC X.
           03  D-140       PIC X(80) OCCURS 15.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(54)  VALUE
           "NFM TYPE TITLE TYP PRE PG IK LN TP BT CT FDOPTION DEL ".
           03  FILLER      PIC X(44)  VALUE
           "FMTYPE MEMO BRNO CHGCD DAYS BAD PROFILE 010 ".
           03  FILLER      PIC X(56)  VALUE
           "020 030 040 050 060 070 080 090 100 110 120 130 140 150.".

       01  LINE-BUF        VALUE " ".
           03  D-LN        PIC X(80) OCCURS 15.
       01  LINE-LISTS.
           03  FILLER      PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(20) VALUE
           "110 120 130 140 150.".
       01  LINE-LIST REDEFINES LINE-LISTS
                           PIC X(60).
       01  CUR-LISTS.
           03  CUR-ROW     PIC 99.
           03  FILLER      PIC XX VALUE "0.".
       01  CUR-LIST REDEFINES CUR-LISTS
                           PIC X(4).
       01  ERRCD           PIC X.

       01  EDT-LINE-WORK   PIC X(80) VALUE SPACES OCCURS 15 TIMES.

       01  TEST-LOAN       PIC 99.
           88  LOAN-FIELD  VALUE
           01, 02, 03, 04, 05, 06, 07, 08, 09, 10,
           11, 12, 13, 14, 15, 16, 17, 18, 19, 20.
       01  TEST-NOTE       PIC 99.
           88  NOTE-FIELD  VALUE 03, 09,
           21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
           33, 32, 33, 34, 35, 36, 37, 38, 39, 40.
       01  TEST-VARIABLE   PIC XX.
           88  VARIABLE-FM VALUE "MB", "MC" "TC".

       COPY "LIBMN/FDMAIN_CMD.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBLP/LPEDTW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  CS-SEQ                 VALUE 0.
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBWI/FDSCANW.CPY".
       01  FDMAIN-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/FDMAIN_DEF.CPY".
       COPY "LIBMN/FDMAIN_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/FDMAIN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      /**********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "FDMAIN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS FDMAIN-WINDOW-HANDLE.

           PERFORM GET-GPENV.

           PERFORM OPEN-FDB1-FILE.
       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ************************************
       ENTER-KEYS SECTION.
      ************************************
       ENTER-FMNO.
           MOVE "F7-EXIT, F6-SCAN, ENTER FORM CODE" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE 0 TO KEY-NO EDIT-NO F-F-SUB.
           PERFORM SCAN-EDIT.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.
           MOVE "EN N030FMNO." TO VDU.
           PERFORM SCREENIO.
      * TEST FOR F6 - SCAN FOR FORM
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS.
           IF VDUSTATUS = "1<"
              PERFORM FORM-SCAN
              IF IO-FG NOT = 0
                 GO TO ENTER-FMNO
              ELSE
                 MOVE SEL-FMNO TO VDUNUM0
           ELSE
              IF VDUSTATUS NOT = "00" OR VDUNUM0 = 0
                 GO TO ENTER-FMNO.
      *        IF VDUNUM0 = 0
      *           GO TO ENTER-FMNO.
           MOVE VDUNUM0 TO FD-FMNO HOLD-FD-FMNO.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
           IF IO-FG = 0
              MOVE FD-REC TO ORIGINAL-FD-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.
       END-ENTER-KEYS.
           EXIT.

      *************************************************
      *    ENTRY-MODULE
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ROW FORM-LINE.
           IF NEW-FLAG = 1
              PERFORM ENTER-TITLE
              IF VARIABLE-FM
                 PERFORM ENTER-PROMPT.

       FORM-ENTRY.
           MOVE 0 TO COL-NO.
           ADD 1 TO FORM-LINE.
           SUBTRACT ROW FROM FORM-LINE.
           MOVE 1 TO ROW.
           MOVE "F1-TITLE, F2-PROMPT, F3-FIELD, F7-OPTION" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ENTER-TITLE
              GO TO FORM-ENTRY.
           IF VDUSTATUS = "12"
              IF VARIABLE-FM
                 PERFORM ENTER-PROMPT
                 GO TO FORM-ENTRY.
           IF VDUSTATUS = "16"
              IF VARIABLE-FM
                 PERFORM ENTER-FIELD
                 GO TO FORM-ENTRY.
           IF VDUSTATUS = "1>"
              GO TO OPTION-ENTRY.
           GO TO FORM-ENTRY.
       OPTION-ENTRY.
           MOVE "F1-CLEAR, F7-ENTRY, DELETE, S-SAVE" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              GO TO FORM-ENTRY.
           IF VDUSTATUS NOT = "00"
              GO TO OPTION-ENTRY.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "     S"
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           GO TO OPTION-ENTRY.
       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       ENTER-TITLE SECTION.
      ******************************************************************

           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
       ENTER-NEXT-FORM.

           MOVE "ENNN030NFM." TO VDU.
           PERFORM SCREENIO.
           IF ( F1-KEY )
              MOVE "S  A030NFM." TO VDU
              MOVE FD-FMLINK     TO VDUBUF
              PERFORM SCREENIO
              GO TO EXIT-ENTER-TITLE.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-NEXT-FORM.

           MOVE VDUNUM0 TO FD-NEXTFM.

       ENTER-DOCUMENT-TYPE.

           MOVE "E  A020TYPE." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "S  A020TYPE." TO VDU
              MOVE FD-TYPE-OF-DOC TO VDUBUF
              PERFORM SCREENIO
              GO TO EXIT-ENTER-TITLE.

           IF ( UP-KEY )
              GO TO ENTER-NEXT-FORM.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-DOCUMENT-TYPE.

           MOVE VDUBUF TO FD-TYPE-OF-DOC.

       ENTER-DESC.

           MOVE "E  A250TITLE." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "S  A250TITLE." TO VDU
              MOVE FD-DESC         TO VDUBUF
              PERFORM SCREENIO
              GO TO EXIT-ENTER-TITLE.

           IF ( UP-KEY )
              GO TO ENTER-DOCUMENT-TYPE.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-DESC.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-DESC.

           MOVE VDUBUF TO FD-DESC.

      *-----------------------------------------------------------------
       ENTER-FD-OPTION.

           MOVE "E  A010FDOPTION." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "S  A010FDOPTION." TO VDU
              MOVE FD-OPTION          TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-DESC.

           IF ( UP-KEY )
              GO TO ENTER-DESC.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-FD-OPTION.

           IF NOT ( VDUBUF = "Y" OR "N" OR "C" OR "E" OR "P" OR "T")
              MOVE "ENTER 'Y', 'N', 'C', 'E', 'P'  OR 'T' ONLY" TO MESS
              PERFORM SEND-MESS
              MOVE  "SEE F8-HELP FOR EXPLANATION" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FD-OPTION.

           MOVE VDUBUF TO FD-OPTION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( FD-OPTION-CUSTMT )
              MOVE "TO USE CUST STMT ASCII FILE" TO PASSWDW-PROMPT2
              MOVE CS-SEQ TO PASSWDW-SEQ
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-FD-OPTION.

      *-----------------------------------------------------------------
       ENTER-TYPE.

           MOVE "E  A010TYP." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "S  A010TYP." TO VDU
              MOVE FD-TYPE       TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-DESC.

           IF ( UP-KEY )
              GO TO ENTER-FD-OPTION.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-TYPE.

           IF ( VDUBUF NOT = "L" AND NOT = "N" AND NOT = "A")
              MOVE "ENTER 'L' FOR LOANS, 'N' FOR NOTES, 'A' FOR A/P"
                   TO MESS PERFORM SEND-MESS
              GO TO ENTER-TYPE.

           MOVE VDUBUF TO FD-TYPE.

      *-----------------------------------------------------------------
       ENTER-PROGRAM-PREFIX.

           MOVE "E  A010PRE." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "S  A010PRE."  TO VDU
              MOVE FD-PROG-PREFIX TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-DESC.

           IF ( UP-KEY )
              GO TO ENTER-TYPE.

           IF NOT ( ENTER-KEY OR DOWN-KEY )
              GO TO ENTER-PROGRAM-PREFIX.

           MOVE VDUBUF TO FD-PROG-PREFIX.

           IF NOT ( FD-PROG-PREFIX-VALID )
              MOVE "NOT A VALID PREFIX: USE F8-HELP FOR OPTIONS ONLY!!!"
                   TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROGRAM-PREFIX.

           IF ( FD-OPTION-PCL-LASER    ) AND
              ( NOT FD-PROG-PREFIX-PCL )
              MOVE "NOT A VALID PREFIX: USE F8-HELP FOR OPTIONS ONLY!!!"
                   TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROGRAM-PREFIX.

           IF ( FD-OPTION-PST-LASER    ) AND
              ( NOT FD-PROG-PREFIX-PST )
              MOVE "NOT A VALID PREFIX: USE F8-HELP FOR OPTIONS ONLY!!!"
                   TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROGRAM-PREFIX.

      *-----------------------------------------------------------------
       ENTER-PROGRAM.

           MOVE "E  A020PG." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "S  A020PG." TO VDU
              MOVE FD-PROG      TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-DESC.

           IF ( UP-KEY )
              GO TO ENTER-PROGRAM-PREFIX.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-PROGRAM.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-PROGRAM.

           IF ( VDUBUF = "MC" )
              IF ( FD-TYPE NOT = "L" AND NOT = "A")
                 GO TO ENTER-PROGRAM.

           IF ( VDUBUF = "TC" )
              IF ( FD-TYPE NOT = "N" )
                 GO TO ENTER-PROGRAM.

           MOVE VDUBUF TO FD-PROG.
           PERFORM LOAD-TEST-VARIABLE.

      *-----------------------------------------------------------------
       ENTER-1STFILE.

           MOVE "ENNN020IK." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "SN N020IK." TO VDU
              MOVE FD-1STFILE   TO VDUNUM0
              PERFORM SCREENIO
              GO TO ENTER-DESC.

           IF ( UP-KEY )
              GO TO ENTER-PROGRAM.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-1STFILE.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENTER-1STFILE.

           IF ( FD-TYPE = "L" )
              IF NOT ( VDUNUM0 = 1 OR 2 OR 4 OR 13 OR 14 OR 16 OR 30)
                 GO TO ENTER-1STFILE.

           IF ( FD-TYPE = "A" )
              IF NOT ( VDUNUM0 = 17 )
                 GO TO ENTER-1STFILE.

           IF ( FD-TYPE = "N" )
              IF NOT ( VDUNUM0 = 21 OR 22 )
                 GO TO ENTER-1STFILE.

           MOVE VDUNUM0 TO FD-1STFILE.

      *-----------------------------------------------------------------
       ENTER-BODY.

           MOVE "ENNN020LN." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "SN N020LN." TO VDU
              MOVE FD-BODY TO VDUNUM0
              PERFORM SCREENIO
              GO TO ENTER-DESC.

           IF ( UP-KEY )
              GO TO ENTER-1STFILE.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-BODY.

           IF ( VDUNUM0 < 1 )
              PERFORM INVALID-MSG
              GO TO ENTER-BODY.

           IF ( VDUNUM0 > MAX-LINE )
              IF ( VARIABLE-FM )
                 PERFORM INVALID-MSG
                 GO TO ENTER-BODY.

           MOVE VDUNUM0 TO FD-BODY.

      *-----------------------------------------------------------------
       ENTER-TOP.

           MOVE "ENNN020TP." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "SN N020TP." TO VDU
              MOVE FD-TOP       TO VDUNUM0
              PERFORM SCREENIO
              GO TO ENTER-DESC.

           IF ( UP-KEY )
              GO TO ENTER-BODY.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-TOP.

           MOVE VDUNUM0 TO FD-TOP.

      *-----------------------------------------------------------------
       ENTER-BOTTOM.

           MOVE "ENNN020BT." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "SN N020BT." TO VDU
              MOVE FD-BOTTOM    TO VDUNUM0
              PERFORM SCREENIO
              GO TO ENTER-DESC.

           IF ( UP-KEY )
              GO TO ENTER-TOP.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-BOTTOM.

           MOVE VDUNUM0 TO FD-BOTTOM.

      *-----------------------------------------------------------------
       ENTER-PRCONT.

           MOVE "E  A010CT." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "S  A010CT." TO VDU
              MOVE FD-PRCONT    TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-DESC.

           IF ( UP-KEY )
              GO TO ENTER-BOTTOM.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-PRCONT.

           IF NOT ( VDUBUF = "Y" OR "N" )
              MOVE "Y OR N" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRCONT.

           MOVE VDUBUF TO FD-PRCONT.

      *-----------------------------------------------------------------
       ENTER-INCRCNT.

      * AUTO CREDIX EXPRESS RIGHT TO CURE LETTERS
      *KC->IF FD-PROG-PREFIX = "0" AND (FD-PROG = "18" OR "1R")
           PERFORM LPFRFD-FIND-PREFIX.
           IF ( LPWSFD-PREFIX-PRT      ) AND
              ( FD-PROG = "18" OR "1R" )
                 MOVE "N" TO FD-INCR-DELCNT FD-LNMEMO
                 MOVE "C" TO FD-FMTYPE
                 GO TO ENTER-VALID-BRANCH.

           IF NOT ( LPWSFD-PREFIX-LONP )
              GO TO ENTER-INCRCNT-CONTINUE.

      * DOC DESIGN FORMS AND STATEMENTS
           IF NOT ( FD-PROG = "MC" OR "TC" OR "M9" OR "1S" OR "EX"
                              OR "M5")
      * COUPONS
              IF NOT ( FD-PROG = "95" OR "33" OR "M3"
                              OR "84" OR "83" OR "CP" OR "8L")
                 MOVE "N" TO FD-INCR-DELCNT FD-LNMEMO
                 MOVE " " TO FD-FMTYPE
                 GO TO ENTER-VALID-BRANCH
              ELSE
                 MOVE "N" TO FD-INCR-DELCNT
                 MOVE " " TO FD-FMTYPE
                 GO TO ENTER-LNMEMO.

      *-----------------------------------------------------------------
       ENTER-INCRCNT-CONTINUE.
      * ACCOUNTS PAYABLE
           IF (FD-1STFILE = 17)
              MOVE "N" TO FD-INCR-DELCNT FD-LNMEMO
              MOVE " " TO FD-FMTYPE FD-VALID-BRANCH
              GO TO EXIT-ENTER-TITLE
           ELSE
      * APPLICATIONS
           IF (FD-1STFILE = 13 OR 14)
              MOVE "N" TO FD-INCR-DELCNT FD-LNMEMO
              MOVE " " TO FD-FMTYPE
              IF FD-1STFILE = 13
      * APPS - ALLOW MEMO Y/N FOR APP MEMO
                 GO TO ENTER-LNMEMO
              ELSE
                 GO TO ENTER-VALID-BRANCH.

           MOVE "E  A010DEL." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "S  A010DEL."  TO VDU
              MOVE FD-INCR-DELCNT TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-DESC.

           IF ( UP-KEY )
              GO TO ENTER-PRCONT.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-INCRCNT.

           IF ( FD-1STFILE = 1 OR 4 OR 21 )
              IF NOT ( VDUBUF = "Y" OR "N" )
                 MOVE "Y OR N" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-INCRCNT
              END-IF
           ELSE
              IF ( VDUBUF NOT = "N" )
                 MOVE "ENTER 'N' ONLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-INCRCNT.

           MOVE VDUBUF TO FD-INCR-DELCNT.

      *-----------------------------------------------------------------
       ENTER-FMTYPE.

           MOVE "E  A010FMTYPE." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "S  A010FMTYPE." TO VDU
              MOVE FD-FMTYPE        TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-DESC.

           IF ( UP-KEY )
              GO TO ENTER-INCRCNT.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-FMTYPE.

           IF ( FD-TYPE = "L" )
              IF NOT ( VDUBUF = " " OR "C" OR "S" )
                 MOVE "ENTER ' ' OR 'C' OR 'S'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-FMTYPE.

           IF ( FD-TYPE = "N" OR "A")
              IF ( VDUBUF NOT = SPACES )
                 MOVE "ENTER A ' '"  TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-FMTYPE.

           MOVE VDUBUF TO FD-FMTYPE.

      *-----------------------------------------------------------------
       ENTER-LNMEMO.

           MOVE "E  A010MEMO." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "S  A010MEMO." TO VDU
              MOVE FD-LNMEMO TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-DESC.

           IF ( UP-KEY )
              GO TO ENTER-FMTYPE.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-LNMEMO.

           IF NOT ( VDUBUF = "Y" OR "N" )
              MOVE "ENTER Y OR N" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LNMEMO.

           IF NOT ( FD-1STFILE = 1 OR 4 OR 13 OR 21 OR 22)
              IF VDUBUF NOT = "N"
                 MOVE "ENTER 'N' ONLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-LNMEMO.

           MOVE VDUBUF TO FD-LNMEMO.


       ENTER-VALID-BRANCH.

           MOVE "E  A040BRNO." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "S  A040BRNO." TO VDU
              MOVE FD-VALID-BRANCH TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-DESC.

           IF ( UP-KEY )
              GO TO ENTER-LNMEMO.

           IF ( VDUSTATUS = "1<" )
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-VALID-BRANCH
              ELSE
                 MOVE "S  A000BRNO." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO
                 GO TO READ-GROUP-FILE.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-VALID-BRANCH.


       READ-GROUP-FILE.
           MOVE VDUBUF TO WGR-WORD
                          ENT-GROUP.

      * ALLOW ALL BRANCHES, GROUPS
           IF WGR-WORD = SPACES
              MOVE SPACES TO FD-VALID-BRANCH
              GO TO ENTER-CHGCD.

           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-VALID-BRANCH.

           IF WGR-TYPE = "A"
              MOVE "Y" TO GROUP-FLAG
              MOVE VDUBUF TO FD-VALID-BRANCH
              GO TO ENTER-CHGCD.

       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO FD-VALID-BRANCH.
           MOVE "SN N040BRNO." TO VDU.
           MOVE FD-VALID-BRANCH  TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
      *    AUTO COLLECTION OTHER CHARGE CODE (FD-FORM-CHG-CD)
      *    IS ONLY ALLOWED:
      *        -  IF GP-FORM-CHG-CD OPTION IS 'Y'
      *        -  IF FD-TYPE               IS 'L'    LOAN FORM
      *        -  IF FD-FMTYPE             IS 'C'    COLLECTION FORM
      *        -  IF FD-1STFILE            IS 1 OR 4 DRIVE OF LOAN FILE
      *
      *    PER BELOW;
      *        -  AND AUTOMATIC LOAN MEMO ENTRY IS SET TO 'Y'
      *-----------------------------------------------------------------
       ENTER-CHGCD.

           IF (GP-FORM-CHG-CD NOT = "Y")
                    OR (
                        NOT (FD-TYPE = "L"
                                 AND FD-FMTYPE = "C"
                                  AND (FD-1STFILE = 1 OR 4)
                            )
                       )
              MOVE " " TO FD-FORM-CHG-CD
              MOVE "S  A010CHGCD." TO VDU
              PERFORM SCREENIO
              GO TO ENTER-MIN-DAYS.

           MOVE "E  A010CHGCD." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "S  A010CHGCD." TO VDU
              MOVE FD-FORM-CHG-CD  TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-DESC.

           IF ( UP-KEY )
               GO TO ENTER-VALID-BRANCH.

           IF ( F6-KEY )
              PERFORM CODE-SCAN
              IF ( IO-FG NOT = ZEROES )
                 GO TO ENTER-CHGCD
              ELSE
                 MOVE "00" TO VDUSTATUS.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-CHGCD.

           MOVE VDUBUF TO FD-FORM-CHG-CD.

           IF ( FD-FORM-CHG-CD NOT = SPACES )
              IF ( FD-LNMEMO NOT = "Y" )
                 MOVE "DOC DESIGN MEMO FLAG MUST BE 'Y'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-CHGCD
              ELSE
                 MOVE "FC"           TO CD-TYPE
                 MOVE FD-FORM-CHG-CD TO CD-CODE
                 PERFORM GET-CD
                 IF ( IO-FG = 9 )
                    GO TO ENTER-CHGCD.

       ENTER-MIN-DAYS.
           IF (GP-USE-MIN-FORM-SENT-DAYS NOT = "Y")
              MOVE 99 TO FD-MIN-FORM-SENT-DAYS
              MOVE "SNNN020DAYS." TO VDU
              PERFORM SCREENIO
              GO TO ENTER-SKIP-BAD-ADR.

           MOVE "ENNN020DAYS." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "SN N020DAYS." TO VDU
              MOVE FD-MIN-FORM-SENT-DAYS TO VDUNUM0
              PERFORM SCREENIO
              GO TO ENTER-DESC.

           IF ( UP-KEY )
              IF (GP-FORM-CHG-CD NOT = "Y") OR
                   ( NOT (FD-TYPE = "L" AND FD-FMTYPE = "C"
                           AND (FD-1STFILE = 1 OR 4) ) )
                 GO TO ENTER-VALID-BRANCH
              ELSE
                 GO TO ENTER-CHGCD.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-MIN-DAYS.

           MOVE VDUNUM0 TO FD-MIN-FORM-SENT-DAYS.

       ENTER-SKIP-BAD-ADR.
           MOVE "E  A010BAD." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "S  A010BAD." TO VDU
              MOVE FD-SKIP-BAD-ADR TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-DESC.

           IF ( UP-KEY )
              GO TO ENTER-MIN-DAYS.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-SKIP-BAD-ADR.

           IF VDUBUF NOT = "Y" AND NOT = "N" AND NOT = " "
                                           AND NOT  = "M"
              GO TO ENTER-SKIP-BAD-ADR.

           MOVE VDUBUF TO FD-SKIP-BAD-ADR.

       ENTER-SECURITY-PROFILE.
           MOVE "E  A010PROFILE." TO VDU.
           PERFORM SCREENIO.
           IF ( UP-KEY )
              GO TO ENTER-SKIP-BAD-ADR.
           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-SECURITY-PROFILE.

           MOVE VDUBUF TO FD-SECURITY-PROFILE.
           CALL "C$TOLOWER" USING TEST-PROFILE, VALUE 1.
           MOVE FD-SECURITY-PROFILE TO TEST-PROFILE.
           IF NOT VALID-PROFILE
              MOVE "PLEASE ENTER SPACE OR A-Z (UPPER OR LOWER)"
                 TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-SECURITY-PROFILE.

       EXIT-ENTER-TITLE.
           EXIT.

      **********************************
       ENTER-PROMPT SECTION.
      **********************************
      *  "F1V,F2^,F3-U1,F4-U5,F5-D5,F6-D1,F7-DONE" 
           MOVE LEGEND-01 TO MESS.
           PERFORM SEND-LEGEND.
       ENTER-TEXT.
           MOVE "S  A020ROW." TO VDU.
           MOVE FORM-LINE TO VDUBUF.
           PERFORM SCREENIO.
           PERFORM GET-LINE.
           MOVE "R  A800  0." TO VDU.
           MOVE ROW TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM RESEND-LINE
              IF ROW < MAX-ROW AND FORM-LINE < FD-BODY
                 ADD 1 TO ROW FORM-LINE
                 GO TO ENTER-TEXT
              ELSE
                 GO TO ENTER-TEXT.
           IF VDUSTATUS = "12"
              PERFORM RESEND-LINE
              IF ROW > 1 AND FORM-LINE > 1
                 SUBTRACT 1 FROM ROW FORM-LINE
                 GO TO ENTER-TEXT
              ELSE
                 GO TO ENTER-TEXT.
           IF VDUSTATUS = "16"
              MOVE 1 TO SCROLL-AMT
              PERFORM SCROLL-DOWN
              GO TO  ENTER-TEXT.
           IF VDUSTATUS = "18"
              MOVE 5 TO SCROLL-AMT
              PERFORM SCROLL-DOWN
              GO TO ENTER-TEXT.
           IF VDUSTATUS = "1:"
              MOVE 5 TO SCROLL-AMT
              PERFORM SCROLL-UP
              GO TO ENTER-TEXT.
           IF VDUSTATUS = "1<"
              MOVE 1 TO SCROLL-AMT
              PERFORM SCROLL-UP
              GO TO ENTER-TEXT.
           IF VDUSTATUS = "1>"
              PERFORM RESEND-LINE
              GO TO EXIT-ENTER-PROMPT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TEXT.
           MOVE VDUBUF TO EDT-LINE.
           MOVE FORM-LINE TO ROW-NO.
           PERFORM SET-FIELDS.
           PERFORM RESEND-LINE.
           IF FORM-LINE = FD-BODY
              GO TO ENTER-TEXT.
           IF ROW = MAX-ROW
              MOVE 5 TO SCROLL-AMT
              PERFORM SCROLL-UP
              ADD 10 TO FORM-LINE
              MOVE 11 TO ROW
              GO TO ENTER-TEXT.
           ADD 1 TO ROW FORM-LINE.
           GO TO ENTER-TEXT.
       EXIT-ENTER-PROMPT.
           EXIT.

      **********************************
       ENTER-FIELD SECTION.
      **********************************
       ENTER-ROW.
      * "F3-U1,F4-U5,F5-D5,F6-D1,F7-DONE" 
           MOVE LEGEND-02 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020ROW." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "16"
              MOVE 1 TO SCROLL-AMT
              PERFORM SCROLL-DOWN
              GO TO  ENTER-ROW.
           IF VDUSTATUS = "18"
              MOVE 5 TO SCROLL-AMT
              PERFORM SCROLL-DOWN
              GO TO ENTER-ROW.
           IF VDUSTATUS = "1:"
              MOVE 5 TO SCROLL-AMT
              PERFORM SCROLL-UP
              GO TO ENTER-ROW.
           IF VDUSTATUS = "1<"
              MOVE 1 TO SCROLL-AMT
              PERFORM SCROLL-UP
              GO TO ENTER-ROW.
           IF VDUSTATUS = "1>"
              GO TO EXIT-ENTER-FIELD.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ROW.
           IF VDUNUM0 = 0 OR VDUNUM0 > FD-BODY
              PERFORM INVALID-MSG
              GO TO ENTER-ROW.
           MOVE VDUNUM0 TO ROW-NO.
       ENTER-COL.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020COL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ROW.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-COL.
           IF VDUNUM0 = 0 OR VDUNUM0 > 80
              GO TO ENTER-COL.
           MOVE VDUNUM0 TO COL-NO.
       ENTER-KEYNO.
           MOVE "ENNN040KY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
      *       DELETES LAST FIELD ENTERED (BY ROW-NO & COL-NO)
              PERFORM DELETE-FIELD
              GO TO ENTER-ROW.
           IF VDUSTATUS = "16"
              PERFORM RULER-LINE
              GO TO ENTER-ROW.
           IF VDUSTATUS = "1U"
              GO TO ENTER-COL.
      * KEY 0 RESERVED FOR FIELDS THAT DO NOT REQUIRE (OR CAN'T)
      * BE RE-KEYED (E.G. GLOBAL, MULTI-COMPONENT KEYS)
           IF VDUSTATUS = "1<"
              MOVE "SN N040KY." TO VDU
              MOVE 0 TO KEY-NO VDUNUM0
              PERFORM SCREENIO
              GO TO ENTER-FIELDNO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FIELDNO.
           IF VDUNUM0 = 0
              MOVE "SN N040KY." TO VDU
              MOVE KEY-NO TO VDUNUM0
              PERFORM SCREENIO.
           IF VDUNUM0 = 0
              GO TO ENTER-KEYNO.

           MOVE VDUNUM0 TO KEY-NO.
           MOVE SPACES TO HOLD-FDB1-KEY.

           PERFORM GET-DATABASE.

           PERFORM CHECK-DB-IO.
           PERFORM CHECK-PROG-FIELD.
           MOVE SPACES TO HOLD-FDB1-KEY.
           IF IO-FG = 9
              GO TO ENTER-KEYNO.
       ENTER-FIELDNO.
           MOVE "ENNN040FN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-KEYNO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FIELDNO.
      * SEE NOTE ABOVE REGARDING 0
           IF VDUNUM0 = 0
              GO TO ENTER-FIELDNO.
           MOVE VDUNUM0 TO FIELD-NO.
           MOVE SPACES TO HOLD-FDB1-KEY.

           PERFORM GET-DATABASE.
           IF IO-FG = 0
              MOVE FDB-DTYPE(D-B-SUB) TO SAVE-DTYPE.

           PERFORM CHECK-DB-IO.
           PERFORM CHECK-PROG-FIELD.
           MOVE SPACES TO HOLD-FDB1-KEY.

           IF IO-FG = 9
              GO TO ENTER-FIELDNO.
       ENTER-EDIT.
           MOVE "ENNN020EN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-FIELDNO.
           IF VDUSTATUS = "1<"
              PERFORM SCAN-EDIT
              GO TO ENTER-EDIT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-EDIT.
           IF VDUNUM0 = 0
              MOVE "S  A020EN." TO VDU
              MOVE EDIT-NO TO VDUBUF
              PERFORM SCREENIO
              MOVE EDIT-NO TO VDUNUM0.
           IF VDUNUM0 = 0 OR VDUNUM0 > EDIT-MAX
              GO TO ENTER-EDIT.
           SUBTRACT 1 FROM VDUNUM0 GIVING EDIT-NO.
           PERFORM SCAN-EDIT.
           IF SAVE-DTYPE = 1
              IF EDIT-NO > 1
                 PERFORM INVALID-MSG
                 GO TO ENTER-EDIT.
           IF SAVE-DTYPE NOT = 1
              IF EDIT-NO = 1
                 PERFORM INVALID-MSG
                 GO TO ENTER-EDIT.
           PERFORM GET-FREE-FIELD.
           IF IO-FG = 9
              MOVE "> 50 FIELDS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ROW.
           MOVE ROW-NO TO FD-FIELDROW(F-F-SUB).
           MOVE COL-NO TO FD-FIELDCOL(F-F-SUB).
           MOVE FIELD-NO TO FD-FIELDNO(F-F-SUB).
           MOVE KEY-NO TO FD-FIELDKEY(F-F-SUB).
           MOVE EDIT-NO TO FD-FIELDED(F-F-SUB).
           PERFORM GET-EDIT-LINE.
           PERFORM RESEND-LINE.
           GO TO ENTER-ROW.
       EXIT-ENTER-FIELD.
           EXIT.

      *********************************************
       GET-EDIT-LINE SECTION.
      *********************************************
           COMPUTE TOP-LINE = FORM-LINE - ROW + 1.
           COMPUTE BOTTOM-LINE = TOP-LINE + 14.
           MOVE MAX-ROW TO SCROLL-AMT.
           IF ROW-NO < TOP-LINE
              PERFORM SCROLL-DOWN.
           IF ROW-NO > BOTTOM-LINE
              PERFORM SCROLL-UP.
           COMPUTE ROW = ROW + (ROW-NO - FORM-LINE).
           MOVE ROW-NO TO FORM-LINE.
           PERFORM GET-LINE.
           PERFORM SET-FIELDS.

      **********************************
       DELETE-FIELD SECTION.
      **********************************

           PERFORM GET-EDIT-LINE.
           MOVE 0 TO D-F-SUB.
       NEXT-DELETE-CHECK.
           ADD 1 TO D-F-SUB.
           IF FD-FIELDROW(D-F-SUB) NOT = ROW-NO OR
              FD-FIELDCOL(D-F-SUB) NOT = COL-NO
              GO TO CHECK-DELETE-END.

           MOVE FD-FIELDCOL(D-F-SUB) TO EDT-COL.
           MOVE FD-FIELDED(D-F-SUB) TO EDT-EDIT.
           MOVE FD-FIELDNO(D-F-SUB) TO VDUNUM0.

           PERFORM GET-DATABASE.
           IF IO-FG = 9
              MOVE SPACES TO HOLD-FDB1-KEY
              GO TO DELETE-FIELD-EXIT.

           MOVE 9 TO DATA-NUMOUT.
           MOVE FDB-DDIGITS(D-B-SUB) TO DATA-DIGITS.
           MOVE FDB-DTYPE(D-B-SUB) TO DATA-TYPE.
           MOVE FDB-DDECIMAL(D-B-SUB) TO DATA-DECIMAL.
           MOVE "Y" TO EDT-CLEAR-FG.
           PERFORM EDIT-DATA.
           PERFORM XFER-LINE.
           MOVE SPACES TO HOLD-FDB1-KEY.
           MOVE 0 TO FD-FIELDNO(D-F-SUB) FD-FIELDROW(D-F-SUB)
              FD-FIELDCOL(D-F-SUB) FD-FIELDKEY(D-F-SUB)
              FD-FIELDED(D-F-SUB).

           PERFORM GET-EDIT-LINE.
           PERFORM RESEND-LINE.

           GO TO DELETE-FIELD-EXIT.
       CHECK-DELETE-END.
           IF D-F-SUB < 50
              GO TO NEXT-DELETE-CHECK.
       DELETE-FIELD-EXIT.
           EXIT.

      **********************************
       RULER-LINE SECTION.
      **********************************
           PERFORM GET-EDIT-LINE.

           MOVE "S  A800  0." TO VDU.
           MOVE ROW           TO VDUROW.
           MOVE RULER-EDIT    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "PRESS <ENTER> TO RESUME" TO MESS.
           PERFORM SEND-MESS.

           PERFORM RESEND-LINE.
       EXIT-RULER-LINE.
           EXIT.

      **********************************
      *   G E T   D A T A B A S E      *
      *   INPUT : VDUNUM0 (FIELD#)     *
      *   OUTPUT: FDB-DBNO             *
      *           D-B-SUB  (SUBSCRIPT) *
      **********************************
       GET-DATABASE SECTION.
      **********************************
           MOVE 0 TO IO-FG.
           COMPUTE FDB-DBNO = (VDUNUM0 - 1) / 200 + 1.
           COMPUTE D-B-SUB = VDUNUM0 - (FDB-DBNO - 1) * 200.
           IF HOLD-FDB1-KEY NOT = FDB1-KEY
              PERFORM READ-FDB1-FILE.
           IF IO-FG = 0
              IF FDB-DNAME(D-B-SUB) = " "
                 MOVE 9 TO IO-FG.

           MOVE FDB1-KEY TO HOLD-FDB1-KEY.

      **********************************
      *   G E T   F R E E   F I E L D  *
      *   INPUT : FD-FIELDINFO         *
      *   OUTPUT: IO-FG, F-F-SUB       *
      **********************************
       GET-FREE-FIELD SECTION.
      **********************************
           MOVE 1 TO F-F-SUB IO-FG.
       NEXT-FREE-CHECK.
           IF FD-FIELDROW(F-F-SUB) = 0
              GO TO EXIT-GET-FREE-FIELD.
           IF F-F-SUB < 50
              ADD 1 TO F-F-SUB
              GO TO NEXT-FREE-CHECK.
           MOVE 9 TO IO-FG.
       EXIT-GET-FREE-FIELD.
           EXIT.

      **********************************
      *     S E T   F I E L D S        *
      *   INPUT : EDT-LINE, ROW-NO     *
      *   OUTPUT: VARIABLE FIELDS      *
      *   PLACED IN EDT-LINE           *
      **********************************
       SET-FIELDS SECTION.
      **********************************
           MOVE FD-INFO TO STR-INFO.
           MOVE SPACES TO HOLD-FDB1-KEY.
           MOVE 1 TO S-F-SUB.
       NEXT-ROW-CHECK.
           IF STR-FIELDROW(S-F-SUB) NOT = ROW-NO
              GO TO CHECK-SET-FIELD-END.

           MOVE STR-FIELDCOL(S-F-SUB) TO EDT-COL.
           MOVE STR-FIELDED(S-F-SUB) TO EDT-EDIT.
           MOVE STR-FIELDNO(S-F-SUB) TO VDUNUM0.
           PERFORM GET-DATABASE.
           MOVE ALL-X TO A-BUF.
           IF IO-FG = 9
              MOVE 0 TO DATA-DECIMAL
              MOVE 1 TO DATA-DIGITS
              MOVE 1 TO DATA-TYPE
              MOVE "?" TO A-BUF
           ELSE
              MOVE FDB-DDIGITS(D-B-SUB) TO DATA-DIGITS
              MOVE FDB-DTYPE(D-B-SUB) TO DATA-TYPE
              MOVE FDB-DDECIMAL(D-B-SUB) TO DATA-DECIMAL.
           IF DATA-TYPE = 1
              GO TO SET-FIELD-EDIT.
           MOVE 999999999999 TO DATA-NUMOUT.
           COMPUTE EDT-SUB = 13 - (DATA-DIGITS - DATA-DECIMAL).
           PERFORM SET-FIELD-0 UNTIL EDT-SUB = 0.
           GO TO SET-FIELD-EDIT.
       SET-FIELD-0.
           MOVE "0" TO D-N-CHAR(EDT-SUB).
           SUBTRACT 1 FROM EDT-SUB.
       SET-FIELD-EDIT.
           PERFORM EDIT-DATA.
       CHECK-SET-FIELD-END.
           IF S-F-SUB < 50
              ADD 1 TO S-F-SUB
              GO TO NEXT-ROW-CHECK.
           MOVE SPACES TO HOLD-FDB1-KEY.
           MOVE EDT-LINE TO FD-PROMPTS(ROW-NO).

      ****************************
       FIELD-SORT SECTION.
      ****************************
       FLD-SORT.
           MOVE 1 TO SUB.
           MOVE 2 TO SUB1.
       NEXT-FIELD-SORT.
           IF FD-ROWCOL(SUB) > FD-ROWCOL(SUB1)
              MOVE FD-FIELDINFO(SUB) TO HOLD-FIELDINFO
              MOVE FD-FIELDINFO(SUB1) TO FD-FIELDINFO(SUB)
              MOVE HOLD-FIELDINFO TO FD-FIELDINFO(SUB1)
              MOVE "Y" TO SWITCH-FLAG.
           IF SUB1 < 50
              ADD 1 TO SUB SUB1
              GO TO NEXT-FIELD-SORT.
           IF SWITCH-FLAG = "Y"
              MOVE "N" TO SWITCH-FLAG
              GO TO FLD-SORT.

      **********************************
       SCROLL-UP SECTION.
      **********************************
           IF FD-BODY < MAX-ROW
              GO TO EXIT-SCROLL-UP.

           COMPUTE LINE-WK = FORM-LINE - ROW + MAX-ROW + SCROLL-AMT.
           IF LINE-WK > FD-BODY
              MOVE FD-BODY TO LINE-WK.
           COMPUTE LINE-WK = LINE-WK - MAX-ROW + 1.
           MOVE LINE-WK TO FORM-LINE.
           PERFORM SETUP-LINES.
           PERFORM DISPLAY-ALL-LINES.
           MOVE 1 TO ROW.
       EXIT-SCROLL-UP.
           EXIT.

      **********************************
       SCROLL-DOWN SECTION.
      **********************************
           IF FD-BODY < MAX-ROW
              GO TO EXIT-SCROLL-DOWN.

           COMPUTE LINE-WK = FORM-LINE - ROW - SCROLL-AMT + 1.
           IF LINE-WK < 1
              MOVE 1 TO LINE-WK.
           MOVE LINE-WK TO FORM-LINE.
           PERFORM SETUP-LINES.
           PERFORM DISPLAY-ALL-LINES.
           MOVE 1 TO ROW.
       EXIT-SCROLL-DOWN.
           EXIT.

      **********************************
       SETUP-LINES SECTION.
      **********************************
           MOVE " " TO LINE-BUF.
           MOVE 1 TO ROW.
       NEXT-LINE-IN.
           MOVE FD-PROMPTS(LINE-WK) TO D-LN(ROW).
           IF ROW = MAX-ROW
              GO TO EXIT-SETUP-LINES.
           ADD 1 TO ROW.
           IF LINE-WK < FD-BODY
              ADD 1 TO LINE-WK
              GO TO NEXT-LINE-IN.
       EXIT-SETUP-LINES.
           EXIT.

      *********************************
       CODE-SCAN SECTION.
      *********************************

           MOVE "FC"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE 0 TO IO-FG.

           MOVE "S  A010CHGCD." TO VDU.
           MOVE CDSCAN-CODE      TO VDUBUF CD-CODE.
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF.

       EXIT-CODE-SCAN.
           EXIT.

      ***********************************
       WRITE-MODULE SECTION.
      ***********************************
           MOVE "UPDATE IN PROGRESS............" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM FIELD-SORT.

           PERFORM OPEN-FD1-FILE.
           MOVE HOLD-FD1-KEY TO FD1-KEY.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
           MOVE FD-REC TO CHANGED-FD-REC.
           IF ORIGINAL-FD-REC = CHANGED-FD-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-FD1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF FD-REC NOT = ORIGINAL-FD-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-FD-REC TO FD-REC.
           PERFORM REWRITE-FD1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-FD1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-FD1-FILE.

      ***********************************
       DELETE-MODULE SECTION.
      ***********************************
           MOVE "DELETE IN PROGRESS............" TO MESS.
           PERFORM SEND-LEGEND.

           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF FD-REC NOT = ORIGINAL-FD-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-FD1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-FD1-FILE.

      *********************************
       NEW-RECORD-SETUP SECTION.
      *********************************
           MOVE 1 TO NEW-FLAG.

           PERFORM CLEAR-FD-FILE.
      
           MOVE HOLD-FD1-KEY TO FD1-KEY.
           MOVE 0 TO ELE.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE " " TO DISPLAY-BUF.
           MOVE FD-NEXTFM      TO D-000.
           MOVE FD-TYPE-OF-DOC TO D-TYPE.
           MOVE FD-DESC        TO D-010.
           MOVE FD-TYPE        TO D-011.
           MOVE FD-PROG-PREFIX TO D-PROG-PREFIX.

           MOVE FD-PROG        TO D-020.
           PERFORM LOAD-TEST-VARIABLE.

           MOVE FD-1STFILE     TO D-030.
           MOVE FD-BODY        TO D-040.
           MOVE FD-TOP         TO D-050.
           MOVE FD-BOTTOM      TO D-060.
           MOVE FD-PRCONT      TO D-070.
           MOVE FD-OPTION      TO D-FD-OPTION.
           MOVE FD-INCR-DELCNT TO D-080.
           MOVE FD-FMTYPE      TO D-090.
           MOVE FD-LNMEMO      TO D-100.
           MOVE FD-VALID-BRANCH       TO D-110.
           MOVE FD-FORM-CHG-CD        TO D-120.
           MOVE FD-MIN-FORM-SENT-DAYS TO D-130.
           MOVE FD-SKIP-BAD-ADR       TO D-131.
           MOVE FD-SECURITY-PROFILE   TO D-132.
           MOVE 0 TO ROW-NO.
       NEXT-FIELD-DISP.
           ADD 1 TO ROW-NO.
           MOVE FD-PROMPTS(ROW-NO) TO D-140(ROW-NO).
           IF ROW-NO < MAX-ROW
              GO TO NEXT-FIELD-DISP.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************
       FORM-SCAN SECTION.
      *******************************************
           MOVE 0   TO FDSCAN-KEY.
           MOVE "C" TO FDSCAN-FMTYPE.
           MOVE "S" TO FDSCAN-ALLOW-CHG.
           MOVE "WI/FDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH FDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF FDSCAN-STATUS NOT = "00" OR FDSCAN-KEY = " "
              MOVE 9 TO IO-FG
              GO TO EXIT-FORM-SCAN.
           MOVE "SNNN030FMNO." TO VDU.
           MOVE FDSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "00" TO VDUSTATUS.
           MOVE FDSCAN-KEY-ALP TO SEL-FMNOX.

       EXIT-FORM-SCAN.
           EXIT.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "FDMAIN" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ******************************************************************
      *
      *    L O A D - T E S T - V A R I A B L E
      *
      *    IN  :  FD-PROG
      *
      *    OUT :  TEST-VARIABLE   [VARIABLE-FM]
      *
      *    DESC:  USED TO ALLOW ACCESS TO "F2-PROMPT" & "F3-FIELD".
      *           HAVE TO ALLOW ACCESS IF IT IS A LASER FORM AND
      *           THE LASER FORM IS LINKED TO LP/PCLPMC OR LP/PSTPMC.
      *           *NOTE* PCLPMC IS THE LASER FORM VERSION OF LP/LONPMC.
      *           *NOTE* PSTPMC IS THE LASER FORM VERSION OF LP/LONPMC.
      *
      ******************************************************************
       LOAD-TEST-VARIABLE SECTION.

           MOVE FD-PROG TO TEST-VARIABLE.

           IF ( FD-OPTION-PCL-LASER  ) AND
              ( FD-PROG-PREFIX = "0" )
              MOVE "MC" TO TEST-VARIABLE.

           IF ( FD-OPTION-PST-LASER  ) AND
              ( FD-PROG-PREFIX = "0" )
              MOVE "MC" TO TEST-VARIABLE.

       LOAD-TEST-VARIABLE-EXIT.
           EXIT.

       COPY "LIBLP/LPFDCLR.CPY".
       COPY "LIBLP/LPFRFD.CPY".
       COPY "LIBLP/LPEDT.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           DISPLAY SPACES UPON FDMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FDMAIN-SCREEN UPON FDMAIN-WINDOW-HANDLE.
           MOVE FDMAIN-FORM-FIELDS TO WR-BUF.
           MOVE FDMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/FDMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************
       SETUP-LINK-INFO SECTION.
      ****************************************
           MOVE FDMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE FDMAIN-HELP--DIR TO HELP-LINK-DIR.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       INVALID-MSG.
           MOVE "INVALID" TO MESS.
           PERFORM SEND-MESS.

       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
       GET-LINE.
           MOVE " " TO EDT-LINE.
           MOVE ROW TO CUR-ROW.
           MOVE EDT-LINE-WORK(CUR-ROW) TO EDT-LINE.
       XFER-LINE.
           MOVE EDT-LINE TO EDT-LINE-WORK(CUR-ROW).
       RESEND-LINE.
           MOVE ROW TO CUR-ROW.
           PERFORM XFER-LINE.
           MOVE EDT-LINE-WORK(CUR-ROW) TO WR-BUF.
           MOVE CUR-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-ALL-LINES.
           MOVE LINE-BUF TO WR-BUF.
           MOVE LINE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SCAN-EDIT.
           IF EDIT-NO = EDIT-MAX
              MOVE 1 TO EDIT-NO
           ELSE
              ADD 1 TO EDIT-NO.
           MOVE "S  A020EN." TO VDU.
           MOVE EDIT-NO TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A150ENX." TO VDU.
           MOVE EDIT-DESC(EDIT-NO) TO VDUBUF.
           PERFORM SCREENIO.
       CHECK-DB-IO.
           IF IO-FG = 9
              PERFORM INVALID-MSG
           ELSE
              MOVE "S  A080FNX." TO VDU
              MOVE FDB-DNAME(D-B-SUB) TO VDUBUF
              PERFORM SCREENIO.
       CHECK-PROG-FIELD.
      * FIND RELATIVE FILE #
           MULTIPLY FDB-DBNO BY 2 GIVING TEST-LOAN.
           IF D-B-SUB NOT > 100
              SUBTRACT 1 FROM TEST-LOAN.
           MOVE TEST-LOAN TO TEST-NOTE.
           IF IO-FG = 0
      * IF NOT VALID FIELD#, NO NEED TO FLAG FOR FIELD CLASS
              IF FD-TYPE = "L" AND NOT LOAN-FIELD
                 MOVE "INVALID LOAN FIELD" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
              ELSE
                 IF FD-TYPE = "N" AND NOT NOTE-FIELD
                    MOVE "INVALID NOTE FIELD" TO MESS
                    PERFORM SEND-MESS
                    MOVE 9 TO IO-FG.
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

      ***************************************
       IO-ROUTINE SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-FDB1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPFDBGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPFD1IN.CPY".
       COPY "LIBLP/LPFDB1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/FDMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FDMAIN-SCREEN.
           DESTROY FDMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
