      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      * THIS IS A COPY OF MN/STMAIN; USED FOR JAY TESTING RANDOM ITEMS
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  KCMAIN.
      *****************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   STATE FILE MAINTENANCE
      *
      * REV:
      *  081124 BAH ADDED ST-REINSTATE-WINDOW FOR REGACC PR# 228
      *             IF 'Y', WHEN POSTING 'SX', WI/REINST WILL OPEN
      *  080127 BAH ADDED ST-REPO-RENOTICE FOR REGACC PR# 236
      *  120725  CS SEE NOTE IN LIBWI/WINDOWSW REGARDING THE SETTING OF THE
      *             WINDOW-BOXED
      *  130116 MJD ADDED WINDOW/WINDOW HANDLE LOGIC.
      *  120906 BAH ADDED ST-TITLE-RELEASE-TYPE FOR REGACC PR# 320
      *  140421 BAH ADDED ST-AUCTION-DATE-REQ FOR REGACC PR# 352
      *
      * KEC 150116 {PR#00356} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *            ADDED SU-FILE; #6 SU-GEOGRAPHIC-CODE
      *
      * KEC 151022 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *     INCREASE FILE SIZE; ADDED ST-SIZE VALUE 300;
      *     MERGED SU-FILE <A15> INTO ST-FILE.
      *
      * 170719 BAH ADDED SU-MAX-LOAN-AVAIL #7. MAX LOAN AVAIL, WORLD PR#1260
      *  180914 BAH ADDED SU-MAX-TAX-LOAN-AMT (#8) AND SU-NSF-FEE-AMT (#9)
      *                                               WORLD PR#1334
      * BAH 2019-0628 WASNT ALLOWING YOU TO FIELD 7. QA0039
      * BAH 2019-0723 CLEAR FUTURE FIELDS
      * JKC 2020-1120 ADDED F5-SEARCH; F6-SCAN ALREADY EXISTS
      * JKC 2023-0411 PR#1234 TESTING THIS
      *****************************************************
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
           "@(#)UT/JCMAIN S35 04/04/25-12:32:58 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       01  HEX-LOWER-X5                PIC X(2)      VALUE X"7835".
       01  HEX-LOWER-Z1                PIC X(2)      VALUE X"7A31".
      *01  HEX-LOWER-X5.
      *    03  HEX-LOWER-X5-X          PIC X(1)      VALUE X"78".
      *    03  HEX-LOWER-X5-5          PIC X(1)      VALUE X"35".
      *01  HEX-LOWER-Z1.
      *    03  HEX-LOWER-Z1-Z          PIC X(1)      VALUE X"7A".
      *    03  HEX-LOWER-Z1-1          PIC X(1)      VALUE X"31".
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       01  TMPERR-DATE-NOW.
           03  TMPERR-DATE-CC          PIC 9(2).
           03  TMPERR-DATE-YY          PIC 9(2).
           03  TMPERR-DATE-MM          PIC 9(2).
           03  TMPERR-DATE-DD          PIC 9(2).
       01  FILLER            REDEFINES TMPERR-DATE-NOW.
           03  FILLER                  PIC 9(2).
           03  TMPERR-DATE-YYMMDD      PIC 9(6).

       01  TMPERR-DETAIL-LINE          PIC X(80)     VALUE SPACES.
       01  FILLER                      REDEFINES TMPERR-DETAIL-LINE.
           03  TMPERR-DETAIL-TIME      PIC 99/99/99.
           03  FILLER                  PIC X(1).
           03  TMPERR-DETAIL-LOGNAME   PIC X(10).
           03  FILLER                  PIC X(1).
           03  TMPERR-DETAIL-TEXT      PIC X(60).

       01  TMPERR-ECHO-CMD.
           03  FILLER                  PIC X(05)     VALUE "ECHO ".
           03  FILLER                  PIC X(01)     VALUE X"22".
           03  TMPERR-ECHO-TEXT        PIC X(80)     VALUE SPACES.
           03  FILLER                  PIC X(01)     VALUE X"22".

       01  TMPERR-OUTPUT-FILE.
           03  TMPERR-OUTPUT-BASE      PIC X(09)     VALUE "/TMP/ERR.".
           03  TMPERR-OUTPUT-YYMMDD    PIC 9(06)     VALUE ZEROES.
           03  FILLER                  REDEFINES TMPERR-OUTPUT-YYMMDD.
               05  TMPERR-OUTPUT-YYMMDD-YY           PIC 9(02).
               05  TMPERR-OUTPUT-YYMMDD-MM           PIC 9(02).
               05  TMPERR-OUTPUT-YYMMDD-DD           PIC 9(02).

       01  TMPERR-SYSTEM.
           03  TMPERR-SYSTEM-CMD       PIC X(120)    VALUE SPACES.
           03  FILLER                  PIC X(04)     VALUE " >> ".
           03  TMPERR-SYSTEM-OUTPUT    PIC X(55)     VALUE SPACES.

       01  TMPERR-TIME-NOW.
           03  TMPERR-TIME-HH          PIC 9(2).
           03  TMPERR-TIME-MM          PIC 9(2).
           03  TMPERR-TIME-SS          PIC 9(2).
           03  TMPERR-TIME-HD          PIC 9(2).
       01  FILLER                      REDEFINES TMPERR-TIME-NOW.
           03  TMPERR-TIME-HHMMSSHD    PIC 9(8).
       01  FILLER                      REDEFINES TMPERR-TIME-NOW.
           03  TMPERR-TIME-HHMMSS      PIC 9(6).
           03  FILLER                  PIC 9(2).


       01  TEST-NUMERIC                PIC X(10)   VALUE SPACES.
       01  FILLER                      REDEFINES TEST-NUMERIC.
           03  TEST-NUMERIC-3              PIC 9(3).
       01  FILLER                      REDEFINES TEST-NUMERIC.
           03  WSS-CDB-GC-COMM-GLFEENO     PIC 9(11)   COMP-6.
       01  FILLER                      REDEFINES TEST-NUMERIC.
           03  WSS-CDB-GC-COMM-PERCENT     PIC 999V99   COMP-6.
       01  FILLER                      REDEFINES TEST-NUMERIC.
           03  WSS-PD-PRPAID-AHEAD         PIC S9(7)V99   COMP.
       01  FILLER                      REDEFINES TEST-NUMERIC.
           03  WSS-PD-RESTRICT-ADJ         PIC S9(7)V99   COMP.
       01  FILLER                      REDEFINES TEST-NUMERIC.
           03  WSS-PD-DUAL-LOAN-ADJ        PIC S9(7)V99   COMP.
       01  FILLER                      REDEFINES TEST-NUMERIC.
           03  WSS-GT-POST-DATE         PIC 9(6) COMP-6.

       01  TEST-DATE PIC X(6).

       01  BETH-TEST   PIC 9(6).

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       01  ERRCD           PIC X VALUE " ".
       01  FIRST-SCAN      PIC 9       VALUE 0.
       01  CHAN            PIC X(4)    VALUE "CHAN".
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-TIME      PIC 9       VALUE 0.
       01  HOLD-ST1-KEY.
           05  HOLD-ST-CODE            PIC X(02).

      *=================================================================
      *   STCH-REC FROM 'REPLACING LEADING' ON ST-REC. (CHANGED-REC)
      *   STOR-REC FROM 'REPLACING LEADING' ON ST-REC. (ORIGINAL-REC)
      *=================================================================
       COPY "LIBLP/LP01ST.CPY" REPLACING LEADING "ST" BY "STOR".
       COPY "LIBLP/LP01ST.CPY" REPLACING LEADING "ST" BY "STCH".

      ******************************************************************
      *
      *    L M C - W O R K E R S
      *
      *=================================================================
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-FIELD               PIC X(03).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(14).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(16).
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(16).
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.

       01  STMAIN-WINDOW-HANDLE  PIC X(10).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(16).
           03  F02         PIC X.
           03  F03         PIC X.
           03  F04         PIC X.
           03  F05         PIC X.
           03  F06         PIC 9(04).
           03  F07         PIC Z(6).99.
           03  F08         PIC Z(5).99.
           03  F09         PIC Z(3).99.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(20)  VALUE "010 020 030 040 050 ".
           03  FILLER      PIC X(16)   VALUE "060 070 080 090.".

      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       78  MAX             VALUE 09.
       01  SPEC-TABLE.
      * 1. DESCRIPTION
           03  FILLER      PIC X(8) VALUE "E  A160 ".
      * 2. REINSTATE WINDOW
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 3. REPO RENOTICE FLAG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 4. TITLE RELEASE TYPE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 5. AUCTION DATE REQUIRED
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 6. GEOGRAPHIC CODE
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      * 7. MAX LOAN AVAILABLE
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 8. MAX TAX LOAN AMOUNT
           03  FILLER      PIC X(8) VALUE "ENNN052 ".
      * 9. NSF FEE AMOUNT
           03  FILLER      PIC X(8) VALUE "ENNN032 ".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/STMAIN_DEF.CPY".
       COPY "LIBMN/STMAIN_WKS.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/STSCANW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/STMAIN_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ****************************************
       MAIN-PROGRAM SECTION.
      ****************************************
      D    STOP MESS.
           PERFORM SQL-CONNECT.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *    PERFORM TMPERR-ROUTINE.

           MOVE SPACES TO TEST-NUMERIC.

           IF ( TEST-NUMERIC-3 = X"323032303230" )
               MOVE "BAD" TO MESS.

           IF TEST-NUMERIC NOT NUMERIC
               MOVE "BAD" TO MESS.

           MOVE  WSS-GT-POST-DATE TO BETH-TEST.
           IF BETH-TEST NOT NUMERIC
              MOVE "BAD" TO MESS.

           MOVE  TEST-NUMERIC TO BETH-TEST.
           IF BETH-TEST NOT NUMERIC
              MOVE "BAD" TO MESS.

   
      D    STOP TEST-NUMERIC.
      D    STOP WSS-CDB-GC-COMM-GLFEENO.
      D    STOP WSS-CDB-GC-COMM-PERCENT.
      D    STOP WSS-PD-PRPAID-AHEAD.
      D    STOP WSS-PD-RESTRICT-ADJ.
      D    STOP WSS-PD-DUAL-LOAN-ADJ.

           IF WSS-CDB-GC-COMM-GLFEENO NOT NUMERIC
              MOVE "BAD" TO MESS.
           IF WSS-CDB-GC-COMM-PERCENT NOT NUMERIC
              MOVE "BAD" TO MESS.
           IF WSS-PD-PRPAID-AHEAD     NOT NUMERIC
              MOVE "BAD" TO MESS.
           IF WSS-PD-RESTRICT-ADJ     NOT NUMERIC
              MOVE "BAD" TO MESS.
           IF WSS-PD-DUAL-LOAN-ADJ    NOT NUMERIC
              MOVE "BAD" TO MESS.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "STMAIN" TO VDU-FORM-NAME.
           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS STMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME.

           PERFORM REC-STATE-CTY-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************************************
      *
      *    T M P E R R - R O U T I N E
      *
      *=================================================================
      * DESC: DURING USER TESTING, WAS CRASHING OUT OF OTIS WHEN
      *       TRYING OPEN THE /XXX/XXXX/R#/LG/ERR.YYMMDD BUT GET
      *       "FILE ERROR 35 ON          /  /LG/ERR.250317".
      *       CREATE THE TMPERR ROUTINE TO LOG MORE INFORMATION SO THAT
      *       WE CAN NARROW THIS DOWN.  WE CAN LEAVE THIS FOR FUTURE
      *       USE BUT JUST COMMENT OUT THE 'PERFORM TMPERR-ROUTINE'.
      ******************************************************************
       TMPERR-ROUTINE SECTION.

           ACCEPT TMPERR-DATE-NOW        FROM CENTURY-DATE.

           CALL "C$TOLOWER" USING TMPERR-OUTPUT-FILE, VALUE 9.
           CALL "C$TOLOWER" USING TMPERR-ECHO-CMD,    VALUE 4.

           MOVE TMPERR-DATE-YYMMDD         TO TMPERR-OUTPUT-YYMMDD.
           MOVE TMPERR-OUTPUT-FILE         TO TMPERR-SYSTEM-OUTPUT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ALL "="                    TO TMPERR-DETAIL-TEXT.
           PERFORM TMPERR-WRITE-DETAIL.

           STRING "FORM-PATHNAME = ", FORM-PATHNAME
                  DELIMITED BY "  "      INTO TMPERR-DETAIL-TEXT.
           PERFORM TMPERR-WRITE-DETAIL.

           STRING "EXT-ACUSQL-DSN-NAME = ", EXT-ACUSQL-DSN-NAME
                  DELIMITED BY "  "      INTO TMPERR-DETAIL-TEXT.
           PERFORM TMPERR-WRITE-DETAIL.

           STRING "EXT-ACUSQL-DB-NAME = ", EXT-ACUSQL-DB-NAME
                  DELIMITED BY "  "      INTO TMPERR-DETAIL-TEXT.
           PERFORM TMPERR-WRITE-DETAIL.

           STRING "EXT-ACUSQL-SERVER-NAME = ", EXT-ACUSQL-SERVER-NAME
                  DELIMITED BY "  "      INTO TMPERR-DETAIL-TEXT.
           PERFORM TMPERR-WRITE-DETAIL.

           STRING "EXT-CONAME-USER-ID = ", EXT-CONAME-USER-ID
                  DELIMITED BY "  "      INTO TMPERR-DETAIL-TEXT.
           PERFORM TMPERR-WRITE-DETAIL.

           STRING "EXT-CONAME-SYSDATE = ", EXT-CONAME-SYSDATE
                  DELIMITED BY "  "      INTO TMPERR-DETAIL-TEXT.
           PERFORM TMPERR-WRITE-DETAIL.

           STRING "EXT-FILPATH = ", EXT-FILPATH
                  DELIMITED BY "  "      INTO TMPERR-DETAIL-TEXT.
           PERFORM TMPERR-WRITE-DETAIL.

           STRING "EXT-ROUTE-BUF = ", EXT-ROUTE-BUF
                  DELIMITED BY "  "      INTO TMPERR-DETAIL-TEXT.
           PERFORM TMPERR-WRITE-DETAIL.

      *=================================================================
       TMPERR-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T M P E R R - W R I T E - D E T A I L
      *
      ******************************************************************
       TMPERR-WRITE-DETAIL SECTION.

           ACCEPT TMPERR-TIME-NOW        FROM TIME.
           MOVE TMPERR-TIME-HHMMSS         TO TMPERR-DETAIL-TIME.
           INSPECT TMPERR-DETAIL-TIME REPLACING ALL "/" BY ":".
           MOVE "JAY 123"                  TO TMPERR-DETAIL-LOGNAME.

           MOVE TMPERR-DETAIL-LINE         TO TMPERR-ECHO-TEXT.
           MOVE TMPERR-ECHO-CMD            TO TMPERR-SYSTEM-CMD.
           CALL 'SYSTEM' USING TMPERR-SYSTEM.

           MOVE SPACES                     TO TMPERR-DETAIL-LINE.

      *=================================================================
       TMPERR-WRITE-DETAIL-EXIT.
           EXIT.

      ***************************************
       REC-STATE-CTY-MODULE SECTION.
      ***************************************
       REC-STATE-CTY-MODULE-NO.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.
           MOVE "E  A020STATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-STATE-CTY-MODULE.
           IF ( F5-KEY ) PERFORM SEARCH-DRIVER.
           IF VDUSTATUS = "1<"
              PERFORM STATE-SCAN
              IF IO-FG NOT = 0
                 GO TO REC-STATE-CTY-MODULE-NO
              ELSE
                 MOVE "S  A020STATE." TO VDU
                 MOVE STSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO
                 MOVE STSCAN-KEY TO VDUBUF.
           IF VDUSTATUS NOT = "00"
              GO TO REC-STATE-CTY-MODULE-NO.
           IF VDUBUF = SPACES
              GO TO REC-STATE-CTY-MODULE-NO.
           MOVE VDUBUF TO ST1-KEY HOLD-ST-CODE.

      *=================================================================
       READ-STATE-CTY-FILE.

           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
           IF ( IO-BAD )
              PERFORM NEW-RECORD-SETUP
              GO TO END-REC-STATE-CTY-MODULE.

           MOVE ST-REC                     TO STOR-REC.
           PERFORM DISPLAY-FIELDS.

       END-REC-STATE-CTY-MODULE.
           EXIT.

      ***************************************
       SEARCH-DRIVER SECTION.
      ***************************************

           MOVE SPACES TO ST-CODE.
           PERFORM OPEN-ST1-FILE.
           PERFORM START-ST1-FILE.

       SEARCH-SCANKEY-ENTRY.

           MOVE "E RA020SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO SEARCH-DRIVER-END.
           IF (        F5-KEY ) GO TO SEARCH-READ-NEXT.
           IF ( NOT ENTER-KEY ) GO TO SEARCH-SCANKEY-ENTRY.

           IF (VDUBUF = SPACES)
              IF ( FIRST-SCAN NOT = 0 )
                 MOVE ST1-KEY TO HOLD-ST1-KEY
                 MOVE ST-REC  TO STOR-REC
                 GO TO SEARCH-DRIVER-END
              ELSE
                 GO TO SEARCH-SCANKEY-ENTRY.

           MOVE VDUBUF TO ST-CODE.
           PERFORM START-ST1-FILE.

       SEARCH-READ-NEXT.

           PERFORM READ-ST1-FILE-NEXT.
           IF IO-BAD
              MOVE VDU---F1-KEY TO VDUSTATUS
              GO TO SEARCH-DRIVER-END.

           MOVE ST1-KEY TO HOLD-ST1-KEY.

           MOVE "S  A020STATE." TO VDU.
           MOVE ST-CODE TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.

           GO TO SEARCH-SCANKEY-ENTRY.

       SEARCH-DRIVER-END.

           IF ( F1-KEY )
              PERFORM FORM-RESET.

           MOVE ZEROES TO FIRST-SCAN.
           PERFORM CLOSE-ST1-FILE.

       SEARCH-DRIVER-EXIT.
           EXIT.

      ***************************************
       STATE-SCAN SECTION.
      ***************************************
           MOVE SPACES      TO STSCAN-KEY.
           MOVE STSCAN-BUF  TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF    TO STSCAN-BUF.

           IF  ( SCAN-STATUS NOT = "00" )
                      OR STSCAN-KEY = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-STATE-SCAN.

           MOVE 0               TO IO-FG.

       EXIT-STATE-SCAN.
           EXIT.

      ***************************************
       ENTRY-MODULE SECTION.
      ***************************************

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
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           CALL "C$JUSTIFY" USING ACTION "R".

           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
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
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO GOTO-FIELD.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06
                 F-07 F-08 F-09
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUBUF TO ST-DESC.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO ST-REINSTATE-WINDOW.
           IF VDUBUF NOT = "Y" AND NOT = "N" AND NOT = " "
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO ST-REPO-RENOTICE.
           IF VDUBUF NOT = "Y" AND NOT = "N" AND NOT = " "
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO ST-TITLE-RELEASE-TYPE.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO ST-AUCTION-DATE-REQ.
           IF ST-AUCTION-DATE-REQ NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM0 TO ST-GEOGRAPHIC-CODE.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM2 TO ST-MAX-LOAN-AVAIL.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM2 TO ST-MAX-TAX-LOAN-AMT.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM2 TO ST-NSF-FEE-AMT.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ***************************************
       WRITE-MODULE SECTION.
      ***************************************
           PERFORM OPEN-ST1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE ST-REC TO STCH-REC.
           IF STOR-REC = STCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-ST1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF ST-REC NOT = STOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE STCH-REC TO ST-REC.
           PERFORM REWRITE-ST1-FILE.
           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-ST1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-ST1-FILE.

      *******************************************
       DELETE-MODULE SECTION.
      *******************************************

           PERFORM OPEN-ST1-FILE.

           IF ( NEW-FLAG NOT = ZEROES )
              GO TO DELETE-MODULE-END.

           PERFORM READ-ST1-FILE.
           IF ( IO-BAD )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO DELETE-MODULE-END.

           IF ( ST-REC NOT = STOR-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO DELETE-MODULE-END.

           PERFORM DELETE-ST1-FILE.

      * MAKE ENTRY IN ERRLOG REGARDING DELETE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY.
           STRING ST1-KEY, " ", "DELETED" DELIMITED BY SIZE
                                    INTO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       DELETE-MODULE-END.

           PERFORM CLOSE-ST1-FILE.

       DELETE-MODULE-EXIT.
           EXIT.

      ***************************************
       NEW-RECORD-SETUP SECTION.
      ***************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-ST-FILE.

           MOVE HOLD-ST1-KEY     TO ST1-KEY.

           MOVE 0 TO ELE.
           MOVE "S  A010020." TO VDU.
           MOVE "N" TO ST-REINSTATE-WINDOW VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010030." TO VDU.
           MOVE "N" TO ST-REPO-RENOTICE VDUBUF.
           PERFORM SCREENIO.
           
      ***************************************
       DISPLAY-FIELDS SECTION.
      ***************************************
           MOVE ST-DESC TO F01.
           MOVE ST-REINSTATE-WINDOW   TO F02.
           MOVE ST-REPO-RENOTICE      TO F03.
           MOVE ST-TITLE-RELEASE-TYPE TO F04.
           MOVE ST-AUCTION-DATE-REQ   TO F05.
           MOVE ST-GEOGRAPHIC-CODE    TO F06.
           MOVE ST-MAX-LOAN-AVAIL     TO F07.
           MOVE ST-MAX-TAX-LOAN-AMT   TO F08.
           MOVE ST-NSF-FEE-AMT        TO F09.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF  TO WR-BUF.
           PERFORM CALL-WRFORM.

      ***************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ***************************************

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE " TO "                     TO LMC-TO-X.

           IF STOR-DESC NOT = STCH-DESC
             MOVE "01 "                   TO LMC-FIELD
             MOVE "STATE NAME    "        TO LMC-DESC
             MOVE STOR-DESC               TO LMC-ORG-TEXT
             MOVE STCH-DESC               TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF STOR-REINSTATE-WINDOW NOT = STCH-REINSTATE-WINDOW
             MOVE "02 "                   TO LMC-FIELD
             MOVE "REINSTATE WIND"        TO LMC-DESC
             MOVE STOR-REINSTATE-WINDOW   TO LMC-ORG-TEXT
             MOVE STCH-REINSTATE-WINDOW   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF STOR-REPO-RENOTICE NOT = STCH-REPO-RENOTICE
             MOVE "03 "                   TO LMC-FIELD
             MOVE "REPO RENOTICE "        TO LMC-DESC
             MOVE STOR-REPO-RENOTICE      TO LMC-ORG-TEXT
             MOVE STCH-REPO-RENOTICE      TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF STOR-TITLE-RELEASE-TYPE NOT = STCH-TITLE-RELEASE-TYPE
             MOVE "04 "                   TO LMC-FIELD
             MOVE "TITLE REL TYPE"        TO LMC-DESC
             MOVE STOR-TITLE-RELEASE-TYPE TO LMC-ORG-TEXT
             MOVE STCH-TITLE-RELEASE-TYPE TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF STOR-AUCTION-DATE-REQ NOT = STCH-AUCTION-DATE-REQ
             MOVE "05 "                   TO LMC-FIELD
             MOVE "AUC DATE REQ  "        TO LMC-DESC
             MOVE STOR-AUCTION-DATE-REQ   TO LMC-ORG-TEXT
             MOVE STCH-AUCTION-DATE-REQ   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF STOR-GEOGRAPHIC-CODE NOT = STCH-GEOGRAPHIC-CODE
             MOVE "06 "                   TO LMC-FIELD
             MOVE "GEOGRAPHIC CD "        TO LMC-DESC
             MOVE STOR-GEOGRAPHIC-CODE    TO LMC-ORG-TEXT
             MOVE STCH-GEOGRAPHIC-CODE    TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF STOR-MAX-LOAN-AVAIL NOT = STCH-MAX-LOAN-AVAIL
             MOVE "07 "                   TO LMC-FIELD
             MOVE "MAX LOAN AVAIL"        TO LMC-DESC
             MOVE STOR-MAX-LOAN-AVAIL     TO LMC-ORG-AMT
             MOVE STCH-MAX-LOAN-AVAIL     TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.
           IF STOR-MAX-TAX-LOAN-AMT NOT = STCH-MAX-TAX-LOAN-AMT
             MOVE "08 "                   TO LMC-FIELD
             MOVE "MAX LOAN AMT  "        TO LMC-DESC
             MOVE STOR-MAX-TAX-LOAN-AMT     TO LMC-ORG-AMT
             MOVE STCH-MAX-TAX-LOAN-AMT     TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.
           IF STOR-NSF-FEE-AMT NOT = STCH-NSF-FEE-AMT
             MOVE "09 "                   TO LMC-FIELD
             MOVE "MAX LOAN AMT  "        TO LMC-DESC
             MOVE STOR-NSF-FEE-AMT     TO LMC-ORG-AMT
             MOVE STCH-NSF-FEE-AMT     TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

      ***************************************
       LOG-FIELD-CHANGE SECTION.
      ***************************************

           MOVE "F"           TO ERRLOGW-ACTION.
           MOVE LMC-DETAIL    TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       LOG-FIELD-CHANGE-EXIT.
           EXIT.

      ***************************************
       SETUP-LINK-INFO SECTION.
      ***************************************
           MOVE STMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE STMAIN-HELP--DIR TO HELP-LINK-DIR.

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO STMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO STMAIN-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY STMAIN-SCREEN UPON STMAIN-WINDOW-HANDLE.
           MOVE STMAIN-FORM-FIELDS TO WR-BUF. 
           MOVE STMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

       FORM-RESET-EXIT.
           EXIT.


      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/STMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBLP/LPSTCL.CPY".
       COPY "LIBGB/DATER.CPY".


      ********************************
       MISC-PARAGRAPHS SECTION.
      ********************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
             TO MESS.
           PERFORM SEND-MESS.

      ***************************************
       IO-ROUTINES SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
      *COPY "LIBLP/LPST1IN.CPY".
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      * COPYMEMBER: LIBLP/LPST1IN
       LOAD-ST1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ST-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ST-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ST-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO ST-ALL-MACHINE
              MOVE ST-ALL-PATH             TO ST-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ST-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ST-ALL-BASE
              MOVE FILPATH-MACHINE         TO ST-ALL-MACHINE
              MOVE ST-ALL-PATH             TO ST-PATH.

      *-----------------------------------------------------------------
       OPEN-ST1-FILE.
           PERFORM LOAD-ST1-FILE.
           PERFORM OPEN-IT.
           MOVE ST-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-ST1-FILE.
           PERFORM START-IT.
           MOVE ST-PATH-SQL TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.

           IF ( ST1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-ST1-FILE.

           IF ( QST1-WEND-CODE = SPACES )
              MOVE ST-CODE        TO QST1-WBEG-CODE
              MOVE ALL "Z"        TO QST1-WEND-CODE.

           EXEC SQL
            DECLARE ST1_CURSOR CURSOR FOR
            SELECT STFILE.ST_CODE,
                   CAST(STFILE.ST_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   STFILE.ST_DESC,
                   STFILE.ST_REINSTATE_WINDOW,
                   STFILE.ST_REPO_RENOTICE,
                   STFILE.ST_TITLE_RELEASE_TYPE,
                   STFILE.ST_AUCTION_DATE_REQ,
                   STFILE.ST_GEOGRAPHIC_CODE,
                   STFILE.ST_MAX_LOAN_AVAIL,
                   STFILE.ST_MAX_TAX_LOAN_AMT,
                   STFILE.ST_NSF_FEE_AMT
            FROM DBO.STFILE
            WHERE STFILE.ST_CODE >= :QST1-WBEG-CODE
              AND STFILE.ST_CODE <= :QST1-WEND-CODE
            ORDER BY STFILE.ST_CODE
           END-EXEC.

           EXEC SQL
               OPEN ST1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QST1-WBEG-CODE
                          QST1-WEND-CODE.

           MOVE STAT---GOOD TO ST1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-ST1-FILE.
           PERFORM READ-IT.
           MOVE ST-PATH-SQL TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.
           INITIALIZE QST-REC.

           MOVE ST-CODE   TO QST-CODE.

           EXEC SQL
            SELECT STFILE.ST_CODE,
                   CAST(STFILE.ST_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   STFILE.ST_DESC,
                   STFILE.ST_REINSTATE_WINDOW,
                   STFILE.ST_REPO_RENOTICE,
                   STFILE.ST_TITLE_RELEASE_TYPE,
                   STFILE.ST_AUCTION_DATE_REQ,
                   STFILE.ST_GEOGRAPHIC_CODE,
                   STFILE.ST_MAX_LOAN_AVAIL,
                   STFILE.ST_MAX_TAX_LOAN_AMT,
                   STFILE.ST_NSF_FEE_AMT,
                   STFILE.ST_JAY_BOGUS_TEST_FIELD
            INTO :QST-REC
            FROM DBO.STFILE
            WHERE STFILE.ST_CODE = :QST-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ST1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-ST-FIELDS.

      *-----------------------------------------------------------------
       READ-ST1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ST-PATH-SQL TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.
           INITIALIZE QST-REC.

           IF ( ST1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH ST1_CURSOR INTO :QST-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ST1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-ST-FIELDS.

      *-----------------------------------------------------------------
       WRITE-ST1-FILE.

           ACCEPT ST-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE ST-PATH-SQL TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.
           PERFORM SET-ST-FIELDS.

           EXEC SQL
            INSERT INTO DBO.STFILE
             (STFILE.ST_CODE,
              STFILE.ST_LTOUCH_DATE,
              STFILE.ST_DESC,
              STFILE.ST_REINSTATE_WINDOW,
              STFILE.ST_REPO_RENOTICE,
              STFILE.ST_TITLE_RELEASE_TYPE,
              STFILE.ST_AUCTION_DATE_REQ,
              STFILE.ST_GEOGRAPHIC_CODE,
              STFILE.ST_MAX_LOAN_AVAIL,
              STFILE.ST_MAX_TAX_LOAN_AMT,
              STFILE.ST_NSF_FEE_AMT)
            VALUES
             (:QST-CODE,
              :QST-LTOUCH-DATE,
              :QST-DESC,
              :QST-REINSTATE-WINDOW,
              :QST-REPO-RENOTICE,
              :QST-TITLE-RELEASE-TYPE,
              :QST-AUCTION-DATE-REQ,
              :QST-GEOGRAPHIC-CODE,
              :QST-MAX-LOAN-AVAIL,
              :QST-MAX-TAX-LOAN-AMT,
              :QST-NSF-FEE-AMT)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-ST1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-ST1-FILE.

           ACCEPT ST-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE ST-PATH-SQL TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.
           PERFORM SET-ST-FIELDS.

           EXEC SQL
            UPDATE DBO.STFILE
             SET
              STFILE.ST_CODE = :QST-CODE,
              STFILE.ST_LTOUCH_DATE =
                 :QST-LTOUCH-DATE,
              STFILE.ST_DESC = :QST-DESC,
              STFILE.ST_REINSTATE_WINDOW = :QST-REINSTATE-WINDOW,
              STFILE.ST_REPO_RENOTICE = :QST-REPO-RENOTICE,
              STFILE.ST_TITLE_RELEASE_TYPE =
                   :QST-TITLE-RELEASE-TYPE,
              STFILE.ST_AUCTION_DATE_REQ = :QST-AUCTION-DATE-REQ,
              STFILE.ST_GEOGRAPHIC_CODE = :QST-GEOGRAPHIC-CODE,
              STFILE.ST_MAX_LOAN_AVAIL = :QST-MAX-LOAN-AVAIL,
              STFILE.ST_MAX_TAX_LOAN_AMT = :QST-MAX-TAX-LOAN-AMT,
              STFILE.ST_NSF_FEE_AMT = :QST-NSF-FEE-AMT
            WHERE STFILE.ST_CODE = :QST-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-ST1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-ST1-FILE.

           PERFORM DELETE-IT.
           MOVE ST-PATH-SQL TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.
           PERFORM SET-ST-FIELDS.

           EXEC SQL
            DELETE FROM DBO.STFILE
            WHERE STFILE.ST_CODE = :QST-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-ST1-FILE.

      *-----------------------------------------------------------------
       CLOSE-ST1-FILE.

           PERFORM CLOSE-IT.

           IF ( ST1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE ST1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO ST1-CURSOR-STAT.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       COPY "LIBGB/FERRORS.CPY".

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
       END-RUN.
           PERFORM CLOSE-FILES.
%%%    EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY STMAIN-SCREEN.
           DESTROY STMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
