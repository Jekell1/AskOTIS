       IDENTIFICATION DIVISION.
       PROGRAM-ID.   COLLET.
       DATE-WRITTEN. 032101.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  MOST RECENT COLLECTION LETTER REPORT PR# 256
      *
      *=================================================================
      * REV:
      * KEC 032196 ADDED CHECK ON DATE RANGE.
      * KEC 041696 COMPLETED THE 'TOTALS' ROUTINE.
      * KEC 062196 FIXED 'CHECK-COLLECTOR'.  WAS A PARAGRAPH, SHOULD
      *            HAVE BEEN A SECTION.
      * KEC 110796 CHANGE THE 'CHECK-SET-PARAMETERS' SECTION TO REDUCE
      *            AND SPEED-UP I/O.
      * MJD 013096 CHANGED TO INCLUDE CURRENT WITH POTENTIALS.
      * GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      * BLV 990514 FIXED LOAN MEMO DATE ROUTINES FOR YEAR 2000,
      *            TRASHED SHIFT DATE ROUTINES (SHIFTY)
      * BLV 990728 FIXED FIND-DELINQUENCY ROUTINE TO PUT MISPAYS
      *            POT + CUR BUCKETS (WERE GOING TO 150+)
      * MJD 991020 ADDED START TIME RANGE
      * BLV 991116 FIXED MMDDYY SECTION IN THE MIDDLE OF MISC.
      *            PARAGRAPHS - FELL THROUGH TO SEND-LEGEND &
      *            DIDN'T WORK PIGGY BACK
      *
      * GLB 010321 FROM COLRPT.C PR# 256 WORLD
      *            PRINT LAST 6 COLLECTION LETTER NO.'S AND DATES
      * BLV 010426 FIX SOME BATCH STUFF, ADD OPTION TO PRINT-RANGES
      * BAH 020115 CHANGED TO EXIT TO LPR2MU, SECOND REPORT MENU
      * BAH 160125 SPLIT OUT LTFILE, PD0003
      * KEC 2016.0210 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *  CS 160830 CHANGED HOLD-CLASS 9(2) TO 9(3)
      *  BAH 170517 ACTIVATE 8 DIGIT DATES, PD0006
      *  CS 170523 ADD OPTION TO INCLUDE/SKIP/ONLY ZERO BALANCE PR#1248
      *                                                WORLD
      *  CS 170602 WHEN ZERO BAL IS INCLUDE OR ONLY, ONLY INCLUDE IF THE
      *            PAID OFF ACCOUNT HAS A QUALIFYING FORM WITHIN DATE RANGE
      *                                            WORLD PR#1248
      * KEC 2017.0607 ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 2018.1127 GLOBAL LMFILE
      * BAH 2018.1205 GLOBAL LNFILE
      * BAH 2019.0517 WAS INCLUDING ALL P&L ACCOUNTS, MISSING PLDATE
      *               CHECK THAT A15 HAD. QA0009
      * BAH 2019.0925 REMOVED ACCESS-CALL LNFILE
      * RMZ 2023.0830 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      * BAH 2024.0624 DISPLAY PARAMETERS WAS MISSING "PRINT RANGES" AND
      *               ENTER PARAMETERS WAS DISPLAYING "ZERO BAL I,S,O" FOR
      *               ENTER RANGE WHEN IT SHOULD BE Y,N #1661 S35Q-72
      ******************************************************************
 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/COLLET.CBL S35 11/22/23-07:32:39 >".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *-----------------------------------------------------------------
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS-ZBAL-ISO         PIC X        VALUE SPACE.
       01  QWS-LM-CLASS         PIC 9(3)     VALUE 0.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN3-CONN-OPEN-SW     PIC X        VALUE "N".
       01  LM1-CONN-OPEN-SW     PIC X        VALUE "N".
       01  PR-IFN               PIC X(2)     VALUE "PR".

      ******************************************************************
      *
      *    L I N E - W O R K E R S
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      ******************************************************************
      *
      *   M I S C - W O R K E R S
      *
      ******************************************************************
       01  MSEL-LIST            PIC X(5)            VALUE "MSEL.".
       01  SUB                  PIC 9(2)            COMP-3.

       01  RECORDS-FOUND        PIC X               VALUE " ".
           88  NO-RECORDS-FOUND                     VALUE "N".
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).

       01  CLASS-FG             PIC X(3)    VALUE "   ".
           88  CLASS-BEG                      VALUE "BEG".
           88  CLASS-END                      VALUE "END".

       01  HOLD-FIELDS.
           03  HOLD-LN-OWNBR                 PIC 9(4)  VALUE ZEROES.
           03  HOLD-LN-CLASS                    PIC 9(3)  VALUE ZEROES.
      * THESE DATES ARE YYYYMMDD SUBTRACTED FROM ALL 9'S
           03  HOLD-DATES.
               05  HOLD-BEG-DATE             PIC 9(8)  VALUE ZEROES.
               05  HOLD-END-DATE             PIC 9(8)  VALUE ZEROES.

       01  HOLD-LPS-NUM.
           03  H-LPS-CLASS      PIC XXX.
           03  H-LPS-NUM        PIC X(13).

       01  WS-LN-KEY.
           03  WS-LN-OWNBR      PIC 9(4).
           03  WS-LN-CLASS      PIC 9(3).
           03  WS-LN-ACCTNO     PIC 9(8).

       01  WS-LM-KEY.
           03  WS-LM-BRNO       PIC 9(4).
           03  WS-LM-CLASS      PIC 9(3).
           03  WS-LM-ACCTNO     PIC 9(8).

      *=================================================================
      *    TEST-LM-MEMO1 - USED TO TEST THE VALUE IN LM-MEMO1.
      *
      *    TEST-LM-FORMX-A15 AND TEST-LM-FORMX-A30 SHOULD HAVE
      *    THE VALUE "FORM # " IN IT FOR A VALID LM-MEMO1 (FOR FMNO).
      *
      *    NEED TO CHECK IF TEST-LM-EXTRA-A15 IS NUMERIC (OR NOT SPACES)
      *    IF NUMERIC, THEN IT IS AN A30 FMNO (4 BYTES) OTHERWISE
      *    IT IS AN A15 FMNO.  LOAD THE ACTUAL FMNO TO
      *    TEST-LM-MEMO-FMNO FOR EASIER USE.
      *=================================================================
       01  TEST-LM-MEMO1               PIC X(30)     VALUE SPACES.
       01  TEST-LM-MEMO1-A15 REDEFINES TEST-LM-MEMO1.
           03  TEST-LM-FORMX-A15       PIC X(07).
           03  TEST-LM-FMNO-A15        PIC 9(03).
           03  TEST-LM-EXTRA-A15       PIC 9(01).
           03  TEST-LM-EXTRAX-A15      REDEFINES TEST-LM-EXTRA-A15
                                       PIC X(01).
           03  FILLER                  PIC X(19).
       01  TEST-LM-MEMO1-A30 REDEFINES TEST-LM-MEMO1.
           03  TEST-LM-FORMX-A30       PIC X(07).
           03  TEST-LM-FMNO-A30        PIC 9(04).
           03  FILLER                  PIC X(19).

       01  TEST-LM-FMNO                PIC 9(04).

      *=================================================================
       01  LEGEND               PIC X(70).

       01  WS-INVALID-LM-FG              PIC X(1)     VALUE SPACES.

       01  LOAN-MEMO-TABLE.
           03  LOAN-MEMO-FORM                PIC 999 OCCURS 20.
           03  LOAN-MEMO-DATE                PIC 9(6) OCCURS 20.
       01  LOAN-MEMO-MAX                     PIC 99  VALUE  20.
       01  LOAN-PRINT-MAX                    PIC 99  VALUE 6.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  AGE-DATE            PIC 9(8).
           03  BEG-BRANCH          PIC 9(4).
           03  END-BRANCH          PIC 9(4).
           03  BEG-CLASS           PIC 9(3).
           03  END-CLASS           PIC 9(3).
           03  BEG-FORM            PIC 9(4).
           03  END-FORM            PIC 9(4).
           03  BEG-DATE            PIC 9(8).
           03  END-DATE            PIC 9(8).
           03  BEG-RAGE            PIC 9(4).
           03  END-RAGE            PIC 9(4).
           03  CMISS               PIC X.
           03  RPOT                PIC X.
           03  ZBAL-ISO            PIC X. 
           03  GROUP-FLAG          PIC X.
           03  ENT-GROUP           PIC X(4).
           03  PRINT-RANGES        PIC X(1).

       01  VERSION-CONTROL         PIC X VALUE "E".

       01  SV-BEG-BR               PIC 9999  VALUE 9999.
       01  SV-END-BR               PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG           PIC X     VALUE SPACES.
       01  SV-ENT-GROUP            PIC X(4)  VALUE SPACES.

      *****************************************************************
      *
      *  P R I N T  B U F F E R S
      *
      *****************************************************************
       01  HEAD1.
           03  H1-DATE       PIC X(8)    VALUE " ".
           03  FILLER        PIC XX      VALUE " ".
           03  H1-TIME       PIC X(8)    VALUE " ".
           03  FILLER        PIC X(28)   VALUE " ".
           03  H1-NAME       PIC X(40)   VALUE " ".
           03  FILLER        PIC X(37)   VALUE " ".
           03  FILLER        PIC X(5)    VALUE "PAGE".
           03  H1-PAGE-NO    PIC ZZZ9.

      *---------------------------------------------------------------
       01  HEAD2.
           03  FILLER        PIC X(11)   VALUE "USER ID  : ".
           03  H2-USERID     PIC X(10).
           03  FILLER        PIC X(20)   VALUE " ".
           03  FILLER        PIC X(25)   VALUE
               "COLLECTION LETTER REPORT ".
           03  H2-DATE1      PIC 99/99/99.
           03  FILLER        PIC X(6)    VALUE " THRU ".
           03  H2-DATE2      PIC 99/99/99.
           03  FILLER        PIC X(19)   VALUE " ".
           03  FILLER        PIC X(12)   VALUE "AGED AS OF: ".
           03  H2-AGEDATE    PIC X(10).
           03  H2-JULIAN     PIC 999.


      *---------------------------------------------------------------
       01  HEAD3.
           03  FILLER        PIC X(11)   VALUE "BRANCH   : ".
           03  H3-BRNO       PIC 9(4).
           03  FILLER        PIC X(2)     VALUE " ".
           03  H3-BRDESC     PIC X(40)    VALUE " ".
           03  FILLER        PIC X(75)    VALUE " ".

      *---------------------------------------------------------------
       01  HEAD4.
           03  FILLER        PIC X(11)   VALUE "CLASS    : ".
           03  H4-CLASS      PIC 9(2).
           03  FILLER        PIC X(2)     VALUE " ".
           03  H4-CLDESC     PIC X(40)    VALUE " ".
           03  FILLER        PIC X(75)    VALUE " ".

      *---------------------------------------------------------------
       01  HEAD5.
           03  FILLER        PIC X(8)    VALUE "ACCT NO.".
           03  FILLER        PIC X(6)    VALUE SPACES.
           03  FILLER        PIC X(8)    VALUE "BORROWER".
           03  FILLER        PIC X(10)   VALUE SPACES.
           03  FILLER        PIC X(18)   VALUE
               "REC DEL   BALANCE ".
           03  FILLER        PIC X(28)   VALUE
           "- - - - - - - - - - - - - - ".
           03  FILLER        PIC X(24)   VALUE
           "L E T T E R S    S E N T".
           03  FILLER        PIC X(29)   VALUE
           "- - - - - - - - - - - - - - ".

       01  DETAIL-LINE      PIC X(132)   VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM           PIC X(13).
           03  FILLER               PIC X(1).
           03  D-BW-NAME            PIC X(20).
           03  FILLER               PIC X(1).
           03  D-RECENCYX.
               05  D-RECENCY   PIC ZZZZ.
           03  D-CURRENT-BAL       PIC ZZZZZZ9.99-.
           03  D-FORM-AREA.
               05  D-FORMS-AREA OCCURS 6.
                   07  D-MEMO-FORM    PIC 999.
                   07  D-MEMO-DASH1   PIC X.
                   07  D-MEMO-DATE    PIC 99/99/99.
                   07  FILLER         PIC X(2).

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(7)9.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/COLLET_DEF.CPY".
       COPY "LIBLP/COLLET_WKS.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/COLLET_SCN.CPY".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LM1-CONNECTION.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ***************************************************************
       MAIN-MODULE SECTION.
      ***************************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS. 

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "COLLET" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "REPORT IN PROGRESS.............", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LM-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           MOVE 99 TO LN-COUNT.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LM1-FILE.
           PERFORM OPEN-LN3-FILE.

           INITIALIZE WS-LN-KEY WS-LM-KEY.  
    
           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE. *> SSP - LN,BW,LC

           MOVE LN-OWNBR      TO LM-BRNO
                                 QLM1-WBEG-BRNO
                                 QLM1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LM-ACCTNO
                                 QLM1-WBEG-ACCTNO
                                 QLM1-WEND-ACCTNO.
           MOVE HOLD-END-DATE TO LM-DATE
                                 QLM1-WBEG-DATE-REV.
           MOVE 0             TO LM-SEQ
                                 QLM1-WBEG-SEQ.
      *    MOVE ALL "9"       TO QLM1-WEND-DATE-REV
      *                          QLM1-WEND-SEQ.
           MOVE HOLD-BEG-DATE TO QLM1-WEND-DATE-REV.
           MOVE ALL "9"       TO QLM1-WEND-SEQ.

           PERFORM START-LM1-FILE. *> SSP - LMFILE

       NEXT-LN-RECORD.
           PERFORM READ-LN3-FILE-NEXT. *> EMBEDDED - LN,BW,LC
           IF ( IO-FG = 9 ) OR ( LN-OWNBR NOT = LN-PATH-OWNBR )
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           IF LN-CLASS  > END-CLASS
              GO TO END-OF-GROUP.

           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN-RECORD.

           IF ZBAL-ISO = "O"
              IF (LN-CURBAL NOT = 0) OR
                 (LN-INTBAL NOT = 0) OR
                 (LN-LCBAL  NOT = 0) OR
                 (LN-OTHBAL NOT = 0)
                 GO TO NEXT-LN-RECORD.

 
           IF ZBAL-ISO = "S"
              IF LN-CURBAL = 0
                 IF LN-INTBAL = 0
                   IF LN-LCBAL = 0
                     IF LN-OTHBAL = 0
                       GO TO NEXT-LN-RECORD. 

           IF LN-PLDATE NOT = 0
              GO TO NEXT-LN-RECORD.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *       PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF IO-FG NOT = 0
                 GO TO NEXT-LN-RECORD.

           PERFORM COMPUTE-RECENCY.
           PERFORM COMPUTE-CONTRACTUAL.

           IF CMISS = "S"
              IF CPOT30 = "M"
                 GO TO NEXT-LN-RECORD
              END-IF
           ELSE
              IF CMISS = "O"
                 IF CPOT30 NOT = "M"
                    GO TO NEXT-LN-RECORD.

           IF RPOT = "S"
              IF RDPOT30 NOT = " "
                  GO TO NEXT-LN-RECORD
              END-IF
           ELSE
              IF RPOT = "O"
                 IF RDPOT30 = " "
                    GO TO NEXT-LN-RECORD.


           IF NOT (BEG-RAGE = 0 AND END-RAGE = 9999)
              IF RECDEL < BEG-RAGE OR RECDEL > END-RAGE
                 GO TO NEXT-LN-RECORD.


           IF ( FIRST-TIME = "Y" )
              MOVE LN-OWNBR     TO HOLD-LN-OWNBR
              MOVE LN-CLASS     TO HOLD-LN-CLASS
              PERFORM SET-BRANCH-HEADINGS
              PERFORM SET-CLASS-HEADINGS
              MOVE "N"          TO FIRST-TIME
              MOVE "Y"          TO RECORDS-FOUND
              MOVE 1            TO LN-FEED
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

           IF LN-OWNBR NOT = HOLD-LN-OWNBR
              MOVE LN-OWNBR TO HOLD-LN-OWNBR
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-BRANCH-HEADINGS
              PERFORM SET-CLASS-HEADINGS.

           IF LN-CLASS NOT = HOLD-LN-CLASS
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS.

           PERFORM CLEAR-MEMO-TABLE.

           PERFORM LOAD-MEMO-TABLE.

           PERFORM PRINT-DETAIL-LINE.

           GO TO NEXT-LN-RECORD.

       END-OF-GROUP.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN3-FILE.

           MOVE "Y" TO FIRST-TIME.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF PRINT-RANGES = "Y"
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      *********************************************************
       PRINT-DETAIL-LINE SECTION.
      *********************************************************

      *ONLY WANT PAID OFF ACCOUNTS WITH FORMS WITHIN THE 
      *FORM DATE RANGE
           IF LN-CURBAL = 0
              IF LOAN-MEMO-FORM(1) = 0 AND
                  LOAN-MEMO-DATE(1) = 0
                GO TO PRINT-DETAIL-LINE-EXIT.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO HOLD-LPS-NUM.
           MOVE H-LPS-NUM        TO D-LOAN-NUM.

           PERFORM GET-BORROWER.
           MOVE NAME-WORKER      TO D-BW-NAME.

           IF RDPOT30 = "Y"
              MOVE "<P>" TO D-RECENCYX
           ELSE
           IF RDPOT30 = "M"
              MOVE "<M>" TO D-RECENCYX
           ELSE
           IF RDPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-RECENCYX
           ELSE
           IF RDPOTCHGOFF = "M"
              MOVE "<PL>" TO D-RECENCYX
           ELSE
              MOVE RECDEL TO D-RECENCY.

           MOVE LN-CURBAL        TO D-CURRENT-BAL.

           PERFORM VARYING SUB FROM 1 BY 1
              UNTIL SUB > LOAN-PRINT-MAX
                 IF LOAN-MEMO-FORM(SUB) NOT = 0
                    MOVE  LOAN-MEMO-FORM(SUB) TO D-MEMO-FORM(SUB)
                    MOVE  "-"                 TO D-MEMO-DASH1(SUB)
                    MOVE  LOAN-MEMO-DATE(SUB) TO D-MEMO-DATE(SUB)
                 END-IF
           END-PERFORM.

           PERFORM WRITE-DETAIL-LINE.

       PRINT-DETAIL-LINE-EXIT.      
           EXIT.

      ***********************************************************
       INITIALIZATION SECTION.
      ***********************************************************
           MOVE COLLET-QUEUE-POS TO Q-POS.
           MOVE COLLET-COPIES-POS TO C-POS.
           MOVE COLLET-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".
 
           MOVE "N" TO RECORDS-FOUND.
           MOVE 0 TO PAGE-NUMBER.

           MOVE AGE-DATE    TO DATE-YYYYMMDD AGEING-DATE NUM-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE    TO H2-AGEDATE.
           PERFORM JUL.
           MOVE JULIAN-DATE TO H2-JULIAN.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME    TO H1-NAME.
           MOVE EXT-CONAME-SYSDATE   TO H1-DATE.
           MOVE BEG-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO H2-DATE1.
           MOVE END-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO H2-DATE2.
           MOVE EXT-CONAME-USER-ID    TO H2-USERID.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP TO ENT-GROUP
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              MOVE "Y" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE.

           COMPUTE HOLD-BEG-DATE = 99999999 - BEG-DATE.
           COMPUTE HOLD-END-DATE = 99999999 - END-DATE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      *    PERFORM OPEN-SP1-FILE.
           PERFORM ARRAYSP-LOAD.
           PERFORM OPEN-LC1-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF ( VDUSTATUS = "1U" )
              GO TO RANGES-YN.

      *-----------------------------------------------------------------
       BEGIN-BRANCH.
           MOVE "ENTER BEGINNING BRANCH"  TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF ( VDUSTATUS = "1<" )
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BEGIN-BRANCH
              ELSE
                 MOVE "S  A000BEG-BRANCH." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-BRANCH.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-BRANCH.

           IF WGR-TYPE = "A"
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENDING-BRANCH.
           MOVE "ENTER ENDING BRANCH"  TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-BRANCH.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDING-BRANCH.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-BRANCH.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BEGIN-BRANCH.

      *----------------------------------------------------------------
       CL-01.
           MOVE "ENTER BEGINNING CLASS, F6-SCAN"  TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO ENDING-BRANCH.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.

           MOVE VDUNUM0 TO BEG-CLASS.

      *----------------------------------------------------------------
       CL-02.
           MOVE "ENTER ENDING CLASS, F6-SCAN"  TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.

           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.

           GO TO BEGIN-DATE.

      *----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0             TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99            TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BEGIN-DATE.
           IF ( EXT-ROUTE-BUF NOT = "00" ) AND ( VDUSTATUS = "1U" )
              GO TO CL-02.

           IF ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO FM-01.

           MOVE "ENTER BEGINNING FORM DATE"  TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNM080BEG-DATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGIN-DATE.

           IF ( VDUNUM0 = ZEROES )
              GO TO BEGIN-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *-----------------------------------------------------------------
       ENDING-DATE.
           IF EXT-ROUTE-BUF NOT = "00" AND VDUSTATUS = "1U"
              GO TO CL-02.

           MOVE "ENTER ENDING FORM DATE"  TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNM080END-DATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U" GO TO BEGIN-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENDING-DATE.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENDING-DATE.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-DATE.

      *----------------------------------------------------------------
       FM-01.
           MOVE "ENTER BEGINNING FORM NO."  TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030FM1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-FM.
           IF VDUSTATUS = "1U" GO TO BEGIN-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO FM-01.

           MOVE VDUNUM0 TO BEG-FORM.

       FM-02.
           MOVE "ENTER ENDING FORM NO."  TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030FM2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.

           MOVE VDUNUM0 TO END-FORM.

           IF BEG-FORM > END-FORM
              PERFORM INVALID-RANGE
              GO TO FM-01.

           GO TO CMISSYN.

      *----------------------------------------------------------------
       ALL-FM.
           MOVE "SNNN030FM1." TO VDU.
           MOVE 0             TO BEG-FORM VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN030FM2." TO VDU.
           MOVE 999           TO END-FORM VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       CMISSYN.
           MOVE "ENTER MISSED PAY FLAG."  TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010CMISS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FM-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CMISSYN.
           IF VDUSTATUS NOT = "00" GO TO CMISSYN.

           MOVE VDUBUF TO CMISS.

           IF CMISS NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO CMISSYN.

      *----------------------------------------------------------------
       RPOTYN.
           MOVE "ENTER RECENCY POTENTIAL FLAG."  TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010RPOT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CMISSYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RPOTYN.
           IF VDUSTATUS NOT = "00" GO TO RPOTYN.

           MOVE VDUBUF TO RPOT.

           IF RPOT NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO RPOTYN.
      *----------------------------------------------------------------
       AGEDT.
           IF ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO RAGE01.
           MOVE "ENTER AGEING DATE "  TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060AGEDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RPOTYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO AGEDT.
           IF VDUSTATUS NOT = "00" GO TO AGEDT.

           MOVE VDU-DATE-YYYYMMDD TO AGE-DATE.

           IF AGE-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000AGEDATE." TO VDU
              MOVE DATE-YYYYMMDD TO AGE-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

       RAGE01.
           MOVE "ENTER BEGINNING RECENCY AGEING"  TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040RAGE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALRAGE.
           IF VDUSTATUS = "1U" GO TO AGEDT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RAGE01.

           MOVE VDUNUM0 TO BEG-RAGE.

      *----------------------------------------------------------------
       RAGE02.
           MOVE "ENTER ENDING RECENCY AGEING"  TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040RAGE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RAGE01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RAGE02.

           MOVE VDUNUM0 TO END-RAGE.

           IF BEG-RAGE > END-RAGE
              PERFORM INVALID-RANGE
              GO TO RAGE01.

           IF NOT (BEG-RAGE < 30)
               MOVE "SNNA010RPOT." TO VDU
               MOVE " " TO VDUBUF
               PERFORM SCREENIO
               GO TO ZEROBAL.  

           GO TO ZEROBAL.    

      *----------------------------------------------------------------
       ALRAGE.
           MOVE "SNNN040RAGE1." TO VDU.
           MOVE 0               TO BEG-RAGE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040RAGE2." TO VDU.
           MOVE 9999            TO END-RAGE VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
           
       ZEROBAL.
           MOVE "ZERO BALANCE -  (I)NCLUDE, (S)KIP, (O)NLY" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.
      
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RAGE02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ZEROBAL.   

           MOVE VDUBUF TO ZBAL-ISO.

           IF ZBAL-ISO NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO ZEROBAL.

      *----------------------------------------------------------------
       RANGES-YN.
           MOVE "PRINT RANGES - (Y)ES, (N)NO" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010RANGES." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ZEROBAL.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RANGES-YN.

           MOVE VDUBUF TO PRINT-RANGES.

           IF NOT (PRINT-RANGES = "Y" OR "N")
              GO TO RANGES-YN.

      *----------------------------------------------------------------
       EXIT-PARM.
           MOVE SPACES  TO LEGEND.
           PERFORM SEND-LEGEND.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE AGE-DATE.


      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BEG-BRANCH." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH." TO VDU
              MOVE BEG-BRANCH    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH." TO VDU
              MOVE END-BRANCH    TO VDUNUM0
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N020CL1." TO VDU.
           MOVE BEG-CLASS     TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N020CL2." TO VDU.
           MOVE END-CLASS     TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N030FM1." TO VDU.
           MOVE BEG-FORM      TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N030FM2." TO VDU.
           MOVE END-FORM      TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010CMISS." TO VDU.
           MOVE CMISS           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010RPOT." TO VDU.
           MOVE RPOT            TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040RAGE1." TO VDU.
           MOVE BEG-RAGE        TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040RAGE2." TO VDU.
           MOVE END-RAGE        TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL-ISO TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010RANGES." TO VDU.
           MOVE PRINT-RANGES TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK GREATER THAN EXT-CONAME-RPT-LINES )
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       SET-CLASS-HEADINGS SECTION.
      ******************************************************************
           MOVE HOLD-LN-CLASS TO H4-CLASS.

           IF HOLD-LN-CLASS NOT = LC-CODE
              MOVE HOLD-LN-CLASS TO LC-CODE
      *       PERFORM READ-LC1-FILE
      *       IF IO-FG NOT = 0
              IF LC-DESC = SPACES
                 MOVE "INVALID" TO LC-DESC.

           MOVE LC-DESC TO H4-CLDESC.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H1-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT   TO H1-TIME.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD4 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD5 TO PR-REC.
           MOVE 2     TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ALL "-" TO PR-REC.
           MOVE 1       TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 8 TO LN-COUNT.
           MOVE 1 TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       LOAD-MEMO-TABLE SECTION.
      ******************************************************************

           MOVE 0 TO SUB.

      *    MOVE LM-PATH-OWNBR TO LM-BRNO
      *                          QLM1-WBEG-BRNO
      *                          QLM1-WEND-BRNO.
      *    MOVE LN-ACCTNO     TO LM-ACCTNO
      *                          QLM1-WBEG-ACCTNO
      *                          QLM1-WEND-ACCTNO.
      *    MOVE HOLD-END-DATE TO LM-DATE
      *                          QLM1-WBEG-DATE-REV.
      *    MOVE 0             TO LM-SEQ 
      *                          QLM1-WBEG-SEQ.
      *    MOVE ALL "9"       TO QLM1-WEND-DATE-REV
      *                          QLM1-WEND-SEQ.
      *    MOVE HOLD-BEG-DATE TO QLM1-WEND-DATE-REV.
      *    MOVE ALL "9"       TO QLM1-WEND-SEQ.
      *
      *    PERFORM START-LM1-FILE.

           MOVE LN-OWNBR      TO WS-LN-OWNBR.
           MOVE LN-CLASS      TO WS-LN-CLASS.
           MOVE LN-ACCTNO     TO WS-LN-ACCTNO.

           PERFORM UNTIL WS-LM-KEY NOT < WS-LN-KEY
              PERFORM READ-LM1-FILE-NEXT *> EMBEDDED - LMFILE
           END-PERFORM.

       NEXT-LM-RECORD.

           IF WS-LM-KEY > WS-LN-KEY
              GO TO EXIT-LOAD-MEMO-TABLE.

      *    IF ( IO-FG = 9 ) OR (LM-BRNO NOT   = LN-PATH-OWNBR) OR
      *                        (LM-ACCTNO NOT = LN-ACCTNO)
           IF ( IO-FG = 9 ) OR (LM-BRNO NOT   = LN-PATH-OWNBR) 
              GO TO EXIT-LOAD-MEMO-TABLE.

           IF WS-LM-KEY    < WS-LN-KEY 
              PERFORM READ-LM1-FILE-NEXT *> EMBEDDED - LMFILE
              GO TO NEXT-LM-RECORD
           ELSE   
              IF WS-LN-KEY = WS-LM-KEY
                 PERFORM CHECK-LM-RECORD
              ELSE 
                 GO TO EXIT-LOAD-MEMO-TABLE.

           IF ( WS-INVALID-LM-FG = "Y" )
              GO TO EXIT-LOAD-MEMO-TABLE.

           PERFORM READ-LM1-FILE-NEXT. *> EMBEDDED - LMFILE

           GO TO NEXT-LM-RECORD.

       EXIT-LOAD-MEMO-TABLE.
           EXIT.

      ******************************************************************
       CHECK-LM-RECORD SECTION.
      ******************************************************************
           MOVE "N" TO WS-INVALID-LM-FG.

           IF LM-DATE > HOLD-BEG-DATE
              MOVE "Y" TO WS-INVALID-LM-FG
              GO TO EXIT-CHECK-LM.

      *=================================================================
      *    NEED TO TEST LM-MEMO1 FOR A FORM NUMBER (FMNO).
      *    THE FMNO IN THE LM-MEMO1 MAYBE 3 (A15) OR 4 BYTES (A30).
      *    FIND WHICH FMNO TO USE AND LOAD IT INTO TEST-LM-MEMO.
      *=================================================================
           MOVE LM-MEMO1                   TO TEST-LM-MEMO1.

           IF ( TEST-LM-FORMX-A15 NOT = "FORM # " )
              MOVE "S"                     TO WS-INVALID-LM-FG
              GO TO EXIT-CHECK-LM.

           IF ( TEST-LM-EXTRA-A15 NOT NUMERIC )
              MOVE TEST-LM-FMNO-A15        TO TEST-LM-FMNO
           ELSE
              MOVE TEST-LM-FMNO-A30        TO TEST-LM-FMNO.

           IF TEST-LM-FMNO < BEG-FORM OR TEST-LM-FMNO > END-FORM
              MOVE "S" TO WS-INVALID-LM-FG
              GO TO EXIT-CHECK-LM.

           ADD 1 TO SUB.
           IF SUB > LOAN-MEMO-MAX
              MOVE "Y" TO WS-INVALID-LM-FG
              GO TO EXIT-CHECK-LM.

           SUBTRACT LM-DATE FROM 99999999 GIVING DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE TEST-LM-FMNO TO LOAN-MEMO-FORM(SUB).
           MOVE DATE-MMDDYY TO LOAN-MEMO-DATE(SUB).

       EXIT-CHECK-LM.
           EXIT.

      **********************************************************
       CLEAR-MEMO-TABLE SECTION.
      **********************************************************
           PERFORM VARYING SUB FROM 1 BY 1
              UNTIL SUB > LOAN-MEMO-MAX
                 MOVE 0      TO LOAN-MEMO-FORM(SUB)
                 MOVE 0      TO LOAN-MEMO-DATE(SUB)
           END-PERFORM.

      ****************************************************
       CLASS-SCAN SECTION.
      ****************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SNNN020CL1." TO VDU
               MOVE LCSCAN-KEY TO BEG-CLASS VDUNUM0
           ELSE
              MOVE "SNNN020CL2." TO VDU.
               MOVE LCSCAN-KEY TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE 0 TO IO-FG.
       EXIT-CLASS-SCAN.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************

           MOVE EXT-CONAME-RPT-LINES           TO LN-COUNT.
           MOVE 4                      TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG BRANCH"    TO RANGE-LINE.
           MOVE BEG-BRANCH      TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END BRANCH"    TO RANGE-LINE.
           MOVE END-BRANCH      TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG DATE"      TO RANGE-LINE.
           MOVE H2-DATE1        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END DATE"      TO RANGE-LINE.
           MOVE H2-DATE2        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG FORM"      TO RANGE-LINE.
           MOVE BEG-FORM        TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END FORM"      TO RANGE-LINE.
           MOVE END-FORM        TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MISS PAYS          :" TO RANGE-LINE.
           MOVE CMISS                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "POTENTIALS          :" TO RANGE-LINE.
           MOVE RPOT                    TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "AGED AS OF          :" TO RANGE-LINE.
           MOVE AGE-DATE                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "RECENCY AGE        :" TO RANGE-LINE.
           MOVE BEG-RAGE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-RAGE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ******************************************************************
      *    C O P Y - L I B R A R Y - S E C T I O N S
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/ARRAYSP.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO COLLET-CONAME.
           MOVE EXT-CONAME-SYSDATE TO COLLET-SYSDATE.
           DISPLAY COLLET-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE COLLET-FORM-FIELDS TO WR-BUF.
           MOVE COLLET-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE COLLET-HELP--FILE TO HELP-LINK-FILE.
           MOVE COLLET-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/COLLET_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
      *-----------------------------------------------------------------
       SET-BRANCH-HEADINGS.
           MOVE BR-NO     TO H3-BRNO.
           MOVE BR-NAME   TO H3-BRDESC.

       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       GET-BORROWER.
           MOVE LN-SSNO(1) TO BW-SSNO.
      *    PERFORM READ-BW1-FILE.
      *    IF IO-FG = 9
           IF BW-OWNBR = 0
              MOVE SPACES TO BW-FNAME
              MOVE "** MISSING **" TO BW-LNAME.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
                   BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***************************************
       IO-ROUTINES SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
   
      * COPY "LIBLP/LPLN3RN.CPY".
      * COPYMEMBER: LIBLP/LPLN3RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LN3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *-----------------------------------------------------------------
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

           IF LN3-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO LN3-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE. *> EMBEDDED - LN,BW,CD
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           IF ( QLN3-WEND-ACCTNO = ZEROES    ) OR
              ( QLN3-WEND-ACCTNO NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN3-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN3-WEND-OWNBR
              MOVE LN-CLASS             TO QLN3-WBEG-CLASS
              MOVE ALL "9"              TO QLN3-WEND-CLASS
              MOVE LN-ACCTNO            TO QLN3-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN3-WEND-ACCTNO.

           MOVE ZBAL-ISO                TO QWS-ZBAL-ISO.


      *  STORED PROCEDURE:
      *
      *  SELECT LNFILE.LN_OWNBR,
      *         LNFILE.LN_ACCTNO,
      *         LNFILE.LN_CLASS,
      *         LNFILE.LN_SUBCLASS,
      *         LNFILE.LN_LAWCODE,
      *         LNFILE.LN_SSNO1,
      *         LNFILE.LN_MAKERCD1,
      *         LNFILE.LN_SSNO2,
      *         LNFILE.LN_MAKERCD2, 
      *         LNFILE.LN_SSNO3,
      *         LNFILE.LN_MAKERCD3, 
      *         LNFILE.LN_SSNO4,
      *         LNFILE.LN_MAKERCD4, 
      *         LNFILE.LN_ORGST, 
      *         LNFILE.LN_SPRCLASS,
      *         LNFILE.LN_POCD, 
      *         LNFILE.LN_PLCD, 
      *         LNFILE.LN_SERVACCT, 
      *         LNFILE.LN_PAY_SCHLD_FG, 
      *         LNFILE.LN_STATFLAGS, 
      *         CAST(LNFILE.LN_LOANDATE       AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_1STPYDATE      AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_PLDATE         AS VARCHAR(10)), 
      *         LNFILE.LN_ORGTERM,
      *         LNFILE.LN_1STPYAMT,
      *         LNFILE.LN_NOREGPY,
      *         LNFILE.LN_REGPYAMT,
      *         LNFILE.LN_LASTPYAMT,
      *         LNFILE.LN_PY_YYYYMM_RECDEL,
      *         LNFILE.LN_MTD_PAYS_RECDEL,
      *         LNFILE.LN_CURBAL,
      *         LNFILE.LN_INTBAL,
      *         LNFILE.LN_LCBAL,
      *         LNFILE.LN_OTHBAL,
      *         LNFILE.LN_ANTICERN_1,
      *         LNFILE.LN_ANTICERN_2,
      *         LNFILE.LN_ANTICERN_3,
      *         LNFILE.LN_ANTICERN_4,
      *         LNFILE.LN_ANTICERN_5,
      *         LNFILE.LN_ANTICERN_6,
      *         LNFILE.LN_ANTICERN_7,
      *         LNFILE.LN_ANTICERN_8,
      *         LNFILE.LN_ANTICERN_9,
      *         LNFILE.LN_ANTICERN_10,
      *         LNFILE.LN_ANTICADJ_1,
      *         LNFILE.LN_ANTICADJ_2,
      *         LNFILE.LN_ANTICADJ_3,
      *         LNFILE.LN_ANTICADJ_4,
      *         LNFILE.LN_ANTICADJ_5,
      *         LNFILE.LN_ANTICADJ_6,
      *         LNFILE.LN_ANTICADJ_7,
      *         LNFILE.LN_ANTICADJ_8,
      *         LNFILE.LN_ANTICADJ_9,
      *         LNFILE.LN_ANTICADJ_10,
      *         LNFILE.LN_UFERN_1,
      *         LNFILE.LN_UFERN_2,
      *         LNFILE.LN_UFERN_3,
      *         LNFILE.LN_UFERN_4,
      *         LNFILE.LN_UFERN_5,
      *         LNFILE.LN_UFERN_6,
      *         LNFILE.LN_UFERN_7,
      *         LNFILE.LN_UFERN_8,
      *         LNFILE.LN_UFERN_9,
      *         LNFILE.LN_UFERN_10,
      *         LNFILE.LN_ACCERN_1,
      *         LNFILE.LN_ACCERN_2,
      *         LNFILE.LN_ACCERN_3,
      *         LNFILE.LN_ACCERN_4,
      *         LNFILE.LN_ACCERN_5,
      *         LNFILE.LN_ACCERN_6,
      *         LNFILE.LN_ACCERN_7,
      *         LNFILE.LN_ACCERN_8,
      *         LNFILE.LN_ACCERN_9,
      *         LNFILE.LN_ACCERN_10,
      *         LNFILE.LN_TOTPAYMNTD,
      *         LNFILE.LN_TOTNODEF,
      *         LNFILE.LN_TOTNOALDEL,
      *         CAST(LNFILE.LN_DATE_PAID_LAST AS VARCHAR(10)), 
      *         LNFILE.LN_SUM_ORGTERM,
      *         LNFILE.LN_UNITPER_CD,
      *         LNFILE.LN_UNITPER_FREQ,
      *         ISNULL(BWFILE.BW_OWNBR, 0),
      *         ISNULL(BWFILE.BW_SSNO, 0),
      *         ISNULL(BWFILE.BW_LNAME,' '),
      *         ISNULL(BWFILE.BW_FNAME,' '),
      *         ISNULL(BWFILE.BW_SOLICIT,' '),
      *         ISNULL(LCFILE.LC_CODE, 0),
      *         ISNULL(LCFILE.LC_DESC,' ') 
      *
      *    FROM DBO.LNFILE  LNFILE
      *
      *    LEFT OUTER JOIN 
      *         DBO.BWFILE  BWFILE
      *      ON (  LNFILE.LN_OWNBR          = BWFILE.BW_OWNBR	
      *     AND    LNFILE.LN_SSNO1          = BWFILE.BW_SSNO )
      *
      *    LEFT OUTER JOIN 
      *         DBO.LCFILE  LCFILE 
      *      ON (  LNFILE.LN_CLASS         = LCFILE.LC_CODE  )
      *
      *   WHERE (  LNFILE.LN_OWNBR         = @QLN3_WBEG_OWNBR
      *     AND    LNFILE.LN_CLASS   BETWEEN @QLN3_WBEG_CLASS
      *                                  AND @QLN3_WEND_CLASS  )
      *
      *     AND (  LNFILE.LN_POCD     NOT IN ('**', '*I', '*R', '*U'))
      *
      *     AND (  LNFILE.LN_PLDATE        = '1900-01-01'  )         
      *
      *     AND  (( @QWS_ZBAL_ISO          = 'I' )
      *      OR  (  @QWS_ZBAL_ISO          = 'O' 
      *     AND     LNFILE.LN_CURBAL       = 0
      *     AND     LNFILE.LN_INTBAL       = 0
      *     AND     LNFILE.LN_LCBAL        = 0 
      *     AND     LNFILE.LN_OTHBAL       = 0 )
      *      OR  (  @QWS_ZBAL_ISO          = 'S' 
      *      OR     LNFILE.LN_CURBAL      != 0
      *      OR     LNFILE.LN_INTBAL      != 0
      *      OR     LNFILE.LN_LCBAL       != 0 
      *      OR     LNFILE.LN_OTHBAL      != 0 )) 
      *
      *   ORDER BY LNFILE.LN_OWNBR,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_ACCTNO; 


           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = EXEC SSP_COLLET_LNFILE_SELECT 
              ( :QLN3-WBEG-OWNBR, 
                :QLN3-WBEG-CLASS, 
                :QLN3-WEND-CLASS,
                :QWS-ZBAL-ISO )
           END-EXEC.
 
           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN3-WBEG-OWNBR
                          QLN3-WEND-OWNBR
                          QLN3-WBEG-CLASS
                          QLN3-WEND-CLASS
                          QLN3-WBEG-ACCTNO
                          QLN3-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT. *> EMBEDDED - LN,BW,CD
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.
           INITIALIZE QBW-REC.
           INITIALIZE QLC-REC.
    
           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

              EXEC SQL 
                   FETCH NEXT LN3_CURSOR INTO
                     :QLN-OWNBR, 
                     :QLN-ACCTNO,
                     :QLN-CLASS,
                     :QLN-SUBCLASS,
                     :QLN-LAWCODE,
                     :QLN-SSNO1,
                     :QLN-MAKERCD1,
                     :QLN-SSNO2,
                     :QLN-MAKERCD2, 
                     :QLN-SSNO3,
                     :QLN-MAKERCD3, 
                     :QLN-SSNO4,
                     :QLN-MAKERCD4, 
                     :QLN-ORGST, 
                     :QLN-SPRCLASS,
                     :QLN-POCD, 
                     :QLN-PLCD, 
                     :QLN-SERVACCT, 
                     :QLN-PAY-SCHLD-FG, 
                     :QLN-STATFLAGS, 
                     :QLN-LOANDATE,  
                     :QLN-ORIG-1STPYDATE,  
                     :QLN-1STPYDATE, 
                     :QLN-PLDATE, 
                     :QLN-ORGTERM,
                     :QLN-1STPYAMT,
                     :QLN-NOREGPY,
                     :QLN-REGPYAMT,
                     :QLN-LASTPYAMT,
                     :QLN-PY-YYYYMM-RECDEL,
                     :QLN-MTD-PAYS-RECDEL,
                     :QLN-CURBAL,
                     :QLN-INTBAL,
                     :QLN-LCBAL,
                     :QLN-OTHBAL,
                     :QLN-ANTICERN-1,
                     :QLN-ANTICERN-2,
                     :QLN-ANTICERN-3,
                     :QLN-ANTICERN-4,
                     :QLN-ANTICERN-5,
                     :QLN-ANTICERN-6,
                     :QLN-ANTICERN-7,
                     :QLN-ANTICERN-8,
                     :QLN-ANTICERN-9,
                     :QLN-ANTICERN-10,
                     :QLN-ANTICADJ-1,
                     :QLN-ANTICADJ-2,
                     :QLN-ANTICADJ-3,
                     :QLN-ANTICADJ-4,
                     :QLN-ANTICADJ-5,
                     :QLN-ANTICADJ-6,
                     :QLN-ANTICADJ-7,
                     :QLN-ANTICADJ-8,
                     :QLN-ANTICADJ-9,
                     :QLN-ANTICADJ-10,
                     :QLN-UFERN-1,
                     :QLN-UFERN-2,
                     :QLN-UFERN-3,
                     :QLN-UFERN-4,
                     :QLN-UFERN-5,
                     :QLN-UFERN-6,
                     :QLN-UFERN-7,
                     :QLN-UFERN-8,
                     :QLN-UFERN-9,
                     :QLN-UFERN-10,
                     :QLN-ACCERN-1,
                     :QLN-ACCERN-2,
                     :QLN-ACCERN-3,
                     :QLN-ACCERN-4,
                     :QLN-ACCERN-5,
                     :QLN-ACCERN-6,
                     :QLN-ACCERN-7,
                     :QLN-ACCERN-8,
                     :QLN-ACCERN-9,
                     :QLN-ACCERN-10,
                     :QLN-TOTPAYMNTD,
                     :QLN-TOTNODEF,
                     :QLN-TOTNOALDEL,
                     :QLN-DATE-PAID-LAST, 
                     :QLN-SUM-ORGTERM,
                     :QLN-UNITPER-CD,
                     :QLN-UNITPER-FREQ,
                     :QBW-OWNBR, 
                     :QBW-SSNO,  
                     :QBW-LNAME, 
                     :QBW-FNAME, 
                     :QBW-SOLICIT, 
                     :QLC-CODE,  
                     :QLC-DESC 
              END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
               GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
               PERFORM GET-LN-FIELDS
               PERFORM GET-BW-FIELDS
               PERFORM GET-LC-FIELDS.
 
          EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.
  
      *-----------------------------------------------------------------
       CLOSE-LN3-CONNECTION.

           IF LN3-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LN3_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LN3-CONN-OPEN-SW.


      * COPY "LIBLP/LPLM1RN.CPY".
      * COPYMEMBER: LIBLP/LPLM1RN
       LOAD-LM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LM-ALL-BASE
              MOVE FILPATH-MACHINE         TO LM-ALL-MACHINE
              MOVE FILPATH-DATA            TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH.

      *-----------------------------------------------------------------
       OPEN-LM1-FILE.
           PERFORM LOAD-LM1-FILE.
           PERFORM OPEN-IT.
           MOVE LM-PATH-SQL TO E-FILE.

           IF LM1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO LM1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LM1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LM1-FILE. *> SSP - LM,LN
           PERFORM START-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.

           IF ( LM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LM1-FILE.

           IF ( QLM1-WEND-BRNO = ZEROES    ) OR
              ( QLM1-WEND-BRNO NOT NUMERIC )
              MOVE LM-PATH-OWNBR        TO QLM1-WBEG-BRNO
              MOVE LM-PATH-OWNBR        TO QLM1-WEND-BRNO
              MOVE LM-ACCTNO            TO QLM1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLM1-WEND-ACCTNO
              MOVE LM-DATE-REVERSED     TO QLM1-WBEG-DATE-REV
              MOVE ALL "9"              TO QLM1-WEND-DATE-REV
              MOVE LM-SEQ               TO QLM1-WBEG-SEQ
              MOVE ALL "9"              TO QLM1-WEND-SEQ.

      
      *  STORED PROCEDURE:
      *
      *   SELECT LMFILE.LM_BRNO,
      *          LNFILE.LN_CLASS,
      *          LMFILE.LM_ACCTNO,
      *          LMFILE.LM_DATE_REVERSED,
      *          LMFILE.LM_SEQ,
      *          LMFILE.LM_MEMO
      *
      *    FROM DBO.LMFILE  LMFILE
      *
      *   INNER JOIN
      *         DBO.LNFILE  LNFILE
      *      ON (( LNFILE.LN_OWNBR              = LMFILE.LM_BRNO 
      *     AND    LNFILE.LN_ACCTNO             = LMFILE.LM_ACCTNO )
      *     AND (  LNFILE.LN_POCD          NOT IN ('**', '*I', '*R', '*U'))
      *     AND (  LNFILE.LN_PLDATE             = '1900-01-01'  )
      *     AND (( @QWS_ZBAL_ISO                = 'I' )
      *      OR (  @QWS_ZBAL_ISO                = 'O' 
      *     AND    LNFILE.LN_CURBAL             = 0
      *     AND    LNFILE.LN_INTBAL             = 0
      *     AND    LNFILE.LN_LCBAL              = 0 
      *     AND    LNFILE.LN_OTHBAL             = 0 )
      *      OR (  @QWS_ZBAL_ISO                = 'S' 
      *      OR    LNFILE.LN_CURBAL            != 0
      *      OR    LNFILE.LN_INTBAL            != 0
      *      OR    LNFILE.LN_LCBAL             != 0 
      *      OR    LNFILE.LN_OTHBAL            != 0 )))
      *
      *   WHERE (  LMFILE.LM_BRNO                = @QLM1_WBEG_BRNO
      *     AND    LMFILE.LM_DATE_REVERSED BETWEEN @QLM1_WBEG_DATE_REV
      *                                        AND @QLM1_WEND_DATE_REV )
      *   ORDER BY LMFILE.LM_BRNO,
      *            LNFILE.LN_CLASS,
      *            LMFILE.LM_ACCTNO,
      *            LMFILE.LM_DATE_REVERSED 


           EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LM1_CURSOR CURSOR FOR
                :LM1-RETURN-CODE = EXEC SSP_COLLET_LMFILE_SELECT 
              ( :QLM1-WBEG-BRNO,      
                :QLM1-WBEG-DATE-REV,  
                :QLM1-WEND-DATE-REV, 
                :QWS-ZBAL-ISO   )      
           END-EXEC.


           EXEC SQL
               OPEN LM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLM1-WBEG-BRNO 
                          QLM1-WEND-BRNO
                          QLM1-WBEG-ACCTNO
                          QLM1-WEND-ACCTNO
                          QLM1-WBEG-DATE-REV
                          QLM1-WEND-DATE-REV
                          QLM1-WBEG-SEQ
                          QLM1-WEND-SEQ.

           MOVE STAT---GOOD TO LM1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LM1-FILE-NEXT. *> EMBEDDED - LM,LN
           PERFORM READ-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           INITIALIZE QLM-REC.

           EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC.

           IF ( LM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LM1_CURSOR 
                    INTO 
                     :QLM-BRNO,
                     :QWS-LM-CLASS,
                     :QLM-ACCTNO,
                     :QLM-DATE-REVERSED,
                     :QLM-SEQ,
                     :QLM-MEMO
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LM-FIELDS
              MOVE LM-BRNO      TO WS-LM-BRNO
              MOVE QWS-LM-CLASS TO WS-LM-CLASS
              MOVE LM-ACCTNO    TO WS-LM-ACCTNO
           ELSE
              MOVE ALL "9"      TO WS-LM-KEY.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LM1-FILE.

           PERFORM CLOSE-IT.
           IF ( LM1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LM1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LM1-CONNECTION.

           IF LM1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LM1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LM1-CONN-OPEN-SW.

       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
             MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND )
              IF ( EXT-ROUTE-BUF = "00" OR "70" )
                 MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
                 PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.
           IF ( SV-BEG-BR NOT = 9999 )
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH
              MOVE SV-GROUP-FLAG TO GROUP-FLAG
              MOVE SV-ENT-GROUP TO ENT-GROUP.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY COLLET-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
