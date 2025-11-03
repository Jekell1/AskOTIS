       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHLOCK.
       DATE-WRITTEN.  02/01/99.
      *******************************************************
      *                 (LP)
      *  DESCRIPTION:   ACCOUNT FROZEN STATUS
      *
      *                 THIS PROGRAM ALLOWS THE ENTRY OF A
      *                 NEW FROZEN STATUS
      *  REV:
      *  GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  BAH 051027 ADDED LN-FROZEN-REASON, REGACC PR# 114
      *   CS 130725 A30 CONVERSION STARTED
      *  BAH 160126 REMOVED SPPDTH AND LTFILE
      *
      * KEC 2016.0324 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-CD-??-KEY (NEW).
      * CS 160808 CHANGED H-SP-SPRCLASS FROM 9(2) TO 9(3), H-SP1-KEY
      * BAH 161101 MADE CH-FILE INDEXED, NEW LAYOUT
      * BAH 170517 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181002 GLOBAL BWFILE
      * BAH 181204 GLOBAL LNFILE
      * BAH 190122 SPLIT CDFILE
      * BAH 190307 REMOVED LN-FROZEN-REASON1, USE A WORKING STORAGE FIELD
      *            INSTEAD. THAT FIELD REPLACED A COMP-3 OLD FIELD AND
      *            NEEDS TESTED FOR A X'00'
      *  BAH 190513 NEW CHFILE
      *  BLM 211004 FIX CAST ERROR IF ALPHA ENTERED IN SSNO SCAN
      *  BAH 20230713 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      *******************************************************
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
           "@(#)LP/CHLOCK.CBL S35 02/20/24-15:29:34 >".
      ***********************************
      *    PROGRAM WORKERS
      ***********************************
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

       01  ONE               PIC S9.
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  NEW-FLAG          PIC 9       VALUE 1.
       01  IO-TYPE           PIC X(3).
       01  ELE               PIC 9(2).
       01  SUB               PIC 9(6)    COMP-3 VALUE 0.
       01  ORIGINAL-LN-REC   PIC X(LN-SIZE).
       01  CHANGED-LN-REC    PIC X(LN-SIZE).
       01  DIS-BRANCH.
           03  FILLER         PIC X(26)  VALUE
           "ACCOUNT OWNED BY BRANCH # ".
           03  DIS-BR         PIC ZZZ9.
       01  FIRST-SCAN        PIC 9       VALUE 0.
       01  DEFAULT1-KEY       PIC X.
       01  SCAN-TYPES.
           03  FILLER        PIC X(49) VALUE
           "1-SSNO, 2-NAME, 3-SPS SSNO, 4-SPS NAME, F1-CANCEL".
       01  SCAN-MESSAGES.
           03  TYPE-1        PIC X(55) VALUE
           "SSNO, F1-CANCEL, ^, F5-SCAN LOAN, F6-SCAN".
           03  TYPE-2        PIC X(55) VALUE
           "NAME (LAST,FIRST), F1-CANCEL, ^, F5-SCAN LOAN, F6-SCAN".
           03  TYPE-3        PIC X(55) VALUE
           "SPOUSE SSNO, F1-CANCEL, ^, F5-SCAN LOAN, F6-SCAN".
           03  TYPE-4        PIC X(55) VALUE
           "SPOUSE NAME, F1-CANCEL, ^, F5-SCAN LOAN, F6-SCAN".
       01  SCAN-FLAG         PIC 9     VALUE 0.
       01  SCAN              PIC X(2)  VALUE SPACES.
           88  SCAN-ON       VALUE "ON".
       01  NAME-WORKER       PIC X(40) VALUE SPACES.
       01  NAME              PIC X(16) VALUE SPACES.
       01  HOLD-CH-SEQ       PIC 9(3).
       01  HOLD-FROZEN       PIC X.
       01  TYP               PIC 9     VALUE 0.
       01  BORROWER-SCAN     PIC X     VALUE SPACES.
       01  IX1               PIC 99    COMP-3.
       01  IX2               PIC 99    COMP-3.
       01  TEST-FROZEN-REASON.
           03  TEST-FROZEN-REASON1 PIC X.
           03  FILLER              PIC X.

      **********************************
      *    ELEMENT SPECIFICATIONS
      **********************************
       78  MAX                        VALUE 1.
       01  SPEC-TABLE.
      *01. NEW ACCOUNT FROZEN STATUS
           03  FILLER        PIC X(8) VALUE "E  A060 ".
       01  SPEC-TAB          REDEFINES SPEC-TABLE.
           03  SPEC-REC      OCCURS MAX TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

       01  NUM-BW            PIC 9(9).
       01  NUM-BW-WK         REDEFINES NUM-BW.
           03  NUM-BW1       PIC 999.
           03  NUM-BW2       PIC 99.
           03  NUM-BW3       PIC 9999.
       01  ALP-BW.
           03  ALP-BW1       PIC 999.
           03  ALP-HYPHEN1   PIC X.
           03  ALP-BW2       PIC 99.
           03  ALP-HYPHEN2   PIC X.
           03  ALP-BW3       PIC 9999.
       01  BW-ALP-TABLE.
           03  BW-ALP-TBL    PIC X     OCCURS 70.

       01  PROFILE-EDIT      PIC 9(12).
       01  PROFILE-ED REDEFINES PROFILE-EDIT
                             PIC X(12).

       01  DELINQ-VALUE      PIC X(12) VALUE SPACES.

       01  TEST-SSNO       PIC 9(9).
       01  SCREEN-LIST.
           03  FILLER            PIC X(40) VALUE
           "010 MESSAGE SOC A1 A2 A3 A4 SP0 SP1 SP2 ".
           03  FILLER            PIC X(44) VALUE
           "LNNO LNSEQ MAKER ".
           03  FILLER            PIC X(30) VALUE
           "PMPT1 PMPT2 PMPT3 PMPT4 PMPT5 ".
           03  FILLER            PIC X(28) VALUE
           "LN1 GL8 F21 GD8 F22 GA8 F23 ".
           03  FILLER            PIC X(28) VALUE
           "GL9 F31 GD9 F32 GA9 F33 SEL.".
       01  D-MESSAGE         PIC X(30).
      *01  MESSAGE-LIST      PIC X(8)  VALUE "MESSAGE.".
      *01  MESSAGE-ATTR      PIC XX.
       01  MESSAGE-LIST.
           03  MESS-LIST     PIC X(7) VALUE "MESSAGE".
           03  MESS-END   PIC X(4) VALUE "    ".


      *-----------------------------------------------------------------
      *BORROWER INFORMATION
      *-----------------------------------------------------------------
       01  DISPLAY-BORR-BUF  VALUE " ".
           03  D-SOC         PIC X(11).
           03  D-NAME        PIC X(34) OCCURS 4.
           03  D-SP0         PIC X(7).
           03  D-SP1         PIC X(11).
           03  D-SP2         PIC X(25).
       01  DISPLAY-BORR-LIST.
           03  FILLER        PIC X(28) VALUE
           "SOC A1 A2 A3 A4 SP0 SP1 SP2.".

       01  DISPLAY-LOAN-BUF     VALUE " ".
           03  D-LOAN-NUM       PIC X(16).
           03  D-MAKER          PIC X(8).
           03  D-FROZEN         PIC X.
           03  D-FROZEN-REASON  PIC XX.
           03  D-REASON-DESC    PIC X(15).

       01  DISPLAY-LOAN-LIST.
           03  FILLER        PIC X(63) VALUE
           "LNNO LNSEQ MAKER OLOCK OREASON DESC.".

       01  LOAN-INFO-LIST.
           03  FILLER          PIC X(30)    VALUE
           "PMPT1 PMPT2 PMPT3 PMPT4 PMPT5 ".
           03  FILLER          PIC X(28) VALUE
           "LN1 GL8 F21 GD8 F22 GA8 F23 ".
           03  FILLER          PIC X(24) VALUE
           "GL9 F31 GD9 F32 GA9 F33.".

       01  LOAN-INFO-BUF.
           03  FILLER          PIC X(40) VALUE
           "ACCOUNT #         DATE  LOAN AMT TRM  DE".
           03  FILLER          PIC X(16) VALUE
           "ALER COL PL/JD  ".
           03  FILLER          PIC X(11) VALUE "CONTRACTUAL".
           03  FILLER          PIC X(13) VALUE
           "  BAL/PDOFF M".
           03  LOAN-INFO-TABLE.
               05  LOAN-INFO-TAB OCCURS 3.
                   07  LI-LOAN-NUM PIC X(16).
                   07  FILLER      PIC X.
                   07  LI-LNDATE   PIC 9(6).
                   07  LI-LOAN-AMT PIC ZZZZZZ.ZZ.
                   07  FILLER      PIC X.
                   07  LI-TRM      PIC ZZZ.
                   07  FILLER      PIC X.
                   07  LI-DEALER   PIC ZZZZ.ZZ.
                   07  FILLER      PIC X.
                   07  LI-COLL     PIC XX.
                   07  FILLER      PIC X.
                   07  LI-PLFG     PIC X.
                   07  LI-JDPLDATE   PIC 9(6)  BLANK ZERO.
                   07  FILLER      PIC X.
                   07  LI-CONTRACT PIC X(12).
                   07  FILLER      PIC X.
                   07  LI-BAL.
                       09  LI-LOAN-BAL PIC ZZZZZZ.ZZ-.
                       09  LI-MKR      PIC X.
                   07  LI-POFF REDEFINES LI-BAL.
                       09  LI-POCD     PIC XX.
                       09  FILLER      PIC X.
                       09  LI-PAIDOFF  PIC 9(6).
                       09  FILLER      PIC XX.

       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CHLOCK_DEF.CPY".
       COPY "LIBLP/CHLOCK_WKS.CPY".


       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/CHLOCK_SCN.CPY".


       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.

      ********************************
      *    MAIN PROGRAM MODULE
      ********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CHLOCK" TO VDU-FORM-NAME.

           PERFORM OPEN-BR-FILE.

           MOVE BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           MOVE " " TO ERRCD.
       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM READ-BR-FILE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           GO TO MAIN-MODULE.

      ***********************************
       ENTER-KEYS SECTION.
      ***********************************
       ENTER-KEYS-NO.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           IF LOAN-KEY NOT = 0
              MOVE LOAN-KEY TO LN-ACCTNO
              MOVE 0 TO LOAN-KEY
              GO TO READ-LNFILE1.

           MOVE "ENTER ACCOUNT NUMBER, F6-SCAN, F7-EXIT" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N140LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              IF IO-TYPE = "CLR"
                 PERFORM RESET-SCREEN
                 GO TO ENTER-KEYS-NO
              ELSE
                 MOVE LN-ACCTNO TO VDUNUM0
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEYS-NO.
           IF VDUNUM0 = 0
              GO TO ENTER-KEYS-NO.

           MOVE BRANCH  TO LN-OWNBR.
           MOVE VDUNUM0 TO LN-ACCTNO.
       READ-LNFILE1.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-BAD
              MOVE "INVALID ACCOUNT NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-INRESCISSION )
              MOVE "ACCOUNT IS IN RESCISSION" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           PERFORM OPEN-BW1-FILE.
           MOVE LN-SSNO(1) TO BW-SSNO. 
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.

           PERFORM DISPLAY-BORROWER.

           PERFORM DISPLAY-LOAN.

           MOVE LN-REC TO ORIGINAL-LN-REC.
           IF LN-OWNBR NOT = BRANCH
              MOVE LN-OWNBR TO DIS-BR
              MOVE DIS-BRANCH TO MESS
              PERFORM SEND-MESS.

       END-ENTER-KEYS.
           EXIT.

      *************************************************
       ENTER-SCANKEY SECTION.
      *************************************************
           MOVE "Y" TO DEFAULT1-KEY.
       ENTER-SCAN.
           MOVE 1 TO SCAN-FLAG.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           IF DEFAULT1-KEY = "N"
              GO TO ENTER-TYPE.

           MOVE "N" TO DEFAULT1-KEY.

           MOVE "SN N010TYP." TO VDU.
           MOVE 2 TO TYP VDUNUM0.
           PERFORM SCREENIO.

           MOVE SPACES TO BW-NAME.

           PERFORM START-BW2-FILE.

           GO TO DISPLAY-TYPE.
       ENTER-TYPE.
           MOVE SCAN-TYPES TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "EN N010TYP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO TYP.
           IF TYP < 1 OR TYP > 4
              GO TO ENTER-TYPE.

           IF TYP = 1
              MOVE 0 TO BW-SSNO
              PERFORM START-BW1-FILE
           ELSE
              IF TYP = 2
                 MOVE SPACES TO BW-NAME
                 PERFORM START-BW2-FILE
              ELSE
                 IF TYP = 3
                    MOVE 0 TO BW-SPSSNO
                    PERFORM START-BW3-FILE
                 ELSE
                    IF TYP = 4
                       MOVE SPACES TO BW-SPNAME
                       PERFORM START-BW4-FILE.
       DISPLAY-TYPE.
           MOVE TYPE-2 TO MESS.
           IF TYP = 1
              MOVE TYPE-1 TO MESS
           ELSE
              IF TYP = 3
                 MOVE TYPE-3 TO MESS
              ELSE
                 IF TYP = 4
                    MOVE TYPE-4 TO MESS.
           PERFORM SEND-LEGEND.
       ENTER-KEY.
           MOVE "E RA160SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1U"
              PERFORM CLOSE-SCANKEY
              PERFORM RESET-SCREEN
              GO TO ENTER-SCAN.
           IF VDUSTATUS = "1:"
              IF FIRST-SCAN = 0
                 MOVE " ENTER BORROWER # FIRST" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-KEY.
           IF VDUSTATUS = "1:"
              PERFORM SCAN-LOANS
              IF IO-TYPE = "CLR"
                 MOVE SPACES TO IO-TYPE
                 MOVE SPACES TO WR-BUF
                 MOVE DISPLAY-LOAN-LIST TO WR-BUF
                 PERFORM CALL-WRFORM
                 GO TO ENTER-KEY
              ELSE
                 GO TO ENTER-KEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF SCAN-ON
                 MOVE SPACES TO WR-BUF
                 MOVE LOAN-INFO-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           IF TYP = 1 OR TYP = 3
              INSPECT VDUBUF REPLACING ALL " " BY "0"
              MOVE VDUBUF TO TEST-SSNO
              IF TEST-SSNO NOT NUMERIC
              GO TO ENTER-KEY.
           IF TYP = 1 OR 3
              MOVE VDUBUF TO BW-SSNO BW-SPSSNO
           ELSE
              IF TYP = 2 OR 4
                 PERFORM ARRANGE-BW-KEY
                 MOVE BW-ALP-TABLE TO BW-NAME BW-SPNAME.

           IF TYP = 1
              PERFORM START-BW1-FILE
           ELSE
              IF TYP = 2
                 PERFORM START-BW2-FILE
              ELSE
                 IF TYP = 3
                    PERFORM START-BW3-FILE
                 ELSE
                    IF TYP = 4
                       PERFORM START-BW4-FILE.
           MOVE 0 TO SCAN-FLAG.
       READ-FILES-NEXT.
           MOVE 0 TO FIRST-SCAN.
           IF TYP = 1 PERFORM READ-BW1-FILE-NEXT.
           IF TYP = 2 PERFORM READ-BW2-FILE-NEXT.
           IF TYP = 3 PERFORM READ-BW3-FILE-NEXT.
           IF TYP = 4 PERFORM READ-BW4-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR) 
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           PERFORM DISPLAY-BORROWER.
           PERFORM DISPLAY-LOAN-INFO.

           MOVE SPACES TO SCAN.
           MOVE SPACES TO WR-BUF.
           MOVE DISPLAY-LOAN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 1 TO SCAN-FLAG.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.

       CLOSE-SCANKEY.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.

       END-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "S  A010TYP." TO VDU.
           PERFORM SCREENIO.
           MOVE "S  A160SORTKEY." TO VDU.
           PERFORM SCREENIO.
       EXIT-ENTER-SCANKEY.
           EXIT.

      **************************************
       ARRANGE-BW-KEY SECTION.
      **************************************
           MOVE 1 TO IX1 IX2.
           MOVE SPACES TO BW-ALP-TABLE.
       ARR-BW-KEY-AGAIN.
           IF IX1 NOT < 35
              GO TO ARR-BW-KEY-EXIT.
           IF VDUTAB(IX1) NOT = ","
              MOVE VDUTAB(IX1) TO BW-ALP-TBL(IX2)
           ELSE
              MOVE 35 TO IX2.
           ADD 1 TO IX1.
           ADD 1 TO IX2.
           GO TO ARR-BW-KEY-AGAIN.
       ARR-BW-KEY-EXIT.
           EXIT.

      **************************************
       SCAN-LOANS SECTION.
      **************************************
      *    GO TO START-LN4 START-LN5 START-LN6 START-LN7
           GO TO START-LN4 START-LN5 
                 DEPENDING ON SCAN-FLAG.
       START-LN4.
           IF SCAN-ON
              GO TO READ-LN4.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN4-WBEG-OWNBR
                                 QLN4-WEND-OWNBR.
           MOVE BW-SSNO       TO LN-SSNO(1) 
                                 QLN4-WBEG-SSNO1
                                 QLN4-WEND-SSNO1.
           PERFORM START-LN4-FILE.
           MOVE "ON" TO SCAN.
       READ-LN4.
           PERFORM READ-LN4-FILE-NEXT.
           IF IO-GOOD
              IF (LN-SSNO(1) = BW-SSNO) AND (LN-OWNBR = LN-PATH-OWNBR)
                 IF ( LN-POCD-NONLOAN )
                    GO TO READ-LN4
                 ELSE
                    PERFORM DISPLAY-LOAN
                    GO TO END-SCAN-LOANS.
           ADD 1 TO SCAN-FLAG.
           MOVE SPACES TO SCAN.
       START-LN5.
           IF SCAN-ON
              GO TO READ-LN5.
           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN5-WBEG-OWNBR
                                 QLN5-WEND-OWNBR.
           MOVE BW-SSNO       TO LN-SSNO(2) 
                                 QLN5-WBEG-SSNO2
                                 QLN5-WEND-SSNO2.
           PERFORM START-LN5-FILE.
           MOVE "ON" TO SCAN.
       READ-LN5.
           PERFORM READ-LN5-FILE-NEXT.
           IF IO-GOOD
              IF (LN-SSNO(2) = BW-SSNO) AND (LN-OWNBR = LN-PATH-OWNBR)
                 IF ( LN-POCD-NONLOAN )
                    GO TO READ-LN5
                 ELSE
                    PERFORM DISPLAY-LOAN
                    GO TO END-SCAN-LOANS.

      *    ADD 1 TO SCAN-FLAG.
      *    MOVE SPACES TO SCAN.
      *START-LN6.
      *    IF SCAN-ON
      *       GO TO READ-LN6.
      *    MOVE LN-PATH-OWNBR TO LN-OWNBR
      *                          QLN6-WBEG-OWNBR
      *                          QLN6-WEND-OWNBR.
      *    MOVE BW-SSNO       TO LN-SSNO(3) 
      *                          QLN6-WBEG-SSNO3
      *                          QLN6-WEND-SSNO3.
      *    PERFORM START-LN6-FILE.
      *    MOVE "ON" TO SCAN.
      *READ-LN6.
      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF IO-GOOD
      *       IF (LN-SSNO(3) = BW-SSNO) AND (LN-OWNBR = LN-PATH-OWNBR)
      *          IF ( LN-POCD-NONLOAN )
      *             GO TO READ-LN6
      *          ELSE
      *             PERFORM DISPLAY-LOAN
      *             GO TO END-SCAN-LOANS.
      *    ADD 1 TO SCAN-FLAG.
      *    MOVE SPACES TO SCAN.
      *START-LN7.
      *    IF SCAN-ON
      *       GO TO READ-LN7.
      *    MOVE LN-PATH-OWNBR TO LN-OWNBR
      *                          QLN7-WBEG-OWNBR
      *                          QLN7-WEND-OWNBR.
      *    MOVE BW-SSNO       TO LN-SSNO(4) 
      *                          QLN7-WBEG-SSNO4
      *                          QLN7-WEND-SSNO4.
      *    PERFORM START-LN7-FILE.
      *    MOVE "ON" TO SCAN.
      *READ-LN7.
      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF IO-GOOD
      *       IF (LN-SSNO(4) = BW-SSNO) AND (LN-OWNBR = LN-PATH-OWNBR)
      *          IF ( LN-POCD-NONLOAN )
      *             GO TO READ-LN7
      *          ELSE
      *             PERFORM DISPLAY-LOAN
      *             GO TO END-SCAN-LOANS.

           MOVE "CLR" TO IO-TYPE.
           MOVE 0 TO SCAN-FLAG.
           MOVE SPACES TO SCAN LN-REC.
       END-SCAN-LOANS.
           EXIT.

      *****************************************
       DISPLAY-BORROWER SECTION.
      *****************************************

           MOVE " " TO DISPLAY-BORR-BUF.
           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO D-SOC.
           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ", ?".
           INSPECT NAME-WORKER REPLACING FIRST "?               "
             BY NAME.
           MOVE NAME-WORKER TO D-NAME(1).
           MOVE BW-ADR1 TO D-NAME(2).
           IF BW-ADR2 NOT = SPACES
              MOVE BW-ADR2 TO D-NAME(3).
           MOVE " " TO NAME-WORKER.
           MOVE BW-CITY TO NAME-WORKER.
           MOVE BW-STATE TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ", *".
           INSPECT NAME-WORKER REPLACING FIRST "*               "
             BY NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY " *".
           MOVE BW-ZIP TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "*               "
             BY NAME.
           IF BW-ADR2 = SPACES
              MOVE NAME-WORKER TO D-NAME(3)
           ELSE
              MOVE NAME-WORKER TO D-NAME(4).
           IF BW-SPSSNO = 0
            IF BW-SPLNAME = SPACES
             IF BW-SPFNAME = SPACES
              GO TO END-DISPLAY-BORR.
           MOVE "SPOUSE:" TO D-SP0.
           IF BW-SPSSNO NOT = 0
              MOVE BW-SPSSNO TO NUM-BW
              PERFORM NUMBW-TO-ALPBW
              MOVE ALP-BW TO D-SP1.
           MOVE BW-SPLNAME TO NAME-WORKER.
           MOVE BW-SPFNAME TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
             BY NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-SP2.
       END-DISPLAY-BORR.
           MOVE DISPLAY-BORR-BUF TO WR-BUF.
           MOVE DISPLAY-BORR-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *****************************************
       DISPLAY-LOAN SECTION.
      *****************************************

           IF BORROWER-SCAN = "Y"
              GO TO DISPLAY-LOAN-EXIT.
           MOVE " " TO DISPLAY-LOAN-BUF.
           IF ( LN-POCD-NONLOAN )
              GO TO DISPLAY-LOAN-EXIT.

           PERFORM GET-SP.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           IF LN-SSNO(1) = BW-SSNO
              MOVE LN-MAKERCD(1) TO MAKER-CD
              PERFORM DISPLAY-MAKER.
           IF LN-SSNO(2) = BW-SSNO
              MOVE LN-MAKERCD(2) TO MAKER-CD
              PERFORM DISPLAY-MAKER.
           IF LN-SSNO(3) = BW-SSNO
              MOVE LN-MAKERCD(3) TO MAKER-CD
              PERFORM DISPLAY-MAKER.
           IF LN-SSNO(4) = BW-SSNO
              MOVE LN-MAKERCD(4) TO MAKER-CD
              PERFORM DISPLAY-MAKER.
           MOVE LN-ACCT-FROZEN TO D-FROZEN HOLD-FROZEN.

           MOVE LN-FROZEN-REASON TO TEST-FROZEN-REASON.
           IF TEST-FROZEN-REASON1 = X"00"
              MOVE SPACES TO D-FROZEN-REASON D-REASON-DESC
           ELSE
              MOVE LN-FROZEN-REASON TO D-FROZEN-REASON CD-CODE
              PERFORM GET-REASON-DESC
              IF IO-FG = 0
                 MOVE CD-DESC TO D-REASON-DESC.

           MOVE DISPLAY-LOAN-BUF TO WR-BUF.
           MOVE DISPLAY-LOAN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


           MOVE LN-MESSAGE TO D-MESSAGE.
           IF LN-MESSAGE = SPACES
              MOVE "." TO MESS-END
           ELSE
              MOVE ":SR." TO MESS-END.

           MOVE D-MESSAGE TO WR-BUF.
           MOVE MESSAGE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF LN-MESSAGE NOT = SPACES
              PERFORM ALARM
              PERFORM BLINK-STARS 3 TIMES
              PERFORM ALARM.

       DISPLAY-LOAN-EXIT.
           EXIT.

      *********************************************
       DISPLAY-LOAN-INFO SECTION.
      *********************************************
           MOVE 1 TO SCAN-FLAG.
           MOVE " " TO SCAN.
           MOVE "Y" TO BORROWER-SCAN.
       DISPLAY-NEXT-8.
           MOVE 0 TO SUB.
           MOVE " " TO LOAN-INFO-TABLE.
       DISPLAY-LN-INFO-CONT.
           PERFORM SCAN-LOANS.
           IF SCAN-FLAG = 0
              GO TO DISPLAY-LOAN-INFO-XFER.
           ADD 1 TO SUB.
           PERFORM GET-SP.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO LI-LOAN-NUM(SUB).
           IF LN-PLCD NOT = SPACES
              MOVE LN-PLTYPE TO LI-PLFG(SUB)
              MOVE LN-PLDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO LI-JDPLDATE(SUB).
           IF LN-JDDATE NOT = 0
              MOVE "J" TO LI-PLFG(SUB)
              MOVE LN-JDDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO LI-JDPLDATE(SUB).
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO LI-LNDATE(SUB).
           MOVE LN-SUM-LNAMT TO LI-LOAN-AMT(SUB).
           MOVE LN-SUM-ORGTERM TO LI-TRM(SUB).
           MOVE LN-DLNO TO LI-DEALER(SUB).
           MOVE LN-COLLCD TO LI-COLL(SUB).

           PERFORM GET-DELINQ-VALUE.
           MOVE DELINQ-VALUE TO LI-CONTRACT(SUB).

           IF LN-SSNO(1) = BW-SSNO
              MOVE LN-MAKERCD(1) TO LI-MKR(SUB).
           IF LN-SSNO(2) = BW-SSNO
              MOVE LN-MAKERCD(2) TO LI-MKR(SUB).
           IF LN-SSNO(3) = BW-SSNO
              MOVE LN-MAKERCD(3) TO LI-MKR(SUB).
           IF LN-SSNO(4) = BW-SSNO
              MOVE LN-MAKERCD(4) TO LI-MKR(SUB).

           IF LN-POCD NOT = SPACES
              MOVE LN-POCD TO LI-POCD(SUB)
              MOVE LN-POFFDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO LI-PAIDOFF(SUB)
              GO TO DISPLAY-LOAN-SUB-CHECK.
           IF LN-POFFDATE NOT = 0
              MOVE LN-POCD TO LI-POCD(SUB)
              MOVE LN-POFFDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO LI-PAIDOFF(SUB)
              GO TO DISPLAY-LOAN-SUB-CHECK.

           MOVE LN-CURBAL TO LI-LOAN-BAL(SUB).
       DISPLAY-LOAN-SUB-CHECK.
           IF SUB < 3
              GO TO DISPLAY-LN-INFO-CONT.
       DISPLAY-LOAN-INFO-XFER.
           MOVE LOAN-INFO-BUF TO WR-BUF.
           MOVE LOAN-INFO-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           IF SCAN-FLAG NOT = 0
              MOVE "E RA160SORTKEY." TO VDU
              PERFORM SCREENIO
              GO TO DISPLAY-NEXT-8.
       DISPLAY-LOAN-INFO-EXIT.
           MOVE "N" TO BORROWER-SCAN.
           MOVE SPACES TO IO-TYPE.

      *************************************************
      *    ENTRY-MODULE
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
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
           MOVE "ENTER FIELD #, F1-CANCEL, <ENTER>-ACCEPT" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
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
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG (ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC (ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-UP.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-DOWN.
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
           GO TO F-01.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUBUF TO LN-ACCT-FROZEN.
           IF NOT (LN-ACCT-FROZEN = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

       F-011.
           IF LN-ACCT-FROZEN NOT = "Y"
              MOVE "S  A020011." TO VDU
              MOVE "  " TO LN-FROZEN-REASON
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.

           MOVE "E  A020011." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG = 9
                 GO TO F-011.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-011.

           IF VDUBUF = SPACES
              PERFORM RESET-REASON
              MOVE VDUBUF TO LN-FROZEN-REASON
              GO TO CREATE-ROUTINE.

           MOVE VDUBUF TO CD-CODE.
           PERFORM GET-REASON-DESC.
           IF IO-FG = 9
              MOVE " INVALID CODE" TO MESS PERFORM SEND-MESS
              PERFORM RESET-REASON
              GO TO F-011.
           MOVE VDUBUF TO LN-FROZEN-REASON.

           MOVE "S  A150012." TO VDU.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      *******************
       CODE-SCAN SECTION.
      *******************
           MOVE "FZ"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.
           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A020011." TO VDU.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.
           MOVE 0 TO IO-FG.

           MOVE CDSCAN-CODE TO VDUBUF.
       EXIT-CODE-SCAN.
           EXIT.

      *****************************************
       WRITE-MODULE SECTION.
      *****************************************
           PERFORM OPEN-LN1-FILE.
           MOVE LN-REC TO CHANGED-LN-REC.
           IF ORIGINAL-LN-REC = CHANGED-LN-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-LN1-FILE.
           IF NOT IO-GOOD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF LN-REC NOT = ORIGINAL-LN-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-LN-REC TO LN-REC.
           PERFORM REWRITE-LN1-FILE.

      * CREATE A CHANGE-RECORD
           PERFORM CREATE-CHANGE-REC.
           IF BR-CHANGE-MEMO = "Y"
              MOVE "CL/CHMEMO" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH CH-REC 
              CANCEL FORM-PROGX.

       END-WRITE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      ***************************************
       CREATE-CHANGE-REC SECTION.
      ***************************************
           PERFORM GETDAT.

           PERFORM OPEN-CH-FILE.
           MOVE 1               TO HOLD-CH-SEQ.
       CREATE-CHANGE-REC-AGAIN.
           PERFORM CLEAR-CH-FILE.

           MOVE BRANCH             TO CH-BRNO.
           MOVE SYS-DATE           TO CH-DATE.
           MOVE "E"                TO CH-CODE.
           MOVE EXT-CONAME-USER-ID TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS           TO CH-TIME.
           MOVE HOLD-CH-SEQ        TO CH-SEQ.

           MOVE "ACCT FROZEN FLAG" TO CH-DESCRIPT.
           MOVE LN-ACCTNO          TO CH-ACCTNO.

           MOVE HOLD-FROZEN        TO CH-ORG-CHAR.
           MOVE LN-ACCT-FROZEN     TO CH-NEW-CHAR.

           PERFORM WRITE-CH-FILE.
           IF IO-FG NOT = 0
              IF HOLD-CH-SEQ < 999
                 ADD 1 TO HOLD-CH-SEQ
                 GO TO CREATE-CHANGE-REC-AGAIN.

           PERFORM CLOSE-CH-FILE.
       EXIT-CREATE-CHANGE-REC.
           EXIT.

      ******************************************************
       FORM-RESET SECTION.
      ******************************************************
           MOVE LPLOAN-CONAME TO CHLOCK-CONAME.
           MOVE LPLOAN-SYSDATE TO CHLOCK-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CHLOCK-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE CHLOCK-FORM-FIELDS TO WR-BUF.
           MOVE CHLOCK-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE CHLOCK-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHLOCK-HELP--DIR TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CHLOCK_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPCHCL.CPY".

      *****************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************
       COPY "LIBGB/GETDAT.CPY".
       COPY "LIBGB/ACCESS.CPY".
       RESET-REASON.
           MOVE "S  A150012." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
       GET-REASON-DESC.
           MOVE "FZ" TO CD-TYPE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM OPEN-SP1-FILE
              PERFORM READ-SP1-FILE
              PERFORM CLOSE-SP1-FILE
              IF NOT IO-GOOD
                 GO TO IO-ERROR.
       SEND-LEGEND.
           MOVE "S  A000MESS." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       GET-DELINQ-VALUE.
           MOVE LN-CONPROFILE(1) TO PROFILE-EDIT.
           INSPECT PROFILE-ED REPLACING ALL "9" BY "-".
           MOVE PROFILE-ED TO DELINQ-VALUE.
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       BLINK-STARS.
           MOVE "S  A000STAR1:F." TO VDU.
           MOVE "***" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000STAR2:F." TO VDU.
           MOVE "***" TO VDUBUF.
           PERFORM SCREENIO.
      *    MOVE "S  A000STAR1." TO VDU.
      *    PERFORM SCREENIO.
      *    MOVE "S  A000STAR2." TO VDU.
      *    PERFORM SCREENIO.
       DISPLAY-MAKER.
           IF MAKER-CD = "M"
              MOVE "MAKER" TO D-MAKER
           ELSE
              IF MAKER-CD = "J"
                 MOVE "JOINT" TO D-MAKER
              ELSE
                 MOVE "ENDORSER" TO D-MAKER.
       NUMBW-TO-ALPBW.
           MOVE NUM-BW1 TO ALP-BW1.
           MOVE NUM-BW2 TO ALP-BW2.
           MOVE NUM-BW3 TO ALP-BW3.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       RESET-SCREEN.
           MOVE SPACES TO WR-BUF.
           MOVE SCREEN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************
       IO-ROUTINE SECTION.
      *****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPCHIN.CPY".

       COPY "LIBGB/FERRORS.CPY".
      *****************************************
       END-ROUTINE SECTION.
      *****************************************
           PERFORM CLOSE-FILES.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHLOCK-SCREEN.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
