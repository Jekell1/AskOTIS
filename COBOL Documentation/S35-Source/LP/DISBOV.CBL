       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DISBOV.
       DATE-WRITTEN. 09-17-12
      ******************************************************************
      *                      (LP)
      *  DESCRIPTION:  CASH DISBURSEMENT OVERAGE REPORT
      *
      *  PRINTS ONLY TRC TYPES "CA" AND ONLY DISBURSEMENTS
      *  IF DISBURSEMENT IS > ENTERED DISBURSEMENT
      *  REV:
      *   161103 BAH CHANGED WK2-TILLNO TO 9(2)
      * 20171026 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *   190516 CHANGED HOLD-GI-KEY TO 9(7) FROM 9(6)
      *   BAH 20190922 REMOVED ACCESS-CALL TRCFILE
      ********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BR         PIC 9(4).
               05  WK-GLNO       PIC 9(11).
           03  WK-DRGLAMT        PIC S9(9)V99    COMP-3.
           03  WK-CRGLAMT        PIC S9(9)V99    COMP-3.

       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRNO            PIC 9(4).
               05  WK2-DATE            PIC 9(8).
               05  WK2-TYPE            PIC X(2).
               05  WK2-DAY-DATE        PIC 9(8).
               05  WK2-CLASS           PIC 9(3).
               05  WK2-CKNO            PIC 9(6).
               05  WK2-SEQ             PIC 9(4).
           03  WK2-AMT                 PIC S9(7)V99.
           03  WK2-REF                 PIC 9(6).
           03  WK2-REASON              PIC X(30).
           03  WK2-GLDIST     OCCURS 5.
               05  WK2-GLNO            PIC 9(11).
               05  WK2-GLAMT           PIC S9(7)V99.
           03  WK2-VOID                PIC X.
           03  WK2-PAYEE               PIC X(30).
           03  WK2-TILLNO              PIC 99.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/DISBOV.CBL S34 05/26/21-10:57:31 >".
      ****************************
      *   F I L E   P A T H S    *
      ****************************
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       01  PR-IFN               PIC X(2)    VALUE "PR".
       01  WK2-IFN              PIC X(3)    VALUE "WK2".
       01  WK-IFN               PIC X(2)    VALUE "WK".

       01  WK2-PATH             PIC X(35).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUBX                 PIC 9(6) COMP-3.
       01  FIRST-TIME           PIC X               VALUE "Y".
       01  RECORDS-FOUND        PIC X               VALUE " ".
           88  NO-RECORDS-FOUND                     VALUE "N".
       01  GL-SUM-FG            PIC X               VALUE SPACES.
       01  MSEL-LIST            PIC X(5)            VALUE "MSEL.".
       01  MESS-LIST            PIC X(5)            VALUE "MESS.".
       01  WORK-GLNO            PIC 9(11).
       01  WORK-GL              REDEFINES WORK-GLNO.
           03  WORK-GLBR        PIC 9(4).
           03  WORK-GLNUM       PIC 9(7).
       01  HOLD-BRNO            PIC 9(4)            VALUE 0.
       01  HOLD-GI1-KEY         PIC X(7)            VALUE SPACES.
       01  HOLD-TYPE            PIC 9               VALUE 0.
       01  GLMAX                PIC 9       VALUE 0.
       01  GLAMT                PIC S9(9)V99 COMP-3 VALUE 0.
       01  GLNO                 PIC 9(11)    COMP-3 VALUE 0.

       01  DISBURSE-TOTAL       PIC S9(9)V99 COMP-3 VALUE 0.
       01  CK-TOTDR             PIC S9(9)V99 COMP-3 VALUE 0.
       01  CK-TOTCR             PIC S9(9)V99 COMP-3 VALUE 0.

      *-----------------------------------------------------------------
       01  WS-ERROR-GI-FILE              PIC X(1)     VALUE "N".
       01  WS-ERROR-GI-MESSAGE.
           03  FILLER                    PIC X(10)    VALUE SPACES.
           03  FILLER                    PIC X(20)    VALUE ALL "*".
           03  FILLER                    PIC X(45)    VALUE
               "ERROR OCCURED TRYING TO ACCESS GI-RECORD --- ".
           03  FILLER                    PIC X(28)    VALUE
               "THIS BRANCH HAS BEEN SKIPPED".
           03  FILLER                    PIC X(20)    VALUE ALL "*".
           03  FILLER                    PIC X(9)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  LEGEND               PIC X(70).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  DATE1           PIC 9(8).
           03  DATE2           PIC 9(8).
           03  OVERAGE-AMT     PIC 999999.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  LOCAL-PRINT-FG  PIC X(1).

       01  VERSION-CONTROL     PIC X VALUE "B".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(22)    VALUE " ".
           03  FILLER           PIC X(48)    VALUE
           "       - DISBURSEMENT OVERAGE REGISTER -        ".

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC X(2)     VALUE " ".
           03  HD-BRNO     PIC 9(4).
           03  FILLER      PIC X        VALUE " ".
           03  HD-BRNAME   PIC X(20).
           03  FILLER      PIC X(70)    VALUE " ".

       01  HEAD4.
           03  FILLER      PIC X(17) VALUE "TYPE     --DATE--".
           03  FILLER      PIC X(7)  VALUE " ".
           03  FILLER      PIC X(14) VALUE "AMOUNT  PAYEE".
           03  FILLER      PIC X(27) VALUE " ".
           03  FILLER      PIC X(14) VALUE "REF #  REASON".
           03  FILLER      PIC X(29) VALUE " ".
           03  FILLER      PIC X(9)  VALUE "GL-NUMBER".
           03  FILLER      PIC X(7)  VALUE " ".
           03  FILLER      PIC X(7)  VALUE "AMOUNT".

       01  HEAD5.
           03  FILLER      PIC X(35) VALUE
           "------------GENERAL LEDGER SUMMARY:".

       01  HEAD6.
           03  FILLER      PIC X(53) VALUE
           " GL ACCOUNT   NAME".
           03  FILLER      PIC X(35) VALUE
           "DEBIT         CREDIT            NET".
      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

      *-----------------------------------------------------------------
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-TYPE      PIC X(8).
           03  FILLER      PIC X.
           03  D-CKDATE    PIC 99/99/99.
           03  FILLER      PIC X.
           03  D-CKAMT     PIC -----,---.99.
           03  FILLER      PIC XX.
           03  D-PAYEE     PIC X(30).
           03  FILLER      PIC XX.
           03  D-REFNO     PIC ZZZZZ9     BLANK ZERO.
           03  FILLER      PIC XX.
           03  D-REASON    PIC X(30).
           03  FILLER      PIC XX.
           03  D-TILL      PIC Z9.
           03  FILLER      PIC X.
           03  D-GLBR      PIC 9(4).
           03  D-DASH      PIC X.
           03  D-GLNUM     PIC 9(7).
           03  FILLER      PIC X.
           03  D-GLAMT     PIC -----,---.99.

      *-----------------------------------------------------------------
       01  T-LINE REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(16).
           03  T-CKTOTAL   PIC ZZZ,ZZZ,ZZZ.99-.

      *-----------------------------------------------------------------
       01  S-LINE REDEFINES DETAIL-LINE.
           03  S-GLBR      PIC 9(4).
           03  S-DASH      PIC X.
           03  S-GLNUM     PIC 9(7).
           03  FILLER      PIC X(3).
           03  S-GLNAME    PIC X(30).
           03  S-DRGLAMT   PIC ZZZ,ZZZ,ZZZ.99-.
           03  S-CRGLAMT   PIC ZZZ,ZZZ,ZZZ.99-.
           03  S-NET       PIC ZZZ,ZZZ,ZZZ.99-.

      *-----------------------------------------------------------------
       01  ERROR-GI-LINE REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(30).
           03  D-ERR-PROMPT1           PIC X(5).
           03  D-ERR-DAY               PIC 9(2).
           03  FILLER                  PIC X(5).
           03  D-ERR-PROMPT2           PIC X(8).
           03  D-ERR-BRNO              PIC 9(4).
           03  FILLER                  PIC X(5).
           03  D-ERR-PROMPT3           PIC X(6).
           03  D-ERR-CODE              PIC X(1).
           03  FILLER                  PIC X(5).
           03  D-ERR-PROMPT4           PIC X(7).
           03  D-ERR-CLASS             PIC 9(2).
           03  FILLER                  PIC X(5).
           03  D-ERR-PROMPT5           PIC X(8).
           03  D-ERR-SUBKEY            PIC X(9).
           03  FILLER                  PIC X(30).

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.

       COPY "LIBLP/EMSFRG_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/DISBOV_DEF.CPY".
       COPY "LIBLP/DISBOV_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/DISBOV_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK2-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK2-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DISBOV" TO FORM-PROG VDU-FORM-NAME.


           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

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
           IF LOCAL-PRINT-FG = "Y"
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO NEXT-GROUP-LOC.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO TRC-PATH-OVERRIDE.

           PERFORM OPEN-TRC1-FILE.

           PERFORM SORT-ROUTINE.

           IF EXT-EOBUF-ERRCD = "X"
              GO TO NEXT-GROUP-LOC.

           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.

           MOVE SPACES TO WK2-REC.
           MOVE 0 TO WK2-KEY.
           PERFORM START-WK2-FILE.

      *-----------------------------------------------------------------
       NEXT-CK.
           PERFORM READ-WK2-FILE-NEXT.

           IF IO-FG = 9
              GO TO END-OF-GROUP.


           IF FIRST-TIME = "Y"
              MOVE WK2-BRNO TO HOLD-BRNO
              PERFORM SET-BRANCH-HEADINGS.

           IF WK2-BRNO NOT = HOLD-BRNO
              PERFORM BRANCH-TOTALS
              MOVE WK2-BRNO TO HOLD-BRNO
              PERFORM SET-BRANCH-HEADINGS
              PERFORM HEAD-PAGE.

           PERFORM DETAIL-LINE-PROCESS.

           IF ( WS-ERROR-GI-FILE = "Y" )
              PERFORM WRITE-ERROR-LINE
              PERFORM CLOSE-TRC1-FILE
              PERFORM CLOSE-WK2-FILE
              PERFORM OPEN-WK2-FILE-OUTPUT
              GO TO NEXT-GROUP-LOC.

           GO TO NEXT-CK.

      *-----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           IF ( LOCAL-PRINT-FG = "Y" )
              IF ( FIRST-TIME NOT = "Y" )
                 PERFORM BRANCH-TOTALS
                 MOVE "Y" TO FIRST-TIME
                 MOVE 0 TO PAGE-NUMBER
              ELSE
                 MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF LOCAL-PRINT-FG = "N"
              PERFORM BRANCH-TOTALS
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE DISBOV-QUEUE-POS TO Q-POS.
           MOVE DISBOV-COPIES-POS TO C-POS.
           MOVE DISBOV-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO ENDIT.

      * CREATE SORT KEY FILE FOR TRFILE OUTPUT
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD
              GO TO ENDIT.
           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.

      * CREATE WORK FILE FOR GL SUMMARY:
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE GL WORKFILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE LOCAL-PRINT-FG TO SV-LOCAL-PRINT-FG
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG LOCAL-PRINT-FG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS................" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF ( LOCAL-PRINT-FG NOT = "Y" )
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-GI1-FILE.
           PERFORM OPEN-GL1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ********************************************
       EO-EXEC-AUDIT SECTION.
      ********************************************
           MOVE EXT-EOBUF-DATE1 TO DATE1.
           MOVE EXT-EOBUF-DATE2 TO DATE2.


      ********************************************
       ENTER-PARAMETERS SECTION.
      ********************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF NOT  = "00"
                 GO TO ENTER-DT1
              ELSE
                 GO TO ENTER-OVERAGE-AMT.

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       ENTER-DT1.
           IF EXT-ROUTE-BUF NOT  = "00"
              GO TO ENTER-OVERAGE-AMT.

           MOVE LEGEND-DTE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060BEGDT."   TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENTER-DT1.
           MOVE VDU-DATE-YYYYMMDD TO DATE1.

      *----------------------------------------------------------------N
       ENTER-DT2.
           IF EXT-ROUTE-BUF NOT  = "00"
              GO TO ENTER-OVERAGE-AMT.

           MOVE "ERNM060ENDDT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-DT1.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-DT2.
           MOVE VDU-DATE-YYYYMMDD  TO DATE2.

      *-----------------------------------------------------------------
       ENTER-OVERAGE-AMT.
           MOVE "ENNN060OVERAGE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO ENTER-DT2
              ELSE
                 GO TO BR-02.
           IF NOT (VDUSTATUS = "00") GO TO ENTER-OVERAGE-AMT.


           MOVE VDUNUM0 TO OVERAGE-AMT.

      *-----------------------------------------------------------------
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO END-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO END-PARM.

      *-----------------------------------------------------------------
       ASK-LOCAL.
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.
           MOVE LEGEND-LOCAL-SPOOL TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.


           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "SN N060OVERAGE." TO VDU.
           MOVE OVERAGE-AMT    TO VDUNUM0.
           PERFORM SCREENIO.


      *---------------------------------------------------------
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL   :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ***************************************
       SORT-ROUTINE SECTION.
      ***************************************
           MOVE BR-NO       TO TC-BRNO
                               QTRC1-WBEG-BRNO
                               QTRC1-WEND-BRNO.
           MOVE DATE1       TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE.
           MOVE DATE2       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WEND-KEY-DATE.

           MOVE 0                   TO TC-CKNO TC-CLASS TC-SEQ
                                       QTRC1-WBEG-CKNO
                                       QTRC1-WBEG-CLASS
                                       QTRC1-WBEG-SEQ.
           MOVE ALL "9"             TO QTRC1-WEND-CKNO
                                       QTRC1-WEND-CLASS
                                       QTRC1-WEND-SEQ.

           PERFORM START-TRC1-FILE.
       SORT-TR-NEXT.
           PERFORM READ-TRC1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRC-PATH-OWNBR NOT = TC-BRNO)
              GO TO EXIT-SORT-ROUTINE.

           IF TC-KEY-DATE > DATE2
              GO TO EXIT-SORT-ROUTINE.

           IF TC-TYPE NOT = "CA"
              GO TO SORT-TR-NEXT.

           IF TC-AMT < 0
              GO TO SORT-TR-NEXT.

           IF TC-AMT < OVERAGE-AMT
              GO TO SORT-TR-NEXT.

           MOVE TC-BRNO     TO WK2-BRNO.
           MOVE "CA"        TO WK2-TYPE.
           MOVE TC-KEY-DATE TO WK2-DAY-DATE.
           MOVE TC-CLASS    TO WK2-CLASS.
           MOVE TC-CKNO     TO WK2-CKNO.
           MOVE TC-SEQ      TO WK2-SEQ.

           MOVE TC-DATE   TO WK2-DATE.
           MOVE TC-AMT    TO WK2-AMT.
           MOVE TC-REF    TO WK2-REF.
           MOVE TC-REASON TO WK2-REASON.
           MOVE TC-PAYEE  TO  WK2-PAYEE.
           MOVE TC-TILLNO TO WK2-TILLNO.
           MOVE 5 TO GLMAX.

           PERFORM VARYING SUBX FROM 1 BY 1 UNTIL
                                  SUBX >  GLMAX
                 MOVE TC-GLNO(SUBX)  TO WK2-GLNO(SUBX)
                 MOVE TC-GLAMT(SUBX) TO WK2-GLAMT(SUBX)
           END-PERFORM.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TR-NEXT.

       EXIT-SORT-ROUTINE.
           EXIT.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

           MOVE WK2-BRNO   TO GI-BRANCH.

           IF (WK2-CLASS = 0 )
              MOVE BR-GLBANK TO GI-BANK WK-GLNO
              MOVE BR-GLONHAND TO GI-CASH
           ELSE
              MOVE WK2-CLASS TO GI-CLASS
              PERFORM GET-GI
              IF ( WS-ERROR-GI-FILE = "Y" )
                    GO TO END-DETAIL-LN-PROCESS.

           PERFORM UPDATE-CHECK-TYPE.
           MOVE 5 TO GLMAX.
           MOVE WK2-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-CKDATE.
           MOVE WK2-AMT TO D-CKAMT.
           MOVE WK2-PAYEE TO D-PAYEE.
           MOVE WK2-REASON TO D-REASON.
           MOVE WK2-REF TO D-REFNO.

           ADD WK2-AMT TO DISBURSE-TOTAL.
           MOVE "DISBURSE" TO D-TYPE.
           MOVE WK2-TILLNO TO D-TILL.

           MOVE 1 TO SUBX.

           PERFORM SETUP-TC-GL.

           IF GLNO NOT = 0
              PERFORM UPDATE-GL-SUMMARY
              MOVE GLNO TO WORK-GLNO
              MOVE WORK-GLBR TO D-GLBR
              MOVE "-" TO D-DASH
              MOVE WORK-GLNUM TO D-GLNUM
              MOVE GLAMT TO D-GLAMT
              MOVE 2 TO SUBX.

      *-----------------------------------------------------------------
       PRINT-NO-GL.
           IF HOLD-TYPE NOT = WK2-TYPE
              IF FIRST-TIME = "Y"
                 MOVE WK2-TYPE TO HOLD-TYPE
              ELSE
                 MOVE WK2-TYPE TO HOLD-TYPE
                 MOVE 2 TO LN-FEED.

           PERFORM WRITE-DETAIL-LINE.


      *-----------------------------------------------------------------
       PRINT-GLDIST.
           IF SUBX > GLMAX
              GO TO END-DETAIL-LN-PROCESS.

           PERFORM SETUP-TC-GL.

           IF GLAMT = 0
              ADD 1 TO SUBX
              GO TO PRINT-GLDIST.

           PERFORM UPDATE-GL-SUMMARY.
           MOVE GLNO TO WORK-GLNO.
           MOVE WORK-GLBR TO D-GLBR.
           MOVE "-" TO D-DASH.
           MOVE WORK-GLNUM TO D-GLNUM.
           MOVE GLAMT TO D-GLAMT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


           ADD 1 TO SUBX.
           GO TO PRINT-GLDIST.

      *-----------------------------------------------------------------
       END-DETAIL-LN-PROCESS.
           EXIT.



      ********************************************
      *    UPDATE CHECK TYPES
      *       ONLY TRC TYPES "CA" AND "RR" AND "RC"
      *                                "RP" AND "RD" AND "BK"
      *
      *    NOTE: THESE UPDATE BY CHECK TYPE
      *          MUST MATCH LPEOD5.C
      ********************************************
       UPDATE-CHECK-TYPE SECTION.

           IF WK2-TYPE = "CA"
              MOVE GI-CASH TO GLNO
              COMPUTE GLAMT = 0 - WK2-AMT
              PERFORM UPDATE-GL-SUMMARY.

       UPDATE-CHECK-TYPE-END.
           EXIT.

      ***********************************
       UPDATE-GL-SUMMARY SECTION.
      ***********************************
       UPDATE-GL-SUMMARY-REC.
           MOVE WK2-BRNO TO WK-BR.
           MOVE GLNO TO WK-GLNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              GO TO UPDATE-GL-SUMMARY-CREATE.
           IF GLAMT > 0
              ADD GLAMT TO WK-DRGLAMT
           ELSE
              ADD GLAMT TO WK-CRGLAMT.

           PERFORM REWRITE-WK-FILE.
           GO TO UPDATE-GL-SUMMARY-EXIT.

       UPDATE-GL-SUMMARY-CREATE.
           MOVE WK2-BRNO TO WK-BR.
           MOVE GLNO TO WK-GLNO.
           MOVE 0 TO WK-DRGLAMT WK-CRGLAMT.
           PERFORM WRITE-WK-FILE.

           GO TO UPDATE-GL-SUMMARY-REC.

       UPDATE-GL-SUMMARY-EXIT.
           EXIT.

      ****************************
       BRANCH-TOTALS SECTION.
      ****************************

           MOVE "TOTAL DISBURSE :" TO DETAIL-LINE.
           MOVE DISBURSE-TOTAL TO T-CKTOTAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 99 TO LN-COUNT.
           PERFORM PRINT-BR-GL-SUMMARY.
           MOVE 0 TO DISBURSE-TOTAL.
           MOVE 99 TO LN-COUNT.
           MOVE SPACES TO GL-SUM-FG.
       END-BRANCH-TOTALS.
           EXIT.

      ***********************************
       PRINT-BR-GL-SUMMARY SECTION.
      ***********************************
           MOVE "Y" TO GL-SUM-FG.
           MOVE 0 TO CK-TOTDR CK-TOTCR.
           MOVE HOLD-BRNO TO WK-BR.
           MOVE 0 TO WK-GLNO.
           PERFORM  START-WK-FILE.
           IF IO-FG NOT = 0
              GO TO PRINT-BR-GL-SUM-END.
       PRINT-BR-GL-SUM-NEXT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO PRINT-BR-GL-SUM-LINE.
           IF WK-BR NOT = HOLD-BRNO
              GO TO PRINT-BR-GL-SUM-LINE.
           MOVE WK-GLNO TO WORK-GLNO.
           MOVE WORK-GLBR TO S-GLBR.
           MOVE "-" TO S-DASH.
           MOVE WORK-GLNUM TO S-GLNUM.

           MOVE WK-GLNO TO GL-BR-ACCOUNT.
           PERFORM READ-GL1-FILE.
           IF IO-FG NOT = 0
              MOVE "****NOT ON FILE***" TO S-GLNAME
           ELSE
              MOVE GL-DESCRIPTION TO S-GLNAME.

           IF WK-DRGLAMT NOT = 0
              MOVE WK-DRGLAMT TO S-DRGLAMT
              ADD WK-DRGLAMT TO CK-TOTDR.

           IF WK-CRGLAMT NOT = 0
              MOVE WK-CRGLAMT TO S-CRGLAMT
              ADD WK-CRGLAMT TO CK-TOTCR.

           COMPUTE S-NET = WK-DRGLAMT + WK-CRGLAMT.

           PERFORM WRITE-DETAIL-LINE.
           GO TO PRINT-BR-GL-SUM-NEXT.
       PRINT-BR-GL-SUM-LINE.
           MOVE SPACES TO DETAIL-LINE.
           MOVE "TOTAL:" TO S-GLNAME.
           MOVE CK-TOTDR TO S-DRGLAMT.
           MOVE CK-TOTCR TO S-CRGLAMT.
           COMPUTE S-NET = CK-TOTDR + CK-TOTCR.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       PRINT-BR-GL-SUM-END.
           MOVE 99 TO LN-COUNT.
           MOVE SPACES TO GL-SUM-FG.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       SET-BRANCH-HEADINGS SECTION.
      ******************************************************************
           MOVE HOLD-BRNO TO HD-BRNO.
           MOVE BR-NAME   TO HD-BRNAME.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO H-TIME.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE BR-GLBANK TO WK-GLNO.
      *--->MOVE HOLD-BRNO TO HD-BRNO.
      *--->MOVE BR-NAME TO HD-BRNAME.
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GL-SUM-FG NOT = "Y"
              MOVE HEAD4 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GL-SUM-FG = "Y"
              MOVE 4 TO LN-FEED
              MOVE HEAD5 TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE HEAD6 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE 9 TO LN-COUNT
           ELSE
              MOVE 5 TO LN-COUNT.

           MOVE 2 TO LN-FEED.

      ******************************************************************
       WRITE-ERROR-LINE SECTION.
      ******************************************************************
           MOVE WS-ERROR-GI-MESSAGE TO DETAIL-LINE.
           MOVE 3                   TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "DAY: "             TO D-ERR-PROMPT1.
           MOVE TC-KEY-DATE TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-DD    TO D-ERR-DAY.
           MOVE "BRANCH: "          TO D-ERR-PROMPT2.
           MOVE TC-BRNO             TO D-ERR-BRNO.
           MOVE "CODE: "            TO D-ERR-PROMPT3.
           MOVE "C"                 TO D-ERR-CODE.
           MOVE "CLASS: "           TO D-ERR-PROMPT4.
           MOVE TC-CLASS            TO D-ERR-CLASS.
           MOVE "SEQ: "             TO D-ERR-PROMPT5.
           MOVE TC-SEQ              TO D-ERR-SUBKEY.
           MOVE 2                   TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-WRITE-ERROR-LINE.
           EXIT.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 1 TO LN-COUNT.
      * DONT SKIP TO A WHOLE NEW PAGE
           MOVE 4 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE.
      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO DISBOV-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DISBOV-SYSDATE.
           DISPLAY DISBOV-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DISBOV-FORM-FIELDS TO WR-BUF.
           MOVE DISBOV-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE DISBOV-HELP--FILE TO HELP-LINK-FILE.
           MOVE DISBOV-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/DISBOV_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************************************
       MISC SECTION.
      **************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *-----------------------------------------------------------------
       SETUP-TC-GL.
           MOVE WK2-GLNO(SUBX) TO GLNO.
           MOVE WK2-GLAMT(SUBX) TO GLAMT.
      *-----------------------------------------------------------------
       GET-GI.
           IF ( HOLD-GI1-KEY NOT = GI1-KEY )
              PERFORM READ-GI1-FILE
              IF ( IO-FG NOT = 0 )
                 MOVE "Y" TO WS-ERROR-GI-FILE
              ELSE
                 MOVE GI1-KEY TO HOLD-GI1-KEY.

      *******************************************************
       IO-ROUTINE SECTION.
      *******************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPTRC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      **************************************
       END-PROGRAM SECTION.
      **************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
                 MOVE SV-LOCAL-PRINT-FG TO LOCAL-PRINT-FG
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY DISBOV-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
