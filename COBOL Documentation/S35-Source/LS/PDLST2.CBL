       IDENTIFICATION DIVISION.
       PROGRAM-ID.   PDLST2.
       DATE-WRITTEN. 080996.
      ******************************************************************
      *
      *                      (LS)
      *                     PDLST2.C
      *    REVISED PDLIST WHICH INCLUDES GROUP LOGIC, BATCH LOGIC AND
      *            LOCAL SPOOLING.
      *
      *     PRINTS OUT EACH FIELD IN THE PD RECORD
      *
      * REV:
      *  MJD 050995 ADDED CODE TO PRINT 'R' RECORDS
      *  GLF 022398 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  MJD 991230 ADDED PD-TOTNODEFACT AND PD-YTDNODEFACT (WORLD PR#216)
      *  MJD 000119 ADDED LN-TOTNOLCHG (WORLD PR#218)
      *  MJD 000919 ADDED LN-LBOX, LN-CONRATE, AND LN-PRIOR-STMT-COUNTER
      *             (MFC PR#227)
      *  MG  010305 ADDED PD-EXTRA-PRIN-LFP, PD-REMAIN-PRIN-SINCE-LFP
      *             AND PD-REMAIN-PRIN-SINCE-LFP PER KEN & JOHN.
      *  BAH 020712 SIGNED PD-TRADEIN, REGACC PR# 1808
      *
      * KEC 2016.0516 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED USE OF APPNO TO APP-NUMBER FOR MA1-KEY USAGE.
      *
      * BAH 170504 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 190114 GLOBAL PDFILE
      * BAH 20190930 REMOVED ACCESS-CALL PDFILE
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
      *     W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/PDLST2.CBL S35 02/21/24-12:02:15 >".

      ******************************************************************
      *     F I L E   I F N      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PR-IFN               PIC X(2)     VALUE "PR".

      ******************************************************************
      *   LINE WORKERS     *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      ******************************************************************
      *   MISC WORKERS     *
      ******************************************************************
       01  SUB                  PIC 9(2).
       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  RECORDS-FOUND        PIC X     VALUE "N".
           88  NO-RECORDS-FOUND           VALUE "N".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".

       COPY "LIBLP/EMSFRG_CMD.CPY".
       01  LEGEND-OPT-01.
           03  FILLER           PIC X(338)  VALUE
               "^, V, ENTER REPORT OPTION TYPE (L/B/R)".
       01  LEGEND               PIC X(70).

      ******************************************************************
      *     S C R E E N   F I E L D S   *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  OPTION          PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  LOCAL-PRINT-FG  PIC X(1).

       01  VERSION-CONTROL     PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9(4)        VALUE 9999.
       01  SV-END-BR           PIC 9(4)        VALUE 9999.
       COPY "LIBGB/PASSWDW.CPY".

      ******************************************************************
      *     P R I N T   B U F F E R S   *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(30)    VALUE " ".
           03  FILLER           PIC X(38) VALUE
               "- - - SCRATCH PAD FILE LISTING - - -".

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).
           03  FILLER           PIC X(46) VALUE " ".

       01  HEAD4.
           03  FILLER           PIC X(16) VALUE "REPORT OPTION : ".
           03  H-OPT-TYPE       PIC X.
           03  FILLER           PIC X VALUE " ".
           03  H-OPT-DESC       PIC X(20).

       01  HEAD5.
           03  FILLER           PIC X(49)    VALUE
           "BRANCH    SOCIAL SECURITY #    ACCOUNT #     DATE".


      *-----------------------------------------------------------------
       01  DETAIL-LINE      PIC X(132)  VALUE SPACES.

       01  D1-LINE          REDEFINES DETAIL-LINE.
           03  D1-PROMPT       PIC X(30).
           03  D1-TEXT         PIC X(50).
           03  D1-NUMERIC-FLD REDEFINES D1-TEXT.
               05 D1-NUMERIC   PIC 9(12).
               05 FILLER       PIC X(38).
           03  D1-DATE-FLD   REDEFINES D1-TEXT.
               05 FILLER       PIC X(4).
               05 D1-DATE      PIC 99/99/99.
               05 FILLER       PIC X(38).
           03  D1-DECIMAL-FLD REDEFINES D1-TEXT.
               05 D1-DECIMAL   PIC 9(9).99-.
               05 FILLER       PIC X(37).
           03  D1-RATE-03-FLD REDEFINES D1-TEXT.
               05  FILLER      PIC X(05).
               05  D1-RATE-03  PIC 999.999.
               05  FILLER      PIC X(38).


      *    03  D1-NUMERIC   REDEFINES D1-TEXT
      *                     PIC 9(12).




       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.
      *****************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/PDLST2_DEF.CPY".
       COPY "LIBLS/PDLST2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/PDLST2_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *****************************************************************
      *
      *     M A I N   P R O G R A M
      *
      *****************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
           MOVE FORM-PATHNAME  TO FORM-PATH.
           MOVE "PDLST2"       TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           IF LOCAL-PRINT-FG = "Y"
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO NEXT-GROUP-LOC.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO PD-PATH-OVERRIDE.

           PERFORM OPEN-PD1-FILE.
           MOVE PD-PATH-OWNBR TO PD-BRANCH
                                 QPD1-WBEG-BRANCH
                                 QPD1-WEND-BRANCH.
           MOVE OPTION        TO PD-ID
                                 QPD1-WBEG-ID
                                 QPD1-WEND-ID.
           MOVE 0             TO PD-SOC-SEC
                                 QPD1-WBEG-SOC-SEC.
           MOVE ALL "9"       TO QPD1-WEND-SOC-SEC.

           PERFORM START-PD1-FILE.

       NEXT-PD.
           PERFORM READ-PD1-FILE-NEXT.
           IF IO-FG = 9 OR (PD-BRANCH NOT = PD-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF FIRST-TIME = "Y"
              MOVE PD-BRANCH  TO HOLD-BRNO
              PERFORM SET-BRANCH-HEADINGS.

           IF PD-BRANCH NOT = HOLD-BRNO
              MOVE PD-BRANCH  TO HOLD-BRNO
              PERFORM SET-BRANCH-HEADINGS
              PERFORM HEAD-PAGE.

           IF PD-ID NOT = OPTION
              GO TO NEXT-PD.

           PERFORM PRINT-DETAIL-LINE-OF-PD.

           GO TO NEXT-PD.

      *----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-PD1-FILE.

           IF ( LOCAL-PRINT-FG = "Y" )
              IF ( FIRST-TIME NOT = "Y" )
                 MOVE "Y" TO FIRST-TIME
                 MOVE 0 TO PAGE-NUMBER
              ELSE
                 MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

      *----------------------------------------------------------------
       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF LOCAL-PRINT-FG = "N"
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      *****************************************************************
       INITIALIZATION SECTION.
      *****************************************************************
           MOVE PDLST2-QUEUE-POS TO Q-POS.
           MOVE PDLST2-COPIES-POS TO C-POS.
           MOVE PDLST2-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF ( LOCAL-PRINT-FG NOT = "Y" )
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           MOVE 0 TO PAGE-NUMBER.

      *----------------------------------------------------------------
       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
      
           IF ( GROUP-FLAG = "Y" )
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      *****************************************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************************************
           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.

      *****************************************************************
       ENTER-PARAMETERS SECTION.
      *****************************************************************
       ENTER-PARM.
           IF VDUSTATUS = "1U"
                 GO TO OPTION-01.

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

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       OPTION-01.
           MOVE LEGEND-OPT-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010OPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OPTION-01.
           MOVE VDUBUF TO OPTION.
           IF OPTION NOT = "L" AND NOT = "B" AND NOT = "R"
              GO TO OPTION-01.

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
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
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

      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************************************
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

           MOVE "S  A010OPT." TO VDU.
           MOVE OPTION TO VDUBUF.
           PERFORM SCREENIO.
      *--------------------------------------------------------
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       PRINT-DETAIL-LINE-OF-PD SECTION.
      ******************************************************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "Y" TO RECORDS-FOUND
              MOVE "N" TO FIRST-TIME.

           MOVE "PD-ID"                  TO D1-PROMPT.
           MOVE PD-ID                    TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-BRANCH"              TO D1-PROMPT.
           MOVE PD-BRANCH                TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SOC-SEC"             TO D1-PROMPT.
           MOVE PD-SOC-SEC               TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-OWNBR"               TO D1-PROMPT.
           MOVE PD-OWNBR                 TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CLASS"               TO D1-PROMPT.
           MOVE PD-CLASS                 TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ACCTNO"              TO D1-PROMPT.
           MOVE PD-ACCTNO                TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SUBCLASS"            TO D1-PROMPT.
           MOVE PD-SUBCLASS              TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LAWCODE"             TO D1-PROMPT.
           MOVE PD-LAWCODE               TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4

              MOVE "PD-SSNO"             TO D1-PROMPT
              MOVE PD-SSNO(SUB)          TO D1-NUMERIC
              PERFORM WRITE-DETAIL-LINE

              MOVE "PD-MAKERCD"          TO D1-PROMPT
              MOVE PD-MAKERCD(SUB)       TO D1-TEXT
              PERFORM WRITE-DETAIL-LINE
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PD-ORGST"               TO D1-PROMPT.
           MOVE PD-ORGST                 TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ORGBR"               TO D1-PROMPT.
           MOVE PD-ORGBR                 TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-OWNST"               TO D1-PROMPT.
           MOVE PD-OWNST                 TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SPRCLASS"            TO D1-PROMPT.
           MOVE PD-SPRCLASS              TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CONVERCD"            TO D1-PROMPT.
           MOVE PD-CONVERCD              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-MIDSTREAM"           TO D1-PROMPT.
           MOVE PD-MIDSTREAM             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PURCD"               TO D1-PROMPT.
           MOVE PD-PURCD                 TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SRCD"                TO D1-PROMPT.
           MOVE PD-SRCD                  TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-COLLCD"              TO D1-PROMPT.
           MOVE PD-COLLCD                TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-COLLID"              TO D1-PROMPT.
           MOVE PD-COLLID                TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-POCD"                TO D1-PROMPT.
           MOVE PD-POCD                  TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PLCD"                TO D1-PROMPT.
           MOVE PD-PLCD                  TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PLTYPE"              TO D1-PROMPT.
           MOVE PD-PLTYPE                TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CONFIDENTCD"         TO D1-PROMPT.
           MOVE PD-CONFIDENTCD           TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ALLOTCD"             TO D1-PROMPT.
           MOVE PD-ALLOTCD               TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-RECASTCD"            TO D1-PROMPT.
           MOVE PD-RECASTCD              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SKIP-TRW"            TO D1-PROMPT.
           MOVE PD-SKIP-TRW              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-STATUSFG"            TO D1-PROMPT.
           MOVE PD-STATUSFG              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SMALLOAN"            TO D1-PROMPT.
           MOVE PD-SMALLOAN              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SALEFIN"             TO D1-PROMPT.
           MOVE PD-SALEFIN               TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SERVACCT"            TO D1-PROMPT.
           MOVE PD-SERVACCT              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LOANTYPE"            TO D1-PROMPT.
           MOVE PD-LOANTYPE              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CREATION-STATUS"     TO D1-PROMPT.
           MOVE PD-CREATION-STATUS       TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ADDON-FG"            TO D1-PROMPT.
           MOVE PD-ADDON-FG              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PAY-SCHLD-FG"        TO D1-PROMPT.
           MOVE PD-PAY-SCHLD-FG          TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LC-CODE"             TO D1-PROMPT.
           MOVE PD-LC-CODE               TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CAN-LOAN-NO"         TO D1-PROMPT.
           MOVE PD-CAN-LOAN-NO           TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-STATFLAGS"           TO D1-PROMPT.
           MOVE PD-STATFLAGS             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------
           MOVE "PD-ENTDATE"             TO D1-PROMPT.
           MOVE PD-ENTDATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LOANDATE"            TO D1-PROMPT.
           MOVE PD-LOANDATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PURDATE"             TO D1-PROMPT.
           MOVE PD-PURDATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-INTDATE"             TO D1-PROMPT.
           MOVE PD-INTDATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE  "PD-CASHDATE"           TO D1-PROMPT.
           MOVE PD-CASHDATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-FILEDATE"            TO D1-PROMPT.
           MOVE PD-FILEDATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ORIG-1STPAY"         TO D1-PROMPT.
           MOVE PD-ORIG-1STPYDATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-1STPYDATE"           TO D1-PROMPT.
           MOVE PD-1STPYDATE             TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-JDDATE"              TO D1-PROMPT.
           MOVE PD-JDDATE                TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PLDATE"              TO D1-PROMPT.
           MOVE PD-PLDATE                TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ACCELDATE"           TO D1-PROMPT.
           MOVE PD-ACCELDATE             TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-REDUDATE"            TO D1-PROMPT.
           MOVE PD-REDUDATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-POFFDATE"            TO D1-PROMPT.
           MOVE PD-POFFDATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LEGALDATE"           TO D1-PROMPT.
           MOVE PD-LEGALDATE             TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-BNKRPTDATE"          TO D1-PROMPT.
           MOVE PD-BNKRPTDATE            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DEBTAIDATE"          TO D1-PROMPT.
           MOVE PD-DEBTAIDATE            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-REPODATE"            TO D1-PROMPT.
           MOVE PD-REPODATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-REDEEMDATE"          TO D1-PROMPT.
           MOVE PD-REDEEMDATE            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SOLDDATE"            TO D1-PROMPT.
           MOVE PD-SOLDDATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-INS-CONFIRMATION"    TO D1-PROMPT.
           MOVE PD-INS-CONFIRM-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-APRATE"              TO D1-PROMPT.
           MOVE PD-APRATE                TO D1-RATE-03.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-EFFRATE"             TO D1-PROMPT.
           MOVE PD-EFFRATE               TO D1-RATE-03.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SMPRATE"             TO D1-PROMPT.
           MOVE PD-SMPRATE               TO D1-RATE-03.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-YLDRATE"             TO D1-PROMPT.
           MOVE PD-YLDRATE               TO D1-RATE-03.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-OVRDRATE"            TO D1-PROMPT.
           MOVE PD-OVRDRATE              TO D1-RATE-03.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ENDORATE"            TO D1-PROMPT.
           MOVE PD-ENDORATE              TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-JDRATE"              TO D1-PROMPT.
           MOVE PD-JDRATE                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ACCRATE"             TO D1-PROMPT.
           MOVE PD-ACCRATE               TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-REDURATE"            TO D1-PROMPT.
           MOVE PD-REDURATE              TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SERVPER"             TO D1-PROMPT.
           MOVE PD-SERVPER               TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.
      *----------------------------------------------------
           MOVE "PD-SELLPR"              TO D1-PROMPT.
           MOVE PD-SELLPR                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DOWNPY"              TO D1-PROMPT.
           MOVE PD-DOWNPY                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TRADEIN"             TO D1-PROMPT.
           MOVE PD-TRADEIN               TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LNAMT"               TO D1-PROMPT.
           MOVE PD-LNAMT                 TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-FINCHG"              TO D1-PROMPT.
           MOVE PD-FINCHG                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-INTCHG"              TO D1-PROMPT.
           MOVE PD-INTCHG                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SERCHG"              TO D1-PROMPT.
           MOVE PD-SERCHG                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-EXTCHG"              TO D1-PROMPT.
           MOVE PD-EXTCHG                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-MAINTFEE"            TO D1-PROMPT.
           MOVE PD-MAINTFEE              TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-POINTS"              TO D1-PROMPT.
           MOVE PD-POINTS                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PROCEEDS"            TO D1-PROMPT.
           MOVE PD-PROCEEDS              TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.
      *--------------------------------------------------------
           MOVE "PD-ORGTERM"             TO D1-PROMPT.
           MOVE PD-ORGTERM               TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-1STPYAMT"            TO D1-PROMPT.
           MOVE PD-1STPYAMT              TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-NOREGPY"             TO D1-PROMPT.
           MOVE PD-NOREGPY               TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-REGPYAMT"            TO D1-PROMPT.
           MOVE PD-REGPYAMT              TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LASTPYAMT"           TO D1-PROMPT.
           MOVE PD-LASTPYAMT             TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PROCASH"             TO D1-PROMPT.
           MOVE PD-PROCASH               TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TILLNO"              TO D1-PROMPT.
           MOVE PD-TILLNO                TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.
      *-------------------------------------------------------
           MOVE "PD-STATE-FLAG"          TO D1-PROMPT.
           MOVE PD-STATE-FLAG            TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-STATE-WORKER"        TO D1-PROMPT.
           MOVE PD-STATE-WORKER          TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-STATE-WORKER2"       TO D1-PROMPT.
           MOVE PD-STATE-WORKER2         TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.
      *----------------------------------------------------
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               STRING "PD-FEECODE" "(" SUB ")" DELIMITED BY
                  "  " INTO D1-PROMPT
               MOVE PD-FEECODE(SUB)      TO D1-NUMERIC
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-FEEAMT"          TO D1-PROMPT
               MOVE PD-FEEAMT(SUB)       TO D1-DECIMAL
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-FEEOURS"         TO D1-PROMPT
               MOVE PD-FEEOURS(SUB)      TO D1-TEXT
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-FEEFINCHG"       TO D1-PROMPT
               MOVE PD-FEEFINCHG(SUB)    TO D1-TEXT
               PERFORM WRITE-DETAIL-LINE
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PD-INSURED"             TO D1-PROMPT.
           MOVE PD-INSURED               TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-INSTYPES"            TO D1-PROMPT.
           MOVE PD-INSTYPES              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-INSELIGIBLE"         TO D1-PROMPT.
           MOVE PD-INSELIGIBLE           TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
               STRING "PD-INSCOMP" "("  SUB ")"  DELIMITED BY "  "
                  INTO D1-PROMPT
               MOVE PD-INSCOMP(SUB)      TO D1-NUMERIC
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-INSOURS"         TO  D1-PROMPT
               MOVE PD-INSOURS(SUB)      TO  D1-TEXT
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-INSEFF"          TO D1-PROMPT
               MOVE PD-INSEFF-DATE(SUB)  TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY          TO D1-NUMERIC
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-INSEXP"          TO D1-PROMPT
               MOVE PD-INSEXP-DATE(SUB)  TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY          TO D1-DATE
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-INSPREM"         TO D1-PROMPT
               MOVE PD-INSPREM(SUB)      TO D1-DECIMAL
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-INSCOVR"         TO D1-PROMPT
               MOVE PD-INSCOVR(SUB)      TO D1-DECIMAL
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-INSCOMM"         TO D1-PROMPT
               MOVE PD-INSCOMM(SUB)      TO D1-DECIMAL
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-INSANTICERN"     TO D1-PROMPT
               MOVE PD-INSANTICERN(SUB)  TO D1-DECIMAL
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-INSANTICADJ"     TO  D1-PROMPT
               MOVE PD-INSANTICADJ(SUB)  TO D1-DECIMAL
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-INSUFERN"        TO D1-PROMPT
               MOVE PD-INSUFERN(SUB)     TO D1-DECIMAL
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-INSACCERN"       TO D1-PROMPT
               MOVE PD-INSACCERN(SUB)    TO D1-DECIMAL
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-INSEARN-ORDER"   TO D1-PROMPT
               MOVE PD-INSEARN-ORDER(SUB) TO D1-NUMERIC
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-INSFINCHG"       TO D1-PROMPT
               MOVE PD-INSFINCHG(SUB)    TO D1-TEXT
               PERFORM WRITE-DETAIL-LINE
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PD-PYMMYY-RECDEL"       TO D1-PROMPT.
           MOVE PD-PY-YYYYMM-RECDEL      TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-MTD-PAYS-RECDEL"     TO D1-PROMPT.
           MOVE PD-MTD-PAYS-RECDEL       TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SHIP-SAME-FG"        TO D1-PROMPT.
           MOVE PD-SHIP-SAME-FG          TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-AD-D-INS-COVR"       TO D1-PROMPT.
           MOVE PD-AD-D-INS-COVR         TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TOT-PARTIAL-CHGOFF"  TO D1-PROMPT.
           MOVE PD-TOT-PARTIAL-CHGOFF    TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LPO"                 TO D1-PROMPT.
           MOVE PD-LPO                   TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-REP-CD"              TO D1-PROMPT.
           MOVE PD-REP-CD                TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PROCESS-CD"          TO D1-PROMPT.
           MOVE PD-PROCESS-CD            TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DLNO"                TO D1-PROMPT.
           MOVE PD-DLNO                  TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DLPOOL"              TO D1-PROMPT.
           MOVE PD-DLPOOL                TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DLRECOURSE"          TO D1-PROMPT.
           MOVE PD-DLRECOURSE            TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DLPARTRCAMT"         TO D1-PROMPT.
           MOVE PD-DLPARTRCAMT           TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ORIGPURAMT"          TO D1-PROMPT.
           MOVE PD-ORIGPURAMT            TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PURBAL"              TO D1-PROMPT.
           MOVE PD-PURBAL                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
               STRING "PD-DLRVPRIN" "(" SUB ")" DELIMITED BY "  "
                    INTO D1-PROMPT
               MOVE PD-DLRVPRIN(SUB)     TO  D1-DECIMAL
               PERFORM WRITE-DETAIL-LINE

               STRING "PD-DLRVCHGS" "(" SUB ")" DELIMITED BY "  "
                  INTO D1-PROMPT
               MOVE PD-DLRVCHGS(SUB)     TO  D1-DECIMAL
               PERFORM WRITE-DETAIL-LINE
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PD-DLPARVHELD"          TO D1-PROMPT.
           MOVE PD-DLPARVHELD            TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DLPARVPAID"          TO D1-PROMPT.
           MOVE PD-DLPARVPAID            TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DLDISC"              TO D1-PROMPT.
           MOVE PD-DLDISC                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LCPDTH"              TO D1-PROMPT.
           MOVE PD-LCPDTH-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-INTPDTH"             TO D1-PROMPT.
           MOVE PD-INTPDTH-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CURBAL"              TO D1-PROMPT.
           MOVE PD-CURBAL                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-INTBAL"              TO D1-PROMPT.
           MOVE PD-INTBAL                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LCBAL"               TO D1-PROMPT.
           MOVE PD-LCBAL                 TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-OTHBAL"              TO D1-PROMPT.
           MOVE PD-OTHBAL                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-OT2BAL"              TO D1-PROMPT.
           MOVE PD-OT2BAL                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LCPAID"              TO D1-PROMPT.
           MOVE PD-LCPAID                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LCPARTIALS"          TO D1-PROMPT.
           MOVE PD-LCPARTIALS            TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-STMT-COUNTER"        TO D1-PROMPT.
           MOVE PD-STMT-COUNTER          TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-MONTHEND-BAL"        TO D1-PROMPT.
           MOVE PD-MONTHEND-BAL          TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ANTICTERM"           TO D1-PROMPT.
           MOVE PD-ANTICTERM             TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6

               IF SUB = 1
                  MOVE "PD-ANTICERN-INTEREST"        TO D1-PROMPT
               END-IF
               IF SUB = 2
                  MOVE "PD-ANTICERN-1ST PAY EXT"     TO D1-PROMPT
               END-IF
               IF SUB = 3
                  MOVE "PD-ANTICERN-SERVICE-CHG"     TO D1-PROMPT
               END-IF
               IF SUB = 4
                  MOVE "PD-ANTICERN-MAINT-FEE"       TO D1-PROMPT
               END-IF
               IF SUB = 5
                  MOVE "PD-ANTICERN-DISCOUNT"         TO D1-PROMPT
               END-IF
               IF SUB = 6
                  MOVE "PD-ANTICERN-OTHER"            TO D1-PROMPT
               END-IF

               MOVE PD-ANTICERN(SUB)     TO D1-DECIMAL
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-ANTICADJ"        TO D1-PROMPT
               MOVE PD-ANTICADJ(SUB)     TO D1-DECIMAL
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-UFERN"           TO D1-PROMPT
               MOVE PD-UFERN(SUB)        TO D1-DECIMAL
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-ACCERN"          TO D1-PROMPT
               MOVE PD-ACCERN (SUB)      TO D1-DECIMAL
               PERFORM WRITE-DETAIL-LINE

               MOVE "PD-EARN-ORDER"      TO D1-PROMPT
               MOVE PD-EARN-ORDER(SUB)   TO D1-NUMERIC
               PERFORM WRITE-DETAIL-LINE
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PD-LMO-PRIOR-INTEREST"  TO D1-PROMPT.
           MOVE PD-LMO-PRIOR-INTEREST    TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PRIOR-UNEARNED"      TO D1-PROMPT.
           MOVE PD-PRIOR-UNEARNED        TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ERNDELQ-STOP"        TO D1-PROMPT.
           MOVE PD-ERNDELQ-STOP          TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-NONACRERN"           TO D1-PROMPT.
           MOVE PD-NONACRERN             TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PRIOR-NONACRERN"     TO D1-PROMPT.
           MOVE PD-PRIOR-NONACRERN       TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ERN-IB-PDTH"         TO D1-PROMPT.
           MOVE PD-ERN-IB-PDTH-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ERN-IB-PRINBAL"      TO D1-PROMPT.
           MOVE PD-ERN-IB-PRINBAL        TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ERN-IB-INTOWE"       TO D1-PROMPT.
           MOVE PD-ERN-IB-INTOWE         TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TOTERN"              TO D1-PROMPT.
           MOVE PD-TOTERN                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TOTOTH"              TO D1-PROMPT.
           MOVE PD-TOTOTH                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TOTINT"              TO D1-PROMPT.
           MOVE PD-TOTINT                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TOTLCHG"             TO D1-PROMPT.
           MOVE PD-TOTLCHG               TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TOTNOLCHG"           TO D1-PROMPT.
           MOVE PD-TOTNOLCHG             TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TOTDEF"              TO D1-PROMPT.
           MOVE PD-TOTDEF                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TOTSERACCT"          TO D1-PROMPT.
           MOVE PD-TOTSERACCT            TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.
      *-------------------------------------------------------
           MOVE "PD-LCWAIVERS"           TO D1-PROMPT.
           MOVE PD-LCWAIVERS             TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-INTWAIVERS"          TO D1-PROMPT.
           MOVE PD-INTWAIVERS            TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TOTPAYMNTD"          TO D1-PROMPT.
           MOVE PD-TOTPAYMNTD            TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TOTEXCPAYMNTD"       TO D1-PROMPT.
           MOVE PD-TOTEXCPAYMNTD         TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TOTNODEF"            TO D1-PROMPT.
           MOVE PD-TOTNODEF              TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-YTDNODEF"            TO D1-PROMPT.
           MOVE PD-YTDNODEF              TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TOTNODEFACT"         TO D1-PROMPT.
           MOVE PD-TOTNODEFACT           TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-YTDNODEFACT"         TO D1-PROMPT.
           MOVE PD-YTDNODEFACT           TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.
      *-------------------------------------------------------
           MOVE "PD-TOTNOALDEL"          TO D1-PROMPT.
           MOVE PD-TOTNOALDEL            TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-YTDINTPD"            TO D1-PROMPT.
           MOVE PD-YTDINTPD              TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LYRINTPD"            TO D1-PROMPT.
           MOVE PD-LYRINTPD              TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TOTINTPD"            TO D1-PROMPT.
           MOVE PD-TOTINTPD              TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-RECASTINT"           TO D1-PROMPT.
           MOVE PD-RECASTINT             TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-RECASTPRIN"          TO D1-PROMPT.
           MOVE PD-RECASTPRIN            TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.
      *-------------------------------------------------
           MOVE "PD-TRW-DOFD  "          TO D1-PROMPT.
           MOVE PD-TRW-DOFD-DATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE. 
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CL-REBATE"           TO D1-PROMPT.
           MOVE PD-CL-REBATE             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-AH-REBATE"           TO D1-PROMPT.
           MOVE PD-AH-REBATE             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PP-REBATE"           TO D1-PROMPT.
           MOVE PD-PP-REBATE             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-INT-REBATE"          TO D1-PROMPT.
           MOVE PD-INT-REBATE            TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SC-REBATE"           TO D1-PROMPT.
           MOVE PD-SC-REBATE             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-MF-REBATE"           TO D1-PROMPT.
           MOVE PD-MF-REBATE             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DF-REBATE"           TO D1-PROMPT.
           MOVE PD-DF-REBATE             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-EX-REBATE"           TO D1-PROMPT.
           MOVE PD-EX-REBATE             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-O1-REBATE"           TO D1-PROMPT.
           MOVE PD-O1-REBATE             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-O2-REBATE"           TO D1-PROMPT.
           MOVE PD-O2-REBATE             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-O3-REBATE"           TO D1-PROMPT.
           MOVE PD-O3-REBATE             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-O4-REBATE"           TO D1-PROMPT.
           MOVE PD-O4-REBATE             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-O5-REBATE"           TO D1-PROMPT.
           MOVE PD-O5-REBATE             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-COLLECTOR"           TO D1-PROMPT.
           MOVE PD-COLLECTOR             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ACTIONCD"            TO D1-PROMPT.
           MOVE PD-ACTIONCD              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ACTIONCD2"           TO D1-PROMPT.
           MOVE PD-ACTIONCD2             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ATCODE"              TO D1-PROMPT.
           MOVE PD-ATCODE                TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-RECODE"              TO D1-PROMPT.
           MOVE PD-RECODE                TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PROMDATE"            TO D1-PROMPT.
           MOVE PD-PROMDATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PROMAMT"             TO D1-PROMPT.
           MOVE PD-PROMAMT               TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DATE-PAID-LAST"      TO D1-PROMPT.
           MOVE PD-DATE-PAID-LAST        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-AMT-PAID-LAST"       TO D1-PROMPT.
           MOVE PD-AMT-PAID-LAST         TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LASTCFORMNO"         TO D1-PROMPT.
           MOVE PD-LASTCFORMNO           TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LASTCFORMDATE"       TO D1-PROMPT.
           MOVE PD-LASTCFORMDATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DELCNT"              TO D1-PROMPT.
           MOVE PD-DELCNT                TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DELCNT2"             TO D1-PROMPT.
           MOVE PD-DELCNT2               TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CREDITLIM"           TO D1-PROMPT.
           MOVE PD-CREDITLIM             TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CRSCORE"             TO D1-PROMPT.
           MOVE PD-CRSCORE               TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LASTSFORMNO"         TO D1-PROMPT.
           MOVE PD-LASTSFORMNO           TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LASTSFORMDATE"       TO D1-PROMPT.
           MOVE PD-LASTSFORMDATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SOLCNT"              TO D1-PROMPT.
           MOVE PD-SOLCNT                TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-MESSAGE"             TO D1-PROMPT.
           MOVE PD-MESSAGE               TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PROMDATE2"           TO D1-PROMPT.
           MOVE PD-PROMDATE2             TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-OLDACCTNO"           TO D1-PROMPT.
           MOVE PD-OLDACCTNO             TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PRLNNO"              TO D1-PROMPT.
           MOVE PD-PRLNNO                TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PRBAL"               TO D1-PROMPT.
           MOVE PD-PRBAL                 TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PRINTDUE"            TO D1-PROMPT.
           MOVE PD-PRINTDUE              TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PRFINCHGRB"          TO D1-PROMPT.
           MOVE PD-PRFINCHGRB            TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PRINSRB"             TO D1-PROMPT.
           MOVE PD-PRINSRB               TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PRPOCD"              TO D1-PROMPT.
           MOVE PD-PRPOCD                TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PRPODATE"            TO D1-PROMPT.
           MOVE PD-PRPODATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PRCONPROFILE"        TO D1-PROMPT.
           MOVE PD-PRCONPROFILE          TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PRDELINQAMT"         TO D1-PROMPT.
           MOVE PD-PRDELINQAMT           TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.
      *----------------------------------------------------
           MOVE "PD-SUM-ORIGPURAMT"      TO D1-PROMPT.
           MOVE PD-SUM-ORIGPURAMT        TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SUM-LNAMT"           TO D1-PROMPT.
           MOVE PD-SUM-LNAMT             TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SUM-FINCHG"          TO D1-PROMPT.
           MOVE PD-SUM-FINCHG            TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SUM-INTCHG"          TO D1-PROMPT.
           MOVE PD-SUM-INTCHG            TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SUM-ORGTERM"         TO D1-PROMPT.
           MOVE PD-SUM-ORGTERM           TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SUM-REGPYAMT"        TO D1-PROMPT.
           MOVE PD-SUM-REGPYAMT          TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SUM-EXTCHG"          TO D1-PROMPT.
           MOVE PD-SUM-EXTCHG            TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SUM-SERCHG"          TO D1-PROMPT.
           MOVE PD-SUM-SERCHG            TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SUM-MAINTFEE"        TO D1-PROMPT.
           MOVE PD-SUM-MAINTFEE          TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.
      *----------------------------------------------------
           MOVE "PD-UNITPER-CD"          TO D1-PROMPT.
           MOVE PD-UNITPER-CD            TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-UNITPER-FREQ"        TO D1-PROMPT.
           MOVE PD-UNITPER-FREQ          TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DAYS-IN-UNITPER"     TO D1-PROMPT.
           MOVE PD-DAYS-IN-UNITPER       TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-UNITPER-PER-YEAR"    TO D1-PROMPT.
           MOVE PD-UNITPER-PER-YEAR      TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-REPOCD"              TO D1-PROMPT.
           MOVE PD-REPOCD                TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-BR-TFR-FG"           TO D1-PROMPT.
           MOVE PD-BR-TFR-FG             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-O3INS-DLPROC"        TO D1-PROMPT.
           MOVE PD-O3INS-DLPROC          TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-INSPREM-DLADJ"       TO D1-PROMPT.
           MOVE PD-INSPREM-DLADJ         TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PRLNAMT"             TO D1-PROMPT.
           MOVE PD-PRLNAMT               TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PRPDUE-DAYS"         TO D1-PROMPT.
           MOVE PD-PRPDUE-DAYS           TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PLAMT"               TO D1-PROMPT.
           MOVE PD-PLAMT                 TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-MTD-PRIN"            TO D1-PROMPT.
           MOVE PD-MTD-PRIN              TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-MTD-LC"              TO D1-PROMPT.
           MOVE PD-MTD-LC                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-MTD-INT"             TO D1-PROMPT.
           MOVE PD-MTD-INT               TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-MTD-OT"              TO D1-PROMPT.
           MOVE PD-MTD-OT                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-MTD-EX"              TO D1-PROMPT.
           MOVE PD-MTD-EX                TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-BNKRPT-CHAPTER"      TO D1-PROMPT.
           MOVE PD-BNKRPT-CHAPTER        TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-BMO-CON-STATUS"      TO D1-PROMPT.
           MOVE PD-BMO-CON-STATUS        TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-BMO-REC-STATUS"      TO D1-PROMPT.
           MOVE PD-BMO-REC-STATUS        TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-APP-NUMBER"          TO D1-PROMPT.
           MOVE PD-APP-NUMBER            TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-GIFTNET"             TO D1-PROMPT.
           MOVE PD-GIFTNET               TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-GIFTNET-OUR-PO"      TO D1-PROMPT.
           MOVE PD-GIFTNET-OUR-PO        TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PROC-DISBURSED"      TO D1-PROMPT.
           MOVE PD-PROC-DISBURSED        TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-NOODDPY"             TO D1-PROMPT.
           MOVE PD-NOODDPY               TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CONPROFILE(1)"       TO D1-PROMPT.
           MOVE PD-CONPROFILE(1)         TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CONPROFILE(2)"       TO D1-PROMPT.
           MOVE PD-CONPROFILE(2)         TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CONPROFILE(3)"       TO D1-PROMPT.
           MOVE PD-CONPROFILE(3)         TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CONPROFILE(4)"       TO D1-PROMPT.
           MOVE PD-CONPROFILE(4)         TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CONPROFILE(5)"       TO D1-PROMPT.
           MOVE PD-CONPROFILE(5)         TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-RECPROFILE(1)"       TO D1-PROMPT.
           MOVE PD-RECPROFILE(1)         TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-RECPROFILE(2)"       TO D1-PROMPT.
           MOVE PD-RECPROFILE(2)         TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-RECPROFILE(3)"       TO D1-PROMPT.
           MOVE PD-RECPROFILE(3)         TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-RECPROFILE(4)"       TO D1-PROMPT
           MOVE PD-RECPROFILE(4)         TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-RECPROFILE(5)"       TO D1-PROMPT.
           MOVE PD-RECPROFILE(5)         TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TRW-STATUS"          TO D1-PROMPT.
           MOVE PD-TRW-STATUS            TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ORIGIN"              TO D1-PROMPT.
           MOVE PD-ORIGIN                TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-FOLLOWUP-DATE"       TO D1-PROMPT.
           MOVE PD-FOLLOWUP-DATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PO-TRCD"             TO D1-PROMPT.
           MOVE PD-PO-TRCD               TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PRDLDISC-UNEARN"     TO D1-PROMPT.
           MOVE PD-PRDLDISC-UNEARN       TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-YTDNOALDEL"          TO D1-PROMPT.
           MOVE PD-YTDNOALDEL            TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ACCT-FROZEN"         TO D1-PROMPT.
           MOVE PD-ACCT-FROZEN           TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LAST-ACCRUAL-DATE"   TO D1-PROMPT.
           MOVE PD-LAST-ACCRUAL-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D1-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-TRW-SPECIAL-CODE"    TO D1-PROMPT.
           MOVE PD-TRW-SPECIAL-CODE      TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PRIOR-STMT-COUNTER"  TO D1-PROMPT.
           MOVE PD-PRIOR-STMT-COUNTER    TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CONRATE         "    TO D1-PROMPT.
           MOVE PD-CONRATE               TO D1-RATE-03.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-LBOX            "    TO D1-PROMPT.
           MOVE PD-LBOX                  TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-EXTRA-PRIN-LFP  "    TO D1-PROMPT.
           MOVE PD-EXTRA-PRIN-LFP        TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-REMAIN-INT-SINCE-LFP"  TO D1-PROMPT.
           MOVE PD-REMAIN-INT-SINCE-LFP    TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-REMAIN-PRIN-SINCE-LFP" TO D1-PROMPT.
           MOVE PD-REMAIN-PRIN-SINCE-LFP   TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - -{ END OF LOAN RECORD  }- - - - - - - - - - - - - -
           MOVE "PD-MSG1"                 TO D1-PROMPT.
           MOVE PD-MSG1                   TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-MSG2"                 TO D1-PROMPT.
           MOVE PD-MSG2                   TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DLPAID"               TO D1-PROMPT.
           MOVE PD-DLPAID                 TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CKNO"                 TO D1-PROMPT.
           MOVE PD-CKNO                   TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

      *--->MOVE PD-FREE-SPACE             TO
      *--->MOVE PD-FREE-SPACE             TO
      *--->PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-FINAMT"               TO D1-PROMPT.
           MOVE PD-FINAMT                 TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PROCEEDS-FG"          TO D1-PROMPT.
           MOVE PD-PROCEEDS-FG            TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-POINTS-FG"            TO D1-PROMPT.
           MOVE PD-POINTS-FG              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-OVRDRATE-FG"          TO D1-PROMPT.
           MOVE PD-OVRDRATE-FG            TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
                   MOVE "PD-INS-FG" TO D1-PROMPT
                   MOVE PD-INS-FG(SUB)    TO D1-TEXT
                   PERFORM WRITE-DETAIL-LINE
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PD-SERCHG-FG"            TO D1-PROMPT.
           MOVE PD-SERCHG-FG              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-MAINTFEE-FG"          TO D1-PROMPT.
           MOVE PD-MAINTFEE-FG            TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-EXTCHG-FG"            TO D1-PROMPT.
           MOVE PD-EXTCHG-FG              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-REGPYAMT-FG"          TO D1-PROMPT.
           MOVE PD-REGPYAMT-FG            TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
                   MOVE "PD-FEECD"  TO D1-PROMPT
                   MOVE PD-FEECD(SUB)     TO D1-TEXT
                   PERFORM WRITE-DETAIL-LINE
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PD-PRPAID-AHEAD"         TO D1-PROMPT.
           MOVE PD-PRPAID-AHEAD           TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-RESTRICT-ADJ"         TO D1-PROMPT.
           MOVE PD-RESTRICT-ADJ           TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DUAL-LOAN-ADJ"        TO D1-PROMPT.
           MOVE PD-DUAL-LOAN-ADJ          TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CAN-LOAN-MAX"         TO D1-PROMPT.
           MOVE PD-CAN-LOAN-MAX           TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PERDOWN"              TO D1-PROMPT.
           MOVE  PD-PERDOWN               TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SELLPR-FG"            TO D1-PROMPT.
           MOVE PD-SELLPR-FG              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DOWNPY-FG"            TO D1-PROMPT.
           MOVE PD-DOWNPY-FG              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-PERDOWN-FG"           TO D1-PROMPT.
           MOVE PD-PERDOWN-FG             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-SELL-DOWN-FG"         TO D1-PROMPT.
           MOVE PD-SELL-DOWN-FG           TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-BALLOON-FG"           TO D1-PROMPT.
           MOVE PD-BALLOON-FG             TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-BALLOON-LAST-FG"      TO D1-PROMPT.
           MOVE PD-BALLOON-LAST-FG        TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
                   MOVE "PD-FEECD-FG"  TO D1-PROMPT
                   MOVE PD-FEECD-FG(SUB)  TO D1-TEXT
                   PERFORM WRITE-DETAIL-LINE
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
                  MOVE "PD-INS-TERM-SW"   TO D1-PROMPT
                  MOVE PD-INS-TERM-SW    (SUB) TO D1-TEXT
                  PERFORM WRITE-DETAIL-LINE

                  MOVE "PD-COVERAGE-SW"    TO D1-PROMPT
                  MOVE PD-COVERAGE-SW(SUB) TO D1-TEXT
                  PERFORM WRITE-DETAIL-LINE

                  MOVE "PD-PREMIUM-SW"  TO D1-PROMPT
                  MOVE PD-PREMIUM-SW (SUB) TO D1-TEXT
                  PERFORM WRITE-DETAIL-LINE
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PD-DLDISC-FG"            TO D1-PROMPT.
           MOVE PD-DLDISC-FG              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
                 MOVE "PD-DLRVPRIN-FG"   TO D1-PROMPT
                 MOVE PD-DLRVPRIN-FG(SUB) TO D1-TEXT
                 PERFORM WRITE-DETAIL-LINE

                 MOVE "PD-DLRVCHGS-FG" TO D1-PROMPT
                 MOVE PD-DLRVCHGS-FG(SUB) TO D1-TEXT
                 PERFORM WRITE-DETAIL-LINE
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PD-DLPARVHELD-FG"       TO D1-PROMPT.
           MOVE PD-DLPARVHELD-FG         TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-DLPARVPAID-FG"       TO D1-PROMPT.
           MOVE PD-DLPARVPAID-FG         TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ONE-MTH-NO-DAY-EXT-SW" TO D1-PROMPT.
           MOVE PD-ONE-MTH-NO-DAY-EXT-SW TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-ASSIGN-RN-ACCTNO"    TO D1-PROMPT.
           MOVE PD-ASSIGN-RN-ACCTNO      TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-APR-FINAMT"          TO D1-PROMPT.
           MOVE PD-APR-FINAMT            TO D1-DECIMAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-POSTING-BR"          TO D1-PROMPT.
           MOVE PD-POSTING-BR            TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-OTHBR-FG"            TO D1-PROMPT.
           MOVE PD-OTHBR-FG              TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-APPROVAL-LIMIT"      TO D1-PROMPT.
           MOVE PD-APPROVAL-LIMIT        TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-APPROVAL-USERID"     TO D1-PROMPT.
           MOVE PD-APPROVAL-USERID       TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-MIN-CRED-SCORE"      TO D1-PROMPT.
           MOVE PD-MIN-CRED-SCORE        TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD-CRED-SCORE-USERID"   TO D1-PROMPT.
           MOVE PD-CRED-SCORE-USERID     TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PD2-ID"                 TO D1-PROMPT.
           MOVE PD2-ID                   TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD2-BRANCH"             TO D1-PROMPT.
           MOVE PD2-BRANCH               TO D1-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PD2-BWNAME"             TO D1-PROMPT.
           MOVE PD2-BWNAME               TO D1-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 99 TO LN-COUNT.
       PRINT-DET-LN-OF-PD-EXIT.
           EXIT.

      *****************************************************************
       WRITE-DETAIL-LINE SECTION.
      *****************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *****************************************************************
       SET-BRANCH-HEADINGS SECTION.
      *****************************************************************
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.

      *****************************************************************
       HEAD-PAGE SECTION.
      *****************************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

              MOVE HEAD3 TO PR-REC.
              PERFORM WRITE-PR-REC.

              MOVE OPTION TO H-OPT-TYPE.
              IF OPTION = "L"
                 MOVE "LOANS" TO H-OPT-DESC
              ELSE
                 IF OPTION = "B"
                    MOVE "BORROWERS" TO H-OPT-DESC
                 ELSE
                    MOVE "RENEWALS" TO H-OPT-DESC.
              MOVE HEAD4 TO PR-REC.
              PERFORM WRITE-PR-REC.
              MOVE HEAD5 TO PR-REC.
              MOVE 2 TO LN-FEED.
              PERFORM WRITE-PR-REC.
              MOVE 6 TO LN-COUNT.
              MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
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
      
           MOVE "REPORT OPTION" TO RANGE-LINE.
           MOVE OPTION TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      *****************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO PDLST2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PDLST2-SYSDATE.
           DISPLAY PDLST2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE PDLST2-FORM-FIELDS TO WR-BUF.
           MOVE PDLST2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE PDLST2-HELP--FILE TO HELP-LINK-FILE.
           MOVE PDLST2-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/PDLST2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
       MISC-PARA SECTION.
      *****************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************************************
       IO-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPPD1RN.CPY".
      *----------------------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *****************************************************************
       END-PROGRAM SECTION.
      *****************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              IF EXT-ROUTE-BUF = "00" OR "70"
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *----------------------------------------------------------------
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY PDLST2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
