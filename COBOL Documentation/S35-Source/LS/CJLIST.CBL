       IDENTIFICATION DIVISION.
       PROGRAM-ID.   CJLIST.
       DATE-WRITTEN. 041696.
      ******************************************************************
      *
      *                      (LS)
      *    DESCRIPTION:  COLLECTOR JOURNAL FILE LISTING
      *
      *=================================================================
      * REV:
      * GLF 022398 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      * BAH 160602 CHANGED CJ-DATE FROM JULIAN TO YYYYMMDD
      * KEC 2016.1004 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         SINCE THE COMP-3 WAS REMOVED FROM CJ-BRNO, CJ-ACCTNO,
      *         CJ-DATE & CJ-START-TIME, NO LONGER NEED HOLD-FIELDS
      *         TO UNCOMP THE FIELDS TO BE USED IN A STRING.
      * BAH 170425 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181004 GLOBAL BWFILE
      * BAH 181211 GLOBAL LNFILE
      *  CS 190118 GLOBAL CJFILE
      * BAH 20190930 REMOVED ACCESS-CALL LNFILE, BWFILE, CJFILE
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
           "@(#)LS/CJLIST.CBL S35 02/23/24-11:43:37 >".
      ******************************************************************
      *
      *   F I L E - P A T H S
      *
      ******************************************************************
       COPY "LIBSP/SP01CJ.CPY".
       COPY "LIBSP/SP01CJ_SQL.CPY".
       COPY "LIBSP/SPWSCJ.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(2)     VALUE "PR".

      ******************************************************************
      *
      *   LINE WORKERS
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
      *   MISC WORKERS
      *
      ******************************************************************
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".
       01  ACCTNO-SUB           PIC 9(2)  COMP-3.

       01  RECORDS-FOUND        PIC X     VALUE "N".
           88  NO-RECORDS-FOUND           VALUE "N".

       01  SP-EXPD-FRMLA        PIC X(1).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  HOLD-ACCTNO                   PIC 9(8)  VALUE ZEROES.
       01  HOLD-NAME                     PIC X(21) VALUE SPACES.

       01  WS-NEXT-CJ                    PIC X(1)  VALUE SPACES.

      *-----------------------------------------------------------------
       01  LEGEND               PIC X(70).

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-ACCTNO      PIC 9(8).
           03  END-ACCTNO      PIC 9(8).
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  LOCAL-PRINT-FG  PIC X(1).

       01  VERSION-CONTROL     PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

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
           03  FILLER        PIC X(10)   VALUE "USER ID:".
           03  H2-USERID     PIC X(10).
           03  FILLER        PIC X(29)   VALUE " ".
           03  FILLER        PIC X(30)   VALUE
               "COLLECTOR JOURNAL FILE LISTING".
           03  FILLER        PIC X(53)   VALUE " ".

      *---------------------------------------------------------------
       01  HEAD3.
           03  FILLER        PIC X(10)   VALUE "BRANCH :".
           03  H3-BRNO       PIC 9(4).
           03  FILLER        PIC X(2)     VALUE " ".
           03  H3-BRDESC     PIC X(40)    VALUE " ".
           03  FILLER        PIC X(76)    VALUE " ".

      *---------------------------------------------------------------
       01  HEAD4.
           03  FILLER        PIC X(63)   VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "ATV".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "RST".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "ACTN".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "CON".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "REC".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(7)    VALUE "PROMISE".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(7)    VALUE "PROMISE".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(7)    VALUE "CURRENT".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(8)    VALUE "PAST-DUE".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "COLL".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "NEW".

      *---------------------------------------------------------------
       01  HEAD5.
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(7)    VALUE "ACCOUNT".
           03  FILLER        PIC X(4)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "NAME".
           03  FILLER        PIC X(20)   VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "DATE".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(8)    VALUE "BEG-TIME".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(8)    VALUE "END-TIME".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "COD".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "COD".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "CODE".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "STA".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "STA".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "DATE".
           03  FILLER        PIC X(6)    VALUE SPACES.
           03  FILLER        PIC X(6)    VALUE "AMOUNT".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(7)    VALUE "BALANCE".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(7)    VALUE "BALANCE".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(2)    VALUE "ID".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "COLL".

      *****************************************************************
      *
      *    D E T A I L  L I N E
      *
      *****************************************************************
       01  DETAIL-LINE      PIC X(132)   VALUE SPACES.

      *----------------------------------------------------------------
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D1-ACCTNO            PIC X(13).
           03  FILLER               PIC X(1).
           03  D1-BW-NAME           PIC X(21).
           03  FILLER               PIC X(1).
           03  D1-DATE              PIC 99/99/99.
           03  FILLER               PIC X(1).
           03  D1-START-TIME        PIC X(8).
           03  FILLER               PIC X(1).
           03  D1-END-TIME          PIC X(8).
           03  FILLER               PIC X(1).
           03  D1-ACTIVITY-CD       PIC X(3).
           03  FILLER               PIC X(1).
           03  D1-RESULT-CD         PIC X(3).
           03  FILLER               PIC X(1).
           03  D1-ACTION-CD         PIC X(4).
           03  FILLER               PIC X(1).
           03  D1-CON-STATUS        PIC X(3).
           03  FILLER               PIC X(1).
           03  D1-REC-STATUS        PIC X(3).
           03  FILLER               PIC X(1).
           03  D1-PROMISE-DATE      PIC 99/99/99        BLANK WHEN ZERO.
           03  FILLER               PIC X(1).
           03  D1-PROMISE-AMT       PIC ZZZZZ9.99.
           03  FILLER               PIC X(1).
           03  D1-CURRENT-BAL       PIC ZZZZZ9.99.
           03  FILLER               PIC X(1).
           03  D1-PASTDUE-BAL       PIC ZZZZZ9.99.
           03  FILLER               PIC X(2).
           03  D1-COLLID            PIC X(3).
           03  FILLER               PIC X(2).
           03  D1-NEW-COLLID        PIC X(3).

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
       COPY "LIBSP/COLRPT_CMD.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/CJLIST_DEF.CPY".
       COPY "LIBLS/CJLIST_WKS.CPY".

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/CJLIST_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
      *-----------------------------------------------------------------
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CJ2-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CJLIST" TO FORM-PROG VDU-FORM-NAME.

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
           IF LOCAL-PRINT-FG = "Y"
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO NEXT-GROUP-LOC.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              CJ-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM SET-BRANCH-HEADINGS.
           PERFORM OPEN-CJ2-FILE.

           MOVE BR-NO         TO CJ-BRNO
                                 QCJ2-WBEG-BRNO
                                 QCJ2-WEND-BRNO.
           MOVE BEG-ACCTNO    TO CJ-ACCTNO
                                 QCJ2-WBEG-ACCTNO.
           MOVE END-ACCTNO    TO QCJ2-WEND-ACCTNO.
           MOVE BEG-DATE      TO CJ-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ2-WBEG-DATE.
           MOVE END-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ2-WEND-DATE.
     
           MOVE 0                   TO CJ-START-TIME
                                       QCJ2-WBEG-START-TIME.
           MOVE ALL "9"             TO QCJ2-WEND-START-TIME.

           PERFORM START-CJ2-FILE.

       NEXT-CJ2-REPORT.

           PERFORM READ-CJ2-FILE-NEXT.
           IF ( IO-FG = 9 ) OR (CJ-BRNO NOT = CJ-PATH-OWNBR)
              GO TO END-OF-GROUP.

           PERFORM CHECK-SET-PARAMETERS.

           IF ( WS-NEXT-CJ = "Y" )
              GO TO NEXT-CJ2-REPORT.

      * VALID CJ RECORD

           PERFORM CHECK-ACCTNO-CHANGE.
           PERFORM PRINT-DETAIL-LINE.
           GO TO NEXT-CJ2-REPORT.

       END-OF-GROUP.

           PERFORM CLOSE-CJ2-FILE.

           IF ( LOCAL-PRINT-FG = "Y" )
              MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.

           IF ( NO-RECORDS-FOUND )
              GO TO END-PROGRAM.

           IF ( LOCAL-PRINT-FG = "N" )
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE CJLIST-QUEUE-POS TO Q-POS.
           MOVE CJLIST-COPIES-POS TO C-POS.
           MOVE CJLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE 0 TO PAGE-NUMBER.

           MOVE EXT-CONAME-CONAME TO H1-NAME.
           MOVE EXT-CONAME-SYSDATE TO H1-DATE.
           MOVE EXT-CONAME-USER-ID TO H2-USERID.

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


           IF EXT-ROUTE-BUF = "00"
              MOVE "COLLECTOR JOURNAL FILE LISTING IN PROGRESS....."
                                                     TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF LOCAL-PRINT-FG NOT = "Y"
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF ( VDUSTATUS = "1U" )
              GO TO ENDING-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - DEFAULT TO USER BRANCH  - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF ( GROUP-FLAG NOT = "Y" )
      *       MOVE "SNNN040BR1." TO VDU
      *       MOVE 0 TO BEG-BRANCH VDUNUM0
      *       PERFORM SCREENIO
      *       MOVE "SNNN040BR2." TO VDU
      *       MOVE 0 TO END-BRANCH VDUNUM0
      *------>PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BEGIN-BRANCH.
           MOVE LEGEND-BR-01 TO LEGEND.
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
           MOVE LEGEND-BR-02 TO LEGEND.
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

      *-----------------------------------------------------------------
       BEGIN-ACCTNO.
           MOVE LEGEND-ACCTNO-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ACCTNO.
           IF VDUSTATUS = "1U" GO TO ENDING-BRANCH.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-ACCTNO.
           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENDING-ACCTNO.
           MOVE LEGEND-ACCTNO-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060END-ACCTNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-ACCTNO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDING-ACCTNO.
           MOVE VDUNUM0 TO END-ACCTNO.

           IF ( BEG-ACCTNO > END-ACCTNO )
              MOVE "INVALID ACCOUNT NUMBER RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-ACCTNO.

           GO TO BEGIN-DATE.

      *-----------------------------------------------------------------
       ALL-ACCTNO.
           MOVE "SNNN060BEG-ACCTNO." TO VDU.
           MOVE ZEROES               TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060END-ACCTNO." TO VDU.
           MOVE 999999               TO END-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BEGIN-DATE.
           IF ( EXT-ROUTE-BUF NOT = "00" ) AND ( VDUSTATUS = "1U" )
              GO TO ENDING-ACCTNO.

           IF ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO LOCAL-PRINT.

           MOVE LEGEND-DATE-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNM080BEG-DATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U" GO TO ENDING-ACCTNO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGIN-DATE.

           IF ( VDUNUM0 = 0 )
              GO TO BEGIN-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *-----------------------------------------------------------------
       ENDING-DATE.
           IF EXT-ROUTE-BUF NOT = "00" AND VDUSTATUS = "1U"
              GO TO ENDING-ACCTNO.

           MOVE LEGEND-DATE-02 TO LEGEND.
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
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENDING-ACCTNO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO ASK-LOCAL.

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
              MOVE "S  A000BEG-BRANCH." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH." TO VDU
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
       
           MOVE "SNNN060BEG-ACCTNO." TO VDU.
           MOVE BEG-ACCTNO           TO VDUNUM0.
           PERFORM SCREENIO.
       
           MOVE "SNNN060END-ACCTNO." TO VDU.
           MOVE END-ACCTNO           TO VDUNUM0.
           PERFORM SCREENIO.
       
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
       CHECK-SET-PARAMETERS SECTION.
      ******************************************************************

           MOVE "N" TO WS-NEXT-CJ.

           IF ( CJ-BRNO < BEG-BRANCH ) OR ( CJ-BRNO > END-BRANCH )
              GO TO END-CHECK-SET-PARM.

           IF ( CJ-ACCTNO < BEG-ACCTNO ) OR ( CJ-ACCTNO > END-ACCTNO )
              GO TO END-CHECK-SET-PARM.
       
           IF CJ-DATE < BEG-DATE OR CJ-DATE > END-DATE
              GO TO END-CHECK-SET-PARM.

           GO TO EXIT-CHECK-SET-PARM.

       END-CHECK-SET-PARM.
           MOVE "Y" TO WS-NEXT-CJ.

       EXIT-CHECK-SET-PARM.
           EXIT.

      ******************************************************************
       CHECK-ACCTNO-CHANGE SECTION.
      ******************************************************************

           IF ( FIRST-TIME = "Y" )
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1            TO LN-FEED
              MOVE "Y"          TO RECORDS-FOUND
              MOVE "N"          TO FIRST-TIME
              MOVE 1            TO ACCTNO-SUB
              MOVE CJ-ACCTNO    TO HOLD-ACCTNO.

           IF ( CJ-ACCTNO NOT = HOLD-ACCTNO )
              MOVE 1         TO ACCTNO-SUB
              MOVE CJ-ACCTNO TO HOLD-ACCTNO
              MOVE ALL "-"   TO DETAIL-LINE
              MOVE 2         TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

       EXIT-CHECK-ACCTNO-CHANGE.
           EXIT.

      ******************************************************************
       PRINT-DETAIL-LINE SECTION.
      ******************************************************************

           MOVE SPACES TO HOLD-NAME.

      * PRINTING LINE 1 

           IF ( ACCTNO-SUB = 1 )
              MOVE CJ-ACCTNO TO LN-ACCTNO
              PERFORM GET-BW-NAME
              PERFORM LPSTAT-DISPLAY
              MOVE LPS-DISPLAY-LNNO TO D1-ACCTNO
              MOVE HOLD-NAME        TO D1-BW-NAME
              ADD  1                TO ACCTNO-SUB
           ELSE
              ADD  1                TO ACCTNO-SUB.

           MOVE CJ-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D1-DATE.

           MOVE CJ-START-TIME    TO H-AND-M.
           PERFORM FORMAT-TIME.
           MOVE TIME-EDIT        TO D1-START-TIME.

           MOVE CJ-START-TIME    TO H-AND-M.
           PERFORM FORMAT-TIME.
           MOVE TIME-EDIT        TO D1-END-TIME.

           MOVE CJ-ACTIVITY-CD   TO D1-ACTIVITY-CD.
           MOVE CJ-RESULT-CD     TO D1-RESULT-CD.
           MOVE CJ-ACTION-CD     TO D1-ACTION-CD.
           MOVE CJ-CON-STAT      TO D1-CON-STATUS.
           MOVE CJ-REC-STAT      TO D1-REC-STATUS.
           MOVE CJ-PROMISE-DATE1 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D1-PROMISE-DATE.
           MOVE CJ-PROMISE-AMT   TO D1-PROMISE-AMT.
           MOVE CJ-CUR-BAL       TO D1-CURRENT-BAL.
           MOVE CJ-PAST-DUE      TO D1-PASTDUE-BAL.
           MOVE CJ-COLLID        TO D1-COLLID.
           MOVE CJ-NEW-COLLID    TO D1-NEW-COLLID.
           MOVE 1                TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-DETAIL-LINE.
           EXIT.

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
       SET-BRANCH-HEADINGS SECTION.
      ******************************************************************
           MOVE BR-NO     TO H3-BRNO.
           MOVE BR-NAME   TO H3-BRDESC.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H1-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT   TO H1-TIME.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD2 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD4 TO PR-REC.
           MOVE 2     TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD5 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE ALL "-" TO PR-REC.
           MOVE 1       TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 7 TO LN-COUNT.
           MOVE 1 TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 99 TO LN-COUNT.
           MOVE 4  TO LN-FEED.
       
           IF ( GROUP-FLAG = "Y" )
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP      TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH   TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH   TO R-NUM
              PERFORM WRITE-DETAIL-LINE.
       
           MOVE "BEG ACCTNO   " TO RANGE-LINE.
           MOVE BEG-ACCTNO      TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "END ACCTNO   " TO RANGE-LINE.
           MOVE END-ACCTNO      TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "BEG DATE"      TO RANGE-LINE.
           MOVE BEG-DATE        TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "END DATE"      TO RANGE-LINE.
           MOVE END-DATE        TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
       
      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO CJLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CJLIST-SYSDATE.
           DISPLAY CJLIST-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CJLIST-FORM-FIELDS TO WR-BUF.
           MOVE CJLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE CJLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE CJLIST-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/CJLIST_EVA.CPY".
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
       GET-BW-NAME.
           MOVE CJ-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "*** NONE ***" TO HOLD-NAME
           ELSE
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM OPEN-BW1-FILE
              PERFORM READ-BW1-FILE
              PERFORM CLOSE-BW1-FILE
              IF ( IO-FG NOT = ZEROES )
                 MOVE "*** NONE ***" TO HOLD-NAME
              ELSE
                 STRING BW-LNAME,", ",BW-FNAME
                        DELIMITED BY "   " INTO HOLD-NAME.

           PERFORM CLOSE-LN1-FILE.

      *-----------------------------------------------------------------
       FORMAT-TIME.
           IF ( H-AND-M NOT GREATER THAN 1159 )
              MOVE "AM" TO T-AMPM
           ELSE
              MOVE "PM" TO T-AMPM.

           IF ( H-AND-M IS NOT LESS THAN 1300 )
              SUBTRACT 1200 FROM H-AND-M.

           IF ( CUR-HH = 0 )
              MOVE 12 TO CUR-HH.

           MOVE CUR-HH TO T-HH.
           MOVE CUR-MM TO T-MM.
           MOVE ":"    TO T-COL.
           MOVE SPACE  TO T-SPACE.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPCJGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBSP/SPCJ2RN.CPY".
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

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CJLIST-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
