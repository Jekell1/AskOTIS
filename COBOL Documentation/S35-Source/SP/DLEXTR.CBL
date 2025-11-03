       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DLEXTR.
       DATE-WRITTEN.    012102.
      **************************************************************************
      *                      (SP)
      *    DESCRIPTION:  DATA WAREHOUSE DEALER EXTRACTION
      *
      * CREATES AN OUTPUT EXTRACT FILE  /USR/EXTRACT/DEALER
      * RSZ 1142 INCLUDING CR,LF
      *
      * REV:
      *             REGIONAL ACCEPTANCE, PR 1675
      *  040907     ADD BR-REGION-NUMBER TO XDL-REGION-NUMBER [PR#76 REGACC]
      *  100917 BAH ADDED XDL-WAIVE-ACH-FEE MIDATL PR# 3650
      *  140310 BAH ADDED XDL-TRACK-CHANGE-FLAG PR#350 REGACC
      *  150615 BAH EXPANDED XDL-KEY TO 9(9) FROM 9(7), WORLD PR# 1088
      *  160225  CS CHANGED MOVE OF DL-BRANCH TO DL-BRNO
      *  BAH 170830 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 20200831 REMOVED RESET ON BR1 
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBEX/EXFSXDL.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBEX/EXFDXDL.CPY".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./SP/DLEXTR.CBL S35 06/08/23-09:09:25 >".

       01  XDL-PATH.
           03  XDL-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  FILLER             PIC X(6) VALUE "DEALER".

       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *----------------------------------------------------------------
       01  XDL-KEY       PIC 9(9)     VALUE 0.

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  EXITPROG             PIC X(9)     VALUE "SP/DWEXMU".

       01  STARTING-TIME        PIC X(8)     VALUE SPACES.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH             PIC 9(4).
           03  END-BRANCH             PIC 9(4).
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9999 VALUE 9999.
       01  SV-END-BR           PIC 9999 VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
      *-----------------------------------------------------------------
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
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

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(27)    VALUE " ".
           03  H-TITLE          PIC X(22)    VALUE
           "DEALER FILE EXTRACTION".

      *-----------------------------------------------------------------

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6)     COMP VALUE 0.
       01  SUB2                 PIC 9(6)     COMP VALUE 0.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  WK-DISPLAY-MESS.
           03  FILLER           PIC X(29)    VALUE
           "BUILDING EXTRACTION FILE....".
           03  DISPLAY-MESS-BR  PIC 9(4).
           03  FILLER           PIC X(10).

       01  RECORD-COUNT           PIC 9(9)      VALUE 0.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(21).
           03  RANGE-SELECTION   PIC X(111).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/DLEXTR_DEF.CPY".
       COPY "LIBSP/DLEXTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/DLEXTR_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE XDL-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               XDL-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N   M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DLEXTR" TO FORM-PROG VDU-FORM-NAME.

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

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.
           MOVE "B" TO DL-PATH-OVERRIDE.

           PERFORM OPEN-DL1-FILE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
                 MOVE BR-NO TO DISPLAY-MESS-BR
                 MOVE WK-DISPLAY-MESS TO MESS
                 PERFORM SEND-LEGEND.

           MOVE 0 TO DL-DLNO.
           PERFORM START-DL1-FILE.

       NEXT-DL.
           PERFORM READ-DL1-FILE-NEXT.
           IF IO-FG = 9 OR (DL-PATH-OWNBR NOT = DL-BRNO)
              GO TO END-OF-GROUP.

           MOVE SPACES              TO XDL-REC.

           MOVE BR-NO               TO XDL-BRNO.
           MOVE BR-REGION-NUMBER     TO XDL-REGION-NUMBER.

           MOVE DL-NUM               TO XDL-NUM.
           MOVE DL-NAME              TO XDL-NAME.
           MOVE DL-ADR1              TO XDL-ADR1.
           MOVE DL-ADR2              TO XDL-ADR2.
           MOVE DL-CITY              TO XDL-CITY.
           MOVE DL-STATE             TO XDL-STATE.
           MOVE DL-ZIP               TO XDL-ZIP.
           MOVE DL-SHORT             TO XDL-SHORT.
           MOVE DL-PHONE             TO XDL-PHONE.
           MOVE DL-BRNO              TO XDL-BRANCH.
           MOVE DL-PURPOSE           TO XDL-PURPOSE.
           MOVE DL-MFGCODE           TO XDL-MFGCODE.
           MOVE DL-ORIGDATE          TO XDL-ORIGDATE.
           MOVE DL-TERMDATE          TO XDL-TERMDATE.

           MOVE DL-LC-CODE           TO XDL-LC-CODE.
           MOVE DL-FULLCEIL          TO XDL-FULLCEIL.
           INSPECT XDL-FULLCEIL REPLACING LEADING "+" BY " ".
           MOVE DL-PARTCEIL          TO XDL-PARTCEIL.
           INSPECT XDL-PARTCEIL REPLACING LEADING "+" BY " ".
           MOVE DL-NONCEIL           TO XDL-NONCEIL.
           INSPECT XDL-NONCEIL REPLACING LEADING "+" BY " ".
           MOVE DL-SIGN1             TO XDL-SIGN1.
           MOVE DL-SIGN2             TO XDL-SIGN2.
           MOVE DL-SIGN3             TO XDL-SIGN3.
           MOVE DL-MAXPURTRM         TO XDL-MAXPURTRM.
           MOVE DL-MAXPURPER         TO XDL-MAXPURPER.
           MOVE DL-EARNCD            TO XDL-EARNCD.
           MOVE DL-EARNFAC           TO XDL-EARNFAC.
           MOVE DL-DISC-FRMLA        TO XDL-DISC-FRMLA.
           MOVE DL-DISC-CODE         TO XDL-DISC-CODE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE DL-DISC-TERM(SUB) TO XDL-DISC-TERM(SUB)
               MOVE DL-DISC-RATE(SUB) TO XDL-DISC-RATE(SUB)
           END-PERFORM.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
               MOVE DL-RVPRIN-FRMLA(SUB) TO XDL-RVPRIN-FRMLA(SUB)
               MOVE DL-RVPRIN-CODE(SUB) TO XDL-RVPRIN-CODE(SUB)
               MOVE DL-RVPRIN-ADDON(SUB) TO XDL-RVPRIN-ADDON(SUB)
               INSPECT XDL-RVPRIN-ADDON(SUB)
                           REPLACING LEADING "+" BY " "
               PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 6
                  MOVE DL-RVPRIN-TERM(SUB SUB2)
                   TO XDL-RVPRIN-TERM(SUB SUB2)
                  MOVE DL-RVPRIN-RATE(SUB SUB2)
                   TO XDL-RVPRIN-RATE(SUB SUB2)
               END-PERFORM
           END-PERFORM.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
               MOVE DL-RVCHGS-FRMLA(SUB) TO XDL-RVCHGS-FRMLA(SUB)
               MOVE DL-RVCHGS-CODE(SUB) TO XDL-RVCHGS-CODE(SUB)
               MOVE DL-RVCHGS-ADDON(SUB) TO XDL-RVCHGS-ADDON(SUB)
               INSPECT XDL-RVCHGS-ADDON(SUB)
                           REPLACING LEADING "+" BY " "
               PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 6
                  MOVE DL-RVCHGS-TERM(SUB SUB2)
                   TO XDL-RVCHGS-TERM(SUB SUB2)
                  MOVE DL-RVCHGS-RATE(SUB SUB2)
                   TO XDL-RVCHGS-RATE(SUB SUB2)
               END-PERFORM
           END-PERFORM.
           MOVE DL-PARVHELD-FRMLA    TO XDL-PARVHELD-FRMLA.
           MOVE DL-PARVHELD-CODE     TO XDL-PARVHELD-CODE.
           MOVE DL-PARVHELD-ADDON    TO XDL-PARVHELD-ADDON.
           INSPECT XDL-PARVHELD-ADDON REPLACING LEADING "+" BY " ".
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE DL-PARVHELD-TERM(SUB) TO XDL-PARVHELD-TERM(SUB)
               MOVE DL-PARVHELD-RATE(SUB) TO XDL-PARVHELD-RATE(SUB)
           END-PERFORM.
           MOVE DL-PARVPAID-FRMLA    TO XDL-PARVPAID-FRMLA.
           MOVE DL-PARVPAID-CODE     TO XDL-PARVPAID-CODE.
           MOVE DL-PARVPAID-ADDON    TO XDL-PARVPAID-ADDON.
           INSPECT XDL-PARVPAID-ADDON REPLACING LEADING "+" BY " ".
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE DL-PARVPAID-TERM(SUB) TO XDL-PARVPAID-TERM(SUB)
               MOVE DL-PARVPAID-RATE(SUB) TO XDL-PARVPAID-RATE(SUB)
           END-PERFORM.
           MOVE DL-SVACCT-FRMLA      TO XDL-SVACCT-FRMLA.
           MOVE DL-SVCODE            TO XDL-SVCODE.
           MOVE DL-SVACCT-AMT        TO XDL-SVACCT-AMT.
           INSPECT XDL-SVACCT-AMT REPLACING LEADING "+" BY " ".
           MOVE DL-SVACCT-RATE       TO XDL-SVACCT-RATE.
           MOVE DL-SVACCT-LCHGPER    TO XDL-SVACCT-LCHGPER.
           MOVE DL-SVACCT-INTPER     TO XDL-SVACCT-INTPER.
           MOVE DL-SVACCT-DEFPER     TO XDL-SVACCT-DEFPER.
           MOVE DL-FULLCEIL-BAL      TO XDL-FULLCEIL-BAL.
           INSPECT XDL-FULLCEIL-BAL REPLACING LEADING "+" BY " ".
           MOVE DL-PARTCEIL-BAL      TO XDL-PARTCEIL-BAL.
           INSPECT XDL-PARTCEIL-BAL REPLACING LEADING "+" BY " ".
           MOVE DL-NONCEIL-BAL       TO XDL-NONCEIL-BAL.
           INSPECT XDL-NONCEIL-BAL REPLACING LEADING "+" BY " ".
           MOVE DL-AFP-DEALER        TO XDL-AFP-DEALER.
           MOVE DL-RECOURSE-FRMLA    TO XDL-RECOURSE-FRMLA.
           MOVE DL-FAX-NUMBER        TO XDL-FAX-NUMBER.
           MOVE DL-PARVPAID-FRMLA-IB TO XDL-PARVPAID-FRMLA-IB.
           MOVE DL-PARVPAID-CODE-IB  TO XDL-PARVPAID-CODE-IB.
           MOVE "N"                  TO XDL-REVOLVING.
           MOVE DL-LPO               TO XDL-LPO.
           MOVE DL-ACH-FG            TO XDL-ACH-FG.
           MOVE DL-BANK-ROUTING-NO   TO XDL-BANK-ROUTING-NO.
           MOVE DL-BANK-ACCTNO       TO XDL-BANK-ACCTNO.
           MOVE DL-ACH-PRENOTE-FG    TO XDL-ACH-PRENOTE-FG.
           MOVE DL-WAIVE-ACH-FEE     TO XDL-WAIVE-ACH-FEE.
           MOVE DL-TRACK-CHANGE-FLAG TO XDL-TRACK-CHANGE-FLAG.
           MOVE X"0D" TO XDL-CR.
           MOVE X"0A" TO XDL-LF.

           ADD 1 TO XDL-KEY.
           PERFORM WRITE-XDL-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO NEXT-DL.

      *----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-DL1-FILE.

           MOVE "Y" TO FIRST-TIME.

           GO TO NEXT-GROUP-LOC.

      *----------------------------------------------------------------
       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.


           PERFORM HEAD-PAGE.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE DLEXTR-QUEUE-POS TO Q-POS.
           MOVE DLEXTR-COPIES-POS TO C-POS.
           MOVE DLEXTR-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           CALL "C$TOLOWER" USING XDL-DIRECTORY, VALUE 13.
           MOVE XDL-DIRECTORY TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "EXTRACTION DIRECTORY DOES NOT EXIST!" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE " " TO EXT-EOBUF-ERRCD.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "EXTRACTION IN PROGRESS............." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           PERFORM OPEN-XDL-FILE-OUTPUT.
           PERFORM CLOSE-XDL-FILE.
           PERFORM OPEN-XDL-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

      * DONT GO FURTHER IF NOT A VALID LOGIN
      * NEED USERS BRANCH FOR BR-FILE

           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
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
                 MOVE "S NA000BR1." TO VDU
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
              MOVE "S  A160DISPLAY-USER." TO VDU
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

      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
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

           MOVE "S NA160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A040BR1." TO VDU
              MOVE ENT-GROUP TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNA040BR2." TO VDU
              MOVE SPACES TO VDUNUM0
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.

           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
       
           MOVE "STARTING TIME      :" TO RANGE-LINE.
           MOVE STARTING-TIME         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "ENDING   TIME      :" TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "# RECORDS EXTRACTED:" TO RANGE-LINE.
           MOVE RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       

      ******************************************************************
      *
      *        HEAD PAGE ROUTINE
      *
      ******************************************************************
       HEAD-PAGE SECTION.

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

       END-HEAD-PAGE.
           MOVE 3 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO DLEXTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DLEXTR-SYSDATE.
           DISPLAY DLEXTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DLEXTR-FORM-FIELDS TO WR-BUF.
           MOVE DLEXTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE DLEXTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE DLEXTR-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/DLEXTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       CONDITIONAL-PRINT.
           IF DETAIL-LINE NOT = " "
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBEX/EXXDLI.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXITPROG TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY DLEXTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
