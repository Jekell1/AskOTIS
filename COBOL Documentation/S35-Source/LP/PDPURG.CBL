       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PDPURG.
       DATE-WRITTEN.  101085
      ******************************************************************
      *
      *               (LP)
      *
      * DESC:   PURGE STALE LOANS AND QUOTES IN THE PDFILE
      *
      * REV:
      *  BAH 2023.0923 #1624
      *  BAH 2023.1016 #1624A, INCLUDE ALL PD-ID'S WITH AT LEAST A 2 MONTH OLD
      *                DATE TEST
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/PDPURG.CBL S35 10/16/23-13:46:14 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".

      ******************************************************************
      *    M I S C    W O R K E R S
      ******************************************************************
       01  WORK-READ           PIC 9(9)  VALUE 0.
       01  WORK-PURGE          PIC 9(9)  VALUE 0.
       01  DISPLAY-FLAG        PIC X VALUE "N".

      ******************************************************************
      *
      *    S C R E E N - R A N G E - F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR              PIC 9(04).
           03  END-BR              PIC 9(04).
           03  PURGE-DATE          PIC 9(8).
           03  GROUP-FLAG          PIC X(01).
           03  ENT-GROUP           PIC X(04).

       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  SV-BEG-BR            PIC 9999  VALUE 9999.
       01  SV-END-BR            PIC 9999  VALUE 9999.
       01  SAVE-GROUP-FLAG      PIC X(01).
       01  SAVE-ENT-GROUP       PIC X(04).

       01  MESS-LIST            PIC X(5) VALUE "MESS.".

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/PDPURG_DEF.CPY".
       COPY "LIBLP/PDPURG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/PDPURG_SCN.CPY".

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
           CLOSE PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.

           MOVE "PDPURG" TO VDU-FORM-NAME FORM-PROG.
      D    STOP MESS.

           MOVE "N" TO DISPLAY-FLAG.

           PERFORM INITIALIZATION.
           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO END-PROGRAM.

           MOVE "Y" TO DISPLAY-FLAG.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BR TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.

           IF ( GROUP-FLAG = "N" )
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF ( WGR-SRESULT = 0 )
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD     )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD ) OR ( BR-NO > END-BR )
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO PD-PATH-OVERRIDE.

           PERFORM OPEN-PD1-FILE.

           MOVE "L"           TO PD-ID
                                 QPD1-WBEG-ID
           MOVE "X"           TO QPD1-WEND-ID.
           MOVE PD-PATH-OWNBR TO PD-BRANCH
                                 QPD1-WBEG-BRANCH
                                 QPD1-WEND-BRANCH.
           MOVE ZEROES        TO PD-SOC-SEC
                                 QPD1-WBEG-SOC-SEC.
           MOVE ALL "9"       TO QPD1-WEND-SOC-SEC.

           PERFORM START-PD1-FILE.

       NEXT-PD.
           PERFORM READ-PD1-FILE-NEXT.
           IF ( IO-BAD ) OR ( PD-BRANCH NOT = BR-NO )
              GO TO END-OF-GROUP.

           ADD 1 TO WORK-READ.
 
      * INCLUDE ALL PD-IDS

           IF PD-LOANDATE < PURGE-DATE
              ADD 1 TO WORK-PURGE
              PERFORM DELETE-PD1-FILE.

           GO TO NEXT-PD.

       END-OF-GROUP.
           PERFORM CLOSE-PD1-FILE.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE PDPURG-QUEUE-POS TO Q-POS.
           MOVE PDPURG-COPIES-POS TO C-POS.
           MOVE PDPURG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "PURGE SAVED LOANS & QUOTES IN PROGRESS" TO MESS
              PERFORM SEND-LEGEND.

           IF ( BEG-BR = ZEROES ) AND
              ( END-BR = ZEROES )
              MOVE BEG-BR TO SV-BEG-BR
              MOVE END-BR TO SV-END-BR
              MOVE EXT-CONAME-USER-LOC  TO BEG-BR
                                           END-BR.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BR         TO SV-BEG-BR
              MOVE END-BR         TO SV-END-BR
              MOVE GROUP-FLAG         TO SAVE-GROUP-FLAG
              MOVE ENT-GROUP          TO SAVE-ENT-GROUP
              MOVE EXT-EOBUF-GROUP    TO ENT-GROUP
              MOVE ZEROES             TO BEG-BR
              MOVE 9999               TO END-BR
              MOVE "Y"                TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO PURGE-DATE.
      * DO NOT ALLOW AN END DATE LESS THAN 2 MONTHS OLD
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           MOVE -2            TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           IF PURGE-DATE > NDTE-DATE
              MOVE "PURGE DATE CAN'T BE WITHIN 2 MONTHS" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.
           IF ( UP-KEY )
              GO TO BR-01.

       BR-01.

           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR-FILE.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF ( NOT ENTER-KEY ) GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF ( WGR-WORD-ZERO )
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > 0 )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"           TO GROUP-FLAG
              MOVE ZEROES        TO BEG-BR
              MOVE 9999          TO END-BR
              GO TO DATE-01.

       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR    TO VDUNUM0.
           PERFORM SCREENIO.

       BR-02.

           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF (        UP-KEY ) GO TO BR-01.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF ( NOT ENTER-KEY ) GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.

           IF ( BEG-BR > END-BR )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( END-BR = ZEROES )
              GO TO BR-02.

           GO TO DATE-01.

       ENTER-ALL-BRANCH.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0             TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N"           TO GROUP-FLAG.

       DATE-01.
           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO EXIT-PARM.
           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF (        UP-KEY ) GO TO BR-02.
           IF ( NOT ENTER-KEY ) GO TO DATE-01.
           MOVE VDU-DATE-YYYYMMDD TO PURGE-DATE.

           IF PURGE-DATE = 0
              GO TO DATE-01.

      * DO NOT ALLOW AN END DATE LESS THAN 2 MONTHS OLD
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           MOVE -2            TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           IF PURGE-DATE > NDTE-DATE
              MOVE "PURGE DATE CAN'T BE WITHIN 2 MONTHS" TO MESS
              PERFORM SEND-MESS
              GO TO DATE-01.
         

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF ( GROUP-FLAG = "Y" )
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR    TO VDUNUM0
              PERFORM SCREENIO.

      ******************************************************************
       SCAN-GR-FILE SECTION.
      ******************************************************************

           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF GRSCAN-STATUS NOT = "00"
              MOVE "1<" TO VDUSTATUS
              GO TO SCAN-GR-FILE-EXIT.

           MOVE "S  A000BR1." TO VDU.
           MOVE GRSCAN-KEY    TO VDUBUF GR1-KEY.
           PERFORM SCREENIO.

           MOVE "00"       TO VDUSTATUS.
           MOVE GRSCAN-KEY TO VDUBUF.

       SCAN-GR-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO PDPURG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PDPURG-SYSDATE.
           DISPLAY PDPURG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE PDPURG-FORM-FIELDS TO WR-BUF.
           MOVE PDPURG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.


      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE PDPURG-HELP--FILE TO HELP-LINK-FILE.
           MOVE PDPURG-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/PDPURG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       SEND-LEGEND.
           MOVE MESS      TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FILES-PROCESSED.
           MOVE SPACES               TO MESS.

           MOVE "SNRA090READ-FILES." TO VDU.
           MOVE WORK-READ           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A100TITLE-READ." TO VDU.
           MOVE "RECORDS READ"        TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNRA090PURG-FILES." TO VDU.
           MOVE WORK-PURGE           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A120TITLE-PURG." TO VDU.
           MOVE "RECORDS DELETED"      TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010ANS." TO VDU.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPPD1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           IF ( EXT-ROUTE-BUF = "00" )
             IF DISPLAY-FLAG = "Y"
              PERFORM FILES-PROCESSED.

       EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "LP/EMPUMU"  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY PDPURG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
