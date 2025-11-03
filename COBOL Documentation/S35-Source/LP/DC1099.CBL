       IDENTIFICATION DIVISION.
       PROGRAM-ID.       DC1099.
       DATE-WRITTEN.   02/12/02.
      ******************************************************************
      *
      *                      (LP)
      *       DESCRIPTION:  DEBT CANCELLATION EXTRACTION.
      *                          END OF YEAR MENU
      *
      *      TAKEN FROM BW1098  DOES CORPORATE PRINT.
      *      ALWAYS MOVE IN FM-BRNO2 BEFORE READ OF FM, & MOVE IN FM-BRNO
      *      AFTER READ. NOTE: STILL HAVE A CONFLICT WITH CUSTSHEET PRINTING
      *      & CORPORATE PRINT.

      * REV:
      *     **NEW* HANOVER, UNIVERSAL, GREENVILLE, SMITHFIELD, CORNERSTONE
      *               [PR#1741]
      *
      *
      * CS  20020219 ADDED ZERO BALANCE I/S/O
      * KEC 20160217 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *              FIXED KEYS & RECS FOR FD-REC.
      *              CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      * BAH 20171026 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 20181114 GLOBAL FMFILE
      * BAH 20181207 GLOBAL LNFILE
      * BAH 20190312 MOVED FM-BRNO2 INTO LPWSFM-EDIT-FM-INITKEY TO REMOVE THE
      *              REDEFINE IN THE FM-REC
      * BAH 20190613 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BAH 20190919 REMOVED ACCESS-CALL ON LNFILE
      * BAH 20200827 REMOVED RESET ON BR1 
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/DC1099.CBL S35 02/20/24-13:48:50 >".

       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       01  TRANS-DATE           PIC 9(8).

       01  CREATE-FORM-REQUEST  PIC X VALUE "N".
       01  HOLD-FM1-KEY         PIC X(28)     VALUE SPACES.

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED          PIC 99       VALUE 0.

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH       PIC 9(4).
           03  END-BRANCH       PIC 9(4).
           03  BEG-CL           PIC 9(3).
           03  END-CL           PIC 9(3).
           03  BEG-ACCT         PIC 9(8).
           03  END-ACCT         PIC 9(8).
           03  PL-ISO           PIC X(01).
               88  PL-ISO-INCLUDE       VALUE "I".
               88  PL-ISO-SKIP          VALUE "S".
               88  PL-ISO-ONLY          VALUE "O".
               88  PL-ISO-VALID         VALUE "I" "S" "O".
           03  PL-DATE1         PIC 9(8).
           03  PL-DATE2         PIC 9(8).
           03  PL-AMT1          PIC 9(6).
           03  PL-AMT2          PIC 9(6).
           03  JD-ISO           PIC X(01).
               88  JD-ISO-INCLUDE       VALUE "I".
               88  JD-ISO-SKIP          VALUE "S".
               88  JD-ISO-ONLY          VALUE "O".
               88  JD-ISO-VALID         VALUE "I" "S" "O".
           03  BK-ISO           PIC X(01).
               88  BK-ISO-INCLUDE       VALUE "I".
               88  BK-ISO-SKIP          VALUE "S".
               88  BK-ISO-ONLY          VALUE "O".
               88  BK-ISO-VALID         VALUE "I" "S" "O".
           03  ZBAL-ISO         PIC X(01).
               88  ZBAL-ISO-INCLUDE     VALUE "I".
               88  ZBAL-ISO-SKIP        VALUE "S".
               88  ZBAL-ISO-ONLY        VALUE "O".
               88  ZBAL-ISO-VALID       VALUE "I" "S" "O".

           03  FORMNO           PIC 999.
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).

       01  VERSION-CONTROL      PIC X VALUE "B".

       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  SV-BEG-BR           PIC 9(4)   VALUE 9999.
       01  SV-END-BR           PIC 9(4)   VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/DC1099_DEF.CPY".
       COPY "LIBLP/DC1099_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/DC1099_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ********************************************************
       MAIN-MODULE SECTION.
      ********************************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DC1099" TO VDU-FORM-NAME.
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
              GO TO END-RUN.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-RUN.


      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

      *    MOVE "Y" TO NEW-BRANCH.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CL    TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CL    TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

      *----------------------------------------------------------------
       NEXT-LN.
           MOVE "N" TO CREATE-FORM-REQUEST.

           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           IF LN-CLASS > END-CL
              GO TO END-OF-GROUP.

           IF LN-ACCTNO < BEG-ACCT OR LN-ACCTNO > END-ACCT
              GO TO NEXT-LN.

      D    STOP MESS.
           PERFORM TEST-LN-FIELDS.

           IF NOT (CREATE-FORM-REQUEST = "Y")
              GO TO NEXT-LN.

           PERFORM CREATE-FM-REQUEST.

           GO TO NEXT-LN.
      *------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.

           GO TO NEXT-GROUP-LOC.


      *********************************************
       INITIALIZATION SECTION.
      *********************************************
           MOVE DC1099-QUEUE-POS TO Q-POS.
           MOVE DC1099-COPIES-POS TO C-POS.
           MOVE DC1099-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

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

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "DEBT CANCELLATION EXTRACTION IN PROGRESS" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.


      ***********************************************
       EO-EXEC-AUDIT SECTION.
      ***********************************************
           MOVE EXT-EOBUF-DATE1 TO PL-DATE1.
           MOVE EXT-EOBUF-DATE2 TO PL-DATE2.

           MOVE FORMNO TO FD-FMNO.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
           IF ( IO-BAD )
              MOVE "INVALID FORM NUMBER" TO MESS
              PERFORM SEND-MESS
              MOVE "E" TO EXT-EOBUF-ERRCD.

      *********************************************
       ENTER-PARAMETERS SECTION.
      *********************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO FMNO.

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
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
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
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.

           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              GO TO BR-01.

           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0             TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N"           TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CL.
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CL.
           IF BEG-CL > END-CL
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO BEGACT.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CL VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CL VDUNUM0.
           PERFORM SCREENIO.
       BEGACT.
           MOVE "ENNN060AC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALLACT.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGACT.
           MOVE VDUNUM0 TO BEG-ACCT.
           GO TO ENDACT.
       ALLACT.
           MOVE "SNNN060AC1." TO VDU.
           MOVE 0 TO BEG-ACCT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060AC2." TO VDU.
           MOVE 999999 TO END-ACCT VDUNUM0.
           PERFORM SCREENIO.
           GO TO PL-ISO.
       ENDACT.
           MOVE "ENNN060AC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BEGACT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENDACT.
           MOVE VDUNUM0 TO END-ACCT.
           IF BEG-ACCT > END-ACCT
              MOVE "INVALID ACCOUNT RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGACT.

       PL-ISO.
           MOVE "ENNA010PL." TO VDU.
           PERFORM SCREENIO.

           IF F7-KEY GO TO EXIT-PARM.
           IF F1-KEY GO TO BR-01.
           IF UP-KEY GO TO ENDACT.

           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO PL-ISO.
           MOVE VDUBUF     TO PL-ISO.

           IF NOT (PL-ISO-VALID) GO TO PL-ISO.

           IF NOT PL-ISO-SKIP
              GO TO BEG-PLDATE.

           MOVE "S  8060PLDT1." TO VDU.
           MOVE 0 TO VDU-DATE-YYYYMMDD PL-DATE1.
           PERFORM SCREENIO.

           MOVE "S  8060PLDT2." TO VDU.
           MOVE 0 TO VDU-DATE-YYYYMMDD PL-DATE2.
           PERFORM SCREENIO.

           MOVE "S  A060PLAMT1." TO VDU.
           MOVE SPACES TO VDUBUF.
           MOVE ZEROS TO PL-AMT1.
           PERFORM SCREENIO.

           MOVE "S  A060PLAMT2." TO VDU.
           MOVE SPACES TO VDUBUF.
           MOVE ZEROS TO PL-AMT2.
           PERFORM SCREENIO.

           GO TO JD.
      *------------------------------------------------
       BEG-PLDATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BEG-PLAMT.

           MOVE "ERNM060PLDT1."   TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PL-ISO..
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEG-PLDATE.

           MOVE VDU-DATE-YYYYMMDD TO PL-DATE1.

      *-----------------------------------------------------------
       END-PLDATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BEG-PLAMT.

           MOVE "ERNM060PLDT2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BEG-PLDATE.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO END-PLDATE.

           MOVE VDU-DATE-YYYYMMDD TO PL-DATE2.

           IF PL-DATE2 < PL-DATE1
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-PLDATE.

           GO TO BEG-PLAMT.

      *----------------------------------------------------------
       ALL-DATE.

           MOVE "S  8080PLDT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO PL-DATE1    VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080PLDT2."   TO VDU.
           MOVE EXT-JULIAN-END TO PL-DATE2    VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *-------------------------------------------------------

       BEG-PLAMT.
           MOVE "ENNN060PLAMT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALLPLAMT.
           IF VDUSTATUS = "1U" GO TO END-PLDATE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEG-PLAMT.
           MOVE VDUNUM0 TO PL-AMT1.
           GO TO END-PLAMT.
      *-----------------------------------------------------
       ALLPLAMT.
           MOVE "SNNN060PLAMT1." TO VDU.
           MOVE 0 TO PL-AMT1  VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060PLAMT2." TO VDU.
           MOVE 999999 TO PL-AMT2 VDUNUM0.
           PERFORM SCREENIO.
           GO TO JD.
      *------------------------------------------------------
       END-PLAMT.
           MOVE "ENNN060PLAMT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BEG-PLAMT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO END-PLAMT.
           MOVE VDUNUM0 TO PL-AMT2.

           IF PL-AMT1 > PL-AMT2
              MOVE "INVALID INTEREST PAID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-PLAMT.

       JD.
           MOVE "E  A010JD."   TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO BR-01.
           IF F7-KEY GO TO EXIT-PARM.
           IF UP-KEY
              IF PL-ISO-SKIP
                  GO TO PL-ISO
              ELSE
                  GO TO END-PLAMT.

           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO JD.

           MOVE VDUBUF TO JD-ISO.

           IF NOT (JD-ISO-VALID)
               GO TO JD.

       BK.
           MOVE "E  A010BK."  TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO BR-01.
           IF F7-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO JD.

           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO BK.

           MOVE VDUBUF  TO BK-ISO.

           IF NOT (BK-ISO-VALID)
              GO TO BK.

       ZBAL.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO BR-01.
           IF F7-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO BK.

           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO ZBAL.

           MOVE VDUBUF TO ZBAL-ISO.

           IF NOT(ZBAL-ISO-VALID)
               GO TO ZBAL.

       FMNO.
           MOVE "ENNN030FORMNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ZBAL.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO FMNO.
           MOVE VDUNUM0 TO FORMNO.
           MOVE FORMNO TO FD-FMNO.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
           IF NOT IO-GOOD
              MOVE "INVALID FORM NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO FMNO.

       ACCPT.

       EXIT-PARM.
           EXIT.


      *********************************************
       DISPLAY-PARAMETERS SECTION.
      *********************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH    TO VDUNUM0
              PERFORM SCREENIO.
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060AC1." TO VDU.
           MOVE BEG-ACCT TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060AC2." TO VDU.
           MOVE END-ACCT TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010PL." TO VDU.
           MOVE PL-ISO  TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN060PLAMT1." TO VDU.
           MOVE PL-AMT1 TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060PLAMT2." TO VDU.
           MOVE PL-AMT2 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010JD."   TO VDU.
           MOVE JD-ISO  TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010BK."   TO VDU.
           MOVE BK-ISO  TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL-ISO TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN030FORMNO." TO VDU.
           MOVE FORMNO TO VDUNUM0.
           PERFORM SCREENIO.

      ******************************************************
       TEST-LN-FIELDS SECTION.
      ******************************************************

           IF ( LN-POCD-NONLOAN )
              GO TO TEST-LN-FIELDS-EXIT.

      * SKIP ACCOUNTS "BULK TRANSFERRED" TO ANOTHER BRANCH, THE NEW
      * OWNING BRANCH WILL REPORT IT !!

           IF LN-POCD = "BT"
              GO TO TEST-LN-FIELDS-EXIT.

      *PL
           IF PL-ISO-SKIP
              IF LN-PLDATE NOT = ZEROS
                 GO TO TEST-LN-FIELDS-EXIT.

           IF PL-ISO-ONLY
              IF LN-PLDATE = ZEROS
                 GO TO TEST-LN-FIELDS-EXIT.

      * ONLY TEST IF NOT F3-ALL DATES !!
           IF PL-DATE1   NOT = EXT-JULIAN-BEG OR
                PL-DATE2   NOT = EXT-JULIAN-END
              IF LN-PLDATE < PL-DATE1 OR
                   LN-PLDATE > PL-DATE2
                 GO TO TEST-LN-FIELDS-EXIT.

      *PL AMOUNT
              IF PL-AMT1 = 0 AND PL-AMT2 = 999999
                  NEXT SENTENCE
              ELSE
                  IF LN-PLAMT < PL-AMT1 OR
                      LN-PLAMT > PL-AMT2
                       GO TO TEST-LN-FIELDS-EXIT.
      *JD
           IF JD-ISO-SKIP
              IF LN-JDDATE NOT = ZEROS
                 GO TO TEST-LN-FIELDS-EXIT.

           IF JD-ISO-ONLY
              IF LN-JDDATE = ZEROS
                 GO TO TEST-LN-FIELDS-EXIT.

      *BK
           IF BK-ISO-SKIP
              IF LN-BNKRPTDATE NOT = ZEROS
                 GO TO TEST-LN-FIELDS-EXIT.

           IF BK-ISO-ONLY
              IF LN-BNKRPTDATE = ZEROS
                 GO TO TEST-LN-FIELDS-EXIT.

      *ZBAL
           IF ZBAL-ISO-SKIP
              IF LN-CURBAL = 0
                 GO TO TEST-LN-FIELDS-EXIT.

           IF ZBAL-ISO-ONLY
              IF LN-CURBAL NOT = 0
                 GO TO TEST-LN-FIELDS-EXIT.


       TEST-LN-FIELDS-END.
           MOVE "Y" TO CREATE-FORM-REQUEST.

       TEST-LN-FIELDS-EXIT.
           EXIT.

      ***********************************************
       CREATE-FM-REQUEST SECTION.
      ***********************************************

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF NOT IO-GOOD
              MOVE SPACES TO BW-ZIP.

           PERFORM OPEN-FM1-FILE.

           PERFORM CLEAR-FM-FILE.

           MOVE SPACES             TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LN-ACCTNO          TO LPWSFM-EDIT-FM-ACCTNO.
           MOVE FORMNO             TO LPWSFM-EDIT-FM-FORMNO.
           MOVE LN-OWNBR           TO LPWSFM-EDIT-FM-BRNO2.

           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE FORMNO                 TO FM-FMNO.

           MOVE FM1-KEY            TO HOLD-FM1-KEY.
           PERFORM READ-FM1-FILE.

           MOVE BW-ZIP             TO FM-ZIP.
           MOVE SPACES             TO FM-PRINT.
           MOVE 1                  TO FM-STMT-COUNTER.
           MOVE TRANS-DATE         TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.
           IF IO-GOOD
              PERFORM REWRITE-FM1-FILE
           ELSE
              MOVE HOLD-FM1-KEY TO FM1-KEY
              PERFORM WRITE-FM1-FILE
              IF NOT IO-GOOD
                 GO TO IO-ERROR.

           PERFORM CLOSE-FM1-FILE.

       CREATE-FM-REQUEST-EXIT.
           EXIT.

       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPFMCL.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO DC1099-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DC1099-SYSDATE.
           DISPLAY DC1099-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DC1099-FORM-FIELDS TO WR-BUF.
           MOVE DC1099-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE DC1099-HELP--FILE TO HELP-LINK-FILE.
           MOVE DC1099-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/DC1099_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************************
       MISC SECTION.
      **************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".

      **************************************************
       IO-ROUTINE SECTION.
      **************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      **************************************************
       END-PROGRAM SECTION.
      **************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
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
              MOVE "LP/LPEYMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY DC1099-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
