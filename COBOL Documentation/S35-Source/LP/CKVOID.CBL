       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CKVOID.
       DATE-WRITTEN.    3/18/2024.
      ***************************************************************
      *
      *  DESCRIPTION:  VOIDED CLEARED CHECKS
      *
      *                TO MAKE EOM CHECK REC MORE EFFICIENT, NEEDED
      *                THIS REPORT TO LIST CHECKS VOIDED ON OR AFTER
      *                THE CLEARED DATE.
      *  REV:
      *   BAH 2024.0318 NEW #1650
      *   RMZ 2024.0318 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *  
      ***************************************************************
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/CKVOID S35 03/19/24-05:14:05 >".

       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

      ***********************************
      *   LINE WORKERS / SSP WORKERS    *
      ***********************************

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS1-WBEG-CKDATE     PIC X(10).                   
       01  QWS1-WEND-CKDATE     PIC X(10).                
       01  QWS1-WBEG-CDATE      PIC X(10).                 
       01  QWS1-WEND-CDATE      PIC X(10).               
       01  QWS1-WBEG-VDATE      PIC X(10).                 
       01  QWS1-WEND-VDATE      PIC X(10).               
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  CK1-CONN-OPEN-SW     PIC X        VALUE "N".

       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  GRAND-TOTAL          PIC S9(9)V99 COMP-3 VALUE 0.
       01  GRAND-COUNT          PIC 9(9).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH        PIC 9(4).
           03  END-BRANCH        PIC 9(4).
           03  BEG-CKDATE        PIC 9(8).
           03  END-CKDATE        PIC 9(8).
           03  BEG-CLEAR-DATE    PIC 9(8).
           03  END-CLEAR-DATE    PIC 9(8).
           03  BEG-VOID-DATE     PIC 9(8).
           03  END-VOID-DATE     PIC 9(8).
           03  GROUP-FLAG        PIC X.
           03  ENT-GROUP         PIC X(4).

       01 VERSION-CONTROL      PIC X VALUE "A".

       01  SV-BEG-BR                          PIC 9(4)     VALUE 9999.
       01  SV-END-BR                          PIC 9(4)     VALUE 9999.
       01  SV-GROUP-FLAG                      PIC X(1)     VALUE SPACES.
       01  SV-ENT-GROUP                       PIC X(4)     VALUE SPACES.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(5)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(11)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(10)    VALUE " ".
           03  H-TITLE          PIC X(28)    VALUE
           "VOIDED CLEARED CHECKS REPORT".

       01  HEAD3.
           03  FILLER      PIC X(41) VALUE "BRNO  CHECK#  PAYEE".
           03  FILLER      PIC X(31) VALUE "AMOUNT    CKDATE     CLEAR".
           03  FILLER      PIC X(4)  VALUE "VOID".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO          PIC ZZZ9.
           03  FILLER          PIC XX.
           03  D-CKNO          PIC ZZZZZ9.
           03  FILLER          PIC XX.
           03  D-PAYEE         PIC X(20).
           03  FILLER          PIC XX.
           03  D-CKAMT         PIC ZZZZ,ZZZ.99-.
           03  FILLER          PIC XX.
           03  D-CKDATE        PIC 99/99/99.
           03  FILLER          PIC XX.
           03  D-CLEAR-DATE    PIC 99/99/99.
           03  FILLER          PIC XX.
           03  D-VOID-DATE     PIC 99/99/99.

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER          PIC X(32).
           03  D-TOTAL-COUNT   PIC Z(9).
           03  FILLER          PIC X(5).
           03  D-TOTAL-AMT     PIC Z,ZZZ,ZZZ.99-.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC 9(10).
               04  R-DATE REDEFINES R-NUM PIC 9999/99/99.


       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CKVOID_DEF.CPY".
       COPY "LIBLP/CKVOID_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/CKVOID_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-CK1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CKVOID" TO FORM-PROG VDU-FORM-NAME.

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

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "REPORT IN PROGRESS...............", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO CK-PATH-OVERRIDE.

           PERFORM OPEN-CK1-FILE.

           MOVE BR-NO     TO CK-BRNO
                             QCK1-WBEG-BRNO
                             QCK1-WEND-BRNO.
           MOVE 0         TO CK-NO
                             QCK1-WBEG-CKNO.
           MOVE ALL "9"   TO QCK1-WEND-CKNO.

           PERFORM START-CK1-FILE. *> SSP - CKFILE

       NEXT-CK.
           PERFORM READ-CK1-FILE-NEXT. *> EMBEDDED - CKFILE
           IF IO-FG = 9 OR (CK-BRNO NOT = CK-PATH-OWNBR)
              GO TO END-OF-GROUP.

      * SKIP IF NOT CLEARED AND NOT VOIDED
           IF CK-CLEARED-DATE = 0 OR CK-VOID-DATE = 0
              GO TO NEXT-CK.

      * TEST ALL 3 DATE RANGES
           IF CK-DATE < BEG-CKDATE OR CK-DATE > END-CKDATE
              GO TO NEXT-CK.

           IF CK-CLEARED-DATE < BEG-CLEAR-DATE OR
                  CK-CLEARED-DATE > END-CLEAR-DATE
              GO TO NEXT-CK.

           IF CK-VOID-DATE < BEG-VOID-DATE OR
                  CK-VOID-DATE > END-VOID-DATE
              GO TO NEXT-CK.

      * ONLY INCLUDE CHECKS THAT WERE VOIDED ON OR AFTER CLEARED DATE
           IF CK-VOID-DATE < CK-CLEARED-DATE
              GO TO NEXT-CK.

      * VOIDED ON OR AFTER CLEARED DATE

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-CK.

       END-OF-GROUP.
           PERFORM CLOSE-CK1-FILE.
            
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           MOVE "GRAND TOTAL       :" TO DETAIL-LINE.
           MOVE GRAND-TOTAL TO D-TOTAL-AMT.
           MOVE GRAND-COUNT TO D-TOTAL-COUNT.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE CKVOID-QUEUE-POS TO Q-POS.
           MOVE CKVOID-COPIES-POS TO C-POS.
           MOVE CKVOID-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE " " TO EXT-EOBUF-ERRCD.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS..............." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

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

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           MOVE 0 TO GRAND-TOTAL GRAND-COUNT.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-CKDATE BEG-CLEAR-DATE
                                   BEG-VOID-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-CKDATE END-CLEAR-DATE
                                   END-VOID-DATE.

      ******************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO BR-01.

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

       DT1.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.
           MOVE "ERNM080BEGDT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO DT1.
           MOVE VDU-DATE-YYYYMMDD TO BEG-CKDATE.
       DT2.
           MOVE "ERNM080ENDDT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO DT1.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO DT2.
           MOVE VDU-DATE-YYYYMMDD TO END-CKDATE.

           IF END-CKDATE < BEG-CKDATE
              MOVE "INVALID DATE RANGE !!! " TO MESS
              PERFORM SEND-MESS
              GO TO DT1.
           GO TO BEG-CLR.
       ALL-DT.
           MOVE "S  8080BEGDT." TO VDU.
           MOVE EXT-JULIAN-BEG  TO BEG-CKDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080ENDDT." TO VDU.
           MOVE EXT-JULIAN-END  TO END-CKDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
       BEG-CLR.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.
           MOVE "ERNM080CLRDT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-CLEAR.
           IF VDUSTATUS = "1U" GO TO DT2.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO BEG-CLR.
           MOVE VDU-DATE-YYYYMMDD TO BEG-CLEAR-DATE.
       END-CLR.
           MOVE "ERNM080CLRDT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BEG-CLR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO END-CLR.
           MOVE VDU-DATE-YYYYMMDD TO END-CLEAR-DATE.

           IF END-CLEAR-DATE < BEG-CLEAR-DATE
              MOVE "INVALID DATE RANGE !!! " TO MESS
              PERFORM SEND-MESS
              GO TO BEG-CLR.

           GO TO BEG-VOID.

       ALL-CLEAR.
           MOVE "S  8080CLRDT1." TO VDU.
           MOVE EXT-JULIAN-BEG   TO BEG-CLEAR-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080CLRDT2." TO VDU.
           MOVE EXT-JULIAN-END   TO END-CLEAR-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       BEG-VOID.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.
           MOVE "ERNM080VOID1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-VOID.
           IF VDUSTATUS = "1U" GO TO END-CLR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO BEG-VOID.
           MOVE VDU-DATE-YYYYMMDD TO BEG-VOID-DATE.
       END-VOID.
           MOVE "ERNM080VOID2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BEG-VOID.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO END-VOID
           MOVE VDU-DATE-YYYYMMDD TO END-VOID-DATE.

           IF END-VOID-DATE < BEG-VOID-DATE
              MOVE "INVALID DATE RANGE !!! " TO MESS
              PERFORM SEND-MESS
              GO TO BEG-VOID.
           GO TO EXIT-PARM.
       ALL-VOID.
           MOVE "S  8080VOID1." TO VDU.
           MOVE EXT-JULIAN-BEG  TO BEG-VOID-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080VOID2." TO VDU.
           MOVE EXT-JULIAN-END  TO END-VOID-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
       EXIT-PARM.
           EXIT.

      *************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************

           MOVE "SNNN040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE END-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

           MOVE BR-NO            TO D-BRNO.
           MOVE CK-NO            TO D-CKNO.
           MOVE CK-PAYEE         TO D-PAYEE.
           MOVE CK-AMT           TO D-CKAMT.
           MOVE CK-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-CKDATE.
           MOVE CK-CLEARED-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-CLEAR-DATE.
           MOVE CK-VOID-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-VOID-DATE.
           PERFORM WRITE-DETAIL-LINE.

           ADD CK-AMT TO GRAND-TOTAL.
           ADD 1      TO GRAND-COUNT.

       END-DETAIL-LN-PROCESS.
           EXIT.

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

      *******************************
       HEAD-PAGE SECTION.
      *******************************
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
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.


       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE 4 TO LN-FEED.
           MOVE "BEGINNING BRANCH" TO RANGE-LINE.
           MOVE BEG-BRANCH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING BRANCH" TO RANGE-LINE.
           MOVE END-BRANCH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CHECK DATE" TO RANGE-LINE.
           MOVE BEG-CKDATE TO R-DATE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    CHECK DATE" TO RANGE-LINE.
           MOVE END-CKDATE TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CLEAR DATE" TO RANGE-LINE.
           MOVE BEG-CLEAR-DATE TO R-DATE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    CLEAR DATE" TO RANGE-LINE.
           MOVE END-CLEAR-DATE TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING VOID DATE" TO RANGE-LINE.
           MOVE BEG-VOID-DATE TO R-DATE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    VOID DATE" TO RANGE-LINE.
           MOVE END-VOID-DATE TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO CKVOID-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CKVOID-SYSDATE.
           DISPLAY CKVOID-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CKVOID-FORM-FIELDS TO WR-BUF.
           MOVE CKVOID-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE CKVOID-HELP--FILE TO HELP-LINK-FILE.
           MOVE CKVOID-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CKVOID_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************************
       MISC-ROUTINES SECTION.
      *********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".

      *********************************************************
       IO-ROUTINE SECTION.
      *********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
   
      * COPY "LIBLP/LPCK1RN.CPY".
      * COPYMEMBER: LIBLP/LPCK1RN
       LOAD-CK1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CK-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CK-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CK-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CK-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CK-ALL-DATA
              MOVE CK-ALL-PATH             TO CK-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CK-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CK-ALL-BASE
              MOVE FILPATH-MACHINE         TO CK-ALL-MACHINE
              MOVE FILPATH-DATA            TO CK-ALL-DATA
              MOVE CK-ALL-PATH             TO CK-PATH.

      *-----------------------------------------------------------------
       OPEN-CK1-FILE.
           PERFORM LOAD-CK1-FILE.
           PERFORM OPEN-IT.
           MOVE CK-PATH-SQL TO E-FILE.

           IF CK1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO CK1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS CK1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.


      *-----------------------------------------------------------------
       START-CK1-FILE. *> SSP - CKFILE
           PERFORM START-IT.
           MOVE CK-PATH-OWNBR  TO CK-BRNO.
           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.

           IF ( CK1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CK1-FILE.

           IF ( QCK1-WEND-BRNO = ZEROES    ) OR
              ( QCK1-WEND-BRNO NOT NUMERIC )
              MOVE CK-PATH-OWNBR        TO QCK1-WBEG-BRNO
              MOVE CK-PATH-OWNBR        TO QCK1-WEND-BRNO
              MOVE CK-NO                TO QCK1-WBEG-CKNO
              MOVE ALL "9"              TO QCK1-WEND-CKNO.

              MOVE BEG-CKDATE           TO SQL-DATE-YYYYMMDD.
              PERFORM SQL-SET-DATE.
              MOVE SQL-DATE-YYYY-MM-DD  TO QWS1-WBEG-CKDATE.
              MOVE END-CKDATE           TO SQL-DATE-YYYYMMDD.
              PERFORM SQL-SET-DATE.
              MOVE SQL-DATE-YYYY-MM-DD  TO QWS1-WEND-CKDATE.

              MOVE BEG-CLEAR-DATE       TO SQL-DATE-YYYYMMDD.
              PERFORM SQL-SET-DATE.
              MOVE SQL-DATE-YYYY-MM-DD  TO QWS1-WBEG-CDATE.
              MOVE END-CLEAR-DATE       TO SQL-DATE-YYYYMMDD.
              PERFORM SQL-SET-DATE.
              MOVE SQL-DATE-YYYY-MM-DD  TO QWS1-WEND-CDATE.

              MOVE BEG-VOID-DATE        TO SQL-DATE-YYYYMMDD.
              PERFORM SQL-SET-DATE.
              MOVE SQL-DATE-YYYY-MM-DD  TO QWS1-WBEG-VDATE.
              MOVE END-VOID-DATE        TO SQL-DATE-YYYYMMDD.
              PERFORM SQL-SET-DATE.
              MOVE SQL-DATE-YYYY-MM-DD  TO QWS1-WEND-VDATE.

      *  STORED PROCEDURE:
      *
      *   SELECT CKFILE.CK_BRNO,
      *          CKFILE.CK_NO,
      *          CAST(CKFILE.CK_DATE         AS VARCHAR(10)),
      *          CKFILE.CK_AMT,
      *          CKFILE.CK_PAYEE,
      *          CAST(CKFILE.CK_VOID_DATE    AS VARCHAR(10)),
      *          CAST(CKFILE.CK_CLEARED_DATE AS VARCHAR(10))
      *
      *    FROM DBO.CKFILE
      *
      *   WHERE (( CKFILE.CK_BRNO               = @QCK1_WBEG_BRNO
      *     AND    CKFILE.CK_NO           BETWEEN @QCK1_WBEG_CKNO
      *                                       AND @QCK1_WEND_CKNO
      *     AND    CKFILE.CK_DATE         BETWEEN @QWS1_WBEG_DATE
      *                                       AND @QWS1_WEND_DATE
      *     AND    CKFILE.CK_CLEARED_DATE BETWEEN @QWS1_WBEG_CDATE
      *                                       AND @QWS1_WEND_CDATE
      *     AND    CKFILE.CK_VOID_DATE    BETWEEN @QWS1_WBEG_VDATE
      *                                       AND @QWS1_WEND_VDATE)
      *     AND  ( CKFILE.CK_CLEARED_DATE      != '1900-01-01' )
      *     AND  ( CKFILE.CK_VOID_DATE         != '1900-01-01' )
      *     AND  ( CKFILE.CK_VOID_DATE >= CKFILE.CK_CLEARED_DATE ))	
      *
      *   ORDER BY CKFILE.CK_BRNO,
      *            CKFILE.CK_NO;

           EXEC SQL SET CONNECTION CK1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE CK1_CURSOR CURSOR FOR
                :CK1-RETURN-CODE = EXEC SSP_CKVOID_CKFILE_SELECT 
              ( :QCK1-WBEG-BRNO,  
                :QCK1-WBEG-CKNO,  
                :QCK1-WEND-CKNO, 
                :QWS1-WBEG-CKDATE, 
                :QWS1-WEND-CKDATE,  
                :QWS1-WBEG-CDATE, 
                :QWS1-WEND-CDATE,
                :QWS1-WBEG-VDATE, 
                :QWS1-WEND-VDATE )
           END-EXEC.

           EXEC SQL
               OPEN CK1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCK1-WBEG-BRNO 
                          QCK1-WEND-BRNO
                          QCK1-WBEG-CKNO
                          QCK1-WEND-CKNO.

           MOVE STAT---GOOD TO CK1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-CK1-FILE-NEXT. *> EMBEDDED - CKFILE
           PERFORM READ-IT.
           MOVE CK-PATH-OWNBR  TO CK-BRNO.
           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.
           INITIALIZE QCK-REC.

           EXEC SQL SET CONNECTION CK1_CONNECT END-EXEC.

           IF ( CK1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH CK1_CURSOR 
                  INTO 
                    :QCK-BRNO,
                    :QCK-NO,
                    :QCK-DATE,        
                    :QCK-AMT,
                    :QCK-PAYEE,
                    :QCK-VOID-DATE,
                    :QCK-CLEARED-DATE
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CK1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CK-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-CK1-FILE.

           PERFORM CLOSE-IT.
           IF ( CK1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION CK1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE CK1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC .
              MOVE STAT---BAD TO CK1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-CK1-CONNECTION.

           IF CK1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION CK1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT CK1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO CK1-CONN-OPEN-SW.


       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *********************************************************
       END-PROGRAM SECTION.
      *********************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
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
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CKVOID-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
