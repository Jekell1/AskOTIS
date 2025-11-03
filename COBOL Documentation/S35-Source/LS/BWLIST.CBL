       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWLIST.
       DATE-WRITTEN. 02-10-84
      *************************************************************
      *                      (LS)
      *    DESCRIPTION:  BORROWER MASTER FILE LIST
      *
      * REV:
      *  JTG 101189 ADDED LOGIC FOR ORIGIN, PRLOANS, WRKHRS
      *  BAH 061391 ADDED BW-EMPLADDR1 BW-EMPLADDR2
      *  JFF 101895 ADDED BORROWER SCAN
      *  KEC 042397 ADDED BW-CREDIT-SCORE.
      *  GLF 092297 ADDED BW-OTHPHONE.
      *  MJD 120497 EXPANDED BW-LOANS TO PIC 99
      *  GLF 030398 DELETED BW-OTHPHONE.
      *  CS  110798 ADDED BW-CREDIT-LIMIT
      *  BAH 010629 ADDED BW-PRIVACY-OPT-OUT FOR TFC PR# 90
      *  BLV 020808 ADDED REF DATE RANGE PER ME, ALSO ADDED COUNT
      *  AMM 070111 RE-DID ENITRE LISTING; CHANGED FORMAT TO BE 4 PAGES
      *  BAH 170425 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 180926 LOCAL TO GLOBAL
      *  SMR 2023.05.15 - CHANGED LPBX1IN TO LPBX1RN
      *  BAH 2024.1121 REPLACED PRINTING BW-CALL-ON-SUNDAY WITH
      *                BW-CEASE-DESIST-TYPE #1686 S35Q-145
      *************************************************************
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
           "@(#)LS/BWLIST.CBL S35 04/22/25-12:19:04 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01BX.CPY".
       COPY "LIBLP/LP01BX_SQL.CPY".
       COPY "LIBLP/LPWSBX.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       01  PR-IFN               PIC X(2)    VALUE "PR".

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
       01  SUB                  PIC 99.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  BORROWER-COUNT       PIC 9(9)     VALUE 0.

       01  ZIP             PIC X(10).
       01  BORROWER-FG     PIC X(3)          VALUE "   ".
           88  BEG-BORROWER                  VALUE "BEG".
           88  END-BORROWER                  VALUE "END".


      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  SEQ             PIC 9.
           03  BEG-SS          PIC 9(9).
           03  END-SS          PIC 9(9).
           03  BEG-NAME        PIC X(16).
           03  END-NAME        PIC X(16).
           03  BEG-REFDATE     PIC 9(8).
           03  END-REFDATE     PIC 9(8).

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
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
           03  FILLER           PIC X(25)    VALUE " ".
           03  FILLER           PIC X(41)    VALUE
           "- - - - - BORROWER FILE LISTING - - - - -".
           03  FILLER           PIC X(45)    VALUE " ".

      ***********************************************
      *    D E T A I L   L I N E   W O R K E R S    *
      ***********************************************
       01  DETAIL-LINE                  PIC X(132) VALUE SPACES.

      *-----------------------------------------------------------------
       01  D-LINE0 REDEFINES DETAIL-LINE.
           03  FILLER                   PIC X(01).
           03  D-LINES                  PIC X(88).

      *-----------------------------------------------------------------
       01  D-LINE1 REDEFINES DETAIL-LINE.
      * COLUMN 1--------------------------------------------------------
           03  FILLER                   PIC X(01).
           03  D-COL-1                  PIC X(15).
           03  FILLER                   PIC X(01).
           03  D-INFO-1                 PIC X(24).
           03  FILLER                   REDEFINES D-INFO-1.
               05  D-STATE              PIC X(02).
           03  FILLER                   REDEFINES D-INFO-1.
               05  D-ZIP                PIC X(10).
               05  FILLER               PIC X(04).
               05  D-ADR-PH             PIC X(08).
               05  FILLER               PIC X(01).
               05  D-ADR-PH-YN          PIC X.
           03  FILLER                   REDEFINES D-INFO-1.
               05  D-MAKER-PHONE        PIC 999/999/9999.
           03  FILLER                   REDEFINES D-INFO-1.
               05  D-DATE               PIC 99/99/99.
           03  FILLER                   REDEFINES D-INFO-1.
               05  D-ORIGIN             PIC X(04).
           03  FILLER                   REDEFINES D-INFO-1.
               05  D-DEPENDENTS         PIC Z9.
           03  FILLER                   REDEFINES D-INFO-1.
               05  D-RENT-OWN           PIC X(04).
           03  FILLER                   REDEFINES D-INFO-1.
               05  D-TIME               PIC X(04).
               05  FILLER               PIC X(02).
               05  D-TEXT               PIC X(13).
               05  FILLER               PIC X(01).
               05  D-DWELL              PIC X(02).
           03  FILLER                   REDEFINES D-INFO-1.
               05  D-LIMIT              PIC ZZZZZ9.99.
           03  FILLER                   REDEFINES D-INFO-1.
               05  D-RATE               PIC X(04).
               05  FILLER               PIC X(16).
               05  D-SCORE              PIC ZZZ9.
           03  FILLER                   REDEFINES D-INFO-1.
               05  D-SMSA               PIC X(06).
           03  FILLER                   REDEFINES D-INFO-1.
               05  D-CENSUS             PIC X(10).
      * COLUMN 2--------------------------------------------------------
           03  FILLER                   PIC X(05).
           03  D-COL-2                  PIC X(14).
           03  FILLER                   PIC X(01).
           03  D-INFO-2                 PIC X(30).
           03  FILLER                   REDEFINES D-INFO-2.
               05  D-BRNO               PIC 9(04).
               05  FILLER               PIC X(20).
               05  D-COUNTY             PIC X(03).
           03  FILLER                   REDEFINES D-INFO-2.
               05  D-CITY-CD            PIC X(03).
           03  FILLER                   REDEFINES D-INFO-2.
               05  D-SLSMN              PIC ZZZ9.
               05  FILLER               PIC X(20).
               05  D-REP                PIC X(04).
           03  FILLER                   REDEFINES D-INFO-2.
               05  D-SOLICIT            PIC X.
               05  FILLER               PIC X(23).
               05  D-CON                PIC X.
           03  FILLER                   REDEFINES D-INFO-2.
               05  D-EMP-PHONE          PIC 999/999/9999.
               05  FILLER               PIC X(11).
               05  D-EXT                PIC ZZZ9.
           03  FILLER                   REDEFINES D-INFO-2.
               05  D-BAD-PHONE          PIC X.
           03  FILLER                   REDEFINES D-INFO-2.
               05  D-YEARS              PIC Z9.
               05  FILLER               PIC X(17).
               05  D-HOURS              PIC X(04).
           03  FILLER                   REDEFINES D-INFO-2.
               05  D-PAY                PIC ZZZ,ZZ9.99.
               05  FILLER               PIC X(10).
               05  D-SRC                PIC X(09).
           03  FILLER                   REDEFINES D-INFO-2.
               05  D-SPSE-SSNO          PIC 999/99/9999.
           03  FILLER                   REDEFINES D-INFO-2.
               05  D-SPSE-BDATE         PIC 99/99/99.
           03  FILLER                   REDEFINES D-INFO-2.
               05  D-OPT-OUT            PIC X.
               05  FILLER               PIC X(07).
               05  D-TEXT-2             PIC X(16).
               05  FILLER               PIC X(01).
               05  D-CALL               PIC X.

      *-----------------------------------------------------------------
       01  D-LINE2 REDEFINES DETAIL-LINE.
      * COLUMN 3--------------------------------------------------------
           03  FILLER                   PIC X(01).
           03  D-COL-3                  PIC X(24).
           03  FILLER                   PIC X(01).
           03  D-INFO-3                 PIC X(14).
           03  FILLER                   REDEFINES D-INFO-3.
               05  D-LOANS              PIC Z9.
           03  FILLER                   REDEFINES D-INFO-3.
               05  D-SOURCE             PIC X(02).
           03  FILLER                   REDEFINES D-INFO-3.
               05  D-INT-LIABILITY      PIC Z,ZZZ,ZZ9.99.
           03  FILLER                   REDEFINES D-INFO-3.
               05  D-REAL-EST           PIC ZZZ,ZZ9.99.
           03  FILLER                   REDEFINES D-INFO-3.
               05  D-PAYOFF             PIC 99/99/99.
      * COLUMN 4--------------------------------------------------------
           03  FILLER                   PIC X(06).
           03  D-COL-4                  PIC X(24).
           03  FILLER                   PIC X(01).
           03  D-INFO-4                 PIC X(08).
           03  FILLER                   REDEFINES D-INFO-4.
               05  D-RN-FOR-10          PIC 99/99/99.
           03  FILLER                   REDEFINES D-INFO-4.
               05  D-LAST-FM            PIC ZZ9.
           03  FILLER                   REDEFINES D-INFO-4.
               05  D-LAST-DATE          PIC 99/99/99.
           03  FILLER                   REDEFINES D-INFO-4.
               05  D-2-SCORE            PIC ZZZ9.

      *-----------------------------------------------------------------
       01  D-LINE3 REDEFINES DETAIL-LINE.
           03  FILLER                   PIC X(01).
           03  D-COL-5                  PIC X(20).
           03  FILLER                   PIC X(05).
           03  D-PHONE-TXT-1            PIC X(07).
           03  FILLER                   PIC X(07).
           03  D-EXT-TXT-1              PIC X(04).
           03  FILLER                   PIC X(03).
           03  D-CODE-TXT               PIC X(04).
           03  FILLER                   PIC X(03).
           03  D-DESCRIPTION-TXT-1      PIC X(11).
           03  FILLER                   PIC X(07).
           03  D-BAD-TXT                PIC X(03).

      *-----------------------------------------------------------------
       01  D-LINE4 REDEFINES DETAIL-LINE.
           03  FILLER                   PIC X(26).
           03  D-P3-PHONE-1             PIC 999/999/9999.
           03  FILLER                   PIC X(02).
           03  D-P3-EXT-1               PIC ZZZ9.
           03  FILLER                   PIC X(04).
           03  D-P3-CODE                PIC X(02).
           03  FILLER                   PIC X(04).
           03  D-P3-DESCRIPTION         PIC X(14).
           03  FILLER                   PIC X(05).
           03  D-P3-BAD                 PIC X.

      *-----------------------------------------------------------------
       01  D-LINE5 REDEFINES DETAIL-LINE.
           03  FILLER                   PIC X(02).
           03  D-COL-6                  PIC X(22).
           03  FILLER                   REDEFINES D-COL-6.
               05  D-COL-6-PROMPT       PIC Z9.
               05  D-COL-6-PERIOD       PIC X.
           03  FILLER                   PIC X(02).
           03  D-DESCRIPTION-TXT-2      PIC X(19).
           03  FILLER                   PIC X(12).
           03  D-PHONE-TXT-2            PIC X(07).
           03  FILLER                   PIC X(08).
           03  D-EXT-TXT-2              PIC X(04).

      *-----------------------------------------------------------------
       01  D-LINE6 REDEFINES DETAIL-LINE.
           03  FILLER                   PIC X(07).
           03  D-P3-INFO-1              PIC X(46).
           03  FILLER                   REDEFINES D-P3-INFO-1.
               05  FILLER               PIC X(08).
               05  D-P3-INFO-2          PIC X(38).
           03  FILLER                   PIC X(05).
           03  D-P3-PHONE-2             PIC 999/999/9999.
           03  FILLER                   PIC X(03).
           03  D-P3-EXT-2               PIC ZZZ9.

      *-----------------------------------------------------------------
       01  D-LINE7 REDEFINES DETAIL-LINE.
      * COLUMN 1 PAGE 04------------------------------------------------
           03  FILLER                   PIC X(01).
           03  D-COL-7                  PIC X(19).
           03  FILLER                   PIC X(01).
           03  D-P4-INFO                PIC X(24).
           03  FILLER                   REDEFINES D-P4-INFO.
               05  D-P4-STATE           PIC X(02).
           03  FILLER                   REDEFINES D-P4-INFO.
               05  D-P4-ZIP             PIC X(10).
      * COLUMN 2 PAGE 04------------------------------------------------
           03  FILLER                   PIC X(04).
           03  D-COL-8                  PIC X(23).
           03  FILLER                   PIC X(01).
           03  D-P4-KYC-DATE            PIC 99/99/99.

      *-----------------------------------------------------------------
       01  D-LINE7 REDEFINES DETAIL-LINE.
           03  FILLER                   PIC X(01).
           03  D-P4-KYC-TEXT            PIC X(23).

      *********************************************
      *    R A N G E   L I N E   W O R K E R S    *
      *********************************************
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(21).
           03  RANGE-SELECTION  PIC X(16).
           03  FILLER REDEFINES RANGE-SELECTION.
               05  RANGE-DATE   PIC 9999/99/99.
               05  FILLER       PIC X(6).

       COPY "LIBWI/BWSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01  BWLIST-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/BWLIST_DEF.CPY".
       COPY "LIBLS/BWLIST_WKS.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/BWLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BX1-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BWLIST"  TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS BWLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO END-PROGRAM.                                                

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

      *- - - - - - - - - - - - - - - - - - - - -{ ENABLE TIMER WINDOW }
           MOVE "R" TO TIMER-TYPE.
           PERFORM TIMER-START.
           MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.

       NEXT-BW.
           IF SEQ = 1
              PERFORM READ-BW1-FILE-NEXT
              IF IO-FG = 0
                 IF (BW-SSNO > END-SS) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
                     GO TO END-ROUTINE.

           IF SEQ = 2
              PERFORM READ-BW2-FILE-NEXT
              IF IO-FG = 0
                 IF (BW-LNAME > END-NAME) OR 
                                        (BW-OWNBR NOT = BW-PATH-OWNBR)
                     GO TO END-ROUTINE.

           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF BEG-REFDATE = 0
              IF (BW-REFDATE = 0)
                 GO TO SKIP-REFDATE-TEST
              ELSE
                 GO TO TEST-ENDING-REFDATE.

           IF (BW-REFDATE = 0)
              GO TO NEXT-BW.

           IF BW-REFDATE < BEG-REFDATE
              GO TO NEXT-BW.

       TEST-ENDING-REFDATE.
           IF BW-REFDATE > END-REFDATE
              GO TO NEXT-BW.

       SKIP-REFDATE-TEST.
           MOVE BW-SSNO  TO BX-SSNO.
           PERFORM READ-BX1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO BX-REC
              MOVE 0 TO BX-KYC-ISSDATE
                        BX-KYC-EXPDATE
                        BX-KYC-LAST-VERDATE.

           PERFORM DETAIL-LINE-PROCESS.

           ADD 1 TO BORROWER-COUNT.

           GO TO NEXT-BW.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS
              MOVE SPACES TO PR-REC
              MOVE 99 TO LN-FEED
              PERFORM WRITE-PR-REC.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
       SETRP.
           MOVE " " TO ERRCD.
           MOVE BWLIST-QUEUE-POS TO Q-POS.
           MOVE BWLIST-COPIES-POS TO C-POS.
           MOVE BWLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.
       RRSEQ.
           MOVE "ENNN010SEQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RRSEQ.
           MOVE VDUNUM0 TO SEQ.
           IF SEQ = 1 GO TO SS-01.
           IF SEQ NOT = 2 GO TO RRSEQ.
           GO TO NM-01.
       SS-01.
           MOVE "ENNN090SS1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-SS.
           IF VDUSTATUS = "1U" GO TO RRSEQ.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO BORROWER-FG
              PERFORM BORROWER-SCAN
              IF IO-FG NOT = 0
                 GO TO SS-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SS-01.
           MOVE VDUNUM0 TO BEG-SS.
       SS-02.
           MOVE "ENNN090SS2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO SS-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1<"
              MOVE "END" TO BORROWER-FG
              PERFORM BORROWER-SCAN
              IF IO-FG NOT = 0
                 GO TO SS-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SS-02.
           MOVE VDUNUM0 TO END-SS.
           IF BEG-SS > END-SS
              MOVE " INVALID BORROWER NUMBER RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO SS-01.
           GO TO REFDATE-01.
       ALL-SS.
           MOVE "S  A090SS1." TO VDU.
           MOVE 0 TO BEG-SS VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A090SS2." TO VDU.
           MOVE 999999999 TO END-SS VDUBUF.
           PERFORM SCREENIO.
           GO TO REFDATE-01.
       NM-01.
           MOVE "E  A160NM1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-NM.
           IF VDUSTATUS = "1U" GO TO SS-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO NM-01.
           MOVE VDUBUF TO BEG-NAME.
           IF BEG-NAME = "                " GO TO NM-01.
       NM-02.
           MOVE "E  A160NM2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO NM-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO NM-02.
           MOVE VDUBUF TO END-NAME.
           IF END-NAME = "                " GO TO NM-02.
           IF BEG-NAME > END-NAME
              MOVE " INVALID NAME RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO NM-01.
           GO TO REFDATE-01.
       ALL-NM.
           MOVE "S  A160NM1." TO VDU.
           MOVE SPACE TO BEG-NAME VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A160NM2." TO VDU.
           MOVE "ZZZZZZZZZZZZZZZZ" TO END-NAME VDUBUF.
           PERFORM SCREENIO.
      *---------------------------------------------------------------
       REFDATE-01.
           MOVE "ERNM060REFDATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO RRSEQ.
           IF VDUSTATUS = "16" GO TO ALL-DATES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REFDATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-REFDATE.
      *-----------------------------------------------------------
       REFDATE-02.
           MOVE "ERNM060REFDATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO REFDATE-01.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO REFDATE-02.

           MOVE VDU-DATE-YYYYMMDD TO END-REFDATE.
           GO TO OK-YN.
      *----------------------------------------------------------
       ALL-DATES.

           MOVE "S  8080REFDATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-REFDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080REFDATE2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-REFDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------------

       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           IF VDUBUF = "N" GO TO RRSEQ.
           IF VDUBUF NOT = "Y" GO TO OK-YN.

           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR
                                 QBW1-WBEG-OWNBR
                                 QBW1-WEND-OWNBR
                                 QBW2-WBEG-OWNBR
                                 QBW2-WEND-OWNBR.

           IF SEQ = 1
              MOVE BEG-SS TO BW-SSNO
                             QBW1-WBEG-SSNO
              MOVE END-SS TO QBW1-WEND-SSNO
              PERFORM START-BW1-FILE.

           IF SEQ = 2
              MOVE BEG-NAME TO BW-LNAME
                               QBW2-WBEG-LNAME
              MOVE END-NAME TO QBW2-WEND-LNAME
              MOVE SPACES   TO BW-FNAME
                               QBW2-WBEG-FNAME
              MOVE ALL "Z"  TO QBW2-WEND-FNAME
              PERFORM START-BW2-FILE.

      D    STOP MESS.
           PERFORM OPEN-BX1-FILE.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "LISTING IN PROGRESS...................." TO MESS.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ***************************************
       DETAIL-LINE-PROCESS SECTION.
      ***************************************

           ADD 6 LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
               PERFORM HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *=================================================================
      * PAGE 1
      *=================================================================
           MOVE ALL "-" TO D-LINES.
           PERFORM WRITE-DETAIL-LINE.

           STRING "{PAGE 01}", " ", BW-LNAME, ", ", BW-FNAME, " ",
                   BW-SSNO DELIMITED BY "   " INTO D-LINES.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO D-LINES.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 1. LAST NAME :"              TO D-COL-1.
           MOVE BW-LNAME                       TO D-INFO-1.
           MOVE "19. BRANCH   :"               TO D-COL-2.
           MOVE BW-ORGBR                       TO D-BRNO.
           MOVE " 20. COUNTY CD:"              TO D-TEXT-2.
           MOVE BW-COUNTY                      TO D-COUNTY.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 2. FIRST     :"              TO D-COL-1.
           MOVE BW-FNAME                       TO D-INFO-1.
           MOVE "21. CITY   CD:"               TO D-COL-2.
           MOVE BW-CITY-TAX-CD                 TO D-CITY-CD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 3. ADDRESS #1:"              TO D-COL-1.
           MOVE BW-ADR1                        TO D-INFO-1.
           MOVE "22. ORG SLSMN:"               TO D-COL-2.
           MOVE BW-ORGSLSMN                    TO D-SLSMN.
           MOVE " 23. REP CODE :"              TO D-TEXT-2.
           MOVE BW-REPCODE                     TO D-REP.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 4. ADDRESS #2:"              TO D-COL-1.
           MOVE BW-ADR2                        TO D-INFO-1.
           MOVE "24. SOLICIT  :"               TO D-COL-2.
           MOVE BW-SOLICIT                     TO D-SOLICIT.
           MOVE " 25. CONFIDENT:"              TO D-TEXT-2.
           MOVE BW-CONFIDENTIAL                TO D-CON.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 5. CITY      :"              TO D-COL-1.
           MOVE BW-CITY                        TO D-INFO-1.
           MOVE "26. EMPLOYER :"               TO D-COL-2.
           MOVE BW-EMPLOYER                    TO D-INFO-2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 6. STATE     :"              TO D-COL-1.
           MOVE BW-STATE                       TO D-STATE.
           MOVE "27. EMP ADDR1:"               TO D-COL-2.
           MOVE BW-EMPADDR1                    TO D-INFO-2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 7. ZIP CODE  :"              TO D-COL-1.
           MOVE BW-ZIP                         TO D-ZIP.
           MOVE "ADR BAD:"                     TO D-ADR-PH.
           MOVE BW-BAD-ADR-FG                  TO D-ADR-PH-YN.
           MOVE "28. EMP ADDR1:"               TO D-COL-2.
           MOVE BW-EMPADDR2                    TO D-INFO-2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 8. PHONE NO. :"              TO D-COL-1.
           IF ( BW-PHONE NOT = 0 )
              MOVE BW-PHONE                    TO D-MAKER-PHONE
              INSPECT D-MAKER-PHONE REPLACING ALL "/" BY "-".
           MOVE " PH BAD:"                     TO D-ADR-PH.
           MOVE BW-BAD-PHONE-FG                TO D-ADR-PH-YN.
           MOVE "29. POSITION :"               TO D-COL-2.
           MOVE BW-EMPLPOS                     TO D-INFO-2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "    CHANGE DT :"              TO D-COL-1.
           MOVE BW-ADR-PHONE-CHG-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D-DATE.
           MOVE "30. EMP PHONE:"               TO D-COL-2.
           IF ( BW-EMPPHONE NOT = 0 )
              MOVE BW-EMPPHONE                 TO D-EMP-PHONE
              INSPECT D-EMP-PHONE REPLACING ALL "/" BY "-".
           IF BW-EMPEXT > 0
              MOVE "            EXT"           TO D-TEXT-2
              MOVE BW-EMPEXT                   TO D-EXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 9. REFER DATE:"              TO D-COL-1.
           MOVE BW-REFDATE                     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D-DATE.
           MOVE "    BAD PHONE:"               TO D-COL-2.
           MOVE BW-BAD-EMPPHONE-FG             TO D-BAD-PHONE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "10. BIRTHDATE :"              TO D-COL-1.
           MOVE BW-BDATE                       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                    TO D-DATE.
           MOVE "31. YRS EMPLD:"               TO D-COL-2.
           MOVE BW-EMPYEARS                    TO D-YEARS.
           MOVE " WORK HRS:"                   TO D-TEXT-2.
           MOVE BW-WORKHRS                     TO D-HOURS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "11. ORIGIN    :"              TO D-COL-1.
           MOVE BW-ORIGIN                      TO D-ORIGIN.
           MOVE "32. NET PY-MO:"               TO D-COL-2.
           MOVE BW-EMPPAY                      TO D-PAY.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "12. DEPENDENTS:"              TO D-COL-1.
           MOVE BW-DEPEND                      TO D-DEPENDENTS.
           MOVE "33. OTH PY-MO:"               TO D-COL-2.
           MOVE BW-EMPPAYOT                    TO D-PAY.
           MOVE "        SRC"                  TO D-TEXT-2.
           MOVE BW-EMPPAYSR                    TO D-SRC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "13. RENT/OWN  :"              TO D-COL-1.
           MOVE BW-RENTCD                      TO D-RENT-OWN.
           MOVE "34. SPSE LAST:"               TO D-COL-2.
           MOVE BW-SPLNAME                     TO D-INFO-2.

           PERFORM WRITE-DETAIL-LINE.

           MOVE "14. TIME/ADDR :"              TO D-COL-1.
           MOVE BW-TIMEADDR                    TO D-TIME.
           MOVE "NO. DWELL   :"                TO D-TEXT.
           MOVE BW-DWELLCD                     TO D-DWELL.
           MOVE "35. ''  FIRST:"               TO D-COL-2.
           MOVE BW-SPFNAME                     TO D-INFO-2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "15. CRED LIMIT:"              TO D-COL-1.
           MOVE BW-CREDIT-LIMIT                TO D-LIMIT.
           MOVE "36. ''  SSNO :"               TO D-COL-2.
           IF ( BW-SPSSNO > 0 )
              MOVE BW-SPSSNO                   TO D-SPSE-SSNO
              INSPECT D-SPSE-SSNO REPLACING ALL "/" BY "-".
           PERFORM WRITE-DETAIL-LINE.

           MOVE "16. CREDIT RTE:"              TO D-COL-1.
           MOVE BW-CREDITCD                    TO D-RATE.
           MOVE "CREDIT SCORE:"                TO D-TEXT.
           MOVE BW-CREDIT-SCORE                TO D-SCORE.
           MOVE "37. ''  AGE/B:"               TO D-COL-2.
           IF ( BW-SP-BDATE NOT = 0)
              MOVE BW-SP-BDATE                 TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D-SPSE-BDATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "17. S.M.S.A.  :"              TO D-COL-1.
           MOVE BW-STRTGCD                     TO D-SMSA.
           MOVE "38. OPT OUT  :"               TO D-COL-2.
           MOVE BW-PRIVACY-OPT-OUT             TO D-OPT-OUT.
           MOVE "39. CEASE & D :"              TO D-TEXT-2.
           MOVE BW-CEASE-DESIST-TYPE           TO D-CALL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "18. CENSUS TR :"              TO D-COL-1.
           MOVE BW-CENSUSCD                    TO D-CENSUS.
           PERFORM WRITE-DETAIL-LINE.

      *================================================================
      * PAGE 02
      *================================================================
           MOVE ALL "-" TO D-LINES.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           STRING "{PAGE 02}", " ", BW-LNAME, ", ", BW-FNAME, " ",
                   BW-SSNO DELIMITED BY "   " INTO D-LINES.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO D-LINES.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "11. PRIOR RN FOR < 10% :"     TO D-COL-4.
           IF (BW-PRIOR-RN-FOR10-DATE NOT = 0)
              MOVE BW-PRIOR-RN-FOR10-DATE      TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D-RN-FOR-10.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 1. NO. OF PRIOR LOANS :"     TO D-COL-3.
           MOVE BW-PRLOANS                     TO D-LOANS.
           MOVE "12. LAST  RN FOR < 10% :"     TO D-COL-4.
           IF (BW-LAST-RN-FOR10-DATE NOT = 0)
              MOVE BW-LAST-RN-FOR10-DATE       TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D-RN-FOR-10.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 2. NO. OF OPEN LOANS  :"     TO D-COL-3.
           MOVE BW-LOANS                       TO D-LOANS.
           MOVE "13. LAST FORM NO. SENT :"     TO D-COL-4.
           MOVE BW-LAST-FORM-NO                TO D-LAST-FM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "14. LAST FORM DATE     :"     TO D-COL-4.
           IF (BW-LAST-FORM-DATE NOT = 0)
              MOVE BW-LAST-FORM-DATE           TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D-LAST-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 3. SOURCE CODE        :"     TO D-COL-3.
           MOVE BW-SRCD                        TO D-SOURCE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "15. CREDIT SCORE    Q1 :"     TO D-COL-4.
           MOVE BW-CRED-SCORE(1)             TO D-2-SCORE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 4. INTEREST PAID YTD  :"     TO D-COL-3.
           MOVE BW-INTYTD                      TO D-INT-LIABILITY.
           MOVE "16.                 Q2 :"     TO D-COL-4.
           MOVE BW-CRED-SCORE(2)             TO D-2-SCORE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 5. INTEREST PAID LYR  :"     TO D-COL-3.
           MOVE BW-INTLYR                      TO D-INT-LIABILITY.
           MOVE "17.                 Q3 :"     TO D-COL-4.
           MOVE BW-CRED-SCORE(3)             TO D-2-SCORE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 6. REAL ESTATE INT YTD:"     TO D-COL-3.
           MOVE BW-RE-INTYTD                   TO D-REAL-EST.
           MOVE "18.                 Q4 :"     TO D-COL-4.
           MOVE BW-CRED-SCORE(4)             TO D-2-SCORE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 7. REAL ESTATE INT LYR:"     TO D-COL-3.
           MOVE BW-RE-INTLYR                   TO D-REAL-EST.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "19. BNKRPT SCORE    Q1 :"     TO D-COL-4.
           MOVE BW-BNKRPT-SCORE(1)             TO D-2-SCORE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 8. SMALL LN LIABILITY :"     TO D-COL-3.
           MOVE BW-LIASMLN                     TO D-INT-LIABILITY.
           MOVE "20.                 Q2 :"     TO D-COL-4.
           MOVE BW-BNKRPT-SCORE(2)             TO D-2-SCORE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 9. OTHER LN LIABILITY :"     TO D-COL-3.
           MOVE BW-LIAOTH                      TO D-INT-LIABILITY.
           MOVE "21.                 Q3 :"     TO D-COL-4.
           MOVE BW-BNKRPT-SCORE(3)             TO D-2-SCORE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "22.                 Q4 :"     TO D-COL-4.
           MOVE BW-BNKRPT-SCORE(4)             TO D-2-SCORE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "10. LAST PAYOFF DATE   :"     TO D-COL-3.
           IF (BW-LASTPODATE NOT = 0)
              MOVE BW-LASTPODATE               TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D-PAYOFF.
           PERFORM WRITE-DETAIL-LINE.

      *================================================================
      * PAGE 03
      *================================================================
           MOVE 99 TO LN-COUNT.

           ADD 6 LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
               PERFORM HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO D-LINES.
           PERFORM WRITE-DETAIL-LINE.

           STRING "{PAGE 03}", " ", BW-LNAME, ", ", BW-FNAME, " ",
                   BW-SSNO DELIMITED BY "   " INTO D-LINES.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO D-LINES.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MISC. PHONE NUMBERS:"         TO D-COL-5.
           MOVE "PHONE #"                      TO D-PHONE-TXT-1.
           MOVE "EXT."                         TO D-EXT-TXT-1.
           MOVE "CODE"                         TO D-CODE-TXT.
           MOVE "DESCRIPTION"                  TO D-DESCRIPTION-TXT-1.
           MOVE "BAD"                          TO D-BAD-TXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 1. MISC PHONE #1:"           TO D-COL-6.
           IF ( BW-MISC-PHONE(1) NOT = ZEROES )
              MOVE BW-MISC-PHONE(1)            TO D-P3-PHONE-1
              INSPECT D-P3-PHONE-1 REPLACING ALL "/" BY "-"
              MOVE BW-MISC-EXT(1)              TO D-P3-EXT-1.
           MOVE "PH"                           TO CD1-KEY
           MOVE BW-MISC-PHONE-CD(1)            TO D-P3-CODE CD-CODE.
           PERFORM GET-CD.
           MOVE CD-DESC                        TO D-P3-DESCRIPTION.
           MOVE BW-BAD-MISC-PHONE1-FG          TO D-P3-BAD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 2. MISC PHONE #2:"           TO D-COL-6.
           IF ( BW-MISC-PHONE(2) NOT = ZEROES )
              MOVE BW-MISC-PHONE(2)            TO D-P3-PHONE-1
              INSPECT D-P3-PHONE-1 REPLACING ALL "/" BY "-"
              MOVE BW-MISC-EXT(2)              TO D-P3-EXT-1.
           MOVE "PH"                           TO CD1-KEY
           MOVE BW-MISC-PHONE-CD(2)            TO D-P3-CODE CD-CODE.
           PERFORM GET-CD.
           MOVE CD-DESC                        TO D-P3-DESCRIPTION.
           MOVE BW-BAD-MISC-PHONE2-FG          TO D-P3-BAD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 3. MISC PHONE #3:"           TO D-COL-6.
           IF ( BW-MISC-PHONE(3) NOT = ZEROES )
              MOVE BW-MISC-PHONE(3)            TO D-P3-PHONE-1
              INSPECT D-P3-PHONE-1 REPLACING ALL "/" BY "-"
              MOVE BW-MISC-EXT(3)              TO D-P3-EXT-1.
           MOVE "PH"                           TO CD1-KEY
           MOVE BW-MISC-PHONE-CD(3)            TO D-P3-CODE CD-CODE.
           PERFORM GET-CD.
           MOVE CD-DESC                        TO D-P3-DESCRIPTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 4. MISC PHONE #4:"           TO D-COL-6.
           IF ( BW-MISC-PHONE(4) NOT = ZEROES )
              MOVE BW-MISC-PHONE(4)            TO D-P3-PHONE-1
              INSPECT D-P3-PHONE-1 REPLACING ALL "/" BY "-"
              MOVE BW-MISC-EXT(4)              TO D-P3-EXT-1.
           MOVE "PH"                           TO CD1-KEY
           MOVE BW-MISC-PHONE-CD(4)            TO D-P3-CODE CD-CODE.
           PERFORM GET-CD.
           MOVE CD-DESC                        TO D-P3-DESCRIPTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 5. MISC PHONE #5:"           TO D-COL-6.
           IF ( BW-MISC-PHONE(5) NOT = ZEROES )
              MOVE BW-MISC-PHONE(5)            TO D-P3-PHONE-1
              INSPECT D-P3-PHONE-1 REPLACING ALL "/" BY "-"
              MOVE BW-MISC-EXT(5)              TO D-P3-EXT-1.
           MOVE "PH"                           TO CD1-KEY
           MOVE BW-MISC-PHONE-CD(5)            TO D-P3-CODE CD-CODE.
           PERFORM GET-CD.
           MOVE CD-DESC                        TO D-P3-DESCRIPTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 6. MISC PHONE #6:"           TO D-COL-6.
           IF ( BW-MISC-PHONE(6) NOT = ZEROES )
              MOVE BW-MISC-PHONE(6)            TO D-P3-PHONE-1
              INSPECT D-P3-PHONE-1 REPLACING ALL "/" BY "-"
              MOVE BW-MISC-EXT(6)              TO D-P3-EXT-1.
           MOVE "PH"                           TO CD1-KEY
           MOVE BW-MISC-PHONE-CD(6)            TO D-P3-CODE CD-CODE.
           PERFORM GET-CD.
           MOVE CD-DESC                        TO D-P3-DESCRIPTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CONTACT PHONE NUMBERS:"       TO D-COL-6.
           MOVE "CONTACT DESCRIPTION:"         TO D-DESCRIPTION-TXT-2.
           MOVE "PHONE #"                      TO D-PHONE-TXT-2.
           MOVE "EXT."                         TO D-EXT-TXT-2.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              COMPUTE D-COL-6-PROMPT = SUB + 6
              MOVE "." TO D-COL-6-PERIOD
              IF BW-CONTACT-DESC(SUB) NOT = SPACES
                 MOVE BW-CONTACT-DESC(SUB) TO D-P3-INFO-1
                 IF BW-CONTACT-PHONE(SUB) NOT = 0
                    MOVE BW-CONTACT-PHONE(SUB) TO D-P3-PHONE-2
                    INSPECT D-P3-PHONE-2 REPLACING ALL "/" BY "-"
                 END-IF
                 MOVE BW-CONTACT-EXT(SUB) TO D-P3-EXT-2
              END-IF
              PERFORM WRITE-DETAIL-LINE
           END-PERFORM.

           MOVE "17.  E-MAIL:"                 TO D-COL-6.
           MOVE BW-EMAIL                       TO D-P3-INFO-2.
           PERFORM WRITE-DETAIL-LINE.

      *================================================================
      * PAGE 04
      *================================================================
           MOVE ALL "-" TO D-LINES.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           STRING "{PAGE 04}", " ", BW-LNAME, ", ", BW-FNAME, " ",
                   BW-SSNO DELIMITED BY "   " INTO D-LINES.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO D-LINES.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALTERNATE ADDRESS:"           TO D-COL-7.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 1. ALT ADDRESS #1:"          TO D-COL-7.
           MOVE BX-ALT-ADR1                    TO D-P4-INFO.
           MOVE "14. KYC DATE OF ISSUE :"      TO D-COL-8.
           IF BX-KYC-ISSDATE NOT = 0
              MOVE BX-KYC-ISSDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY    TO D-P4-KYC-DATE.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 2. ALT ADDRESS #2:"          TO D-COL-7.
           MOVE BX-ALT-ADR2                    TO D-P4-INFO.
           MOVE "15. KYC DATE OF EXPIRE:"      TO D-COL-8.
           IF BX-KYC-EXPDATE NOT = 0
              MOVE BX-KYC-EXPDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY    TO D-P4-KYC-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 3. ALT CITY      :"          TO D-COL-7.
           MOVE BX-ALT-CITY                    TO D-P4-INFO.
           MOVE "16. KYC DTE LST VERIFY:"      TO D-COL-8.
           IF BX-KYC-LAST-VERDATE NOT = 0
              MOVE BX-KYC-LAST-VERDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY         TO D-P4-KYC-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 4. ALT STATE     :"          TO D-COL-7.
           MOVE BX-ALT-STATE                   TO D-P4-STATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 5. ALT ZIP CODE  :"          TO D-COL-7.
           MOVE BX-ALT-ZIP                     TO D-P4-ZIP.
           PERFORM WRITE-DETAIL-LINE.

      * KNOW YOUR CUSTOMER INFO 

           MOVE "KNOW YOUR CUSTOMER INFO"      TO D-P4-KYC-TEXT.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 6. KYC ADDRESS #1:"          TO D-COL-7.
           MOVE BX-KYC-ADR1                    TO D-P4-INFO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 7. KYC ADDRESS #2:"          TO D-COL-7.
           MOVE BX-KYC-ADR2                    TO D-P4-INFO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 8. KYC CITY      :"          TO D-COL-7.
           MOVE BX-KYC-CITY                    TO D-P4-INFO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 9. KYC STATE     :"          TO D-COL-7.
           MOVE BX-KYC-STATE                   TO D-P4-STATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "10. KYC ZIP CODE  :"          TO D-COL-7.
           MOVE BX-KYC-ZIP                     TO D-P4-ZIP.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "11. KYC ID TYPE   :"          TO D-COL-7.
           MOVE BX-KYC-IDTYPE                  TO D-P4-STATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "12. KYC ISS AUTH  :"          TO D-COL-7.
           MOVE BX-KYC-ISSAUTH                 TO D-P4-STATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "13. KYC NUMBER    :"          TO D-COL-7.
           MOVE BX-KYC-NUMBER                  TO D-P4-INFO.
           PERFORM WRITE-DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
           MOVE "N" TO FIRST-TIME.
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

           MOVE ALL "-" TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 9 TO LN-COUNT.
           MOVE 2 TO LN-FEED.


      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 99 TO LN-COUNT.

           ADD 6 LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
               PERFORM HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL # BORROWERS  :" TO RANGE-LINE.
           MOVE BORROWER-COUNT TO RANGE-SELECTION.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           GO TO END-RANGE.

           IF SEQ = 2
               GO TO NAME-RANGE.
           MOVE "BEGINNING BORROWER :" TO RANGE-LINE.
           MOVE BEG-SS TO RANGE-SELECTION.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING BORROWER    :" TO RANGE-LINE.
           MOVE END-SS TO RANGE-SELECTION.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           GO TO END-RANGE.

       NAME-RANGE.
           MOVE "BEGINNING NAME     :" TO RANGE-LINE.
           MOVE BEG-NAME TO RANGE-SELECTION.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING NAME        :" TO RANGE-LINE.
           MOVE END-NAME TO RANGE-SELECTION.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       END-RANGE.
           MOVE "BEGINNING REFDATE  :" TO RANGE-LINE.
           MOVE BEG-REFDATE TO RANGE-DATE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING REFDATE     :" TO RANGE-LINE.
           MOVE END-REFDATE TO RANGE-DATE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


      ******************************
       BORROWER-SCAN SECTION.
      ******************************
           MOVE 0   TO BWSCAN-NO.
           MOVE "S" TO BWSCAN-SCAN-TYPE.
           MOVE "WI/BWSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWSCAN-STATUS NOT = "00" OR BWSCAN-NO = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-BORROWER-SCAN.
           IF ( BORROWER-FG = "BEG" )
               MOVE "SNNN090SS1." TO VDU
           ELSE
               MOVE "SNNN090SS2." TO VDU.
           MOVE BWSCAN-NO TO BW-SSNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE BW-SSNO TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-BORROWER-SCAN.
           EXIT.

      ******************************
       FORM-RESET SECTION.
      ******************************
           MOVE EXT-CONAME-CONAME  TO BWLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWLIST-SYSDATE.
           DISPLAY BWLIST-SCREEN UPON BWLIST-WINDOW-HANDLE.
           MOVE BWLIST-FORM-FIELDS TO WR-BUF.
           MOVE BWLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************
       SETUP-LINK-INFO SECTION.
      ******************************
           MOVE BWLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWLIST-HELP--FILE TO HELP-LINK-DIR.

      ******************************
       VDU-CONVERT-FIELD SECTION.
      ******************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/BWLIST_EVA.CPY".
              WHEN OTHER MOVE "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ************************************************
       MISC-ROUTINES SECTION.
      ************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO CD-DESC.

      ************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
      ************************************************
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBX1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ************************************************
       END-PROGRAM SECTION.
      ************************************************
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "LP/LPL2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWLIST-SCREEN.
           DESTROY BWLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
