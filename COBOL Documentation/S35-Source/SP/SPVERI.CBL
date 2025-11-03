       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPVERI.
       DATE-WRITTEN. 08-31-21.
      ******************************************************************
      *    DIR :  SP
      *    DESC:  STATE PARAMETER VERIFICATION REPORT
      * WRITTEN BY: AMM
      *=================================================================
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
           "@(#)SP/SPVERI.CBL S35 11/22/23-14:28:12 >".
      * COPY "LIBLP/LP01SP.CPY".
      * COPY "LIBLP/LP01SPA.CPY".
      * COPY "LIBLP/LP01SPB.CPY".
      * COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  RETR-FILE            PIC X.
       01  RETR-ORGST           PIC XX.
       01  RETR-SPRCLASS        PIC 999.
       01  RETR-SUBCLASS        PIC 99.
       01  RETR-LAWCODE         PIC 99.
       01  RETR-SPA             PIC X.
       01  RETR-SPB             PIC X.
       01  RETR-SPC             PIC X.
       EXEC SQL END DECLARE SECTION END-EXEC.

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED              PIC 99       VALUE 0.
       01  LN-COUNT             PIC 999      VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  TOTAL-SPA-UNMATCH    PIC 9(5)     VALUE 0.
       01  TOTAL-SPB-UNMATCH    PIC 9(5)     VALUE 0.
       01  TOTAL-SPC-UNMATCH    PIC 9(5)     VALUE 0.
       01  RECORDS-FOUND-SW     PIC X    VALUE "N".
          88  NO-RECORDS-FOUND           VALUE "N".
          88  RECORDS-FOUND              VALUE "Y".

       01  CLASS-FG             PIC X(3) VALUE "   ".
           88  CLASS-BEG                 VALUE "BEG".
           88  CLASS-END                 VALUE "END".
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD                   PIC X.
           03  BEG-SP1-KEY.
               05  BEG-SP-ORGST        PIC XX.
               05  BEG-SP-SPRCLASS     PIC 999.
               05  BEG-SP-SUBCLASS     PIC 99.
               05  BEG-SP-LAWCODE      PIC 99.
           03  END-SP1-KEY.
               05  END-SP-ORGST        PIC XX.
               05  END-SP-SPRCLASS     PIC 999.
               05  END-SP-SUBCLASS     PIC 99.
               05  END-SP-LAWCODE      PIC 99.

       01  ACCEPT-BUF          PIC X.

      ******************************************************************
      *
      *    H E A D E R - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-01.
           03  H01-DATE           PIC X(08) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H01-HOUR           PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(01) VALUE ":".
           03  H01-MINUTE         PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(30) VALUE SPACES.
           03  H01-NAME           PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(10) VALUE SPACES.
           03  FILLER             PIC X(09) VALUE "PAGE NO: ".
           03  H01-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER             PIC X(10) VALUE "USER ID:".
           03  H02-USERID         PIC X(10) VALUE SPACES.
           03  FILLER             PIC X(15) VALUE SPACES.
           03  H02-TITLE          PIC X(60) VALUE SPACES.
           03  FILLER             PIC X(20) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-04.
           03  FILLER PIC X(30) VALUE "ORGFILE   ORGST  SPRCLASS  SUB".
           03  FILLER PIC X(30) VALUE "CLASS  LAWCODE  MISSING FILE".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      ******************************************************************
      *    D E T A I L - L I N E
      ******************************************************************
       01  DETAIL-LINE                   PIC X(132) VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01 REDEFINES DETAIL-LINE.
           03  FILLER                 PIC X(03).
           03  D-SP-ORGFILE           PIC X.
           03  FILLER                 PIC X(07).
           03  D-SP-ORGST             PIC XX.
           03  FILLER                 PIC X(06).
           03  D-SP-SPRCLASS          PIC 999.
           03  FILLER                 PIC X(08).
           03  D-SP-SUBCLASS          PIC 99.
           03  FILLER                 PIC X(07).
           03  D-SP-LAWCODE           PIC 99.
           03  FILLER                 PIC X(09).
           03  D-SP-MISSING           PIC X(03).

      *-------------------------------------------------------{ TOTALS }
       01  TOTAL-LINE-01              REDEFINES DETAIL-LINE.
           03  T01-PROMPT             PIC X(19).
           03  FILLER                 PIC X(01).
           03  T01-NUMERIC            PIC Z,ZZZ,ZZZ,ZZ9.

      *-------------------------------------------------------{ RANGES }
       01  RANGE-LINE-01              REDEFINES DETAIL-LINE.
           03  R01-PROMPT             PIC X(22).
           03  FILLER                 PIC X(01).
           03  R01-TEXT               PIC X(50).
           03  R01-NUMERIC            REDEFINES R01-TEXT PIC Z99.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  SPVERI-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/SPVERI_DEF.CPY".
       COPY "LIBSP/SPVERI_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/SPVERI_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SQL-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPVERI" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SPVERI-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE BEG-SP-ORGST      TO QSP1-WBEG-ORGST.
           MOVE END-SP-ORGST      TO QSP1-WEND-ORGST.
           MOVE BEG-SP-SPRCLASS   TO QSP1-WBEG-SPRCLASS.
           MOVE END-SP-SPRCLASS   TO QSP1-WEND-SPRCLASS.
           MOVE BEG-SP-SUBCLASS   TO QSP1-WBEG-SUBCLASS.
           MOVE END-SP-SUBCLASS   TO QSP1-WEND-SUBCLASS.
           MOVE BEG-SP-LAWCODE    TO QSP1-WBEG-LAWCODE.
           MOVE END-SP-LAWCODE    TO QSP1-WEND-LAWCODE.

           PERFORM START-SQL-FILE.   *>  EMBEDDED  SPA SPB SPC

       NEXT-SQL-RETR.
           PERFORM READ-SQL-FILE-NEXT.   *>  EMBEDDED  SPA SPB SPC
           IF IO-FG = 9
               GO TO END-ROUTINE.

           MOVE "Y" TO RECORDS-FOUND-SW.

           PERFORM PRINT-UNMATCH-RECORD-ROUTINE.

           GO TO NEXT-SQL-RETR.

       END-ROUTINE.
           PERFORM PRINT-TOTAL-LINES.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE " " TO ERRCD.
       SETRP.
           MOVE SPVERI-QUEUE-POS TO Q-POS.
           MOVE SPVERI-COPIES-POS TO C-POS.
           MOVE SPVERI-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00" GO TO ENDIT.
           MOVE " " TO ERRCD.
       ST-01.
           MOVE "E  A020ST1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-ST.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ST-01.
           MOVE VDUBUF TO BEG-SP-ORGST.
       ST-02.
           MOVE "E  A020ST2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO ST-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ST-02.
           MOVE VDUBUF TO END-SP-ORGST.
           IF BEG-SP-ORGST > END-SP-ORGST
               MOVE "INVALID STATE CODE RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO ST-01.
           GO TO LC-01.
       ALL-ST.
           MOVE "S  A020ST1." TO VDU.
           MOVE SPACES TO BEG-SP-ORGST VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST2." TO VDU.
           MOVE "ZZ" TO END-SP-ORGST VDUBUF.
           PERFORM SCREENIO.
       LC-01.
           MOVE "ENNN020LC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1U" GO TO ST-02.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO LC-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO LC-01.
           MOVE VDUNUM0 TO BEG-SP-SPRCLASS.
       LC-02.
           MOVE "ENNN020LC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO LC-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO LC-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO LC-02.
           MOVE VDUNUM0 TO END-SP-SPRCLASS.
           IF BEG-SP-SPRCLASS > END-SP-SPRCLASS
               MOVE "INVALID LOAN CLASS RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO LC-01.
           GO TO LSC-01.
       ALL-LC.
           MOVE "S  N020LC1." TO VDU.
           MOVE 0 TO BEG-SP-SPRCLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  N020LC2." TO VDU.
           MOVE 999 TO END-SP-SPRCLASS VDUNUM0.
           PERFORM SCREENIO.
       LSC-01.
           MOVE "ENNN020LSC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-LSC.
           IF VDUSTATUS = "1U" GO TO LC-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO LSC-01.
           MOVE VDUNUM0 TO BEG-SP-SUBCLASS.
       LSC-02.
           MOVE "ENNN020LSC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO LSC-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO LSC-02.
           MOVE VDUNUM0 TO END-SP-SUBCLASS.
           IF BEG-SP-SUBCLASS > END-SP-SUBCLASS
               MOVE "INVALID LOAN SUB-CLASS RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO LSC-01.
           GO TO SP-01.
       ALL-LSC.
           MOVE "S  N020LSC1." TO VDU.
           MOVE 0 TO BEG-SP-SUBCLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  N020LSC2." TO VDU.
           MOVE 99 TO END-SP-SUBCLASS VDUNUM0.
           PERFORM SCREENIO.
       SP-01.
           MOVE "ENNN020SP1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-SP.
           IF VDUSTATUS = "1U" GO TO LSC-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SP-01.
           MOVE VDUNUM0 TO BEG-SP-LAWCODE.
       SP-02.
           MOVE "ENNN020SP2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO SP-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SP-02.
           MOVE VDUNUM0 TO END-SP-LAWCODE.
           IF BEG-SP-LAWCODE > END-SP-LAWCODE
               MOVE "INVALID LAW CODE RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO SP-01.
       ALL-SP.
           MOVE "S  N020SP1." TO VDU.
           MOVE 0 TO BEG-SP-LAWCODE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  N020SP2." TO VDU.
           MOVE 99 TO END-SP-LAWCODE VDUNUM0.
           PERFORM SCREENIO.

       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N" GO TO ST-01.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.

           MOVE EXT-CONAME-CONAME TO H01-NAME.
           MOVE EXT-CONAME-SYSDATE TO H01-DATE.
           MOVE EXT-CONAME-USER-ID TO H02-USERID.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED TO LN-COUNT.

           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM WRITE-PAGE-HEADER-LINES.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       WRITE-PAGE-HEADER-LINES SECTION.
      ******************************************************************

           MOVE 01 TO LN-COUNT.
           ADD  01 TO PAGE-NUMBER.

           PERFORM GET-TIME.

           MOVE EXT-CONAME-SYSDATE               TO H01-DATE.
           MOVE T-HH                             TO H01-HOUR.
           MOVE T-MM                             TO H01-MINUTE.
           MOVE EXT-CONAME-CONAME                TO H01-NAME.
           CALL "C$JUSTIFY"                   USING H01-NAME "C".
           MOVE PAGE-NUMBER                      TO H01-PAGE-NO.

           MOVE HEADER-LINE-01                   TO PR-REC.
           MOVE 99                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE EXT-CONAME-USER-ID               TO H02-USERID.
           MOVE "SP FILE VERIFICATION REPORT"    TO H02-TITLE.
           CALL "C$JUSTIFY"                   USING H02-TITLE "C".

           MOVE HEADER-LINE-02                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-04                   TO PR-REC.
           ADD  02                               TO LN-COUNT.
           MOVE 02                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE ALL "="                          TO PR-REC.
           ADD  01 TO LN-COUNT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 01 TO LN-FEED.

       WRITE-PAGE-HEADER-LN-EXIT.
           EXIT.

      ************************************************************
       CLASS-SCAN SECTION.
      ************************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.

           IF ( CLASS-FG = "BEG" )
               MOVE "SNNN020LC1." TO VDU
           ELSE
               MOVE "SNNN020LC2." TO VDU.

           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.

           MOVE 0 TO IO-FG.
       EXIT-CLASS-SCAN.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO SPVERI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPVERI-SYSDATE.
           DISPLAY SPVERI-SCREEN UPON SPVERI-WINDOW-HANDLE.
           MOVE SPVERI-FORM-FIELDS TO WR-BUF.
           MOVE SPVERI-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE SPVERI-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPVERI-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/SPVERI_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************************
       MISC SECTION.
      ***********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

       PRINT-TOTAL-LINES.
           MOVE 3 TO LN-FEED.

           MOVE "SPA REC UNMATCHED:"   TO T01-PROMPT.
           MOVE TOTAL-SPA-UNMATCH      TO T01-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SPB REC UNMATCHED:"   TO T01-PROMPT.
           MOVE TOTAL-SPB-UNMATCH      TO T01-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SPC REC UNMATCHED:"   TO T01-PROMPT.
           MOVE TOTAL-SPC-UNMATCH      TO T01-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.
 

       PRINT-RANGE-LINES.
           MOVE 3 TO LN-FEED.

           MOVE "BEGINNING STATE CODE :"   TO R01-PROMPT.
           MOVE BEG-SP-ORGST TO R01-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    STATE CODE :"   TO R01-PROMPT.
           MOVE END-SP-ORGST TO R01-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING  SPR CLASS :"   TO R01-PROMPT.
           MOVE BEG-SP-SPRCLASS TO R01-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING     SPR CLASS :"   TO R01-PROMPT.
           MOVE END-SP-SPRCLASS TO R01-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING  SUB CLASS :"   TO R01-PROMPT.
           MOVE BEG-SP-SUBCLASS TO R01-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING     SUB CLASS :"   TO R01-PROMPT.
           MOVE END-SP-SUBCLASS TO R01-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING    LAWCODE :"   TO R01-PROMPT.
           MOVE BEG-SP-LAWCODE TO R01-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING       LAWCODE :"   TO R01-PROMPT.
           MOVE END-SP-LAWCODE TO R01-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-UNMATCH-RECORD-ROUTINE.
           MOVE RETR-FILE       TO D-SP-ORGFILE.
           MOVE RETR-ORGST      TO D-SP-ORGST.
           MOVE RETR-SPRCLASS   TO D-SP-SPRCLASS.
           MOVE RETR-SUBCLASS   TO D-SP-SUBCLASS.
           MOVE RETR-LAWCODE    TO D-SP-LAWCODE.

           IF RETR-FILE = "A"
              ADD 1 TO TOTAL-SPA-UNMATCH
              IF RETR-SPB = "N" AND RETR-SPC = "Y"
                 MOVE "B" TO D-SP-MISSING
              ELSE
                 IF RETR-SPB = "N" AND RETR-SPC = "N"
                    MOVE "B,C" TO D-SP-MISSING
                 ELSE
                    MOVE "C" TO D-SP-MISSING
                 END-IF
              END-IF
           ELSE
              IF RETR-FILE = "B" 
                 ADD 1 TO TOTAL-SPB-UNMATCH
                 IF RETR-SPA = "N" AND RETR-SPC = "Y"
                    MOVE "A" TO D-SP-MISSING
                 ELSE
                    IF RETR-SPA = "N" AND RETR-SPC = "N"
                       MOVE "A,C" TO D-SP-MISSING
                    ELSE
                       MOVE "C" TO D-SP-MISSING
                    END-IF
                 END-IF
      * RETR-FILE = "C"
             ELSE  
                 ADD 1 TO TOTAL-SPC-UNMATCH
                 IF RETR-SPA = "N" AND RETR-SPB = "Y"
                    MOVE "A" TO D-SP-MISSING
                 ELSE
                    IF RETR-SPA = "N" AND RETR-SPB = "N"
                       MOVE "A,B" TO D-SP-MISSING
                    ELSE
                       MOVE "B" TO D-SP-MISSING.

           MOVE 1 TO LN-FEED.

           PERFORM WRITE-DETAIL-LINE.

      *****************************
       IO-ROUTINE SECTION.
      *****************************
       COPY "LIBGB/CONNECT_SQL.CPY".

      *-----------------------------------------------------------------
       START-SQL-FILE.   *>  EMBEDDED  SPA SPB SPC
           PERFORM START-IT.
           MOVE "SPVERI CSR"  TO E-FILE.
           MOVE SPACES        TO E-KEYX.

           IF ( SPA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SQL-FILE.

           EXEC SQL
            DECLARE SPA1_CURSOR CURSOR FOR
             SELECT 'A'
                   ,SPAFILE.SPA_ORGST
                   ,SPAFILE.SPA_SPRCLASS
                   ,SPAFILE.SPA_SUBCLASS
                   ,SPAFILE.SPA_LAWCODE
                   ,'Y'
                   ,CASE WHEN SPBFILE.SPB_ORGST IS NULL 
                          THEN 'N' ELSE 'Y' END
                   ,CASE WHEN SPCFILE.SPC_ORGST IS NULL 
                          THEN 'N' ELSE 'Y' END
           
             FROM DBO.SPAFILE
           
             LEFT OUTER JOIN DBO.SPBFILE
               ON SPBFILE.SPB_ORGST    = SPAFILE.SPA_ORGST
              AND SPBFILE.SPB_SPRCLASS = SPAFILE.SPA_SPRCLASS
              AND SPBFILE.SPB_SUBCLASS = SPAFILE.SPA_SUBCLASS
              AND SPBFILE.SPB_LAWCODE  = SPAFILE.SPA_LAWCODE

             LEFT OUTER JOIN DBO.SPCFILE
               ON SPCFILE.SPC_ORGST    = SPAFILE.SPA_ORGST
              AND SPCFILE.SPC_SPRCLASS = SPAFILE.SPA_SPRCLASS
              AND SPCFILE.SPC_SUBCLASS = SPAFILE.SPA_SUBCLASS
              AND SPCFILE.SPC_LAWCODE  = SPAFILE.SPA_LAWCODE

             WHERE SPAFILE.SPA_ORGST    BETWEEN :QSP1-WBEG-ORGST
                                         AND :QSP1-WEND-ORGST 
               AND SPAFILE.SPA_SPRCLASS BETWEEN :QSP1-WBEG-SPRCLASS 
                                         AND :QSP1-WEND-SPRCLASS
               AND SPAFILE.SPA_SUBCLASS BETWEEN :QSP1-WBEG-SUBCLASS
                                         AND :QSP1-WEND-SUBCLASS
               AND SPAFILE.SPA_LAWCODE  BETWEEN :QSP1-WBEG-LAWCODE 
                                         AND :QSP1-WEND-LAWCODE 
               AND (SPBFILE.SPB_ORGST IS NULL 
                OR SPCFILE.SPC_ORGST IS NULL)

             UNION ALL

             SELECT 'B'
                   ,SPBFILE.SPB_ORGST
                   ,SPBFILE.SPB_SPRCLASS
                   ,SPBFILE.SPB_SUBCLASS
                   ,SPBFILE.SPB_LAWCODE
                   ,CASE WHEN SPAFILE.SPA_ORGST IS NULL
                          THEN 'N' ELSE 'Y' END
                   ,'Y'
                   ,CASE WHEN SPCFILE.SPC_ORGST IS NULL
                          THEN 'N' ELSE 'Y' END

             FROM DBO.SPBFILE

             LEFT OUTER JOIN DBO.SPAFILE
               ON SPAFILE.SPA_ORGST    = SPBFILE.SPB_ORGST
              AND SPAFILE.SPA_SPRCLASS = SPBFILE.SPB_SPRCLASS
              AND SPAFILE.SPA_SUBCLASS = SPBFILE.SPB_SUBCLASS
              AND SPAFILE.SPA_LAWCODE  = SPBFILE.SPB_LAWCODE

             LEFT OUTER JOIN DBO.SPCFILE
               ON SPCFILE.SPC_ORGST    = SPBFILE.SPB_ORGST
              AND SPCFILE.SPC_SPRCLASS = SPBFILE.SPB_SPRCLASS
              AND SPCFILE.SPC_SUBCLASS = SPBFILE.SPB_SUBCLASS
              AND SPCFILE.SPC_LAWCODE  = SPBFILE.SPB_LAWCODE

            WHERE SPBFILE.SPB_ORGST    BETWEEN :QSP1-WBEG-ORGST
                                        AND :QSP1-WEND-ORGST
              AND SPBFILE.SPB_SPRCLASS BETWEEN :QSP1-WBEG-SPRCLASS
                                        AND :QSP1-WEND-SPRCLASS
              AND SPBFILE.SPB_SUBCLASS BETWEEN :QSP1-WBEG-SUBCLASS
                                        AND :QSP1-WEND-SUBCLASS
              AND SPBFILE.SPB_LAWCODE  BETWEEN :QSP1-WBEG-LAWCODE
                                        AND :QSP1-WEND-LAWCODE 
              AND (SPAFILE.SPA_ORGST IS NULL
               OR SPCFILE.SPC_ORGST IS NULL)

             UNION ALL

             SELECT 'C'
                   ,SPCFILE.SPC_ORGST
                   ,SPCFILE.SPC_SPRCLASS
                   ,SPCFILE.SPC_SUBCLASS
                   ,SPCFILE.SPC_LAWCODE
                   ,CASE WHEN SPAFILE.SPA_ORGST IS NULL 
                          THEN 'N' ELSE 'Y' END
                   ,CASE WHEN SPBFILE.SPB_ORGST IS NULL 
                          THEN 'N' ELSE 'Y' END
                   ,'Y'

             FROM DBO.SPCFILE

             LEFT OUTER JOIN DBO.SPAFILE
               ON SPAFILE.SPA_ORGST    = SPCFILE.SPC_ORGST
              AND SPAFILE.SPA_SPRCLASS = SPCFILE.SPC_SPRCLASS
              AND SPAFILE.SPA_SUBCLASS = SPCFILE.SPC_SUBCLASS
              AND SPAFILE.SPA_LAWCODE  = SPCFILE.SPC_LAWCODE

             LEFT OUTER JOIN DBO.SPBFILE
               ON SPBFILE.SPB_ORGST    = SPCFILE.SPC_ORGST
              AND SPBFILE.SPB_SPRCLASS = SPCFILE.SPC_SPRCLASS
              AND SPBFILE.SPB_SUBCLASS = SPCFILE.SPC_SUBCLASS
              AND SPBFILE.SPB_LAWCODE  = SPCFILE.SPC_LAWCODE

            WHERE SPCFILE.SPC_ORGST   BETWEEN  :QSP1-WBEG-ORGST
                                        AND :QSP1-WEND-ORGST 
              AND SPCFILE.SPC_SPRCLASS BETWEEN :QSP1-WBEG-SPRCLASS
                                        AND :QSP1-WEND-SPRCLASS
              AND SPCFILE.SPC_SUBCLASS BETWEEN :QSP1-WBEG-SUBCLASS 
                                        AND :QSP1-WEND-SUBCLASS
              AND SPCFILE.SPC_LAWCODE  BETWEEN :QSP1-WBEG-LAWCODE
                                        AND :QSP1-WEND-LAWCODE 
              AND (SPAFILE.SPA_ORGST IS NULL 
               OR SPBFILE.SPB_ORGST IS NULL)

             ORDER BY 1,2,3,4,5

           END-EXEC.

           EXEC SQL
               OPEN SPA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO SPA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SQL-FILE-NEXT.   *>  EMBEDDED  SPA SPB SPC
           PERFORM READ-IT.
           MOVE "SPVERI CSR"       TO E-FILE.
           MOVE SPACES             TO E-KEYX.

           IF ( SPA1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH SPA1_CURSOR INTO 
                   :RETR-FILE
                  ,:RETR-ORGST
                  ,:RETR-SPRCLASS
                  ,:RETR-SUBCLASS
                  ,:RETR-LAWCODE
                  ,:RETR-SPA
                  ,:RETR-SPB
                  ,:RETR-SPC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SQL-FILE-NEXT.

      *-----------------------------------------------------------------
       CLOSE-SQL-FILE.
           IF ( SPA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SPA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SPA1-CURSOR-STAT.

      *=================================================================
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***********************************************************
       END-PROGRAM SECTION.
      ***********************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE!" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           MOVE "LP/LPL2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY SPVERI-WINDOW-HANDLE.
           EXIT PROGRAM.
