       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ASLNE2.
       DATE-WRITTEN. 112888
      ******************************************************************
      *
      *    DIR :  LP
      *    DESC:  ADVERTISING/SOLICITATION LOAN SELECTION
      *                         (PAGE2) CALLED FROM ASLNEX
      *           INPUT OF BORROWER SELECTION CRITERIA
      *
      *=================================================================
      * REV:
      * 02/06/18 FIXED SCREEN DISPLAY
      * BLV 020917 ALLOW 0 IN MAXLIA, SO YOU CAN SELECT 0 TO
      *            SEE BORROWERS WITH NO OPEN LOANS, PL# 379
      * BLV 020923 ADDED 2 FIELDS TO SCREEN-LISTS, MANY-ACCOUNTS
      *            AND SORT-BY-ZIP; IF YOU DON'T KEEP EOBUF IN
      *            SYNC WITH ASLNEX, THEN BORR-STAT GETS MESSED UP!
      * BAH 040318 ADDED BEG AND END CREDIT SCORE, PACE PR# 2263
      * BAH 040419 ADDED ORIGIN FOR CASHCO, PR# 2273
      *
      * KEC 2018.0426 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF ENT-GROUP AND/OR GROUP-FLAG.
      *         THIS WAS DONE IN RELATION TO THE EOBUF EXTERNAL ISSUE
      *         WHICH IF NOT BEING SET WILL CAUSE ISSUES FROM THOSE
      *         BYTE POSITIONS COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THOSE BYTES.
      *
      ******************************************************************
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
           "@(#)LP/ASLNE2.CBL A30 04/26/18-14:19:32 >".

       01  AGE-YR-HOLD          PIC 999.
       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  LOAN-FIELDS.
               05  BEG-BRANCH      PIC 9(4).
               05  END-BRANCH      PIC 9(4).
               05  BEG-CLASS       PIC 99.
               05  END-CLASS       PIC 99.
               05  BEG-DEALER      PIC 9(4).99.
               05  END-DEALER      PIC 9(4).99.
               05  BEG-LDATE       PIC 99/99/99.
               05  END-LDATE       PIC 99/99/99.
               05  ACTION-CODE     PIC X(4).
               05  MAXDCNTR1       PIC 99.
               05  MAXDCNTR2       PIC 99.
               05  MINPAYS-MADE    PIC 999.
               05  MINPAYS-REMAIN  PIC 999.
               05  XMON            PIC 99.
               05  HCON            PIC 999.
               05  HREC            PIC 999.
               05  MINCR           PIC 9(6).
               05  MAXCR           PIC 9(6).
               05  MINAV           PIC 9(6).
               05  PERAVL          PIC 999.
               05  MLFORM          PIC 99.
               05  BEGLFORM        PIC 999.
               05  ENDLFORM        PIC 999.
               05  OWNED           PIC X.
               05  ZBAL            PIC X.
               05  MINPOMON        PIC 999.
           03  LOAN-FIELDS2.
               05  MAXPOMON        PIC 999.
               05  SCD-TABLE.
                   07  SCD         OCCURS 9 PIC XX.
               05  PCD-TABLE.
                   07  PCD         OCCURS 9 PIC XXX.
               05  SECURITY-CODE   PIC X.
               05  MANY-ACCOUNTS   PIC X.
               05  BWOPT           PIC X.
               05  FORMNO          PIC 999.
               05  REPT            PIC X.
               05  BEG-NXDUE-DT    PIC 99/99/99.
               05  END-NXDUE-DT    PIC 99/99/99.
               05  CORP-FM-FG      PIC X.

      ****************************************
      * PASSED BORROWER SELECTION FIELDS
      * BORR-STAT:
      *       " " - RANGES NEVER ENTERED
      *       "I" - RANGES ENTERED
      ****************************************
           03  BORR-STAT           PIC X.
           03  BORR-FIELDS.
               05  STATE           PIC XX.
               05  BEGZIP          PIC X(10).
               05  ENDZIP          PIC X(10).
               05  COUNTY          PIC XXX.
               05  DWELL           PIC XX.
               05  CENSUS          PIC X(10).
               05  SMSA            PIC X(6).
               05  RENT            PIC X(4).
               05  YRADDR          PIC 99.
               05  MINAGE          PIC 99.
               05  MAXAGE          PIC 99.
               05  MINEMPL         PIC 99.
               05  MINPAY          PIC 9(5).
               05  MINTPAY         PIC 9(5).
               05  MINLIA          PIC 9(6).
               05  MAXLIA          PIC 9(6).
               05  MPO             PIC 99.
               05  BEGSLSMN        PIC 9999.
               05  ENDSLSMN        PIC 9999.
               05  BEGREP          PIC X(4).
               05  ENDREP          PIC X(4).
               05  MARRIED         PIC X.
               05  BEG-CRSCORE     PIC 9(3).
               05  END-CRSCORE     PIC 9(3).
               05  ORIGINX.
                   07  ORIGIN-CD   PIC X OCCURS 4.
           03  SORT-BY-ZIP         PIC X.

       01  SCREEN-BUF          PIC X(106) VALUE SPACES.
       01  SCREEN-LIST.
           03  FILLER          PIC X(53) VALUE
           "STATE ZIP1 ZIP2 COUNTY DWELL CENSUS SMSA RENT YRADDR ".
           03  FILLER          PIC X(51) VALUE
           "MINAGE MAXAGE MINEMPL MINPAY MINTPAY MINLIA MAXLIA ".
           03  FILLER          PIC X(51) VALUE
           "MPO BEGSLSMN ENDSLSMN BEGREP ENDREP MARRIED SCORE1 ".
           03  FILLER          PIC X(14) VALUE
           "SCORE2 ORIGIN.".

       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  ASLNE2-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/ASLNE2_DEF.CPY".
       COPY "LIBLP/ASLNE2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/ASLNE2_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *************************************
       MAIN-MODULE SECTION.
      *************************************

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ASLNE2" TO VDU-FORM-NAME.

      * NEEDS TO BE A WINDOW SO IT CAN RETURN BACK TO ASLNEX

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   AT SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS ASLNE2-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.

           GO TO END-PROGRAM.

      *****************************************************
      *             PROGRAM INITIALIZATION                *
      *****************************************************

       INITIALIZATION SECTION.

           MOVE BORR-FIELDS TO SCREEN-BUF.
           IF BORR-STAT NOT = " "
              MOVE SCREEN-BUF TO WR-BUF
              MOVE SCREEN-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

       SETRP.
           PERFORM ENTER-PARAMETERS.
           IF VDUSTATUS = "10"
              MOVE SPACES TO WR-BUF
              MOVE SCREEN-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE " " TO BORR-STAT
              GO TO SETRP.

           IF VDUSTATUS = "1>"
              GO TO EXIT-INITIALIZE.

           MOVE "I" TO BORR-STAT.

       EXIT-INITIALIZE.
           EXIT.

      /********************************************
      *    ENTER BORROWER SELECTION FIELDS
      *********************************************
       ENTER-PARAMETERS SECTION.
       REC-STATE.
           MOVE "ENTER STATE CODE:   (?-WILD)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020STATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-STATE.
           MOVE VDUBUF TO STATE.

       REC-BEGZIP.
           MOVE "ENTER BEGINNING ZIP CODE, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A100ZIP1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-STATE.
           IF VDUSTATUS = "16" GO TO ALL-ZIP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-BEGZIP.
           MOVE VDUBUF TO BEGZIP.
       REC-ENDZIP.
           MOVE "ENTER ENDING ZIP CODE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A100ZIP2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEGZIP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-ENDZIP.
           MOVE VDUBUF TO ENDZIP.
           IF ENDZIP = SPACES GO TO REC-ENDZIP.
           IF BEGZIP > ENDZIP
              GO TO REC-BEGZIP.
           GO TO END-ZIP.

       ALL-ZIP.
           MOVE "S  A100ZIP1." TO VDU.
           MOVE SPACES TO BEGZIP VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A100ZIP2." TO VDU.
           MOVE "ZZZZZZZZZZ" TO ENDZIP VDUBUF.
           PERFORM SCREENIO.
       END-ZIP.


       REC-COUNTY.
           MOVE "ENTER COUNTY CODE:    (?-WILD)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A030COUNTY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEGZIP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-COUNTY.
           MOVE VDUBUF TO COUNTY.

       REC-DWELL.
           MOVE "ENTER DWELLING CODE:    (?-WILD)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020DWELL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-COUNTY.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-DWELL.
           MOVE VDUBUF TO DWELL.

       REC-CENSUS.
           MOVE "ENTER CENSUS CODE:    (?-WILD)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A100CENSUS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-DWELL.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-CENSUS.
           MOVE VDUBUF TO CENSUS.

       REC-SMSA.
           MOVE "ENTER SMSA CODE:    (?-WILD)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A060SMSA." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-CENSUS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SMSA.
           MOVE VDUBUF TO SMSA.

       REC-RENT.
           MOVE "ENTER RENT CODE:    (?-WILD)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A040RENT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SMSA.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-RENT.
           MOVE VDUBUF TO RENT.

       REC-YRADDR.
           MOVE "ENTER MINIMUM YEARS AT ADDRESS:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020YRADDR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-RENT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-YRADDR.
           MOVE VDUNUM0 TO YRADDR.

       REC-MINAGE.
           MOVE "ENTER BEGINNING BORROWER AGE, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020MINAGE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-YRADDR.
           IF VDUSTATUS = "16" GO TO ALL-AGE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINAGE.
           MOVE VDUNUM0 TO MINAGE.
       REC-MAXAGE.
           MOVE "ENTER ENDING BORROWER AGE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020MAXAGE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINAGE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MAXAGE.
           MOVE VDUNUM0 TO MAXAGE.
           IF MAXAGE = ZERO GO TO REC-MAXAGE.
           IF MINAGE > MAXAGE
              GO TO REC-MINAGE.
           GO TO END-AGE.

       ALL-AGE.
           MOVE "SNNN020MINAGE." TO VDU.
           MOVE ZERO TO MINAGE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020MAXAGE." TO VDU.
           MOVE 99 TO MAXAGE VDUNUM0.
           PERFORM SCREENIO.
       END-AGE.

       REC-MINEMPL.
           MOVE "ENTER MINIMUM YEARS EMPLOYED:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020MINEMPL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINAGE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINEMPL.
           MOVE VDUNUM0 TO MINEMPL.

       REC-MINPAY.
           MOVE "ENTER MINIMUM PAY PER MONTH:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN050MINPAY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINEMPL.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINPAY.
           MOVE VDUNUM0 TO MINPAY.

       REC-MINTPAY.
           MOVE "ENTER MINIMUM TOTAL PAY PER MONTH:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN050MINTPAY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINPAY.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINTPAY.
           MOVE VDUNUM0 TO MINTPAY.

       REC-MINLIA.
           MOVE "ENTER MINIMUM TOTAL LIABILITY, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060MINLIA." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINTPAY.
           IF VDUSTATUS = "16" GO TO ALL-LIA.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINLIA.
           MOVE VDUNUM0 TO MINLIA.
       REC-MAXLIA.
           MOVE "ENTER MAXIMUM TOTAL LIABILITY:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060MAXLIA." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINLIA.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MAXLIA.
           MOVE VDUNUM0 TO MAXLIA.
           IF MINLIA > MAXLIA
              GO TO REC-MINLIA.
           GO TO END-LIA.

       ALL-LIA.
           MOVE "SNNN060MINLIA." TO VDU.
           MOVE ZERO TO MINLIA VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060MAXLIA." TO VDU.
           MOVE 999999 TO MAXLIA VDUNUM0.
           PERFORM SCREENIO.
       END-LIA.
       REC-BEG-SCORE.
           MOVE "ENTER BEGINNING CREDIT SCORE, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030SCORE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINLIA.
           IF VDUSTATUS = "16" GO TO ALL-SCORES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-BEG-SCORE.
           MOVE VDUNUM0 TO BEG-CRSCORE.
       REC-END-SCORE.
           MOVE "ENTER ENDING CREDIT SCORE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030SCORE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEG-SCORE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-END-SCORE.
           MOVE VDUNUM0 TO END-CRSCORE.
           IF BEG-CRSCORE > END-CRSCORE
              GO TO REC-BEG-SCORE.
           GO TO END-SCORE.

       ALL-SCORES.
           MOVE "SNNN030SCORE1." TO VDU.
           MOVE 0 TO BEG-CRSCORE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN030SCORE2." TO VDU.
           MOVE 999 TO END-CRSCORE VDUNUM0.
           PERFORM SCREENIO.

       END-SCORE.

       REC-MPO.
           MOVE "ENTER MIN MONTHS SINCE PAIDOFF, 99=SKIP:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020MPO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEG-SCORE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MPO.
           MOVE VDUNUM0 TO MPO.

       REC-BEGSLSMN.
           MOVE "ENTER BEGINNING SALESMAN NO, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BEGSLSMN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MPO.
           IF VDUSTATUS = "16" GO TO ALL-SLSMN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-BEGSLSMN.
           MOVE VDUNUM0 TO BEGSLSMN.
       REC-ENDSLSMN.
           MOVE "ENTER ENDING SALESMAN NO:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040ENDSLSMN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEGSLSMN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-ENDSLSMN.
           MOVE VDUNUM0 TO ENDSLSMN.
           IF ENDSLSMN = 0
              GO TO REC-BEGSLSMN.
           IF BEGSLSMN > ENDSLSMN
              GO TO REC-BEGSLSMN.
           GO TO END-SLSMN.

       ALL-SLSMN.
           MOVE "SNNN040BEGSLSMN." TO VDU.
           MOVE 0 TO  BEGSLSMN VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040ENDSLSMN." TO VDU.
           MOVE 9999 TO ENDSLSMN VDUNUM0.
           PERFORM SCREENIO.
       END-SLSMN.

       REC-BEGREP.
           MOVE "ENTER BEGINNING REP CODE, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A040BEGREP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEGSLSMN.
           IF VDUSTATUS = "16" GO TO ALL-REP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-BEGREP.
           MOVE VDUBUF TO BEGREP.
       REC-ENDREP.
           MOVE "ENTER ENDING REP CODE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A040ENDREP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEGREP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-ENDREP.
           MOVE VDUBUF TO ENDREP.
           IF BEGREP > ENDREP
              GO TO REC-BEGREP.
           GO TO END-REP.

       ALL-REP.
           MOVE "S  A040BEGREP." TO VDU.
           MOVE SPACES TO BEGREP VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A040ENDREP." TO VDU.
           MOVE "ZZZZ" TO ENDREP VDUBUF.
           PERFORM SCREENIO.
       END-REP.

       REC-MARRIED.
           MOVE "MARRIED? ('I'-INCLUDE,'S'-SKIP,'O'-ONLY):"
                                                           TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010MARRIED." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEGREP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MARRIED.
           MOVE VDUBUF TO MARRIED.
           IF NOT (MARRIED = "I" OR "S" OR "O")
              GO TO REC-MARRIED.

       REC-ORIGIN.
           MOVE "ENTER ORIGIN CODE:  (?-WILD)" TO MESS.
           MOVE "E  A040ORIGIN." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ORIGIN.
           IF VDUSTATUS = "1U" GO TO REC-MARRIED.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO REC-ORIGIN.

           MOVE VDUBUF TO ORIGINX.
           GO TO REC-ACCEPT.

       ALL-ORIGIN.
           MOVE "S  A040ORIGIN." TO VDU.
           MOVE "????" TO ORIGINX VDUBUF.
           PERFORM SCREENIO.

       REC-ACCEPT.
           PERFORM CLEAR-LEGEND.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-ORIGIN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-ACCEPT.
           IF VDUBUF = "N" GO TO REC-STATE.
           IF VDUBUF NOT = "Y" GO TO REC-ACCEPT.


       EXIT-PARM.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".

      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY ASLNE2-SCREEN UPON ASLNE2-WINDOW-HANDLE.
           MOVE ASLNE2-FORM-FIELDS TO WR-BUF.
           MOVE ASLNE2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE ASLNE2-HELP--FILE TO HELP-LINK-FILE.
           MOVE ASLNE2-HELP--DIR TO HELP-LINK-DIR.

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/ASLNE2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************************
      *    MISC ROUTINES SECTION
      ************************************************
       MISC SECTION.
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CLEAR-LEGEND.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.

       END-PROGRAM SECTION.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ASLNE2-SCREEN.
           DESTROY ASLNE2-WINDOW-HANDLE.
           EXIT PROGRAM.
