       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SMLIST.
       DATE-WRITTEN.    102604.
      ******************************************************************
      *                      (LS)
      *    DESCRIPTION:  SPR MEMO LISTING (SMFILE)
      *
      *  REV:
      * 2004.1029  #362 NEW WORLD
      * 050222     HOLD-BEGIN-KEY NOT SET UP WHEN USING AN F3RANGE.
      * BLF 061204 SP-FILE NOT IN DECLARATIVES - IF MEMO FOR DELETED SPR
      *            YOU WOULD EITHER DROP TO A $ OR LOCK UP.  AFTER I FIXED
      *            THAT, THEY DIDN'T LOCK UP BUT THE MESSAGE ABOUT THE SPR
      *            NOT EXISTING WAS OVERWRITTEN & GARBLED, SO CLEANED THAT
      *            UP.  FIXED FOR WORLD PER JR.
      *
      * KEC 2015.1111 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         INCREASED USE OF SM-SEQ FROM 2 TO 3.
      *
      * KEC 2015.1204 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *          FIXED FOR SM-SP1-KEY.
      * BAH 170504 ACTIVATE 8 DIGIT DATES, PD0006
      *
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
           "@(#)LS/SMLIST.CBL J35 12/21/23-09:24:12 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01SM.CPY".
       COPY "LIBLP/LP01SM_SQL.CPY".
       COPY "LIBLP/LPWSSM.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       01  PR-IFN               PIC X(2)    VALUE "PR".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 0.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  LINE-FG              PIC 9        VALUE 2.

       01  CLASS-FG             PIC X(3)     VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".

       01  BEGIN-SM1-KEY.
           03  BEGIN-SM-SP1-KEY.
               05 BEGIN-SM-SP-ORGST            PIC X(02).
               05 BEGIN-SM-SP-SPRCLASS         PIC 9(03).
               05 BEGIN-SM-SP-SUBCLASS         PIC 9(02).
               05 BEGIN-SM-SP-LAWCODE          PIC 9(02).
           03  BEGIN-SM-DATE                   PIC 9(08).
           03  BEGIN-SM-SEQ                    PIC 9(03).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-ST          PIC X(2).
           03  END-ST          PIC X(2).
           03  BEG-CL          PIC 9(2).
           03  END-CL          PIC 9(2).
           03  BEG-SUBCLASS    PIC 9(2).
           03  END-SUBCLASS    PIC 9(2).
           03  BEG-LAWCODE     PIC 9(2).
           03  END-LAWCODE     PIC 9(2).
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  INCL-FREE       PIC X.

       01  VERSION-CONTROL     PIC X VALUE "A".

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

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(42)    VALUE
           "- - - - SPR MEMO FILE LISTING - - - -".
           03  FILLER           PIC X(44)    VALUE " ".

       01  HEAD3.
           03  FILLER      PIC X(53)    VALUE
           "ST  CLASS    SUB  LAW       EFF.       EXP.  TYPE    ".
           03  FILLER      PIC X(18)    VALUE
           "  MEMO SEQ   MEMO ".
           03  FILLER      PIC X(56) VALUE
           "- - - - - - - - - - - - - - - - - - - - - - - - - - - -".

       01  HEAD3B.
           03  FILLER      PIC X(53)    VALUE
           "           CLASS CODE       DATE       DATE          ".
           03  FILLER      PIC X(6)     VALUE
           "  DATE".

       01  HEAD4.
           03  H4-DASH      PIC X(124).

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-STATE            PIC X(2).
           03  FILLER             PIC X(5).
           03  D-CLASS            PIC 99.
           03  FILLER             PIC X(5).
           03  D-SUBCLASS         PIC 99.
           03  FILLER             PIC X(3).
           03  D-LAWCODE          PIC 99.
           03  FILLER             PIC X(3).
           03  D-EFFDATE          PIC 99/99/99.
           03  FILLER             PIC X(3).
           03  D-EXPDATE          PIC 99/99/99.
           03  FILLER             PIC X(5).
           03  D-MEMO-TYPE        PIC X.
           03  FILLER             PIC X(2).
           03  D-MEMO-DATE        PIC 99/99/99.
           03  FILLER             PIC X(2).
           03  D-MEMO-SEQ         PIC 999.
           03  FILLER             PIC X(2).
           03  D-MEMO             PIC X(60).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(50).
           03  RANGE-SELECTION  PIC X(15).
           03  RANGE-DATE REDEFINES RANGE-SELECTION.
               05 RANGE-DT      PIC 99/99/99.
               05 FILLER        PIC X(7).


       COPY "LIBLP/LHSTATW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/SMLIST_DEF.CPY".
       COPY "LIBLS/SMLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/SMLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-SM1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SMLIST" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE ZEROS TO BEGIN-SM-SEQ.
           MOVE BEGIN-SM1-KEY TO SM1-KEY.

           PERFORM START-SM1-FILE.

       NEXT-SM.
           PERFORM READ-SM1-FILE-NEXT.
           IF IO-FG = 9 OR SM-SP-ORGST > END-ST
              GO TO END-ROUTINE.

           IF SM-SP-SPRCLASS < BEG-CL OR SM-SP-SPRCLASS > END-CL
               GO TO NEXT-SM.

           IF SM-SP-SUBCLASS < BEG-SUBCLASS OR
              SM-SP-SUBCLASS > END-SUBCLASS
               GO TO NEXT-SM.

           IF SM-SP-LAWCODE < BEG-LAWCODE OR
              SM-SP-LAWCODE > END-LAWCODE
               GO TO NEXT-SM.

           IF INCL-FREE = "N"
              IF SM-MEMO-TYPE NOT = "A"
                 GO TO NEXT-SM.

      *CONVERT THE MEMO-DATE
           COMPUTE DATE-YYYYMMDD = 99999999 - SM-DATE.

           IF DATE-YYYYMMDD < BEG-DATE OR DATE-YYYYMMDD > END-DATE
               GO TO NEXT-SM.

      **************************************************
           PERFORM DETAIL-LINE-PROCESS.
      **************************************************
           MOVE "N" TO FIRST-TIME.

           GO TO NEXT-SM.

       END-ROUTINE.
           IF FIRST-TIME = "Y" AND
              EXT-ROUTE-BUF = "00"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.
           IF FIRST-TIME = "Y"
              MOVE SPACES TO PR-REC
              MOVE 99 TO LN-FEED
              PERFORM WRITE-PR-REC.
           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE SMLIST-QUEUE-POS TO Q-POS.
           MOVE SMLIST-COPIES-POS TO C-POS.
           MOVE SMLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".


           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-SM1-FILE.
           ADD 1 EXT-CONAME-RPT-LINES GIVING LN-COUNT.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS.................." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ***********************************
       EO-EXEC-AUDIT SECTION.
      ***********************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.


      ***********************************
       ENTER-PARAMETERS SECTION.
      ***********************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO MEMO-01.

       STATE-01.
           MOVE "E  A020ST1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-STATES.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO STATE-01.
           MOVE VDUBUF  TO BEG-ST BEGIN-SM-SP-ORGST.
           IF BEG-ST = SPACES GO TO STATE-01.

       STATE-02.
           MOVE "E  A020ST2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO STATE-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO STATE-02.
           MOVE VDUBUF  TO END-ST.
           IF END-ST = SPACES GO TO STATE-02.
           IF BEG-ST > END-ST
              MOVE "INVALID STATE CODE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO STATE-01.
           GO TO CL-01.

       ALL-STATES.
           MOVE "S  A020ST1." TO VDU.
           MOVE SPACES TO BEG-ST BEGIN-SM-SP-ORGST VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020ST2." TO VDU.
           MOVE "ZZ"          TO END-ST VDUBUF.
           PERFORM SCREENIO.

       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO STATE-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CL BEGIN-SM-SP-SPRCLASS.
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CL.
           IF BEG-CL > END-CL GO TO CL-01
              MOVE "INVALID CLASS RANGE"      TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO SUB-01.

       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CL BEGIN-SM-SP-SPRCLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CL VDUNUM0.
           PERFORM SCREENIO.

       SUB-01.
           MOVE "ENNN020SUB1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-SUBCLASS.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SUB-01.
           MOVE VDUNUM0 TO BEG-SUBCLASS BEGIN-SM-SP-SUBCLASS.

       SUB-02.
           MOVE "ENNN020SUB2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SUB-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SUB-02.
           MOVE VDUNUM0 TO END-SUBCLASS.
           IF BEG-SUBCLASS > END-SUBCLASS GO TO SUB-01
              MOVE "INVALID SUB-CLASS RANGE"      TO MESS
              PERFORM SEND-MESS
              GO TO SUB-01.
           GO TO LAW-01.

       ALL-SUBCLASS.
           MOVE "SNNN020SUB1." TO VDU.
           MOVE 0 TO BEG-SUBCLASS BEGIN-SM-SP-SUBCLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020SUB2." TO VDU.
           MOVE 99 TO END-SUBCLASS VDUNUM0.
           PERFORM SCREENIO.

       LAW-01.
           MOVE "ENNN020LAW1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-LAWCODE.
           IF VDUSTATUS = "1U" GO TO SUB-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO LAW-01.
           MOVE VDUNUM0 TO BEG-LAWCODE BEGIN-SM-SP-LAWCODE.

       LAW-02.
           MOVE "ENNN020LAW2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO LAW-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO LAW-02.
           MOVE VDUNUM0 TO END-LAWCODE.
           IF BEG-LAWCODE  > END-LAWCODE  GO TO LAW-01
              MOVE "INVALID LAW CODE RANGE"      TO MESS
              PERFORM SEND-MESS
              GO TO LAW-01.
           GO TO DT-01.

       ALL-LAWCODE.
           MOVE "SNNN020LAW1." TO VDU.
           MOVE 0 TO BEG-LAWCODE BEGIN-SM-SP-LAWCODE  VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LAW2." TO VDU.
           MOVE 99 TO END-LAWCODE  VDUNUM0.
           PERFORM SCREENIO.

       DT-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO MEMO-01.

           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO LAW-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE BEGIN-SM-DATE.
       DT-02.
           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

           GO TO MEMO-01.

       ALL-DT.
           MOVE "S  8080DT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE
                                  BEGIN-SM-DATE
                                  VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2." TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE
                                  VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.


       MEMO-01.
           MOVE "E  A010MEMOTYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO MEMO-01.
           MOVE VDUBUF TO INCL-FREE
           IF INCL-FREE NOT = "Y" AND NOT = "N"
               GO TO MEMO-01.

       END-IT.

       EXIT-PARM.
           EXIT.

      ***********************************
       DISPLAY-PARAMETERS SECTION.
      ***********************************
           MOVE "S  A020ST1." TO VDU.
           MOVE BEG-ST        TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST2." TO VDU.
           MOVE END-ST        TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CL TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020SUB1." TO VDU.
           MOVE BEG-SUBCLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020SUB2." TO VDU.
           MOVE END-SUBCLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020LAW1." TO VDU.
           MOVE BEG-LAWCODE  TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LAW2." TO VDU.
           MOVE END-LAWCODE  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  8080DT1." TO VDU.
           MOVE BEG-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2." TO VDU.
           MOVE END-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************
       DETAIL-LINE-PROCESS SECTION.
      *****************************************

           IF NOT (SM-SP1-KEY = LPWSSP-SP1-KEY)
              MOVE SM-SP1-KEY TO SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-BAD
                 MOVE SM-SP-ORGST        TO D-STATE
                 MOVE SM-SP-SPRCLASS     TO D-CLASS
                 MOVE SM-SP-SUBCLASS     TO D-SUBCLASS
                 MOVE SM-SP-LAWCODE      TO D-LAWCODE
                 MOVE
                 "*************** SPR DOES NOT EXIST!!! ***************"
                                         TO D-MEMO
                 MOVE 2 TO LN-FEED
                 MOVE 1 TO LINE-FG
                 MOVE SM-SP1-KEY TO LPWSSP-SP1-KEY
                 PERFORM WRITE-DETAIL-LINE.

           IF SM-SP1-KEY NOT = LPWSSP-SP1-KEY
              MOVE 2 TO LN-FEED
              MOVE 1 TO LINE-FG
              MOVE SM-SP1-KEY TO LPWSSP-SP1-KEY
           ELSE
              IF LINE-FG NOT > 1
                 MOVE 2 TO LINE-FG.

      * PRINT MEMO TYPE, SEQ  & DATE ON EACH LINE
           IF SM-MEMO-TYPE NOT = "A"
              MOVE "*"    TO D-MEMO-TYPE.

           COMPUTE DATE-YYYYMMDD = 99999999 - SM-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO D-MEMO-DATE.
           MOVE SM-SEQ          TO D-MEMO-SEQ.

           IF LINE-FG = 1
              PERFORM PRINT-LINE1
           ELSE
              PERFORM PRINT-LINE2.


      ******************************************
       PRINT-LINE1 SECTION.
      ******************************************
           MOVE SM-SP-ORGST        TO D-STATE.
           MOVE SM-SP-SPRCLASS     TO D-CLASS.
           MOVE SM-SP-SUBCLASS     TO D-SUBCLASS.
           MOVE SM-SP-LAWCODE      TO D-LAWCODE.
           MOVE SP-EFFDATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO D-EFFDATE
           MOVE SP-EXPDATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO D-EXPDATE.
           MOVE SM-MEMO            TO D-MEMO.

           PERFORM WRITE-DETAIL-LINE.

       PRINT-LINE1-EXIT.
             EXIT.

      ******************************************
       PRINT-LINE2 SECTION.
      ******************************************
           MOVE SM-MEMO          TO D-MEMO.
           MOVE 1                TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       PRINT-LINE2-EXIT.
             EXIT.

      ******************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************
       HEAD-PAGE SECTION.
      ******************************************
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

           MOVE HEAD3B TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE ALL "=" TO H4-DASH.
           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 2 TO LN-FEED.
           MOVE 8 TO LN-COUNT.

      ******************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************
           MOVE EXT-CONAME-RPT-LINES     TO LN-COUNT.
           MOVE 4 TO LN-FEED.
           MOVE "BEG STATE"       TO RANGE-LINE.
           MOVE BEG-ST            TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END STATE"       TO RANGE-LINE.
           MOVE END-ST            TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG CLASS"       TO RANGE-LINE.
           MOVE BEG-CL            TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END CLASS"       TO RANGE-LINE.
           MOVE END-CL            TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG SUBCLASS"    TO RANGE-LINE.
           MOVE BEG-SUBCLASS      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END SUBCLASS" TO RANGE-LINE.
           MOVE END-SUBCLASS      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG LAWCODE"     TO RANGE-LINE.
           MOVE BEG-LAWCODE       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END LAWCODE"     TO RANGE-LINE.
           MOVE END-LAWCODE       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG DATE"        TO RANGE-LINE.
           MOVE BEG-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO RANGE-DT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END DATE"        TO RANGE-LINE.
           MOVE END-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO RANGE-DT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCL FREE TYPE MEMO" TO RANGE-LINE.
           MOVE INCL-FREE          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.


      ***********************************************************
       CLASS-SCAN SECTION.
      ***********************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SNNN020CL1." TO VDU
           ELSE
              MOVE "SNNN020CL2." TO VDU.
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
           MOVE EXT-CONAME-CONAME TO SMLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SMLIST-SYSDATE.
           DISPLAY SMLIST-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE SMLIST-FORM-FIELDS TO WR-BUF.
           MOVE SMLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE SMLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE SMLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/SMLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************
       MISC-PARA SECTION.
      *******************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".

      **********************************
       IO-ROUTINE SECTION.
      **********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPSMGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPSM1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      **********************************
       END-PROGRAM SECTION.
      **********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPL2MU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY SMLIST-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
