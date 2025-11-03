       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FDLIST.
       DATE-WRITTEN.    060184.
      ******************************************************************
      *    DIR :  LS
      *    DESC:  FORM DESCRIPTOR TITLE LISTING
      * REV:
      * BAW 022688  ADD FD-LNMEMO
      * JTG 012589 CHANGED FD-COLLFMTYPE TO FMTYPE
      *               "    MEANING FROM Y TO C
      * BAH 000327 PRINT FD-SMARTPORT IF "Y", PRINT PROG-PREFIX, KEVIN
      * BAH 020516 PRINT FD-SMARTPORT IF NOT "N" OR " "
      *
      * KEC 050801 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *            ADDED ABILITY TO PRINT 'PCL' ALONG WITH 'PRT',
      *            'LONP' & 'NOTP'.
      * BAH 111121 REPLACED RRFORM WITH SCREENIO
      * BLM 120808 ADD SECURITY PROFILE LETTER TO DETAIL LINE & ADD RANGE
      *            FOR LETTER, REGACC PR# 316
      * KEC 150224 {PL#00876} PARADATA <A15> [JAY CISZEWSKI]
      *            ADDED ABILITY TO PRINT 'PST' ALONG WITH 'PCL','PRT',
      *            'LONP' & 'NOTP'.
      *
      * KEC 2016.0211 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *
      * KEC 2017.0119 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED FD-SMARTPORT TO FD-OPTION [88 LEVELS].
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
           "@(#)LS/FDLIST.CBL S35 04/04/24-08:22:56 >".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999    COMP-3 VALUE 0.
           03  LN-COUNT         PIC 999    COMP-3 VALUE 99.
           03  LN-WK            PIC 999    COMP-3 VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   COMP-3 VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************

      * THESE MUST STAY ALPHA SO FDTALY CAN FLDVAL IN AND GET LEADING ZEROS
       01  LINK-BUF.
           03  ERRCD               PIC X.
           03  FORM-NO             PIC 9(3).
           03  BRANCH              PIC 9(4).
           03  FDLIST-WINDOW-HANDLE PIC X(10).

       01  ACCEPT-BUF          PIC X     VALUE " ".
       01  BEG-PROFILE         PIC X     VALUE " ".
      * LOWER CASE Z
       01  END-PROFILE         PIC X     VALUE X"7A".

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
           03  H-TITLE          PIC X(41).

       01  HEAD3.
           03  FILLER           PIC X(6)     VALUE "  CODE".
           03  FILLER           PIC X(6)     VALUE "  NEXT".
           03  FILLER           PIC X(32)    VALUE "  DESCRIPTION".
           03  FILLER           PIC X(42)    VALUE
           "PROG  OPTION  FILE   BODY    TOP  BOTTOM  ".
           03  FILLER           PIC X(35)    VALUE
           "CONT    CNTR  FMTYPE  MEMO  PROFILE".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(2).
           03  D-FMNO           PIC Z(4).
           03  FILLER           PIC X(2).
           03  D-FMLINK.
               05  D-NEXTFM     PIC Z(4).
           03  FILLER           PIC X(2).
           03  D-DESC           PIC X(25).
           03  FILLER           PIC XXX.
           03  D-PROG-PREFIX    PIC X(4).
           03  FILLER           PIC X.
           03  D-PROG           PIC XX.
           03  FILLER           PIC X(4).
           03  D-FD-OPTION      PIC X.
           03  FILLER           PIC X(5).
           03  D-1STFILE        PIC Z9.
           03  FILLER           PIC X(5).
           03  D-LENGTH         PIC ZZ9.
           03  FILLER           PIC X(5).
           03  D-TOP            PIC Z9.
           03  FILLER           PIC X(5).
           03  D-BOTTOM         PIC Z9.
           03  FILLER           PIC X(5).
           03  D-PRCONT         PIC X.
           03  FILLER           PIC X(6).
           03  D-LNUPDFG        PIC X.
           03  FILLER           PIC X(7).
           03  D-CRNOREC        PIC X.
           03  FILLER           PIC X(6).
           03  D-MEMO           PIC X.
           03  FILLER           PIC X(6).
           03  D-SECURITY       PIC X.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/FDLIST_DEF.CPY".
       COPY "LIBLS/FDLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/FDLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***************************************
       MAIN-MODULE SECTION.
      ***************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "FDLIST" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FDLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.

           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-FD.
           PERFORM READ-FD1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF FD-SECURITY-PROFILE < BEG-PROFILE
              OR FD-SECURITY-PROFILE > END-PROFILE
                 GO TO NEXT-FD.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-FD.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND " TO MESS
              PERFORM SEND-MESS
           ELSE
              MOVE SPACES TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE "S  A010PROFILE1." TO VDU.
           MOVE " " TO VDUBUF BEG-PROFILE.
           PERFORM SCREENIO.
           MOVE "S  A010PROFILE2." TO VDU.
           MOVE "Z" TO END-PROFILE.
           CALL "C$TOLOWER" USING END-PROFILE, VALUE 1.
           MOVE END-PROFILE TO VDUBUF.
           PERFORM SCREENIO.
       SETRP.
           MOVE FDLIST-QUEUE-POS TO Q-POS.
           MOVE FDLIST-COPIES-POS TO C-POS.
           MOVE FDLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.
       OK-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO GETBR.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N" GO TO ENDIT.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.

           GO TO OPEN-FILES.

       GETBR.
           MOVE "ENNN040BR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO OK-YN.
           IF VDUSTATUS = "16" GO TO ALLBR.
           IF VDUSTATUS = "1U" GO TO OK-YN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO GETBR.
           MOVE VDUNUM0 TO BRANCH.
           IF BRANCH = 0 GO TO GETBR.

           GO TO TALLY.

       ALLBR.
           MOVE "SNNN040BR." TO VDU.
           MOVE 0000 TO BRANCH VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050ALLBR." TO VDU.
           MOVE "-ALL-" TO VDUBUF.
           PERFORM SCREENIO.

       TALLY.
           MOVE "ENNN030FM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO OK-YN.
           IF VDUSTATUS = "1U" GO TO GETBR.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO TALLY.
           MOVE VDUNUM0 TO FORM-NO.
           IF FORM-NO = 0 GO TO PROFILE1.

      D    STOP MESS.
           MOVE SPACES TO ERRCD.
           MOVE "LP/FDTALY" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME LINK-BUF.
           CANCEL FORM-PROGX.

           IF ERRCD = "?" GO TO TALLY.

       PROFILE1.
           MOVE "E  A010PROFILE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO OK-YN.
           IF VDUSTATUS = "16" GO TO ALL-PROFILES.
           IF VDUSTATUS = "1U" GO TO TALLY.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PROFILE1.
           MOVE VDUBUF TO BEG-PROFILE.
       PROFILE2.
           MOVE "E  A010PROFILE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO OK-YN.
           IF VDUSTATUS = "1U" GO TO PROFILE1.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PROFILE2.
           MOVE VDUBUF TO END-PROFILE.
           GO TO OK-YN.
       ALL-PROFILES.
           MOVE "S  A010PROFILE1." TO VDU.
           MOVE " " TO VDUBUF BEG-PROFILE.
           PERFORM SCREENIO.
           MOVE "S  A010PROFILE2." TO VDU.
           MOVE "Z" TO END-PROFILE.
           CALL "C$TOLOWER" USING END-PROFILE, VALUE 1.
           MOVE END-PROFILE TO VDUBUF.
           PERFORM SCREENIO.

           GO TO OK-YN.

       OPEN-FILES.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-FD1-FILE.
           MOVE ZEROES TO FD-FMNO.
           PERFORM START-FD1-FILE.

           MOVE " " TO ERRCD.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "- - - FORM DESCRIPTOR TITLE LIST - - -  " TO H-TITLE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************
           MOVE FD-FMNO TO D-FMNO.
           IF FD-NEXTFM NUMERIC
              MOVE FD-NEXTFM TO D-NEXTFM
           ELSE
              MOVE " ? " TO D-FMLINK.
           MOVE FD-DESC TO D-DESC.

           PERFORM LPFRFD-FIND-PREFIX.

           IF ( LPWSFD-PREFIX-PCL )
              STRING "PCL", FD-PROG-PREFIX
                     DELIMITED BY SIZE INTO D-PROG-PREFIX
           ELSE
           IF ( LPWSFD-PREFIX-PST )
              STRING "PST", FD-PROG-PREFIX
                     DELIMITED BY SIZE INTO D-PROG-PREFIX
           ELSE
           IF ( LPWSFD-PREFIX-PRT )
              STRING "PRT", FD-PROG-PREFIX
                     DELIMITED BY SIZE INTO D-PROG-PREFIX
           ELSE
           IF ( FD-TYPE = "N" )
              MOVE "NOTP" TO D-PROG-PREFIX
           ELSE
              MOVE "LONP" TO D-PROG-PREFIX.

           MOVE FD-PROG TO D-PROG.
      * "N" OR " "
           IF NOT ( FD-OPTION-STANDARD )        
              MOVE FD-OPTION TO D-FD-OPTION.
           MOVE FD-1STFILE TO D-1STFILE.
           MOVE FD-BODY TO D-LENGTH.
           MOVE FD-TOP TO D-TOP.
           MOVE FD-BOTTOM TO D-BOTTOM.
           MOVE FD-PRCONT TO D-PRCONT.
           MOVE FD-INCR-DELCNT TO D-LNUPDFG.
           MOVE FD-FMTYPE TO D-CRNOREC.
           MOVE FD-LNMEMO TO D-MEMO.
           MOVE FD-SECURITY-PROFILE TO D-SECURITY.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

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

           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       COPY "LIBLP/LPFRFD.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO FDLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FDLIST-SYSDATE.
           DISPLAY FDLIST-SCREEN UPON FDLIST-WINDOW-HANDLE.
           MOVE FDLIST-FORM-FIELDS TO WR-BUF.
           MOVE FDLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE FDLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE FDLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/FDLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************
       MISC-ROUTINES SECTION.
      *****************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      *****************************
       IO-ROUTINE SECTION.
      *****************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *****************************
       END-PROGRAM SECTION.
      *****************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "LP/FDMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FDLIST-SCREEN.
           DESTROY FDLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
