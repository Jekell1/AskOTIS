       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FIXERN.
       DATE-WRITTEN.    3/28/16.
      *****************************************************
      *                      (FX)
      *    DESCRIPTION:  FIX LN-EARN-ORDER
      * REV:
      * BLM 160328 IF LN-EARN-ORDER DOESN'T MATCH SPR EARN
      *            ORDER, UPDATE LOAN WITH SPR VALUES,
      *            MIDATL PL# 923
      *  CS 160804 CHANGED H-SP-SPRCLASS FROM 9(2) TO 9(3)     
      * BAH 181203 GLOBAL LNFILE
      * BAH 190918 REMOVED ACCESS-CALL ON LNFILE
      *****************************************************
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
           "@(#)FX/FIXERN.CBL A32 12/03/18-08:21:13 >".

       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      *----------------------------------------------------------------
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
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
 
       01  RECORD-COUNT         PIC 9(6) VALUE 0.
       01  BEG-BRANCH           PIC 9(4).
       01  END-BRANCH           PIC 9(4).
       01  BEG-CLASS            PIC 9(2).
       01  END-CLASS            PIC 9(2).
       01  BEG-ACCT             PIC 9(6).
       01  END-ACCT             PIC 9(6).
       01  GROUP-FLAG           PIC X.
       01  ENT-GROUP            PIC X(4).
       01  ERRCD                PIC X VALUE " ".
 
      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
 
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(6)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD1A.
           03  FILLER           PIC X(10)    VALUE "BRANCH :".
           03  FILLER           PIC X(6)     VALUE " ".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X(5)     VALUE " ".
           03  H-BRDESC         PIC X(40).
 
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(6)     VALUE " ".
           03  H-TITLE          PIC X(32)    VALUE
           "      - - EARN ORDER FIX  - -".
 
       01  HEAD3.
           03  FILLER           PIC X(34)    VALUE 
           "ACCOUNT   NAME          PDTH      ".
           03  FILLER           PIC X(33)    VALUE 
           "TRANS DATE  NEWPDTH    1STPYDATE".
       
 
       01  DETAIL-LINE          PIC X(80) VALUE SPACES.
       01  D-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(22).
           03  D-COUNT          PIC ZZZZ,ZZ9.
 
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  FIXERN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/FIXERN_DEF.CPY".
       COPY "LIBFX/FIXERN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/GETPRW.CPY".
 
      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/FIXERN_SCN.CPY".
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***************************************
       MAIN-MODULE SECTION.
      ***************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "FIXERN" TO VDU-FORM-NAME.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FIXERN-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
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
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LN-PATH-OVERRIDE.

           MOVE SPACES TO MESS.
           STRING "PROCESSING BRANCH NO: " BR-NO
                                DELIMITED BY SIZE INTO MESS.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-LN1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN1-WBEG-OWNBR
                                 QLN1-WEND-OWNBR.
           MOVE BEG-ACCT      TO LN-ACCTNO
                                 QLN1-WBEG-ACCTNO.
           MOVE END-ACCT      TO QLN1-WEND-ACCTNO.

           PERFORM START-LN1-FILE.

       NEXT-LN.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-ACCTNO > END-ACCT
              GO TO END-OF-GROUP.
           
           IF (LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS)
              GO TO NEXT-LN.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG = 9
                 GO TO IO-ERROR.

             IF SP-EARN-ORDER(1) = LN-EARN-ORDER(1)
              IF SP-EARN-ORDER(2) = LN-EARN-ORDER(2)
               IF SP-EARN-ORDER(3) = LN-EARN-ORDER(3)
                IF SP-EARN-ORDER(4) = LN-EARN-ORDER(4)
                 IF SP-EARN-ORDER(5) = LN-EARN-ORDER(5)
                  IF SP-EARN-ORDER(6) = LN-EARN-ORDER(6)
                     GO TO NEXT-LN.


            MOVE SP-EARN-ORDER(1) TO LN-EARN-ORDER(1).
            MOVE SP-EARN-ORDER(2) TO LN-EARN-ORDER(2).
            MOVE SP-EARN-ORDER(3) TO LN-EARN-ORDER(3).
            MOVE SP-EARN-ORDER(4) TO LN-EARN-ORDER(4).
            MOVE SP-EARN-ORDER(5) TO LN-EARN-ORDER(5).
            MOVE SP-EARN-ORDER(6) TO LN-EARN-ORDER(6).

           PERFORM REWRITE-LN1-FILE.
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO RECORD-COUNT.

           GO TO NEXT-LN.
       END-OF-GROUP.
           PERFORM CLOSE-LN1-FILE.
           GO TO NEXT-GROUP-LOC.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND " TO MESS
              PERFORM SEND-MESS.
           MOVE "TOTAL RECORDS FIXED:" TO DETAIL-LINE.
           MOVE RECORD-COUNT TO D-COUNT.
           MOVE DETAIL-LINE TO MESS.
           PERFORM SEND-MESS.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO END-PROGRAM.
 
      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE 0 TO Q-POS.
           MOVE 0 TO C-POS.
           MOVE 0 TO P-POS.
           MOVE "V" TO Q-BUF.
           MOVE "1" TO C-BUF.
           MOVE "00" TO P-BUF.
       CHECK-RANGES.
           MOVE " " TO ERRCD.
       GETPR.
           PERFORM PRU-IN.
           IF STAT NOT = "00" GO TO END-INIT.
           PERFORM ENTER-PARAMETERS.
           IF ERRCD NOT = " "
              GO TO GETPR.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-PR-FILE.

           MOVE " " TO ERRCD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           GO TO EXIT-INITIALIZE.
       END-INIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.
 
      ***********************************
       ENTER-PARAMETERS SECTION.
      ***********************************
       ENTER-PARM.
           MOVE " " TO ERRCD.

      * DEFAULT TO USER BRANCH
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 0 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-01.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM-ENDIT.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ENDIT.

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

       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM-ENDIT.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.

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
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
      *     PERFORM BR-RANGE-SECURITY.
      *     IF BR-SECURITY-OKAY = "N"
      *        GO TO BR-01.

      *----------------------------------------------------------------
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
      *----------------------------------------------------------------
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              MOVE " INVALID CLASS RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO AC-01.
      *----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0             TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99            TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       AC-01.
           MOVE "ENNN060AC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-AC.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO AC-01.
           MOVE VDUNUM0 TO BEG-ACCT.
       AC-02.
           MOVE "ENNN060AC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AC-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO AC-02.
           MOVE VDUNUM0 TO END-ACCT.
           IF BEG-ACCT > END-ACCT
              MOVE " INVALID ACCOUNT NUMBER RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO AC-01.
           GO TO OK-YN.
       ALL-AC.
           MOVE "SNNN060AC1." TO VDU.
           MOVE 0 TO BEG-ACCT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060AC2." TO VDU.
           MOVE 999999 TO END-ACCT VDUNUM0.
           PERFORM SCREENIO.
       OK-YN.
           MOVE "E NA010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM-ENDIT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ENDIT.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS NOT = "00" GO TO OK-YN.
           IF VDUBUF = "N" GO TO EXIT-PARM-ENDIT.
           IF VDUBUF NOT = "Y" GO TO OK-YN.
         
           GO TO EXIT-PARM.
       EXIT-PARM-ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-PARM.
           EXIT.
 
      ***********************************
       WRITE-DETAIL-LINE SECTION.
      ***********************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
 
      ***********************************
       HEAD-PAGE SECTION.
      ***********************************
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
 
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
 
           MOVE BR-NO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.
           MOVE HEAD1A TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
 
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
 
       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
 
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO FIXERN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FIXERN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FIXERN-SCREEN UPON FIXERN-WINDOW-HANDLE.
           MOVE FIXERN-FORM-FIELDS TO WR-BUF. 
           MOVE FIXERN-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM. 

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE FIXERN-HELP--FILE TO HELP-LINK-FILE.
           MOVE FIXERN-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FIXERN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************
       MISC SECTION.
      ******************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
 
      ******************************************
       IO-ROUTINE SECTION.
      ******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
 
      ********************************
       END-PROGRAM SECTION.
      ********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FIXERN-SCREEN.
           DESTROY FIXERN-WINDOW-HANDLE.
           EXIT PROGRAM.
