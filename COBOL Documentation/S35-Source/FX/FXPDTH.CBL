       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FXPDTH.
       DATE-WRITTEN.    021297.
      ************************************************************************
      *                    (FX/FXPDTH.C)
      *
      *  DESCRIPTION: MAKES THE FOLLOWING CHANGED TO THE FILES LISTED:
      *               
      *       1:  LN-FILE - PERFORMS PADI-THRU-CALCULATION & UPDATES
      *                     PDTH-DATE-FULL TO LN-LCPDTH-DATE
      *
      *  FOR PEOPLES, WERE DOING MANUAL LATE CHGS; WANT AUTOMATIC, SO
      *  NEED TO FIX LN-LCPDTH-DATE FIRST
      *  
      * REV:
      *  GLF 090297 REMOVED ALL USE OF GLOBAL
      *  BAH 150323 ADDED CLASS RANGE, KSTREET/MULLEN PR# 4721
      *  BAH 160128 SPLIT OUT LTFILE, PD0003
      *   CS 160824 FIX INSPECT STATEMENT REPLACING &&&&&& WITH LN-ACCTNO,
      *             CHANGED INSPECT TO USE 8& TO MAP TO LN-ACCTNO
      *             ADD GETLTRW COPYMEMBER
      *  BAH 181203 GLOBAL LNFILE
      * BAH 190918 REMOVED ACCESS-CALL ON LNFILE
      * BAH 2022.1004 HARD CODED STARTS
      * BAH 2024.0130 CALL CRNOR2 AND WRITE NOTICE RECORD
      ************************************************************************
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

      ******************************************************************
      *
      *      W O R K I N G - S T O R A G E     S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/FXPDTH.CBL S34 05/21/21-08:33:50 >".

       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".


      *-----------------------------------------------------------------
       01  LN-FEED           PIC 9(2) VALUE ZERO.
 
       01  MESS-LIST         PIC X(5)  VALUE "MESS.".
       01  MESS-BUF          PIC X(40) VALUE SPACES.

      * YYYYMMDD
       01  TODAYS-DATE          PIC 9(8).

      ******************************************************************
      *
      *     S C R E E N   F I E L D S   
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR          PIC 9(4).
           03  END-BR          PIC 9(4).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  BEG-CL          PIC 9(2).
           03  END-CL          PIC 9(2).

       01  SV-BEG-BR           PIC 9(4)    VALUE 9999.
       01  SV-END-BR           PIC 9(4)    VALUE 9999.
      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S 
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       01  FXPDTH-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/FXPDTH_DEF.CPY".
       COPY "LIBWI/TIMER_DEF.CPY".
       COPY "LIBFX/FXPDTH_WKS.CPY".
       COPY "LIBWI/TIMER_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBFX/FXPDTH_SCN.CPY".
       COPY "LIBWI/TIMER_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *     M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "FXPDTH" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FXPDTH-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
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
           IF IO-FG = 9 OR BR-NO > END-BR
              GO TO END-ROUTINE.

           STRING "PROGRAM IN PROGRESS......BRNO: " 
                     BR-NO
                       DELIMITED BY SIZE INTO MESS-BUF.
           PERFORM SEND-LEGEND.

       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LN-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE
                                              NO-PATH-OVERRIDE.
           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-NO1-FILE.

           PERFORM PROCESS-LN-FILE.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           GO TO END-PROGRAM.


      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           PERFORM ENTER-PARAMETERS.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO INITIALIZE-EXIT.
 
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
 
           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                             END-BR.

           ACCEPT TODAYS-DATE FROM CENTURY-DATE.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       INITIALIZE-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "16" GO TO ALL-BR.
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
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO INIT-ACCEPT-ACCEPT.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR > END-BR
              PERFORM INVALID-RANGE
              GO TO BR-01.

           GO TO CL-01.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CL.

       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CL.
           IF BEG-CL > END-CL
              PERFORM INVALID-RANGE
              GO TO CL-01.

           GO TO INIT-ACCEPT-ACCEPT.

      *-----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CL VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CL VDUNUM0.
           PERFORM SCREENIO.


      *-----------------------------------------------------------------
       INIT-ACCEPT-ACCEPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS NOT = "00" GO TO INIT-ACCEPT-ACCEPT.
           IF VDUBUF = "N" GO TO BR-01.
           IF VDUBUF NOT = "Y" GO TO INIT-ACCEPT-ACCEPT.

           GO TO EXIT-PARM.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-PARM.
           EXIT.

      ******************************************************************
       PROCESS-LN-FILE SECTION.
      ******************************************************************

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN3-WBEG-OWNBR
                                 QLN3-WEND-OWNBR.
           MOVE BEG-CL        TO LN-CLASS
                                 QLN3-WBEG-CLASS.
           MOVE END-CL        TO QLN3-WEND-CLASS.
           MOVE 0             TO LN-ACCTNO
                                 QLN3-WBEG-ACCTNO.
           MOVE ALL "9"       TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       PROCESS-LN-FILE-NEXT.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO PROCESS-LN-FILE-EXIT.

           IF LN-CLASS < BEG-CL OR LN-CLASS > END-CL
              GO TO PROCESS-LN-FILE-NEXT.

           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM READ-SP1-FILE
           IF IO-FG = 9
              MOVE "BAD READ SP - ACCT # &&&&&&&&" TO MESS
              INSPECT MESS REPLACING FIRST "&&&&&&&&"
                   BY LN-ACCTNO
              PERFORM SEND-MESS
      *       UNLOCK LN-FILE
              GO TO PROCESS-LN-FILE-NEXT.

           MOVE LN-REC TO HOLD-LN-REC.

           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO LN-LCPDTH-DATE.

           IF LN-REC NOT = HOLD-LN-REC
              PERFORM REWRITE-LN3-FILE
              PERFORM UPDATE-NOTICE-FILE.

           GO TO PROCESS-LN-FILE-NEXT.

       PROCESS-LN-FILE-EXIT.
           PERFORM CLOSE-LN3-FILE.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/DATER.CPY".

      ***********************************************
       UPDATE-NOTICE-FILE SECTION.
      ***********************************************
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.

           MOVE TODAYS-DATE    TO CRNOR2-SYSDATE.
           MOVE "CL/CRNOR2"    TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           IF IO-FG = 0
              PERFORM REWRITE-NO1-FILE
           ELSE
              PERFORM WRITE-NO1-FILE.

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO FXPDTH-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FXPDTH-SYSDATE.
           DISPLAY FXPDTH-SCREEN UPON FXPDTH-WINDOW-HANDLE.
           MOVE FXPDTH-FORM-FIELDS TO WR-BUF.
           MOVE FXPDTH-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE FXPDTH-HELP--FILE TO HELP-LINK-FILE.
           MOVE FXPDTH-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FXPDTH_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE MESS-BUF TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN3IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
 
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FXPDTH-SCREEN.
           DESTROY FXPDTH-WINDOW-HANDLE.
           EXIT PROGRAM.
