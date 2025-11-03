       IDENTIFICATION DIVISION.
       PROGRAM-ID.      INSCOM.
       DATE-WRITTEN.    071492.
      ******************************************************************
      *
      *  DESCRIPTION:  CALCULATE INSCOMM BASED ON IC-RECORD
      *
      * REV:
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  BAH 020698 BRANCH/GROUP LOGIC, ALLOW GRAND FINANCE TO RUN, PR# 648
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *   CS 990406 ADDED SKIP OF PAID OFF ACCOUNTS
      * MJD 991001 ADDED LOGIC TO COMPUTE COMMISSION AS EITHER % OR
      *            FIXED AMOUNT  (TFC PR#41)
      * BAH 040318 ADDED INSURANCE COMPANY TO THE SCREEN FOR WORLD
      *            THEY NEED TO RESET COMMISSION FOR INSCOMP 1 ONLY
      * BAH 051004 HAD A BAD SUBSCRIPT, WASNT WORKING SO GREAT ! KM
      *
      * KEC 2018.0423 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *         LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *         WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *         LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      *
      * BAH 190918 REMOVED ACCESS-CALL ON LNFILE
      * BAH 2022.1004 HARD CODED STARTS
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

      ******************************************
      *      WORKING STORAGE SECTION
      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/INSCOM.CBL S34 05/21/21-14:35:12 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LP01IC_SQL.CPY".
       COPY "LIBLP/LPWSIC.CPY".

       01  HOLD-IC           PIC 99 VALUE 0.
       01  IC-SUB            PIC 9(3) VALUE 0.
       01  SUB               PIC 9(3) VALUE 0.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 99.
           03  END-CLASS       PIC 99.
           03  BEG-ACT         PIC 9(6).
           03  END-ACT         PIC 9(6).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  INS-COMPANY     PIC 99.

       01  VERSION-CONTROL     PIC X VALUE "B".
       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

       01  MESS-LIST         PIC X(5)  VALUE "MESS.".
       01  MSEL-LIST         PIC X(5)  VALUE "MSEL.".
       01  LEGEND            PIC X(70).

       01  LN-FEED          PIC 99       VALUE 0.

       01  DISPLAY-MESS.
           03  FILLER           PIC X(18)    VALUE
           "IN PROGRESS.......".
           03  DISPLAY-MESS-BR  PIC 9(4).
           03  FILLER           PIC X        VALUE " ".
           03  DISPLAY-MESS-CL  PIC 99.
           03  FILLER           PIC X        VALUE "-".
           03  DISPLAY-MESS-LN  PIC 9(6).

       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/INSCOM_DEF.CPY".
       COPY "LIBFX/INSCOM_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/INSCOM_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      **********************************
      *       MAIN PROGRAM CONTROL
      **********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "INSCOM" TO VDU-FORM-NAME.

           PERFORM INITIAL-ROUTINE.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-ROUTINE.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              MOVE 0 TO BR-NO
              GO TO END-ROUTINE.
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO IC-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-IC1-FILE.

           PERFORM OPEN-LN3-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN3-WBEG-OWNBR
                                 QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE BEG-ACT   TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE END-ACT   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-FG NOT = 0 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF ( LN-POCD-NONLOAN )
      *       UNLOCK LN-FILE
              GO TO NEXT-LN.

           IF LN-CURBAL = 0
      *       UNLOCK LN-FILE
              GO TO NEXT-LN.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
      *       UNLOCK LN-FILE
              GO TO NEXT-LN.

           IF LN-ACCTNO < BEG-ACT OR LN-ACCTNO > END-ACT
      *       UNLOCK LN-FILE
              GO TO NEXT-LN.

           IF EXT-ROUTE-BUF = "00"
              PERFORM SEND-ACCOUNT.

           MOVE 1 TO SUB.
       NEXT-IC.
           IF SUB > 6
              GO TO DONE-LN.

           IF LN-INSOURS(SUB) NOT = "Y"
              ADD 1 TO SUB
              GO TO NEXT-IC.

      * 0 = FIX ALL INSURANCE COMPANIES
           IF INS-COMPANY NOT = 0
              IF LN-INSCOMP(SUB) NOT = INS-COMPANY
                 ADD 1 TO SUB
                 GO TO NEXT-IC.

           IF LN-INSCOMP(SUB) = 0
              ADD 1 TO SUB
              GO TO NEXT-IC
           ELSE
              IF LN-INSCOMP(SUB) NOT = HOLD-IC
                 MOVE LN-INSCOMP(SUB) TO IC-CODE HOLD-IC
                 PERFORM READ-IC1-FILE
                 IF IO-FG = 9
                    ADD 1 TO SUB
                    GO TO NEXT-IC.

      * CL DECREASING
           IF LN-INSURED = "J"
              MOVE 2 TO IC-SUB
           ELSE
              MOVE 1 TO IC-SUB.
      * CL LEVEL
           IF LN-INSTYPES-LL
              ADD 2 TO IC-SUB.
      * PP
           IF SUB = 3
              IF LN-INSTYPES-PP
                 MOVE 6 TO IC-SUB
              ELSE
      * PP2 - OPTIONAL
                 MOVE 7 TO IC-SUB.

      * A&H
           IF SUB = 2
              MOVE 5 TO IC-SUB.
      * OT1
           IF SUB = 4
              MOVE 8 TO IC-SUB.
      * OT2
           IF SUB = 5
              MOVE 9 TO IC-SUB.
      * OT3
           IF SUB = 6
              MOVE 10 TO IC-SUB.


           IF LN-INSPREM(SUB) NOT = 0
              IF IC-COMMRATE(IC-SUB) >= 0
                 COMPUTE LN-INSCOMM(SUB) ROUNDED =
                           IC-COMMRATE(IC-SUB)
                                 / 100 * LN-INSPREM(SUB)
              ELSE
                 COMPUTE LN-INSCOMM(SUB) = IC-COMMRATE(IC-SUB) * -1
              END-IF
           ELSE
              MOVE 0 TO LN-INSCOMM(SUB).

           ADD 1 TO SUB.
           GO TO NEXT-IC.

       DONE-LN.

           PERFORM REWRITE-LN1-FILE.
           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-IC1-FILE.

           GO TO NEXT-GROUP-LOC.

      /**********************************
      *   INITIAL ROUTINE
      ***********************************
       INITIAL-ROUTINE SECTION.
           MOVE 0 TO Q-POS.
           MOVE 0 TO C-POS.
           MOVE 0 TO P-POS.
           MOVE "V" TO Q-BUF.
           MOVE "1" TO C-BUF.
           MOVE "00" TO P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE " " TO EXT-EOBUF-ERRCD.

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROGRAM IN PROGRESS..........." TO MESS
              PERFORM SEND-LEGEND.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF (BEG-BRANCH = 0 AND END-BRANCH = 0)
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

       EO-EXEC-AUDIT SECTION.

       ENTER-PARAMETERS SECTION.

           IF VDUSTATUS = "1U"
              GO TO ENTER-INSCOMP.

       BR-01.
           MOVE "F7-EXIT, F1, F3-ALL, ^, ENTER BEGINNING BRANCH"
                                                     TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "R  A000BR1." TO VDU.
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
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "F7-EXIT, F1, ^-BACK, ENTER ENDING BRANCH" TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
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
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       CL-01.
           MOVE "F7-EXIT, F1, F3-ALL, ^, ENTER BEGINNING CLASS"
                                                     TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
           MOVE "F7-EXIT, F1, ^-BACK, ENTER ENDING CLASS" TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              MOVE " INVALID LOAN CLASS RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO ENTER-BEGACT.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       ENTER-BEGACT.
           MOVE "F7-EXIT, F1, ^-BACK, ENTER BEG NUMBER" TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN060BEGACT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "16" GO TO ALL-ACCOUNTS.
           IF VDUSTATUS NOT = "00" GO TO ENTER-BEGACT.
           MOVE VDUNUM0 TO BEG-ACT.
       ENTER-ENDACT.
           MOVE "F7-EXIT, F1, ^-BACK, ENTER END NUMBER" TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN060ENDACT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ENTER-BEGACT.
           IF VDUSTATUS NOT = "00" GO TO ENTER-ENDACT.
           MOVE VDUNUM0 TO END-ACT.
           IF BEG-ACT > END-ACT
              MOVE "INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEGACT.
           GO TO ENTER-INSCOMP.
       ALL-ACCOUNTS.
           MOVE "SNNN060BEGACT." TO VDU.
           MOVE 0 TO BEG-ACT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060ENDACT." TO VDU.
           MOVE 999999 TO END-ACT VDUNUM0.
           PERFORM SCREENIO.

       ENTER-INSCOMP.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN020INS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ENTER-ENDACT.
           IF VDUSTATUS = "16" GO TO ALL-INSCOMP.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-INSCOMP.
           MOVE VDUNUM0 TO INS-COMPANY.
           IF INS-COMPANY = 0
              GO TO ENTER-INSCOMP.
           GO TO  ENTER-ACCEPT.

       ALL-INSCOMP.
           MOVE "SNNN020INS." TO VDU.
           MOVE 00 TO INS-COMPANY VDUNUM0.
           PERFORM SCREENIO.
       ENTER-ACCEPT.
           MOVE "F7-EXIT, F1, ^-BACK, ACCEPT (Y/N):" TO LEGEND.
           PERFORM SEND-MSEL.

       EXIT-PARM.
           EXIT.

       DISPLAY-PARAMETERS SECTION.

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "SN N020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N060BEGACT." TO VDU.
           MOVE BEG-ACT TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N060ENDACT." TO VDU.
           MOVE END-ACT TO VDUNUM0.
           PERFORM SCREENIO.

       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO INSCOM-CONAME.
           MOVE EXT-CONAME-SYSDATE TO INSCOM-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY INSCOM-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE INSCOM-HELP--FILE TO HELP-LINK-FILE.
           MOVE INSCOM-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/INSCOM_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************
      *   MISCELLANEOUS PARAGRAPHS *
      ******************************
       MISC-PARA SECTION.
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       SEND-ACCOUNT.
           MOVE LN-OWNBR TO DISPLAY-MESS-BR.
           MOVE LN-CLASS TO DISPLAY-MESS-CL.
           MOVE LN-ACCTNO TO DISPLAY-MESS-LN.
           MOVE DISPLAY-MESS TO MESS.
           PERFORM SEND-LEGEND.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-MSEL.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************
      *      I-O ERROR ROUTINE SECTION
      *******************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLN3IN.CPY".
       COPY "LIBLP/LPIC1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

       END-ROUTINE SECTION.
       END-PROGRAM.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00" 
              MOVE "FX/FXMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY INSCOM-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
