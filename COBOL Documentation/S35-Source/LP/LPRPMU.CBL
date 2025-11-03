       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPRPMU.
      *************************************************************************
      *
      *  DIR : LP
      *  DESC: LOAN REPORTS MENU
      *
      * REV:
      *  MJD 071195 ADDED LP/DELRPT
      *  MJD 101795 ADDED LP/BNKRGA, GEORGIA LOAN TAX REPORT
      *  MJD 103095 ADDED LP/BNKDGA, GEORGIA LOAN TAX REPORT DETAIL
      *  KEC 050196 ADDED "LIBGB/MENUFILES";
      *             ADDED 'DEALER SERVICE/AFP REPORT' OPTION.
      *  KEC 050996 REMOVED "LIBGB/MENUFILES";
      *             REMOVED 'DEALER SERVICE/AFP REPORT' OPTION.
      *  BAH 071196 ADDED 24. AUTO INSURANCE REPORT
      *  KEC 121196 ADDED 25. RESCISSION INDEX REPORT
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  KEC 041497 ADDED 26. LOAN APPLICATION REPORT
      *  BLV 052997 PUT BACK 27(HIDDEN PROGR2) WIPED OUT BY KEC 41497
      *  JTG 111897 REMOVED 'DELRPT.C' DELINQUENCY REPORT OPTIONS SINCE THE
      *             COLLECTION REPORT 'CPCORP.C' NOW HAS ALL IT'S FUNCTIONALLY
      *  MJD 011598 ADDED LP/BWAGE
      *  MJD 022498 ADDED LP/ICMOR2.C FOR WORLD
      *  KEC 072798 ADDED #33 ZIPCODE INQUIRY REPORT
      *  MJD 990330 ADDED LNAGE2 - TFC'S AGED TRIAL BAL
      *  MJD 000110 ADDED LNAGD2 - TFC'S AGED TRIAL BAL BY BRNO/DLNO/LPO
      *             (TFC PR#64)
      *  BLV 000329 SELECTION #15, IF GP-I-AM-MERC CALL MFPROG (CUSTOM)
      *             INSTEAD OF PROGRP, MERC PR# 223
      *  BAH 000810 REMOVED SELECTIO # 32 LP/ICMOR2 , WAS A ONE TIME RUN
      *             FOR WORLD, ITS CONFUSING CUSTOMERS, OK'D BY JR
      *  MJD 000908 ADDED LP/DASHBD - DEALER DASH BOARD REPORT (REGACC #1289)
      *  BLV 001122 SELECTION #15, IF GP-I-AM-WDT CALL PROG48 (CUSTOM)
      *             INSTEAD OF PROGRP, WDT PR# 1340
      *  GLB 010328 SELECTION #36,LP/COLLET COLLECTION LETTER RPT PR#256
      *  BLV 010425 CHANGED SELECTION #27, IF GP-I-AM-MERC MAKE THIS THE
      *             NET PROGRESS REPORT; MERC PR# 240
      *  BLV 010426 CHANGED SELECTION #19 FROM BNKRGA TO BHLIST, BNKRGA WAS
      *             GIVING "PROGRAM NOT FOUND"
      *  BLV 010719 CHANGED SELECTION #35 TO RUN LASTPY IF I-AM-WORLD
      *             (INSTEAD OF LNAGD2), WORLD PR# 270
      *  BAH 020115 ADDED F4-PAGE 2 REPORT MENU, LPR2MU
      *  BAH 020115 MOVED COLLET, LASTPY AND LNAGD2 TO LPR2MU
      *  BAH 041208 CHANGED 20 FROM BNKDGA TO A MENU, GATXMU, WORLD 364
      *   CS 080719 APPRPT PROGRAM WAS REMOVED, ITS NOT IN QA OR SV, BUT THE
      *             MENU WAS NOT FIXED #25
      *  BAH 2024.0808 REMOVED 10. GAGILA AS PER KEVIN S35Q-112
      *  BAH 2025-0115 ADDED 34. AGED TRIAL BALANCE    S35Q-156
      *************************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/LPRPMU_DEF.CPY".
       COPY "LIBLP/LPRPMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/LPRPMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPRPMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "LPRPMU" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

           ACCEPT GP-REC FROM ENVIRONMENT "GPENV".

      * SECURITY CHECK
           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           CALL "CHKSEC" USING CHKSEC-FIELDS.
           CANCEL "CHKSEC".
           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

           IF CHKSEC-STAT NOT = "00"
              MOVE "1>" TO STAT VDUSTATUS
              GO TO MENU-EXIT.

       GET-MENU-SELECTION-CLR.
           MOVE 0 TO VDU-PROGRAMMING.

      ************************************************
      *    GET SELECTION
      ************************************************
       GET-MENU-SELECTION.
           MOVE FORM-LIST TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "E  N020SEL." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUNUM0 TO FORM-BUF.

           IF VDUSTATUS = "10"
              MOVE 1 TO RETURN-CODE
              GO TO EXIT-PROG.

       MENU-EXIT.
           IF VDUSTATUS = "18"
              MOVE "LP/LPR2MU" TO FORM-NAM
              GO TO RUN-PROG.

           IF VDUSTATUS = "1>"
              MOVE "LP/LPMENU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF VDUSTATUS = "1<"
              MOVE "GB/PERUSE" TO FORM-NAM
              GO TO RUN-PROG.

           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

      * MUST INITIALIZE FOR MENU SELECTIONS, IF PREVIOUSLY RAN IN BATCH
      * THE EXT-ROUTE-BUF WONT BE CORRECT
           MOVE "00" TO EXT-ROUTE-BUF.
           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10
                 M11 M12 M13 M14 M15 M16 M17 M18 M19 M20
                 M21 M22 M23 M24 M25 M26 M27 M28 M29 M30
                 M31 M32 M33 M34
              DEPENDING ON FORM-BUF.
           GO TO GET-MENU-SELECTION.

       M01. MOVE "LP/LNAGED" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. GO TO GET-MENU-SELECTION.
       M03. MOVE "LP/BWLNRP" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. GO TO GET-MENU-SELECTION.
       M05. MOVE "LP/BYANRP" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. GO TO GET-MENU-SELECTION.
       M07. MOVE "LP/LNAGPL" TO FORM-NAM.
            GO TO RUN-PROG.
       M08. GO TO GET-MENU-SELECTION.
       M09. MOVE "LP/UERNRP" TO FORM-NAM.
            GO TO RUN-PROG.
       M10. GO TO GET-MENU-SELECTION.
       M11. GO TO GET-MENU-SELECTION.
       M12. GO TO GET-MENU-SELECTION.
       M13. MOVE "LP/NONACR" TO FORM-NAM.
            GO TO RUN-PROG.
       M14. MOVE "LP/AVAILC" TO FORM-NAM.
            GO TO RUN-PROG.
       M15. MOVE "LP/PROGRP" TO FORM-NAM.
            GO TO RUN-PROG.
       M16. GO TO GET-MENU-SELECTION.
       M17. GO TO GET-MENU-SELECTION.
       M18. GO TO GET-MENU-SELECTION.
       M19. MOVE "LP/BHLIST" TO FORM-NAM.
            GO TO RUN-PROG.
       M20. MOVE "LP/GATXMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M21. MOVE "LP/COLRPT" TO FORM-NAM.
            GO TO RUN-PROG.
       M22. MOVE "LP/COLRP2" TO FORM-NAM.
            GO TO RUN-PROG.
       M23. GO TO GET-MENU-SELECTION.
       M24. MOVE "LP/RXREPT" TO FORM-NAM.
            GO TO RUN-PROG.
       M25. GO TO GET-MENU-SELECTION.
       M26. GO TO GET-MENU-SELECTION.
       M27. MOVE "LP/OPREPT" TO FORM-NAM.
            GO TO RUN-PROG.
       M28. MOVE "LP/OPREP2" TO FORM-NAM.
            GO TO RUN-PROG.
       M29. GO TO GET-MENU-SELECTION.
       M30. MOVE "LP/LNAURP" TO FORM-NAM.
            GO TO RUN-PROG.
       M31. MOVE "LP/AULNRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M32. GO TO GET-MENU-SELECTION.
       M33. MOVE "LP/ZIPCOD" TO FORM-NAM.
            GO TO RUN-PROG.
       M34. MOVE "LP/LNAGE2" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPRPMU-SCREEN.

           MOVE FORM-PATH TO FORM-PATHNAME.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF RETURN-CODE = 1
              GO TO EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPRPMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO DISPLAY-MENU.

       EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-FINISH' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPRPMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPRPMU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LPRPMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE LPRPMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPRPMU-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY LPRPMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPRPMU-FORM-FIELDS TO WR-BUF.
           MOVE LPRPMU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       MISC-ROUTINES SECTION.
      *************************************************
       COPY "LIBGB/SENDMESS.CPY".
      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPRPMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

