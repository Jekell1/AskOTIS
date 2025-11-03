       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPM2MU.
      ******************************************************************
      *
      *  DIR : LP
      *  DESC: SECOND LOAN MAINTENANCE MENU
      *
      *=================================================================
      *  REV.
      *  KEC 020996 CHANGED THE CALLING OF MAINTENANCES FOR THE CD-FILE.
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  JTG 040897 ADDED (99 F5) TO GO TO SPCOPY.C PROGRAM
      *             ADDED (99 F6) TO GO TO GICOPY.C PROGRAM
      *  KEC 092898 ADDED #31 REVOLVING PROGRAM CODE FILE MAINTENANCE.
      *  KEC 100798 REVISED TO:
      *                  #31 REVOLVING PROGRAM FILE
      *                  #32 REVOLVING STATEMENT FILE
      *                  #33 REVOLVING TRANSACTION FILE
      *  JTG 101698 CHANGED TO HOUSE ALL LOGIC FOR 'RP' IN RPMAIN.C
      *             AND NOT SHARE CODE WITH CDMAIN.C SO HELP FACILITY
      *             COULD FIND SPECIFIC HELP FOR 'RP'
      *  KEC 021999 ADDED #35 REVOLVING CONVERSION MAINT. (TEMP)
      *              THIS IS ONLY TEMPORARY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      *  KEC 022399 REVISED TO PUT REVOLVING MAINTENANCES ON SEPERATE
      *              MENU (LP/RVMNMU).
      *  KEC 030299 ADDED OPTION TO GO TO THE REVISED SECONDARY MAINT.
      *              MENU (LP/LPM2M2.C)
      *  BLV 990722 ADDED SELECTION 32 BRANCH INFO, BYMISC, WORLD
      *  KEC 000919 {PR#00000} PARADATA FINANCIAL [KEN]
      *              ADDED '33. SCRATCH PAD FILE MAINTENANCE'
      *   CS 030404  ADDED #34. PAYMENT REFCD CHECK/MO  [PR#2034 CASHCO]
      *  KEC 030428 {PR#00026} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *              ADDED '35. LOAN TRIAD FILE MAINTENANCE'
      *  CS  030812  CHANGED #17 TO RMMAIN.(USED TO BE LRMAIN) BUT WAS
      *              OVERWRITTEN WITH TRIAD MAINTENANCE
      *  KEC 040120 {PR#00046} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *              ADDED '36. TRIAD BEHAVIOR FILE MAINTENANCE'
      *  JTG 040219 ALLOWED NON MONTHLY CAN LOANS            WORLD J043
      *             ADDED FLDVAL ON FIELD 'NONMONTHLY' BR-NON-MONTHLY-CAN-LOANS
      *  CS  040610 ADDED '37 LOAN MEMO MAINTENANCE
      *  BLF 060711 ADDED SELECTION #38, PAYEE FILE MAINTENANCE, DEVELOPMENT
      *             PR# 2633
      *  KEC 060724 {PR#00127} REGIONAL ACCEPT. <A15> [KEVIN MULLEN]
      *             CHANGED F4-KEY TO GO TO "LP/LPM3MU".
      *
      *  KEC 060816 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *             ADDED F9-KEY TO GO TO "MN/MNMENU".
      *
      *  KEC 061011 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *             MOVE "LRMAIN" AND "LRBMAN" TO LPM3MU.
      *             MOVE "PCPGRM" FROM LPM3MU.
      *
      *  BLF 070410 ADD TSMAN1 (EMPLOYEE TIME SHEET MAINTENANCE) AS
      *             SELECTION 36, WORLD PR# 488
      *  MJD 131121 CHANGED 23 TRMNM2 TO DO A CALL FORM PATH AS IT POPS
      *             A WINDOW AS OPPOSED TO ANOTHER MENU. DID NOT WANT TO
      *             CALL IT WITH LOADCO.
      * KEC 2015.1118 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         RENAMED MN/TRMNMU (A15) TO MN/MNTRMU (A30).
      *  BLM 161223 FIX TO CANCEL MNTRMU, IT WAS CANCELING FORM-PROGX
      *             WHICH CONTAINED LPM2MU
      * BAH 190204 REMOVED 31. RVMNMU REVOLVING MENU
      * BAH 20190702 REMOVED 32. TSMAN1 TIME SHEET PT0161
      *  CS 20190726 REMOVED 34. NFMAIN
      *  CS 201900806 REMOVED 30 MAMAIN (APPLICATION)
      * BAH 2023.0908 REMOVED 38 PAMAIN
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      * BAH 2024.0925 SELECTIONS 30, 31, AND 34 ARE ON PAGE 3 AND SHOULD NOT
      *               HAVE BEEN ON THIS SECOND MENU S35Q-131
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/LPM2MU_DEF.CPY".
       COPY "LIBLP/LPM2MU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/LPM2MU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPM2MU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "LPM2MU" TO VDU-FORM-NAME.
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
           IF ( VDUSTATUS = "1:" )
              IF ( FORM-BUF = 99 )
                 MOVE "SP/SPCOPY" TO FORM-NAM
                 GO TO RUN-PROG.

           IF ( VDUSTATUS = "1<" )
              IF ( FORM-BUF = 99 )
                 MOVE "SP/GICOPY" TO FORM-NAM
                 GO TO RUN-PROG.

           IF ( VDUSTATUS = "18" )
              MOVE "LP/LPM3MU" TO FORM-NAM
              GO TO RUN-PROG.

           IF ( VDUSTATUS = "1>" )
              MOVE "LP/LPMNMU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF ( VDUSTATUS NOT = "00" )
              GO TO GET-MENU-SELECTION.

           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10
                 M11 M12 M13 M14 M15 M16 M17 M18 M19 M20
                 M21 M22 M23 M24 M25 M26 M27 M28 M29 M30
                 M31 M32 M33 M34 M35 M36 M37
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

      *=================================================================
       M01. MOVE "MN/LCMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "MN/STMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "MN/SPMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. MOVE "MN/RBMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M05. MOVE "MN/DSMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. MOVE "MN/BWMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M07. MOVE "MN/LNMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M08. MOVE "MN/LPMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M09. MOVE "MN/LEMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M10. MOVE "MN/LHMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M11. MOVE "MN/CKMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M12. MOVE "MN/BYMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M13. MOVE "MN/NOMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M14. MOVE "MN/DTMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M15. MOVE "MN/CXMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M16. MOVE "MN/IRMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M17. MOVE "MN/RMMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M18. MOVE "MN/LTMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M19. MOVE "MN/PSMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M20. MOVE "WI/CLQUOT" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----GROUP LEVEL CODE FILE MAINTENANCE---------------------------
       M21. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "LV" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

       M22. MOVE "MN/GRMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M23. MOVE "MN/MNTRMU" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
            CANCEL "MNTRMU".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.
       M24. MOVE "MN/TYMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M25. MOVE "MN/FMMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M26. MOVE "GL/ALMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M27. MOVE "MN/EOMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M28. MOVE "MN/CAMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M29. MOVE "MN/BIMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M30.  GO TO GET-MENU-SELECTION.
       M31.  GO TO GET-MENU-SELECTION.
       M32. MOVE "MN/BYMISC" TO FORM-NAM.
            GO TO RUN-PROG.
       M33. MOVE "MN/PDMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M34.  GO TO GET-MENU-SELECTION.
       M35. MOVE "MN/PCPGRM" TO FORM-NAM.
            GO TO RUN-PROG.
       M36.  GO TO GET-MENU-SELECTION.
       M37. MOVE "MN/LMMAIN" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPM2MU-SCREEN.

           MOVE FORM-PATH TO FORM-PATHNAME.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

      * F1 TAKING YOU ALL THE WAY BACK TO LPMENU (MAIN MENU)
           IF RETURN-CODE = 1
              GO TO EXIT-PROG.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPM2MU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "LPM2MU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPM2MU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LPM2MU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LPM2MU-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPM2MU-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           DISPLAY LPM2MU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPM2MU-FORM-FIELDS TO WR-BUF.
           MOVE LPM2MU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       MISC-ROUTINES SECTION.
      ************************************************
       COPY "LIBGB/SENDMESS.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPM2MU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

