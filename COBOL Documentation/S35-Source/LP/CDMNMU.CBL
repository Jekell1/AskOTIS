       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CDMNMU.
       AUTHOR.        KEC.
       DATE-WRITTEN.  022399.
      **************************************************************************
      *
      *   IDENTIFICATION: LP/CDMNMU.C
      *
      *   DESCRIPTION   : CODE FILE MAINTENANCE MENU
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 022399 **     **
      * MJD 990303 ADDED "BS" CODE FOR REFNO
      * GLF 990305 ADDED "LO" CODE FOR LOAN PROCESSING OFFICE, "PC" CODE
      *            FOR PROCESS, AND "RC" CODE FOR REP
      * GLF 990830 ADDED "TS" CODE FOR TRW SPECIAL CODES FOR TFC PR#51
      * BLV 010924 ADDED "FU" CODE FOR LOAN FOLLOW-UP CODE, PR# 1487
      * JTG 020201 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS  WORLD #288
      * BAH 020626 ADDED "LS" REPO REASON CODE AND "CH" CHARGE OFF REASON
      *            FOR REGACC PR# 1775
      * JTG 021011 ADDED NEW CODE FILE TYPE 'BR' - BATCH PAYMENT REFERENCE CODES
      *            BATCH PAYMENT REVISIONS                          REGACC #0006
      * KEC 030929 {PR#00040} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            ADDED "28. TRIAD SCORING MAINTENANCE"
      *  CS 040809 ADD 29. TRAN CODE PAYMENT POST REFNO CODES [LENDMARK PR#2353
      * KEC 041119 {PR#02407} LENDMARK <A15> [KEVIN MULLEN]
      *            ADDED "30. ADVERTISING SOURCE CODES"
      * BAH 050914 ADDED 31. DEFERMENT REASON CODES, LENDMARK PR# 2568
      * JTG 050927 ADDED 32. SF VOID (UNWIND) CODES
      *            NEW CODE TYPE "UW" UNWIND SALES FINANCE VOID
      *            LN-POCD-UNWIND "*U" A LN-POCD-NONLOAN
      *                  AND LN-UNWIND-REASON-CODE                  REGACC #0105
      * BAH 051027 ADDED 33. FROZEN ACCT REASON CODES "FZ", REGACC PR# 114
      * BLF 060908 ADDED "PA" CODE FOR PAYROLL CODES, TIMESHEET PROCESSING,
      *            WORLD PR# 488
      * MJD 070815 ADDED 35. KNOW YOUR CUSTOMER ID TYPE "ID" REGENCY #3032
      * BAH 071211 ADDED 36. PARTICIPATION HELD REGACC PR# 185
      * BLM 081017 ADDED 37. AUCTION HOUSE (VNFILE) REGACC PR# 216
      * BLM 110128 ADDED 38. "JT" JOB TYPE CODES, WORLD PR# 739
      * BAH 110411 ADDED F4 CDM2MU, REGACC PR# 290
      * BAH 190125 REMOVED M30 CDASMN, NOT USED
      * BAH 190129 REMOVED M28 CDLRMN, TRIAD, NOT USED
      * BAH 190129 REMOVED M01, M09, M14 TRIAD AND REVOLVING, NOT USED
      * BAH 190702 REMOVED M36 PFMAIN PT0051
      * BAH 191105 REMOVED M37 AUMAIN
      * BAH 2023.0720 MOVED PAGE 2 SELECTIONS TO PAGE 1, REMOVED CDM2MU
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      **************************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/CDMNMU_DEF.CPY".
       COPY "LIBLP/CDMNMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/CDMNMU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CDMNMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "CDMNMU" TO VDU-FORM-NAME.
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
           IF ( VDUSTATUS = "1>" )
              MOVE "LP/LPMNMU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF ( VDUSTATUS NOT = "00" )
              GO TO GET-MENU-SELECTION.

           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10
                 M11 M12 M13 M14 M15 M16 M17 M18 M19 M20
                 M21 M22 M23 M24 M25 M26 M27 M28 M29 M30
                 M31 M32 M33 M34 M35 M36 M37 M38
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. GO TO GET-MENU-SELECTION.

      *-----------------------------------{ BORROWER HOUSING CODE FILE }
       M02. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "BW" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-------------------------------------{ CAN LOAN GROUP ID'S FILE }
       M03. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "CL" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----------------------------------------{ COLLATERAL CODE FILE }
       M04. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "CO" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *------------------------------------------------{ FEE CODE FILE }
       M05. MOVE "MN/GCMAIN" TO FORM-NAM.
            GO TO RUN-PROG. 

      *---------------------------------------{ FORM CHARGES CODE FILE }
       M06. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "FC" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.
      
      *----------------------------------------{ LATE CHARGE CODE FILE }
       M07. MOVE "MN/ELMAIN" TO FORM-NAM.
            GO TO RUN-PROG. 

       M08. GO TO GET-MENU-SELECTION.
       M09. GO TO GET-MENU-SELECTION.

      *-----------------------------------------{ P & L TYPE CODE FILE }
       M10. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "PX" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *--------------------------------------------{ PAY OFF CODE FILE }
       M11. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "PO" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *----------------------------------{ PHONE NUMBER TYPE CODE FILE }
       M12. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "PH" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-------------------------------------------------{ POOL ID FILE }
       M13. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "PL" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

       M14. GO TO GET-MENU-SELECTION.

      *--------------------------------------------{ PURPOSE CODE FILE }
       M15. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "PU" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *---------------------------------------------{ REFNO CODE FILE }
       M16. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "BS" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *---------------------------------------------{ SOURCE CODE FILE }
       M17. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "SR" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----------------------------{ LOAN PROCESSING OFFICE CODE FILE }
       M18. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "LO" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *--------------------------------------------{ PROCESS CODE FILE }
       M19. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "PC" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *------------------------------------------------{ REP CODE FILE }
       M20. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "RC" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *----------------------------------------{ TRW SPECIAL CODE FILE }
       M21. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "TS" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----------------------------------------------{ FOLLOW-UP CODE }
       M22. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "FU" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *----------------------------------------{ CITY TAX PERCENT CODE }
       M23. MOVE "MN/CYMAIN" TO FORM-NAM.
            GO TO RUN-PROG.

      *--------------------------------------{ COUNTY TAX PERCENT CODE }
       M24. MOVE "MN/CTMAIN" TO FORM-NAM.
            GO TO RUN-PROG.

      *---------------------------------------------{ REPO REASON CODES)
       M25. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "LS" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *---------------------------------------{ CHARGE OFF REASON CODES)
       M26. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "CH" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-------------------------------------{ BATCH (PAYMENT) REF CODES}
       M27. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "BR" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

       M28. GO TO GET-MENU-SELECTION.

      *-------------------------------------{ TRANS CODE PMNT REF CODES}
       M29. MOVE "MN/TCMAIN" TO FORM-NAM.
            GO TO RUN-PROG.

       M30. GO TO GET-MENU-SELECTION.

      *-------------------------------------{ DEFERMENT REASON CODES }
       M31. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "DF" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *--------------------------------{ SALES FIN UNWIND REASON CODES }
       M32. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "UW" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-------------------------------------{ FROZEN ACCT REASON CODES }
       M33. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "FZ" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      * PAYMENT RESCHEDULE REASON
       M34. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "PS" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      * KNOW YOUR CUSTOMER ID TYPE
       M35. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "ID" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.
      * COLLATERAL LOCATION CODE
       M36. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "XL" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.
      * BORROWER ORIGIN CODE
       M37. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "OR" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.
      *-------------------------------------{ JOB TYPE }
       M38. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "JT" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CDMNMU-SCREEN.

           MOVE FORM-PATH TO FORM-PATHNAME.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

           IF RETURN-CODE = 1
              GO TO EXIT-PROG.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CDMNMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "CDMNMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CDMNMU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CDMNMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE CDMNMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE CDMNMU-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           DISPLAY CDMNMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE CDMNMU-FORM-FIELDS TO WR-BUF.
           MOVE CDMNMU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       MISC-ROUTINES SECTION.
      **************************************************
       COPY "LIBGB/SENDMESS.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CDMNMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

