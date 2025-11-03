       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BCHKI0.
       DATE-WRITTEN.    101100.
      *****************************************************************
      *                 (LP)
      *  DESCRIPTION:   BATCH CHECK PRINTING
      *
      *
      * LONPI0 = CASH RECEIPTS
      * DISBI0 = CASH DISBURSEMENTS
      * CHCKI0 = CHECK WRITING
      * VOIDI0 = VOID CHECKS
      * BNKDI0 = BANK DEPOSITS
      * BNKWI0 = BANK WITHDR
      * BCHKI0 = BATCH CHECK WRITING
      *
      * REV:
      * GLB 030325 WORLD PR# 320;RECOMPILE DUE TO CHECKTRANSW/CHECKTRANS
      * BLF 060719 ADD PA-FILE & GPENV FOR UPDATE OF YTD PAID AMT IN
      *            PAFILE IN CHECKTRANS, DEVELOPMENT PR# 2633
      * BAH 140502 RENAMED FROM 6ONPI0, VERYANT PROGRAMS CANT START NUMERIC
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      * BAH 150713 CHANGED DIRECT-CHAN-TABLE IN CHECKTRANSW TO ALLOW YOU
      *            TO CHANGE FIELDS 5 THROUGH 8, ADR2 - ZIP, REGMGM PR# 4786
      *  CS 160201 REMOVED GIFILE
      * BLM 160511 REMOVE LPSETDS & LPSETDSW
      * BAH 170508 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181009 GLOBAL CXFILE
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      *****************************************************************
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
           "@(#)LP/BCHKI0.CBL S35 02/20/24-15:25:08 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".

      *-----------------------------------------------------------
       COPY "LIBLP/CHECKTRANSW.CPY".
      *-----------------------------------------------------------

       COPY "LIBLP/LPIPBUF.CPY".
       COPY "LIBGB/CAPASSWDW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  DLRVNO3-SEQ            PIC 9  VALUE 0.
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBWI/CKSCANW.CPY".
       COPY "LIBWI/CXSCANW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BCHKI_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/BCHKI_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/BCHKI_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *****************************************
       MAIN-PROGRAM SECTION.
      *****************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BCHKI" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

           PERFORM GET-GPENV.

           PERFORM GET-GLOBAL.

           PERFORM GET-BRANCH.

           MOVE 0 TO Q-POS C-POS.
           MOVE BCHKI-PRU-POS    TO P-POS.
           MOVE "N" TO Q-BUF.
           MOVE "01" TO C-BUF.

           MOVE "S  A020PRU." TO VDU.
           MOVE 00 TO VDUBUF IP-PRU.
           PERFORM SCREENIO.

      * INITIALIZED IN CASE OF EXIT BEFORE FD-FILE KEY IS SET
           MOVE 0 TO FD-FMNO.
           MOVE 0 TO IP-FMNO.

           MOVE "N" TO IP-AOK.
       SETRP.
           MOVE "F7-MENU, ENTER PRINTER NUMBER:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "N" TO STAT.
           PERFORM PRU-IN.
           IF STAT = "1<"
              PERFORM PRU-OUT
              GO TO SETRP.
           IF STAT = "1>"
              PERFORM PRU-OUT
              DESTROY ALL CONTROLS
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO SETRP
              ELSE
                 MOVE 0 TO LOAN-KEY
                 MOVE "R" TO ROUTE-CD
                 GO TO END-PROG.
           IF STAT NOT = "00"
              GO TO SETRP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    PRINTER ALREADY RESERVED AT THIS POINT.
      *    IF A NEW PROGRAM IS SELECTED VIA THE MENU,
      *    MOVE "R" TO ROUTE-CD TO REMOVE LFN 26.
      *    ROUTE-CD REMAINS " " FOR PRINTING.
      *    UPON EXITING THIS PROGRAM ERRCD SHOULD
      *    ALWAYS BE "S" (TO USE SPECIAL TCL SUB-ROUTINES).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      * NEED TO INITIALIZE WHAT THESE FIELDS WERE IN THE A15 FORM THAT
      * THE FLDVAL WOULD HAVE SUCKED IN;

           MOVE SPACES TO IP-PROG BATCH-PR.
           MOVE "N" TO IP-AOK QUEUE-PRINT.
           MOVE "Y" TO CONT-PRINT.
           MOVE "P" TO IP-PRFG.

      * IF UP ARROW AFTER ALIGNMENT; IP-FMNO = "  5" & GIVES READ ERROR
           IF IP-FMNO NOT NUMERIC
              MOVE 5 TO IP-FMNO.

       RETURN-FROM-PRINT.
           MOVE " " TO ROUTE-CD.
           MOVE "S" TO ERRCD.
           IF IP-AOK = "R"
      * RETURNING FROM PRINT PROGRAM
              PERFORM RESET-SCREEN
              PERFORM SEND-PRU-FORM
              MOVE "Y" TO IP-AOK.
      * F7 FROM FORM PRINT
           IF IP-AOK = "X"
              GO TO MENU-KEY.
      * 1U FROM FORM PRINT
           IF IP-AOK = "U"
              MOVE "N" TO IP-AOK
              GO TO SETRP.
      * TEST THAT FORM WAS NOT ALIGNED
           IF IP-AOK NOT = "Y"
              GO TO TEST-FMNO.
      * RECHECK ALIGNMENT
           PERFORM GET-FD1.
           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO FORM-ALIGNED-YN.

      *---------------------------------------------------------
      *   E X I T      P R O G R A M
      *---------------------------------------------------------
       MENU-KEY.
           DESTROY ALL CONTROLS.
           PERFORM MENU-SELECTION.
      * ??
           IF MENU-FG = 99
              PERFORM PRU-OUT
              GO TO ENTER-TRANS-TYPE
           ELSE
              GO TO END-ROUTINE.

      *********************************
      *   P R I N T   O P T I O N S   *
      *********************************
       ENTER-PROG.
      * TO OVERRIDE M2 AS DEFAULT CHECK PROG (CTL P FROM FMNO)
           MOVE "ENTER PROGRAM ID OF PRINT PROGRAM:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020IP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-PROG.
           MOVE VDUBUF TO PRINT-PROG.
       ENTER-FMNO.
           MOVE "F7-MENU, ^-PRU, ENTER FORM NUMBER:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "FMX." TO MESS.
           PERFORM RESET-MESS.
           MOVE "EN N030FM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              PERFORM PRU-OUT
              DESTROY ALL CONTROLS
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-FMNO
              ELSE
                 MOVE 0 TO LOAN-KEY
                 MOVE "R" TO ROUTE-CD
                 GO TO END-PROG.
           IF VDUSTATUS = "1U"
              GO TO SETRP.
           IF VDUSTATUS = "1P"
              GO TO ENTER-PROG.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FMNO.
           MOVE VDUNUM0 TO IP-FMNO.
       TEST-FMNO.
           MOVE "SN N030FM." TO VDU.
           MOVE 5 TO IP-FMNO VDUNUM0.
           PERFORM SCREENIO.
       GET-FMNO.
           PERFORM GET-FD1.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO ENTER-FMNO.
           MOVE "S  A000FMX." TO VDU.
           MOVE FD-DESC TO VDUBUF.
           PERFORM SCREENIO.
           IF FD-PROG NOT = GB-CHECK-PROG
              MOVE "THIS IS NOT YOUR CHECK PRINT FORM" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FMNO.

       FORM-ALIGNED-YN.
           MOVE FD-PROG TO IP-PROG.
           IF PRINT-PROG NOT = " "
              MOVE PRINT-PROG TO IP-PROG.
           MOVE "Y" TO IP-AOK.
           PERFORM RESET-SEL.

      *********************************
      *   C H E C K   E N T R Y       *
      *********************************
       MAIN-MODULE.
           MOVE SPACES TO DISPLAY-BUF DISPLAY-HEAD-BUF.
           IF FIRST-TIME = 0
              PERFORM GET-CX-PRINT-COUNT
              MOVE 1 TO FIRST-TIME.
           PERFORM RESET-SCREEN.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "PRU"
              GO TO ENTER-FMNO.
           IF IO-TYPE = "CLR"
              GO TO END-ROUTINE.
      *    IF IO-TYPE = "END"
      *       GO TO END-ROUTINE.

      *    PERFORM ENTRY-MODULE.

      *---------------------------------------------------------
      * CALL PRINT PROGRAM: LONPM2, LONP2M, ETC.
      *---------------------------------------------------------
           MOVE "Y" TO BATCH-PR.

      * PUT IN LPIPBUF TO PASS
      *    CALL XFER USING STAT PRINT-RANGE-LIST PRINT-RANGE-BUF.

           IF IO-TYPE = "END"
              MOVE IP-PROG TO PROG-WK-IP
              MOVE PROG-WK TO FORM-NAM
              PERFORM XFER-FM-FIELDS
              CALL FORM-PROGX USING FORM-PATH PROG-BUF IP-BUF PRU-PATH
              CANCEL FORM-PROGX
              MOVE 0 TO FIRST-TIME
              GO TO RETURN-FROM-PRINT.

           GO TO MAIN-MODULE.

      ************************************
       ENTER-KEYS SECTION.
      ************************************

      * PRU AND FMNO NOT REENTERED UPON F1 RESET.
           PERFORM SEND-PRU-FORM.

       ENTER-TRANS-TYPE.
           MOVE "N" TO CXFILE-SCAN-FLAG.
           MOVE 0 TO NEW-FLAG CHECK-NO.
           MOVE SPACES TO IO-TYPE.

       CHANGE-P-R.

           MOVE "ENTER 'P'-PRINT NEW FORMS, 'R'-REPRINT FORMS:"
                                                     TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PRFG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM CXFILE-SCAN
              GO TO CHANGE-P-R.
           IF VDUSTATUS = "1>"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS = "1U"
              MOVE "PRU" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS NOT = "00" GO TO CHANGE-P-R.

           MOVE VDUBUF TO IP-PRFG.
           IF IP-PRFG NOT = "P" AND NOT = "R"
              GO TO CHANGE-P-R.

      *-----------------------------------------------------------------

      * INITIAL-PRINT
           IF IP-PRFG = "P"
              GO TO DONE-P-R.

      * TEST PASSWORD IF REPRINTING CHECKS
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT = "OK" GO TO ENTER-CK1.
           MOVE "NOT AUTHORIZED TO REPRINT CHECKS" TO MESS.
           PERFORM SEND-MESS.
           GO TO CHANGE-P-R.

       ENTER-CK1.
           MOVE "ENTER CHECK NO.S TO REPRINT...." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "EN N060CK1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM CKFILE-SCAN
              IF IO-FG = 9
                 GO TO ENTER-CK1
              ELSE
                 MOVE CKSCANW-NO TO CK1
                 MOVE "SN N060CK1." TO VDU
                 MOVE CK1 TO VDUNUM0
                 PERFORM SCREENIO
                 GO TO ENTER-CK2.
           IF VDUSTATUS = "1>"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS = "1U"
              GO TO CHANGE-P-R.
           IF VDUSTATUS NOT = "00" GO TO ENTER-CK1.
           IF VDUNUM0  = 0 GO TO ENTER-CK1.
           MOVE VDUNUM0 TO CK1.
       ENTER-CK2.
           MOVE "EN N060CK2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM CKFILE-SCAN
              IF IO-FG = 9
                 GO TO ENTER-CK2
              ELSE
                 MOVE CKSCANW-NO TO CK2
                 MOVE "SN N060CK2." TO VDU
                 MOVE CK2 TO VDUNUM0
                 PERFORM SCREENIO
                 GO TO DONE-P-R.
           IF VDUSTATUS = "1>"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS = "1U" GO TO ENTER-CK1.
           IF VDUSTATUS NOT = "00" GO TO ENTER-CK2.
           IF VDUNUM0  = 0 GO TO ENTER-CK2.
           MOVE VDUNUM0 TO CK2.

       DONE-P-R.
           MOVE "END" TO IO-TYPE.
           GO TO EXIT-ENTER-KEYS.
       EXIT-ENTER-KEYS.
           EXIT.


      ****************************************
       CXFILE-SCAN SECTION.
      ****************************************
           MOVE BRANCH TO CXSCANW-BRNO.

           MOVE "WI/CXSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CXSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF CXSCAN-STATUS NOT = "00"
              MOVE 9 TO IO-FG
              GO TO EXIT-CXFILE-SCAN.

           MOVE CXSCAN-CX1-KEY TO CX1-KEY.
           PERFORM OPEN-CX1-FILE.
           PERFORM READ-CX1-FILE.
           PERFORM CLOSE-CX1-FILE.
           IF IO-FG = 9
              GO TO EXIT-CXFILE-SCAN.

           MOVE 0 TO IO-FG.

       EXIT-CXFILE-SCAN.
           EXIT.

      *****************************************
       CKFILE-SCAN SECTION.
      *****************************************
           MOVE "Y"       TO CKSCAN-NOSTOP-FG.
           MOVE BRANCH    TO CKSCANW-BRNO.
           MOVE 0         TO CK-NO CKSCANW-NO.
           MOVE "WI/CKSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CKSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CKSCAN-STATUS NOT = "00"
              MOVE 9 TO IO-FG
           ELSE
              MOVE 0 TO IO-FG
              MOVE CKSCAN-CK1-KEY TO CK1-KEY.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " "  TO PASSWDW-STAT.
           MOVE 0    TO PASSWDW-SEQ.
           MOVE "BCHKI0" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO AUTHORIZE REPRINTING"    TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.


      **************************************
       GET-CX-PRINT-COUNT SECTION.
      **************************************

           MOVE 0 TO HOLD-SEQ.

           PERFORM OPEN-CX1-FILE.


           MOVE BRANCH  TO CX-BRNO
                           QCX1-WBEG-BRNO
                           QCX1-WEND-BRNO.
           MOVE SPACES  TO CX-TYPE
                           QCX1-WBEG-TYPE.
           MOVE ALL "Z" TO QCX1-WEND-TYPE.   
           MOVE 0       TO CX-SEQNO
                           QCX1-WBEG-SEQNO.
           MOVE ALL "9"  TO QCX1-WEND-SEQNO.

           PERFORM START-CX1-FILE.
       NEXT-CX-PRINT-COUNT.
           PERFORM READ-CX1-FILE-NEXT.
           IF IO-FG = 9 OR (CX-BRNO NOT = BRANCH) 
              GO TO EXIT-GET-CX-PRINT.
      *    UNLOCK CX-FILE.

           IF CX-IN-RESCISSION
              GO TO NEXT-CX-PRINT-COUNT.

           ADD 1 TO HOLD-SEQ.
           GO TO NEXT-CX-PRINT-COUNT.
       EXIT-GET-CX-PRINT.
           PERFORM CLOSE-CX1-FILE.
           MOVE "SN S060COUNT." TO VDU.
           MOVE HOLD-SEQ TO VDUNUM0.
           PERFORM SCREENIO.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPMENU.CPY".
       COPY "LIBGB/DATER.CPY".

      ********************************************
       FORM-RESET SECTION.
      ********************************************
           DISPLAY BCHKI-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE BCHKI-FORM-FIELDS TO WR-BUF.
           MOVE BCHKI-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.


      ********************************************
       SETUP-LINK-INFO SECTION.
      ********************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE BCHKI-HELP--FILE TO HELP-LINK-FILE
              MOVE BCHKI-HELP--DIR TO HELP-LINK-DIR.

      ********************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BCHKI_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       ALIGNMENT-PRINT.
           MOVE "MC" TO IP-PROG.
           MOVE "N" TO IP-AOK.
           PERFORM RESET-SEL.
           MOVE IP-PROG TO PROG-WK-IP.
           MOVE PROG-WK TO FORM-NAM.
           PERFORM XFER-FM-FIELDS.
           PERFORM CLOSE-FILES.
           CALL FORM-PROGX USING FORM-PATH PROG-BUF IP-BUF PRU-PATH.
           CANCEL FORM-PROGX.
           MOVE FD-PROG TO IP-PROG.
           IF PRINT-PROG NOT = " "
              MOVE PRINT-PROG TO IP-PROG.
           MOVE "Y" TO IP-AOK.

       GET-FD1.
           MOVE IP-FMNO TO FD-FMNO.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
       GET-GLOBAL.
           PERFORM OPEN-GB-FILE.
           MOVE BRANCH TO GB-BRNO.
           MOVE 1      TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
       GET-BRANCH.
           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "BRANCH RECORD NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.
           MOVE "SN N040BR." TO VDU.
           MOVE BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       NOT-FOUND.
           MOVE "NOT FOUND" TO MESS.
           PERFORM SEND-MESS.
       RESET-SCREEN.
           MOVE SPACES TO DISPLAY-BUF.
           PERFORM REFRESH-SCREEN.
       REFRESH-SCREEN.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-PRU-FORM.
           MOVE "S  A000PRU." TO VDU.
           MOVE IP-PRU TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN N030FM." TO VDU.
           MOVE IP-FMNO TO VDUNUM0.
           PERFORM SCREENIO.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       RESET-SEL.
           MOVE "S  A000SEL." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
       XFER-FM-FIELDS.
           MOVE "BCHKI0" TO LPLOAN-CALLED-FROM.
           MOVE FD-FMNO  TO IP-FMNO.
      * IP-PROG INITIALLY M2.
           IF FD-PRCONT = "Y"
              MOVE "1"   TO CONT-PRINT
           ELSE
              MOVE "0"   TO CONT-PRINT.

      * USED TO SET CHFG'S AFTER CHECK IS SELECTED VIA SCAN
       SET-SCAN-CHFG-TABLE.
           IF CX-PGM = "B2"
              MOVE DIRECT-CHAN-TABLE TO CHFG-TABLE.
           IF P-CODE(PAYEE-SUB) = "LN"
              IF CX-PGM = "D0"
                 MOVE DIRECT-CHAN-TABLE TO CHFG-TABLE
              ELSE
                 IF CX-PGM = "F2" OR "FM"
                    MOVE TRANS-CHAN-TABLE TO CHFG-TABLE.


      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCX1IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      **************************************
       END-ROUTINE SECTION.
      **************************************
           PERFORM PRU-OUT.
       END-PROG.
           PERFORM XFER-FM-FIELDS.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DESTROY BCHKI-SCREEN.
       STOP-RUN.
           EXIT PROGRAM.
