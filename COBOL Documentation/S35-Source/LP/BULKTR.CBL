       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BULKTR.
       DATE-WRITTEN. 10-13-94
      *****************************************************
      *    DIR :  LP
      *    DESC:  BULK TRANSFER MENU
      *           SELECT BRANCHES FOR TRANSFER IN BULK
      *           AND PICK MENU SELECTION
      *
      * ---->NOTE:
      *           REPO ACCOUNTS, UNLESS IN P&L STATUS,
      *           CURRENTLY CAN NOT BE TRANSFERRED
      *
      * REV:
      *  BAH 101494 REMOVED ALL OCCURANCES OF LP2
      *  BLL 100695 PASSWORD TRANSFERS ACROSS MACHINES
      *  JTG 091896 ADDED COPY OF "LIBLP/BULKTRW"
      *  MJD 102596 ADDED F6 - VIEW REPORTS OPTION.
      *  GLF 090297 REMOVED ALL USE OF GLOBAL
      *  MJD 123197 CHANGED TO TEST PASSWORD BEFORE ATTEMPTING AN ACCESS
      *             CALL ACROSS MACHINES.
      * GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *            MEETING (082599)
      *  CS 101130 CHECK GP-SECURITY-FG IF 'Y', NEW SECURITY SO CALL GB/CHKSEC
      *            INSTEAD OF OLD ROUTINES WITH BITS IN THE FORM PR#NEWSECURITY
      * BLM 111130 ACTIVATE NEW SECURITY ROUTINE IN A15, PR# NEWSECURITY
      *  CS 150418 MISSING EXIT-PATHNAME ON CALL TO BULKT1 AND BULKT2
      *  CS 191008 WORK FILE (BULK) AND THE TAG UPDATES (BULKUPD) ARE NOW
      *           CREATED IN THE GLOBAL DP DIRECTORY. ONLY 1 TAG UPDATE
      *           TAG IS NECESSARY
      *****************************************************
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./LP/BULKTR.CBL S35 09/20/22-13:23:06 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LPWSBULK.CPY".

       COPY "LIBLP/BULKTRW.CPY".
       01  TRANSFER-BUF.
           03  FR-BR              PIC 9999.
           03  FR-BR-X           REDEFINES FR-BR
                                  PIC XXXX.
           03  FR-MACHINE         PIC XX.
           03  FR-DATA-PATH       PIC X(50).
           03  TO-BR              PIC 9999.
           03  TO-MACHINE         PIC XX.
           03  TO-DATA-PATH       PIC X(50).
           03  TO-DLNO            PIC 9(4)V99.
           03  TRANSFER-ACCT      PIC 9(8).
           03  BLKONE-BUF         PIC X       VALUE " ".

      **********************
      *   MISC WORKERS     *
      **********************
       01  ERRCD                PIC X        VALUE " ".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".

      *----------------------------------------------------------------
      * PROGRAM     SEQ     DESCRIPTION
      * ------      ---     -------------------------------------------
      * BULKTR       0      ACCEPTING A TRANSFER FOR ANOTHER BRANCH.
      *                     INITIATING A TRANSFER FROM ANOTHER BRANCH.
      *                     (YOUR HOME BRANCH BASED ON USERID.)
      *
      * BULKTR       1      TRANSFER BETWEEN DIFFERENT MACHINES.
      *
      * BULKTR       2      TRANSFER OF MULTIPLE ACCOUNTS.
      *                      ( MORE THAN 1 AT A TIME.)
      *----------------------------------------------------------------
       COPY "LIBGB/PASSWDW.CPY".
       01   MACHINE-SEQ       PIC 9  VALUE 1.
      * SEQUENCE 0 AND 2 ARE IN  LP/BULKT1.C
      * WHEN ADDING PASSWORD SEQUENCES, THE LP/BULKT1.C PROGRAM USES "BULKTR"
      * AS THE PROGRAM IN THE PASSWORD FILE, SO YOU MUST MAKE SURE THE
      * SEQUENCES STAY IN SYNC WITH EACH OTHER.
       01  BULKTR-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BULKTR_DEF.CPY".
       COPY "LIBLP/BULKTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/BULKTR_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BULKTR" TO VDU-FORM-NAME.

      * CANT USE THE GETFM2 COPY MEMBER BECAUSE THE EXIT PATH IS NOT
      * SET UP PROPERLY, SO YOU MUST MOVE SPMENU, IF SECURITY IS SET !!
      * OR THE PROGRAM JUST LOOPS.....

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS BULKTR-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.


           GO TO END-PROGRAM.


      *******************************************
       INITIALIZATION SECTION.
      *******************************************
           MOVE " " TO ERRCD.

       FR-BRANCH.
           MOVE "ENNN040FRBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO INITIALIZATION-ABORT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FR-BRANCH.
           MOVE VDUNUM0 TO FR-BR D-FR-BR.
           MOVE FR-BR TO BR-NO.
           PERFORM GET-BR.
           IF NOT IO-GOOD
              MOVE "CAN'T ACCESS THE BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO FR-BRANCH.
           MOVE "S  A210FRBRN." TO VDU.
           MOVE BR-NAME TO VDUBUF D-FR-BR-NAME.
           PERFORM SCREENIO.

           MOVE BR-STATE     TO FR-STATE.
           MOVE BR-MACHINE   TO FR-MACHINE.
           MOVE BR-DATA-PATH TO FR-DATA-PATH.
       TO-BRANCH.
           MOVE "ENNN040TOBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO INITIALIZATION-ABORT.
           IF VDUSTATUS = "10" GO TO FR-BRANCH.
           IF VDUSTATUS = "1U" GO TO FR-BRANCH.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TO-BRANCH.
           MOVE VDUNUM0 TO TO-BR D-TO-BR.
           IF FR-BR = TO-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO TO-BRANCH.
           MOVE TO-BR TO BR-NO.
           PERFORM GET-BR.
           IF NOT IO-GOOD
              MOVE "CAN'T ACCESS THE BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO TO-BRANCH.
           MOVE "S  A180TOBRN." TO VDU.
           MOVE BR-NAME TO VDUBUF D-TO-BR-NAME.
           PERFORM SCREENIO.

           MOVE BR-MACHINE   TO TO-MACHINE.
           MOVE BR-DATA-PATH TO TO-DATA-PATH.
           MOVE 0            TO TO-DLNO.

           IF FR-MACHINE NOT = TO-MACHINE
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO INITIALIZATION-ABORT.

      * TEST FILE ACCESS
           MOVE EXT-FILPATH-BASE TO BULK-ALL-BASE.
           MOVE FR-MACHINE       TO BULK-ALL-MACHINE.
           PERFORM VERIFY-DP-EXISTS.
           IF STAT NOT = "00"
              MOVE "CAN'T ACCESS 'DP DIRECTORY" TO MESS
              PERFORM SEND-MESS
              GO TO FR-BRANCH.

      *    MOVE EXT-FILPATH-BASE TO BULK-ALL-BASE.
      *    MOVE TO-MACHINE TO BULK-ALL-MACHINE.
      *    MOVE TO-DATA-PATH TO BULK-ALL-DATA.
      *    PERFORM VERIFY-BRANCH.
      *    IF STAT NOT = "00"
      *       MOVE "CAN'T ACCESS 'TO' BRANCH" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO TO-BRANCH.

       ACCEPT-IT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO INITIALIZATION-ABORT.
           IF VDUSTATUS = "10" GO TO FR-BRANCH.
           IF VDUSTATUS = "1U" GO TO TO-BRANCH.
           IF VDUSTATUS = "1<"
              PERFORM CALL-PERUSE
              GO TO ACCEPT-IT.
           IF VDUSTATUS NOT = "00" GO TO ACCEPT-IT.
           IF NOT (VDUBUF = "1"  OR "2") GO TO ACCEPT-IT.
           IF VDUBUF = 1
              MOVE "LP/BULKT1" TO FORM-NAM
              CALL "BULKT1" USING FORM-PATH TRANSFER-BUF EXIT-PATHNAME
              CANCEL "BULKT1"
              PERFORM SET-FR-TO
              IF STAT = "00"
                 GO TO ACCEPT-IT
              ELSE
                 MOVE SPACES TO WR-BUF
                 MOVE TRANSFER-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 MOVE SPACES TO WR-BUF
                 MOVE BRN-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
           ELSE
              MOVE "LP/BULKT2" TO FORM-NAM
              CALL "BULKT2" USING FORM-PATH TRANSFER-BUF EXIT-PATHNAME
              CANCEL "BULKT2"
              GO TO ACCEPT-IT.

           GO TO INITIALIZATION-EXIT.

       INITIALIZATION-ABORT.
           MOVE "X" TO ERRCD.

       INITIALIZATION-EXIT.
           EXIT.

      *******************************************
       SET-FR-TO SECTION.
      *******************************************

      * SET FROM BRANCH:
           MOVE "SNNN040FRBR." TO VDU.
           MOVE FR-BR TO VDUNUM0 D-FR-BR.
           PERFORM SCREENIO.

           MOVE FR-BR TO BR-NO.
           PERFORM GET-BR.
           IF NOT IO-GOOD
              GO TO SET-FR-TO-EXIT.
           MOVE "S  A210FRBRN." TO VDU.
           MOVE BR-NAME TO VDUBUF D-FR-BR-NAME.
           PERFORM SCREENIO.

           MOVE BR-STATE     TO FR-STATE.
           MOVE BR-MACHINE   TO FR-MACHINE.
           MOVE BR-DATA-PATH TO FR-DATA-PATH.

      *SET TO BRANCH:
           MOVE "SNNN040TOBR." TO VDU.
           MOVE TO-BR TO VDUNUM0 D-TO-BR.
           PERFORM SCREENIO.
           IF FR-BR = TO-BR
              MOVE "EQ" TO STAT
              GO TO SET-FR-TO-EXIT.
           MOVE TO-BR TO BR-NO.
           PERFORM GET-BR.
           IF NOT IO-GOOD
              GO TO SET-FR-TO-EXIT.
           MOVE "S  A180TOBRN." TO VDU.
           MOVE BR-NAME TO VDUBUF D-TO-BR-NAME.
           PERFORM SCREENIO.


           MOVE BR-MACHINE TO TO-MACHINE.
           MOVE BR-DATA-PATH TO TO-DATA-PATH.
           MOVE "00" TO STAT.

       SET-FR-TO-EXIT.
           EXIT.

      *************************************
       VERIFY-DP-EXISTS SECTION.
      *************************************
     
           IF BULK-ALL-MACHINE = " "
              GO TO VERIFY-DP-EXISTS-EXIT.

           MOVE EXT-FILPATH-BASE TO BULK-ALL-BASE.
           MOVE BULK-DIR TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "N" TO STAT.

       VERIFY-DP-EXISTS-EXIT.
           EXIT.

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "BULKTR" TO PASSWDW-PGM.
           MOVE MACHINE-SEQ TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO TRANSFER ACROSS MACHINES..:" TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL "PASSWD" USING FORM-PATH PASSWDW-BUF.
           CANCEL "PASSWD".
           IF PASSWDW-STAT NOT = "OK" MOVE
              "INTER-MACHINE TRANSFERS ARE NOT ALLOWED WITHOUT PASSWORD"
                                                       TO MESS
              PERFORM SEND-MESS.



      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO BULKTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BULKTR-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY BULKTR-SCREEN UPON BULKTR-WINDOW-HANDLE.
           MOVE BULKTR-FORM-FIELDS TO WR-BUF.
           MOVE BULKTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE BULKTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE BULKTR-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BULKTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       MISC-ROUTINES SECTION.
      *******************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       CALL-PERUSE.
           MOVE "GB/PERUSE" TO FORM-NAM.
           CALL "PERUSE" USING FORM-PATH EXIT-PATHNAME.
           CANCEL "PERUSE".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       GET-BR.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

      ****************************************
       IO-ROUTINE SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "SP/SPMENU" TO  FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BULKTR-SCREEN.
           DESTROY BULKTR-WINDOW-HANDLE.
           EXIT PROGRAM.
