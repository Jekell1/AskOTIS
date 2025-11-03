       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BULKGR.
       DATE-WRITTEN. 3-19-03.
      *****************************************************
      *    DIR :  LP
      *    DESC:  BULK TRANSFER MENU BY GROUP MENU
      *           SELECT A GROUP OF BRANCHES FOR TRANSFER IN BULK
      *           TO ONE TARGET BRANCH
      *
      * ---->NOTE:
      *           REPO ACCOUNTS, UNLESS IN P&L STATUS,
      *           CURRENTLY CAN NOT BE TRANSFERRED
      *
      * REV:
      * CLS  101130 CHECK GP-SECURITY-FG, IF 'Y', NEW SECURITY CALLS GB/CHKSEC
      *             INSTEAD OF BITS IN THE FORM  PR# NEWSECURITY
      * BLM  111130 ACTIVATE NEW SECURITY ROUTINES IN A15, PR# NEWSECURITY
      *  CS  20191008 THE WORK FILE (GRBULK####) IS NOW IN THE GLOBAL DP
      *               DIRECTORY. BULKG2 CREATES 3 UPDATE TAGS, GRBULKUPD####,
      *               A TAG REPLACING GR WITH EACH BRANCH IN THE GROUP AND 
      *               INCLUDES THE NAME OF THE GROUP FOR EXAMPLE, IF FROM
      *               GROUP CS (MEMBERS BR# 3 AND 10), THE 2 TAGS WOULD BE
      *               0003CSBULKUPD0001, 0010CSBULKUPD0001, AND THE GROUP
      *               TAG CSBULKUPD0001
      *  CS 20191015 SAVED BULKGR.191015
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
           "@(#)./LP/BULKGR.CBL S35 09/20/22-13:22:37 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LPWSBULKG.CPY".

       COPY "LIBLP/BULKGRW.CPY".
       01  TRANSFER-BUF.
           03  FR-GR              PIC X(4).
           03  TO-BR              PIC 9999.
           03  TO-MACHINE         PIC XX.
           03  TO-DATA-PATH       PIC X(6).
           03  TO-DLNO            PIC 9(4)V99.


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
      * BULKGR       0      ACCEPTING A TRANSFER FOR ANOTHER BRANCH.
      *                     INITIATING A TRANSFER FROM ANOTHER BRANCH.
      *                     (YOUR HOME BRANCH BASED ON USERID.)
      *
      * BULKGR       1      TRANSFER BETWEEN DIFFERENT MACHINES.
      *
      * BULKGR       2      TRANSFER OF MULTIPLE ACCOUNTS.
      *                      ( MORE THAN 1 AT A TIME.)
      *----------------------------------------------------------------
       COPY "LIBGB/PASSWDW.CPY".
       01   MACHINE-SEQ       PIC 9  VALUE 1.
      * SEQUENCE 0 AND 2 ARE IN  LP/BULKG1.C
      * WHEN ADDING PASSWORD SEQUENCES, THE LP/BULKG1.C PROGRAM USES "BULKGR"
      * AS THE PROGRAM IN THE PASSWORD FILE, SO YOU MUST MAKE SURE THE
      * SEQUENCES STAY IN SYNC WITH EACH OTHER.
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       01  BULKGR-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BULKGR_DEF.CPY".
       COPY "LIBLP/BULKGR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/BULKGR_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *************************************
       MAIN-MODULE SECTION.
      *************************************

      D    STOP MESS.
           PERFORM SQL-CONNECT.

           COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BULKGR" TO VDU-FORM-NAME.

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
                   HANDLE IS BULKGR-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           GO TO END-PROGRAM.


      ****************************************************
       INITIALIZATION SECTION.
      ****************************************************
           MOVE " " TO ERRCD.

       ENTER-FROM-GROUP.
           MOVE "E  A040FRGR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO INITIALIZATION-ABORT.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-FROM-GROUP
              ELSE
                 MOVE "S  A000FRGR." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-FROM-GROUP.

           MOVE VDUBUF TO WGR-WORD FR-GR.

           IF WGR-WORD-ZERO
              MOVE "YOU MUST SELECT A GROUP!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FROM-GROUP.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FROM-GROUP.

           IF WGR-TYPE NOT = "A"
              MOVE "YOU MUST SELECT A GROUP!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FROM-GROUP.

           MOVE "S  A210FRGRNAME." TO VDU.
           MOVE GR-NAME TO VDUBUF D-FR-GR-NAME.
           PERFORM SCREENIO.

       TO-BRANCH.
           MOVE "ENNN040TOBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO INITIALIZATION-ABORT.
           IF VDUSTATUS = "10" GO TO ENTER-FROM-GROUP.
           IF VDUSTATUS = "1U" GO TO ENTER-FROM-GROUP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TO-BRANCH.
           MOVE VDUNUM0 TO TO-BR D-TO-BR.
           MOVE TO-BR TO BR-NO.
           PERFORM GET-BR.
           IF NOT IO-GOOD
              MOVE "CAN'T ACCESS THE BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO TO-BRANCH.
           MOVE "S  A180TOBRNAME." TO VDU.
           MOVE BR-NAME TO VDUBUF D-TO-BR-NAME.
           PERFORM SCREENIO.

           MOVE BR-MACHINE   TO TO-MACHINE.
           MOVE BR-DATA-PATH TO TO-DATA-PATH.
           MOVE 0 TO TO-DLNO.

           MOVE EXT-FILPATH-BASE TO BULK-ALL-BASE.
           MOVE TO-MACHINE       TO BULK-ALL-MACHINE.
           PERFORM VERIFY-DP-EXISTS.
           IF STAT NOT = "00"
              MOVE "CAN'T ACCESS 'TO' BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO TO-BRANCH.

           PERFORM OPEN-GR1-FILE.
           MOVE FR-GR TO GR-ID.
           PERFORM SEARCH-GR-INIT.
           MOVE TO-BR TO WGR-SRESULT.
           PERFORM SEARCH-GR-VERIFY.
           IF WGR-ERR = 0
              MOVE "WARNING: TARGET BRANCH IS IN SOURCE GROUP!"
                 TO MESS
              PERFORM SEND-MESS
              MOVE "ACCOUNTS IN TARGET BRANCH WILL BE SKIPPED."
                 TO MESS
              PERFORM SEND-MESS.
           PERFORM CLOSE-GR1-FILE.

       ACCEPT-IT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO INITIALIZATION-ABORT.
           IF VDUSTATUS = "10" GO TO ENTER-FROM-GROUP.
           IF VDUSTATUS = "1U" GO TO TO-BRANCH.
           IF VDUSTATUS = "1<"
              PERFORM CALL-PERUSE
              GO TO ACCEPT-IT.
           IF VDUSTATUS NOT = "00" GO TO ACCEPT-IT.
           IF NOT (VDUBUF = "1"  OR "2") GO TO ACCEPT-IT.

           IF VDUBUF = 1
              MOVE "LP/BULKG1" TO FORM-NAM
              CALL "BULKG1" USING FORM-PATH TRANSFER-BUF
              CANCEL "BULKG1"
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
              MOVE "LP/BULKG2" TO FORM-NAM
              CALL "BULKG2" USING FORM-PATH TRANSFER-BUF
              CANCEL "BULKG2"
              GO TO ACCEPT-IT.

           GO TO INITIALIZATION-EXIT.

       INITIALIZATION-ABORT.
           MOVE "X" TO ERRCD.

       INITIALIZATION-EXIT.
           EXIT.

      *****************************************************
       SET-FR-TO SECTION.
      *****************************************************

      * SET FROM GROUP:
           MOVE "S  A040FRGR." TO VDU.
           MOVE FR-GR TO VDUBUF D-FR-GR.
           PERFORM SCREENIO.

           MOVE FR-GR TO GR-ID.
           PERFORM OPEN-GR1-FILE.
           PERFORM READ-GR1-FILE.
           PERFORM CLOSE-GR1-FILE.
           IF NOT IO-GOOD
              GO TO SET-FR-TO-EXIT.
           MOVE "S  A210FRGRNAME." TO VDU.
           MOVE GR-NAME TO VDUBUF D-FR-GR-NAME.
           PERFORM SCREENIO.

      *SET TO BRANCH:
           MOVE "SNNN040TOBR." TO VDU.
           MOVE TO-BR TO VDUNUM0 D-TO-BR.
           PERFORM SCREENIO.
           MOVE TO-BR TO BR-NO.
           PERFORM GET-BR.
           IF NOT IO-GOOD
              GO TO SET-FR-TO-EXIT.

           MOVE "S  A180TOBRNAME." TO VDU.
           MOVE BR-NAME TO VDUBUF D-TO-BR-NAME.
           PERFORM SCREENIO.

           MOVE BR-MACHINE TO TO-MACHINE.
           MOVE BR-DATA-PATH TO TO-DATA-PATH.
           MOVE "00" TO STAT.

       SET-FR-TO-EXIT.
           EXIT.

      *****************************************************
       VERIFY-DP-EXISTS SECTION.
      *****************************************************

           IF BULK-ALL-MACHINE = " " 
              GO TO VERIFY-DP-EXISTS-EXIT.

           MOVE BULK-DIR TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "N" TO STAT.

       VERIFY-DP-EXISTS-EXIT.
           EXIT.

      ***************************************************
       CALL-PERUSE SECTION.
      ***************************************************
           MOVE "GB/PERUSE" TO FORM-NAM.
           CALL "PERUSE" USING FORM-PATH EXIT-PATHNAME.
           CANCEL "PERUSE".

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO BULKGR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BULKGR-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY BULKGR-SCREEN UPON BULKGR-WINDOW-HANDLE.
           MOVE BULKGR-FORM-FIELDS TO WR-BUF.
           MOVE BULKGR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE BULKGR-HELP--FILE TO HELP-LINK-FILE.
           MOVE BULKGR-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BULKGR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************************************
       MISC-ROUTINES SECTION.
      *******************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".

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
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "SP/SPMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BULKGR-SCREEN.
           DESTROY BULKGR-WINDOW-HANDLE.
           EXIT PROGRAM.
