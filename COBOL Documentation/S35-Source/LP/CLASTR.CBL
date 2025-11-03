       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CLASTR.
       DATE-WRITTEN. 10-13-94
      ******************************************************************
      *
      *    DIR :  LP
      *    DESC:  CLASS TRANSFER MENU
      *           SELECT CLASSES TO TRANSFER TO ANOTHER CLASS
      *           AND PICK MENU SELECTION
      *
      * ---->NOTE:
      *           REPO ACCOUNTS, UNLESS IN P&L STATUS,
      *           CURRENTLY CAN NOT BE TRANSFERRED
      *
      *=================================================================
      * REV:
      * 101494 BAH REMOVED ALL OCCURANCES OF LP2
      * 100695 BLL PASSWORD TRANSFERS ACROSS MACHINES
      * 091896 JTG ADDED COPY OF "LIBLP/CLASTRW"
      * 102596 MJD ADDED F6 - VIEW REPORTS OPTION
      * 081997 BAH REMOVED TEST ON SMALLOAN FLAG CONSISTENCY
      * GLF 090297 REMOVED ALL USE OF GLOBAL
      * GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *            MEETING (082599)
      *  CS 101107 ADDED 'FROM' CLASS RANGE REGIONAL ACCEPT 277
      *  CS 101130 TEST GP-SECURITY-FG, IF 'Y', NEW SECURITY CALLS GB/CHKSEC
      *            INSTEAD OF OLD PROGRAM WITH BITS IN THE FORM PR# NEWSECURITY
      *  CS 101130 FIXED 'FROM' CLASS RANGE, WAS ALLOWING FROM CLASS 2 TO BE
      *            < THAN FROM CLASS   PR#277
      * BLM 111130 ACTIVATE NEW SECURITY ROUTINES, PR# NEWSECURITY
      *
      * KEC 2017.0223 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED WS CLASS FIELDS TO MATCH LN-CLASS & LC-CODE.
      *
      ******************************************************************
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
           "@(#)./LP/CLASTR.CBL S35 09/20/22-14:06:41 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LPWSCLAS.CPY".


       COPY "LIBLP/CLASTRW.CPY".
       01  TRANSFER-BUF.
           03  FR-LC              PIC 9(3).
           03  FR-LC-X           REDEFINES FR-LC PIC X(3).
           03  FR2-LC             PIC 9(3).
           03  FR2-LC-X          REDEFINES FR2-LC PIC X(3).
           03  TO-LC              PIC 9(3).
           03  TO-DLNO            PIC 9(4)V99.

      **********************
      *   MISC WORKERS     *
      **********************
       01  ERRCD                PIC X        VALUE " ".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  TEST-LC              PIC 9(2).
       01  FR-LC-SMALLOAN       PIC X.
       01  FR-LC-SALEFIN        PIC X.
       01  FR-LC-SERVACCT       PIC X.
       01  FR-LC-LOANTYPE       PIC X.

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
      *----------------------------------------------------------------
      * PROGRAM     SEQ     DESCRIPTION
      * ------      ---     -------------------------------------------
      * CLASTR       0      ACCEPTING A TRANSFER FOR ANOTHER BRANCH.
      *                     INITIATING A TRANSFER FROM ANOTHER BRANCH.
      *                     (YOUR HOME BRANCH BASED ON USERID.)
      *
      * CLASTR       1      TRANSFER BETWEEN DIFFERENT MACHINES.
      *
      * CLASTR       2      TRANSFER OF MULTIPLE ACCOUNTS.
      *                      ( MORE THAN 1 AT A TIME.)
      *----------------------------------------------------------------
       COPY "LIBGB/PASSWDW.CPY".
      * SEQUENCE 0 AND 2 ARE IN  LP/CLAST1.C
      * WHEN ADDING PASSWORD SEQUENCES, THE LP/CLAST1.C PROGRAM USES "CLASTR"
      * AS THE PROGRAM IN THE PASSWORD FILE, SO YOU MUST MAKE SURE THE
      * SEQUENCES STAY IN SYNC WITH EACH OTHER.

       01  CLASTR-WINDOW-HANDLE PIC X(10).
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CLASTR_DEF.CPY".
       COPY "LIBLP/CLASTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".


      ****************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/CLASTR_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CLASTR" TO VDU-FORM-NAME.

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
                   HANDLE IS CLASTR-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           GO TO END-PROGRAM.


      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE " " TO ERRCD.

       FR-CLASS.
           MOVE "ENNN020FRLC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM CLASS-SCAN
              IF IO-BAD
                 GO TO FR-CLASS
              ELSE
                 MOVE "SNNN020FRLC." TO VDU
                 MOVE LCSCAN-KEY TO VDUNUM0
                 PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO INITIALIZATION-ABORT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FR-CLASS.
           MOVE VDUNUM0 TO FR-LC D-FR-LC.


       FR2-CLASS.
           MOVE "ENNN020FRLC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM CLASS-SCAN
              IF IO-BAD
                 GO TO FR2-CLASS
              ELSE
                 MOVE "SNNN020FRLC2." TO VDU
                 MOVE LCSCAN-KEY TO VDUNUM0
                 PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO INITIALIZATION-ABORT.
           IF UP-KEY
              GO TO FR-CLASS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FR2-CLASS.
           MOVE VDUNUM0 TO FR2-LC D-FR2-LC.

           IF FR2-LC < FR-LC
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO FR-CLASS.


      *-----CANT DO THESE TESTS HERE WITH CLASS RANGE
           IF NOT (FR-LC = FR2-LC)
              MOVE "WARNING: MULTIPLE FROM CLASSES" TO MESS
              PERFORM SEND-MESS
              GO TO TO-CLASS.

      *-----FROM CLASS & FROM CLASS 2 ARE THE SAME
           MOVE FR-LC TO TEST-LC.
           PERFORM VERIFY-CLASS.
           IF STAT NOT = "00"
              MOVE "CAN'T ACCESS THE CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO FR-CLASS.

           MOVE LC-SMALLOAN TO FR-LC-SMALLOAN.
           MOVE LC-SALEFIN TO FR-LC-SALEFIN.
           MOVE LC-SERVACCT TO FR-LC-SERVACCT.
           MOVE LC-LOANTYPE TO FR-LC-LOANTYPE.

      *--------------------------------------------
       TO-CLASS.
           MOVE "ENNN020TOLC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM CLASS-SCAN
              IF IO-BAD
                 GO TO TO-CLASS
              ELSE
                 MOVE "SNNN020TOLC." TO VDU
                 MOVE LCSCAN-KEY TO VDUNUM0
                 PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO INITIALIZATION-ABORT.
           IF VDUSTATUS = "10" GO TO FR-CLASS.
           IF VDUSTATUS = "1U" GO TO FR2-CLASS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TO-CLASS.
           MOVE VDUNUM0 TO TO-LC D-TO-LC.
           IF FR-LC = TO-LC
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO TO-CLASS.

           MOVE TO-LC TO TEST-LC.
           PERFORM VERIFY-CLASS.
           IF STAT NOT = "00"
              MOVE "CAN'T ACCESS THE CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO TO-CLASS.
           MOVE "S  A180TOLCN." TO VDU.
           MOVE LC-DESC TO VDUBUF D-TO-LC-DESC.
           PERFORM SCREENIO.

      *VERIFY CLASSES ARE SAME GENERAL TYPES FOR TRANSFER:
      *IF NOT A RANGE OF CLASSES
           IF NOT ( FR-LC = FR2-LC)
              GO TO ACCEPT-IT.

           IF NOT (LC-SMALLOAN = FR-LC-SMALLOAN)
           IF NOT (LC-SALEFIN = FR-LC-SALEFIN
                     AND LC-SERVACCT = FR-LC-SERVACCT
                      AND LC-LOANTYPE = FR-LC-LOANTYPE
                  )
              MOVE "INCONSISTANT CLASSES" TO MESS
              PERFORM SEND-MESS
              GO TO TO-CLASS.


       ACCEPT-IT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO INITIALIZATION-ABORT.
           IF VDUSTATUS = "10" GO TO FR-CLASS.
           IF VDUSTATUS = "1U" GO TO TO-CLASS.
           IF VDUSTATUS = "1<"
              PERFORM CALL-PERUSE
              GO TO ACCEPT-IT.
           IF VDUSTATUS NOT = "00" GO TO ACCEPT-IT.
           IF NOT (VDUBUF = "1"  OR "2") GO TO ACCEPT-IT.
           IF VDUBUF = 1
              MOVE "LP/CLAST1" TO FORM-NAM
              CALL "CLAST1" USING FORM-PATH TRANSFER-BUF EXIT-PATHNAME
              CANCEL "CLAST1"
              PERFORM SET-FR-TO
              IF STAT = "00"
                 GO TO ACCEPT-IT
              ELSE
                 MOVE SPACES TO WR-BUF
                 MOVE TRANSFER-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 MOVE SPACES TO WR-BUF
                 MOVE LCN-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
           ELSE
              MOVE "LP/CLAST2" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH TRANSFER-BUF
              CANCEL FORM-PROGX
              GO TO ACCEPT-IT.

           GO TO INITIALIZATION-EXIT.

       INITIALIZATION-ABORT.
           MOVE "X" TO ERRCD.

       INITIALIZATION-EXIT.
           EXIT.

      *****************************************************
       SET-FR-TO SECTION.
      *****************************************************

      * SET FROM CLASS:
           MOVE "SNNN020FRLC." TO VDU.
           MOVE FR-LC TO VDUNUM0 D-FR-LC.
           PERFORM SCREENIO.

           MOVE "SNNN020FRLC2." TO VDU.
           MOVE FR2-LC TO VDUNUM0 D-FR2-LC.
           PERFORM SCREENIO.

      *SET TO CLASS:
           MOVE "SNNN020TOLC." TO VDU.
           MOVE TO-LC TO VDUNUM0 D-TO-LC.
           PERFORM SCREENIO.
           IF  FR-LC = TO-LC OR FR2-LC = TO-LC
              MOVE "EQ" TO STAT
              GO TO SET-FR-TO-EXIT.
           MOVE TO-LC TO TEST-LC.
           PERFORM VERIFY-CLASS.
           IF STAT NOT = "00"
              GO TO SET-FR-TO-EXIT.
           MOVE "S  A180TOLCN." TO VDU.
           MOVE LC-DESC TO VDUBUF D-TO-LC-DESC.
           PERFORM SCREENIO.

       SET-FR-TO-EXIT.
           EXIT.

      *******************************
       VERIFY-CLASS SECTION.
      *******************************
           MOVE TEST-LC TO LC-CODE.
           PERFORM GET-LC.
           IF IO-BAD
              MOVE "N" TO STAT
           ELSE
              MOVE "00" TO STAT.

       VERIFY-CLASS-EXIT.
           EXIT.

      ***********************************
       CLASS-SCAN SECTION.
      ***********************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-CLASS-SCAN.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      *******************************************************
       CALL-PERUSE SECTION.
      *******************************************************
           MOVE "GB/PERUSE" TO FORM-NAM.
           CALL "PERUSE" USING FORM-PATH EXIT-PATHNAME.
           CANCEL "PERUSE".

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO CLASTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CLASTR-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CLASTR-SCREEN UPON CLASTR-WINDOW-HANDLE.
           MOVE CLASTR-FORM-FIELDS TO WR-BUF.
           MOVE CLASTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE CLASTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLASTR-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CLASTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************************************
       MISC-ROUTINES SECTION.
      *******************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".

       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       GET-LC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.

      ****************************************
       IO-ROUTINE SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LC1-FILE.
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPLC1RN.CPY".

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
           DESTROY CLASTR-SCREEN.
           DESTROY CLASTR-WINDOW-HANDLE.
           EXIT PROGRAM.
