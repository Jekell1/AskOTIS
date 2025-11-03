       IDENTIFICATION DIVISION.
       PROGRAM-ID.      MEMOSC.
       DATE-WRITTEN.    122391.
      *********************************************************************
      *          MEMO INQUIRY / INPUT
      *
      *  REV:
      * MJD 012797 ADDED DELE-VERIFY-PASSWORD. ADDED PASSWORD TO DELETION
      *            OF MEMOS DATED PREVIOSLY TO TODAY.
      * BLV 031097 CHANGED TO USED COLLECTION VERSION OF MEMO ENTRY WITH
      *            NO STOPPING FOR DATE, SEQ, ACCEPT
      * GLF 101698 ADDING NOTES MEMOS AND 2000 DATE FOR NOTES MEMOS
      * GLF 102098 ADDING INVESTOR MEMOS AND 2000 DATE FOR INVESTOR MEMOS
      * BLV 990909 ADD TOGGLE TO DEALER MEMOS BASED ON GP FLAG, TFC PR# 40
      *  MJD 001004 ADDED LOGIC FOR LOAN MEMO TYPE "R" FOR REVERSALS.
      *             IF THIS OPTION IS TURNED ON AND THE USER DOES NOT ENTER
      *             MEMO TEXT THEN A CHANGE MEMO WILL BE GENERATED IN ITS
      *             PLACE. THE USER IS NOT ALLOWED TO DELETE THESE
      *             NEW TRANSACTION REVERSAL MEMOS.
      * GLB 001113  ADD VENDOR MEMO VM-REC
      * MJD/BLV 001116 FIXED BUG.  WHEN MEMO WINDOW IS OPENED AFTER
      *                POSTING REVERSAL, AND USER HITS F7 TO EXIT,
      *                THEY ARE PROMPTED TO ENTER VENDOR MEMO BEFORE
      *                EXITING.
      * BLV 020214 ADDED VERIFY-MEMO-PASSWORD TO AVOID CONFUSIONS &
      *            CONFLICTS WITH VERIFY-PASSWORD ROUTINE IN SPINQ1
      * BAH 021007 WIBMEMO - DISPLAY CONTACT PHONE NO. IN PHONE FORMAT, TFC
      * BAH 021024 ADDED "T" COLLATERAL MEMOS, PCCREDIT PR# 1940
      * BAH 030709 WILMEMO - FIX SEQ 99 AS 8TH MEMO ON PAGE 1, SEE NOTES
      *            IN WILMEMO COPY MEMBER....TOO MUCH TO DUPLICATE ! PL# 415
      * CS  041011 ADD COPY LIBSP/COURTW WITH LIST OF VALID COURT NUMBERS
      *            [MODERN PR#2388]
      * BAH 070313 CHANGED NAME OF MEMO-VERIFY-PASSWORD TO MEMO-PASSWORD-00-OK
      *            AND ADDED NEW ONES FOR 01-OK AND 02-OK, CAUSING TROUBLE
      *             REGENCY PL# 582 (WILMEMO AND WIBMEMO)
      * BLM 101109 IF I-AM-REGENCY, GET FORM WI/RGMEMO INSTEAD OF WI/MEMOSC,
      *            WILL ENTER SMALLER MEMO (51 CHARACTERS INSTEAD OF 60) SO
      *            WE CAN INCLUDE USERID IN MEMO, REGENCY PR# 3682
      * BAH 110208 REWRITING AN EXISTING MEMO WAS NOT WORKING, CHANGE
      *             WAS NOT BEING SAVED, MOVE VDUBUF WAS REMOVED FROM ABOVE
      *             CHANGE, PUT IT BACK. UNITED PL# 724
      *             JUST RECOMPILE THIS PROGRAM, CHANGED ALL WI?MEMO COPIES
      *
      * KEC 2017.0310 {PR#05265} FIRST METRO <A15> [JUDY REIFSTECK]
      *         ADDED FMETRO-MEMO FIELD.  ADDED FORM OF WI/MEMOFM.
      * BAH 170420 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 180912 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      * BAH 20191104 REMOVED VMFILE, VENDOR MEMO PT0192
      * BAH 20240102 REMOVED MEMOFM AND MEMORG
      * BAH 20240321 REMOVED COURTW
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/MEMOSC S35 08/15/23-15:21:44 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01DM.CPY".
       COPY "LIBLP/LP01DM_SQL.CPY".
       COPY "LIBLP/LPWSDM.CPY".

       01  MEMOSC-LINES                PIC 99  VALUE 13.
       01  MEMOSC-COLUMNS              PIC 99  VALUE 78.
       01  MEMOSC-BEGIN-Y              PIC 99  VALUE 2.
       01  MEMOSC-BEGIN-X              PIC 99  VALUE 10.

       01  ERRCD                 PIC X VALUE " ".

      *********************************************************
      *   LMFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LM-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LM-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LM-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LM-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LM-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE "/LP/LMFILE".

       01  MEMO-ENTERED-FG       PIC X      VALUE "N".
       01  LEGEND                PIC X(78).
       01  LEGEND-LIST           PIC X(7)    VALUE "MSEL:D.".
       01  LEGEND-W-LIST         PIC X(17)   VALUE
           "W0:S W1 W2 W3 W4.".

       01  LEGEND-LM-BM.
           03  FILLER            PIC X(14)   VALUE
               "SELECT TYPE: ".
           03  FILLER            PIC X(14)   VALUE
               "LOAN MEMO     ".
           03  FILLER            PIC X(14)   VALUE
               "BORROWER MEMO ".
           03  FILLER            PIC X(14)   VALUE
               "EXIT          ".
       01  LEGEND-LM-BM-DL.
           03  FILLER            PIC X(14)   VALUE
               "SELECT TYPE:  ".
           03  FILLER            PIC X(14)   VALUE
               "LOAN MEMO     ".
           03  FILLER            PIC X(14)   VALUE
               "BORROWER MEMO ".
           03  FILLER            PIC X(14)   VALUE
               "DEALER MEMO   ".
           03  FILLER            PIC X(14)   VALUE
               "EXIT          ".

       01  MEMOSC-WINDOW-HANDLE  PIC X(10).

       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBSP/SPMEMO_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBSP/SPMEMOW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".

       COPY "LIBGB/PASSWDW.CPY".
      * "DELAUD"
      * DELETE MEMO > TODAY
       78  DELAUD-DEL-OLD-SEQ       VALUE 1.
      * DELETE PRIORITY
       78  DELAUD-DEL-PRIORITY-SEQ  VALUE 2.
      * DELETE SYSTEM MEMOS
       78  DELAUD-ADJ-SYS-SEQ       VALUE 3.
      * DELETE MEMOS MADE TODAY
       78  DELAUD-DEL-TODAY         VALUE 4.

      * "AUDMEM"
      * NO EDIT MEMO > TODAY
       78  AUDMEM-ADJ-GT-TODAY      VALUE 0.
      * TO EDIT PRIORITY MEMOS
       78  AUDMEM-ADJ-PRIORITY      VALUE 1.
      * TO EDIT MEMOS MADE TODAY
       78  AUDMEM-ADJ-TODAY         VALUE 2.

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/MEMOSC_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/MEMOSC_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/MEMOSCW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/MEMOSC_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME MEMOSC-BUF EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP FORM-PATHNAME.

           PERFORM GET-GPENV.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "MEMOSC" TO VDU-FORM-NAME

           DISPLAY FLOATING GRAPHICAL WINDOW
              AT  SCREEN LINE MEMOSC-BEGIN-X * VDU-WIN-L-OFFSET
              AT  SCREEN COL  MEMOSC-BEGIN-Y * VDU-WIN-C-OFFSET
              CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
              SIZE    MEMOSC-COLUMNS + VDU-WIN-W-MOD
              LINES   MEMOSC-LINES + VDU-WIN-H-MOD
              BLANK SCREEN
              HANDLE IS MEMOSC-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE EXT-WI-CL-TRANS-DATE TO MEMO-HOLD-SYS.

           MOVE MEMOSC-WIMENU-SELECTION TO VDU-WIMENU-SELECTION.
           MOVE MEMOSC-CLMENU-SELECTION TO VDU-CLMENU-SELECTION.

           MOVE MEMOSC-MEMO-TYPE TO HOLD-MEMO-TYPE.

      *    IF EXT-WI-CL-ARCHIVE-FG = "Y"
      *       PERFORM SETUP-LM-ARCHIVE-PATH
      *       IF IO-BAD
      *          GO TO END-ROUTINE.

       MAIN-MODULE.
           MOVE 0 TO IO-FG.

           IF MEMOSC-ONLY = "B"
              PERFORM BM-MEMO-RESET-LINE1-KEY
              PERFORM BM-MEMO-SCAN
              PERFORM BM-MEMO-MODULE
              GO TO END-ROUTINE
           ELSE
           IF MEMOSC-ONLY = "D"
              PERFORM DM-MEMO-RESET-LINE1-KEY
              PERFORM DM-MEMO-SCAN
              PERFORM DM-MEMO-MODULE
              GO TO END-ROUTINE
           ELSE
              GO TO TOGGLE-TO-LOAN-MEMO.

       TOGGLE-TO-LOAN-MEMO.
      D    STOP MESS.
           PERFORM LM-MEMO-RESET-LINE1-KEY.
           PERFORM LM-MEMO-SCAN.
           PERFORM LM-MEMO-MODULE.
           IF VDUSTATUS = "1<"
              IF MEMOSC-MEMO-TYPE NOT = " "
                 GO TO END-ROUTINE
              ELSE
                 PERFORM BM-MEMO-RESET-LINE1-KEY
                 PERFORM BM-MEMO-SCAN
                 PERFORM BM-MEMO-MODULE
                 IF VDUSTATUS = "1<"
                    IF GP-DL-MEMO-TOGGLE = "Y"
                       PERFORM DM-MEMO-RESET-LINE1-KEY
                       PERFORM DM-MEMO-SCAN
                       PERFORM DM-MEMO-MODULE
                       IF VDUSTATUS = "1<"
                          GO TO TOGGLE-TO-LOAN-MEMO
                       ELSE
                          GO TO END-ROUTINE
                    ELSE
                       GO TO TOGGLE-TO-LOAN-MEMO
                 ELSE
                    GO TO END-ROUTINE.

      *-----------------------------------------------------------
      *  LOAN MEMO ROUTINE FALLS THRU TO HERE; PROTECT THIS GO TO
      *-----------------------------------------------------------
           GO TO END-ROUTINE.

       COPY "LIBSP/SPMEMO.CPY".
       COPY "LIBWI/WILMEMO.CPY".
       COPY "LIBWI/WIBMEMO.CPY".
       COPY "LIBWI/WIDMEMO.CPY".

      ***************************************************
       VERIFY-MEMO-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.
       EXIT-VERIFY-MEMO-PASSWORD.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           DISPLAY MEMOSC-SCREEN UPON MEMOSC-WINDOW-HANDLE
           MOVE MEMOSC-FORM-FIELDS TO WR-BUF
           MOVE MEMOSC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE MEMOSC-HELP--FILE TO HELP-LINK-FILE
           MOVE MEMOSC-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/MEMOSC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************************************************
       MISC-ROUTINES SECTION.
      **************************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETDAT.CPY".

       SEND-HEAD-3.
             MOVE "S  A220TITLE." TO VDU.
             MOVE MEMO-WHICH TO VDUBUF.
             PERFORM SCREENIO.
             MOVE "S  A660HEAD:S." TO VDU.
             MOVE HEAD-3 TO VDUBUF.
             PERFORM SCREENIO.

       DISPLAY-ALL-LINES.
           MOVE MEMO-BUF TO WR-BUF.
           MOVE MEMO-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-W-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-W-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      **************************************************************************
       IO-ROUTINES SECTION.
      **************************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-DM1-FILE.
           PERFORM CLOSE-BM1-FILE.
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBLP/LPDMGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPBM1IN.CPY".
       COPY "LIBLP/LPDM1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE.
           PERFORM CLOSE-FILES.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY MEMOSC-SCREEN.
           DESTROY MEMOSC-WINDOW-HANDLE.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
