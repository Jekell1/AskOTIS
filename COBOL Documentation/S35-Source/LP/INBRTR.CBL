       IDENTIFICATION DIVISION.
       PROGRAM-ID.      INBRTR.
       AUTHOR.  MJD.
       DATE-WRITTEN.    092195.
      ******************************************************************
      *  DESC:  INTER BRANCH TRANSACTION PROCESSING DRIVER
      *          F10 INTER BRANCH PAYMENTS
      *
      *
      *   THIS PROGRAM IS CALLED FROM CL/WIMENU.C. IT IS AVAILABLE
      *    FROM THE INTERRUPT EVERYWHERE EXCEPT LOAN PROCESSING.
      *   WHEN INITIATED, THE USER IS PRESENTED WITH A GLOBAL LOAN SCAN
      *    FROM WHICH THEY CAN SEARCH BY NAME OF SOCIAL SECURITY NUMBER.
      *    WHEN THE APPROPRIATE LOAN IS SELECTED, THE USER IS TAKEN TO
      *    LOAN PROCESSING WITH THE SELECTED LOAN ON THE SCREEN.  USERS
      *    ARE ABLE TO POST PAYMENTS, DEFERMENTS AND REVERSALS (PW PROTECTED).
      *    UPDATES WILL BE MADE TO THE APPROPRIATE BRANCHES DATA FILES.
      *
      *    -----------------------------------------------------------
      **** WHILE POSTING THE USER IS IN THE OWNING BRANCHES DATA PATH. ****
      *    -----------------------------------------------------------
      *
      *   THE CURRENT DAY MUST BE OPEN IN BOTH THE POSTING BRANCH AND
      *    THE OWNING BRANCH.
      *
      * REV:
      *  071995 BLL IF CALLED FROM CL/WIMENU (LOAN-PROC-FG = " "),
      *             CALL MKTEMP FOR USER-PID & REMOVE WK FILE AT END
      *  MJD 111095 ADDED LOGIC TO REPLACE LEADING SPACES WITH 0'S IN
      *              ACCOUNT NUMBER.
      *  BLL 121895 ADD OP-FILE & OPEN TO PREVENT BRANCH YOU ARE POSTING
      *             TO FROM CLOSING & UPDATING
      *  MJD 010396 ADD OP-FILE & OPEN TO PREVENT BRANCH YOU ARE POSTING
      *             FROM FROM CLOSING & UPDATING
      *  BLV 100796 MOVED " " TO VDU-WIMENU-SELECTION, TO STOP RECURSION
      *  MJD 082897 ADDED PASSWORD TO POSTING ACROSS MACHINES AND GLOBAL SCAN.
      *  BLV 061198 ADD UPDATE OF ENV VARIABLE INTERBR TO LOG BR# FOR INTER
      *             BRANCH PMTS UPDATE FOR H_LOGUID.C
      *  BLV 070198 RESTORE ENV VARIABLE INTERBR TO 0 WHEN EXITING & CALL
      *             LOGUID TO UPDATE THIS CHANGE
      *  BAH 071498 ADDED SET OF WK-DATA-PATH AND OLD-DATA-PATH FOR EXITING,
      *             IF WI/INBRTR WAS IN SECURITY, GETFM SENT YOU TO EXIT-PROG
      *             WHICH DOES A "CD" TO OLD-DATA-PATH, AND IT WAS NOT SET,
      *             CAUSING LOOP.. MERCURY,CINDY
      *  BLV 010605 CHECK BR-ALLOW-INTERBR FLAG, IF "N", DON'T ALLOW
      *             INTERBRANCH PAYMENTS TO THIS BRANCH, REGACC PR# 1510
      *  BLF 051013 ADDED REDEFINITION OF FORM-PATHNAME FOR TEST IN
      *             DECLARATIVES
      *  BLM 130829 REMOVE SET-DATA-PATH & PERFORMS OF SET-DATA-PATH, THIS
      *             WAS DOING A CHDIR, DON'T CHANGE DIRS, USE FULL PATHS
      *  BAH 140814 ADDED SAVE-FORM-OBJ, LOOK FOR DETAILED REASON BELOW
      *  BAH 140911 FIXED HANDLE ISSUES WITH LONPF
      *  KEC 2015.1120 FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *  BLM 160914 FIX BILNSC-ACCTNO
      *  BAH 2017.0605 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 2018.0523 SET FULL-DISPLAY TO INITIAL VALUE OF "F"; IN A15, "F"
      *                IS AN INITIAL VALUE IN THE FORM SO IT GETS RESET TO "F"
      *                EVERYTIME YOU GO IN FROM THE MENU. FULL-DISPLAY IS A
      *                TRIGGER FOR WHETHER YOU CAN POST ONE PAYMENT AND ARE
      *                FORCED OUT, OR CAN CONTINUE POSTING. LORI TESTING
      * BAH 2019.0621 MOVE OPEN TAG TO GLOBAL "DP"
      * BAH 2022.0816 COMMENTED OUT LOGUID
      * BAH 2024.0122 ADDED BILNSC-CALLED-FROM
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * NEEDED FULL PATHNAME FOR THESE FILES
           SELECT OP-OWN-FILE ASSIGN OPEN-OWN-PATH
                  ORGANIZATION SEQUENTIAL
                  LOCK MODE AUTOMATIC
                  FILE STATUS FILE-STAT.

           SELECT OP-POST-FILE ASSIGN OPEN-POST-PATH
                  ORGANIZATION SEQUENTIAL
                  LOCK MODE AUTOMATIC
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      *----------------------------------------------------------------
      * OP FILE FOR OWNING BRANCH
      *----------------------------------------------------------------
       FD  OP-OWN-FILE
           LABEL RECORDS ARE STANDARD.
       01  OP-OWN-REC          PIC X.

      *----------------------------------------------------------------
      * OP FILE FOR POSTING BRANCH
      *----------------------------------------------------------------
       FD  OP-POST-FILE
           LABEL RECORDS ARE STANDARD.
       01  OP-POST-REC          PIC X.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/INBRTR.CBL S35 04/23/24-15:40:01 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".

      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       01  WSEL-LIST                   PIC X(5)    VALUE "WSEL.".

       01  SAVE-FORM-OBJ               PIC XX.

       01  PROG-WK.
           03  FILLER                  PIC X(8)  VALUE "LP/LONPF".
           03  PROG-NO                 PIC X     VALUE "0".

      *----------------------------------------------------------------
      *  WK-DATA-PATH  = WORKER USED FOR SETTING UP PATHS.
      *  NEW-DATA-PATH = OWNING BRANCHES DATA PATH
      *  OLD-DATA-PATH = POSTING BRANCHES DATA PATH.
      *----------------------------------------------------------------
       01  WK-DATA-PATH.
           03  WK-ALL-BASE             PIC X(9).
           03  FILLER                  PIC X     VALUE "/".
           03  WK-MACHINE              PIC XX.
           03  FILLER                  PIC X     VALUE "/".
           03  WK-DATA                 PIC X(6).
           03  FILLER                  PIC X(3)  VALUE "/GB".

       01  NEW-DATA-PATH.
           03  FILLER                  PIC X(10).
           03  NEW-DATA-MACHINE        PIC XX.
           03  FILLER                  PIC X.
           03  NEW-DATA-DATA           PIC X(6).
           03  FILLER                  PIC X(3).
       01  OLD-DATA-PATH.
           03  FILLER                  PIC X(10).
           03  OLD-DATA-MACHINE        PIC XX.
           03  FILLER                  PIC X.
           03  OLD-DATA-DATA           PIC X(6).
           03  FILLER                  PIC X(3).

      *----------------------------------------------------------------
      * OP FILE PATH FOR OWNING BRANCH
      *----------------------------------------------------------------
       01  OPEN-OWN-PATH.
           03  OPEN-OWN-BASE           PIC X(09).
           03  FILLER                  PIC X(01)  VALUE "/".
           03  OPEN-OWN-MACHINE        PIC XX.
           03  FILLER                  PIC X(6) VALUE "/DP/OP".
           03  OPEN-OWN-BR             PIC 9(4).
           03  FILLER                  PIC X    VALUE "D".
           03  OPEN-OWN-TRANS-DATE     PIC 9(6).
           03  FILLER                  PIC X(20).

      *----------------------------------------------------------------
      * OP FILE PATH FOR POSTING BRANCH
      *----------------------------------------------------------------
       01  OPEN-POST-PATH.
           03  OPEN-POST-BASE          PIC X(09).
           03  FILLER                  PIC X(01)  VALUE "/".
           03  OPEN-POST-MACHINE       PIC XX.
           03  FILLER                  PIC X(6) VALUE "/DP/OP".
           03  OPEN-POST-BR            PIC 9(4).
           03  FILLER                  PIC X    VALUE "D".
           03  OPEN-POST-TRANS-DATE    PIC 9(6).
           03  FILLER                  PIC X(20).


       01  SOURCE-MACHINE              PIC X(2).

       COPY "LIBWI/PFSWINDOWSW.CPY".
       01  SAVE-TCLP-MAIN-WINDOW-HANDLE   PIC X(10).
       01  INBRTR-WINDOW-HANDLE           PIC X(10).

       COPY "LIBWI/BILNSCW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01  EXT-EOBUF-MESS         REDEFINES MESS
                              PIC X(60).
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/UNAMEW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/INBRTR_DEF.CPY".
       COPY "LIBLP/LONPF_DEF.CPY".
       COPY "LIBWI/INBRTR_WKS.CPY".
       COPY "LIBLP/LONPF_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E
      *   NOTE:
      *        LOAN-PROG-FG IS FOR FUTURE USE.
      *        CURRENTLY LOAN-PROG-FG WILL ALWAYS BE ' '
      *        SINCE IT IS CALLED FROM WIMENU.C ONLY!!!
      **************************************************
       LINKAGE SECTION.
       01  FORM-PATHNAME PIC X(22).
       01  ACCTNO        PIC 9(8).
       01  LOAN-PROC-FG  PIC X.
       01  EXIT-PATHNAME.
           03  FILLER         PIC X(12).
           03  EXIT-PATH      PIC X(9).
           03  FILLER         PIC X.
       COPY "LIBLP/LPLOANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/INBRTR_SCN.CPY".
       COPY "LIBLP/LONPF_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME ACCTNO LOAN-PROC-FG PROG-BUF
                  EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               OP-OWN-FILE OP-POST-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               OP-OWN-FILE OP-POST-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM GET-BR.

           MOVE BR-MACHINE TO SOURCE-MACHINE WK-MACHINE
                              OPEN-POST-MACHINE.

      * ADDED SET OF WK-DATA-PATH AND OLD-DATA-PATH FOR EXITING PURPOSES.
      * IF WI/INBRTR WAS IN SECURITY, GETFM SENT YOU TO EXIT-PROG WHICH
      * DOES A "CD" TO OLD-DATA-PATH, AND IT WAS NOT SET, CAUSING LOOP..
      * OPEN TAG IS NOW IN GLOBAL DP
      *    MOVE BR-DATA-PATH TO WK-DATA OPEN-POST-DATA.
           MOVE BR-DATA-PATH TO WK-DATA.
           MOVE WK-DATA-PATH TO OLD-DATA-PATH.


      *----------------------------------------------------------------
      * CALL GLOBAL LOAN SCAN TO GET ACCOUNT NUMBER.
      * HEY, IF YOU ARE USING BI GLOBAL SCAN WINDOW TO SELECT ACCOUNT
      * (BI-INBR-BILNSC-FG = "Y"), WE NEED TO ASK PASSWORD FOR POSTING
      * ACROSS MACHINES RIGHT UP FRONT.  IF USING LOCAL SCAN WHERE YOU
      * ENTER BRANCH #, WE'LL ONLY ASK IF BRANCH IS NOT ON THIS MACHINE.
      *----------------------------------------------------------------
       GET-ACCOUNT.
           IF BR-INBR-BILNSC-FG = "Y"
              MOVE 0 TO PASSWDW-SEQ
              MOVE "TO POST ACROSS MACHINES" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "POSTING ACROSS MACHINES IS NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
                 PERFORM LOCAL-SCAN
              ELSE
                 MOVE 0                   TO BILNSC-SSNO
                 MOVE "T"                 TO BILNSC-ERROR
                 MOVE EXT-CONAME-SYSDATE  TO BILNSC-SYSDATE
                 MOVE EXT-CONAME-USER-LOC TO BILNSC-BRANCH
                 MOVE "INBRTR"            TO BILNSC-CALLED-FROM
                 MOVE "WI/BILNSC" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATH BILNSC-BUF
                                       EXIT-PATHNAME
                 CANCEL FORM-PROGX
                 IF BILNSC-ERROR = "Y"
                    GO TO END-ROUTINE
                 END-IF
              END-IF
           ELSE
              PERFORM LOCAL-SCAN.

      *----------------------------------------------------------------
      *  IF USERS BRANCH IS DIFFERENT FROM POSTING BRANCH THEN READ
      *     BRANCH FILE AND SET UP NEW DATA PATH.
      *----------------------------------------------------------------

           INSPECT BILNSC-ACCTNO REPLACING ALL SPACES BY ZEROS.
           INSPECT BILNSC-BRANCH REPLACING ALL SPACES BY ZEROS.
           IF EXT-CONAME-USER-LOC = BILNSC-BRANCH
              MOVE "OPTION IS FOR INTERBRANCH TRANSACTIONS ONLY"
                                                            TO MESS
              PERFORM SEND-MESS
              GO TO GET-ACCOUNT.

      *-----------------------------------------------------------------
      *    SAVE POSTING BRANCHES DATA PATH AND SET UP PATH-BUF
      *-----------------------------------------------------------------
           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM GET-BR.
           IF IO-FG = 9
              GO TO IO-ERROR.
           MOVE EXT-FILPATH-BASE TO WK-ALL-BASE.
           MOVE BR-MACHINE TO WK-MACHINE PATH-MACHINE.
           MOVE BR-DATA-PATH TO WK-DATA PATH-DATA.
           MOVE WK-DATA-PATH TO OLD-DATA-PATH.

      *-----------------------------------------------------------------
      *    SET UP OWNING BRANCHES DATA PATH
      *-----------------------------------------------------------------
           MOVE BILNSC-BRANCH TO BR-NO.
           PERFORM GET-BR.
           IF IO-FG NOT = 0
              PERFORM IO-ERROR
              MOVE "BRANCH &&&& DOES NOT EXIST " TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BILNSC-BRANCH
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           IF BR-ALLOW-INTERBR = "N"
              MOVE "BRANCH &&&& DOES NOT ALLOW INTERBRANCH PAYMENTS!"
                 TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BILNSC-BRANCH
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           MOVE BR-MACHINE TO WK-MACHINE.
           MOVE BR-DATA-PATH TO WK-DATA.
           MOVE WK-DATA-PATH TO NEW-DATA-PATH.

           MOVE BILNSC-ACCTNO TO ACCTNO.


       SETUP-WINDOW.
           MOVE 24 TO WINDOW-LINES.
           MOVE 80 TO WINDOW-COLUMNS.
           MOVE 1 TO WINDOW-BEGIN-Y.
           MOVE 1 TO WINDOW-BEGIN-X.
           MOVE " " TO WINDOW-FIT-TO.
           MOVE "LONPF" TO VDU-FORM-NAME.
           MOVE "LP/LONPF" TO WINDOW-FORM-NAM.
      *    PERFORM OPEN-WINDOW.
      **************************************************************************
      * ADDED NEW WINDOW LOGIC HERE.  RONPF IS USED TO LAUNCH LONPF0 FROM
      * F-10 AND FROM WITHING LOAN PROCESSING.  LONPFO IS CODED TO
      * DISPLAY ON THE TCLP-MAIN-WINDOW-HANDLE.  IN ORDER TO ALLOW THAT
      * TO STILL HAPPEN SMOOTHLY, I AM OPENING A TEMP WINDOW HER AND
      * THEN PASSING THE HANDLE OF THIS TEMP WINDOW FORWARD AS THE
      * TCLP-MAIN-WINDOW-HANDLE.  THIS WILL ALLOW ALL LONPF0 ETC TO
      * FUNCTION PROPERLY WHILE THEY ARE LAUNCHED. UPON EXIT, I WILL
      * CLOSE THE TEMP WINDOW, AND RESTORE THE TCLP MAIN WINDOW HANDLE
      * SO THAT CURRENT AND FUTURE PROGRAMS MAY ALSO FLOW SMOOTHLY
      **************************************************************************
           DISPLAY FLOATING GRAPHICAL WINDOW
              AT SCREEN LINE 1 * VDU-WIN-L-OFFSET
              AT SCREEN COL  1 * VDU-WIN-C-OFFSET
              SIZE    80 + VDU-WIN-W-MOD
              LINES   24 + VDU-WIN-H-MOD
              BLANK SCREEN
              CELL  SIZE IS LABEL FONT
              COLOR IS VDU-WCOLOR-TCLP
              CONTROL VALUE IS VDU-WCTRL-TCLP
              HANDLE IS INBRTR-WINDOW-HANDLE.

           MOVE TCLP-MAIN-WINDOW-HANDLE TO SAVE-TCLP-MAIN-WINDOW-HANDLE.
           MOVE INBRTR-WINDOW-HANDLE TO TCLP-MAIN-WINDOW-HANDLE.

           PERFORM CHECK-SECURITY.

           DISPLAY LONPF-SCREEN.

           MOVE LONPF-FORM-FIELDS TO WR-BUF.
           MOVE LONPF-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
      *    FIELDS IN PATH-LIST EXIST IN LONPF SO IT MUST BE OPENED
      *    BEFORE THIS FLDVAL
      *-----------------------------------------------------------------

           IF LOAN-PROC-FG NOT = "Y"
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE LPLOAN-SYSDATE
              PERFORM CONVERT-ALPDATE-TO-MMDDYY
              MOVE DATE-MMDDYY TO OPEN-OWN-TRANS-DATE
                                  OPEN-POST-TRANS-DATE
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO TRANS-DATE.

      *----------------------------------------------------------------
      * TEST FOR POSTING BRANCH OPEN DAY.
      *----------------------------------------------------------------
           MOVE "A1" TO RC-STATUS.
           PERFORM UNAME-CALL.
           MOVE UNAME-BR   TO RC-BRNO.
           MOVE TRANS-DATE TO RC-TRANS-DATE.

           PERFORM OPEN-RC2-FILE.
           PERFORM READ-RC2-FILE.
           PERFORM CLOSE-RC2-FILE.   
           IF IO-FG NOT = 0
              STRING
              "BR:" UNAME-BR
              ": TODAY HAS NOT BEEN OPENED FOR PAYMENT PROCESSING!"
                        DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              PERFORM CLOSE-WINDOW
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LONPF-SCREEN
              GO TO EXIT-PROG.

           MOVE UNAME-BR TO OPEN-POST-BR.
           PERFORM LOAD-OP-POST-FILE.
           MOVE OPEN-POST-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              STRING
              "BR:" UNAME-BR
              ": TODAY HAS NOT BEEN OPENED FOR PAYMENT PROCESSING!"
                        DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              PERFORM CLOSE-WINDOW
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LONPF-SCREEN
              GO TO EXIT-PROG.

           PERFORM OPEN-OP-POST-FILE.

      *----------------------------------------------------------------
      * TEST FOR OWNING BRANCH OPEN DAY.
      *----------------------------------------------------------------
           MOVE "A1"       TO RC-STATUS.
           MOVE BR-NO      TO RC-BRNO.
           MOVE TRANS-DATE TO RC-TRANS-DATE.

           PERFORM OPEN-RC2-FILE.
           PERFORM READ-RC2-FILE.
           PERFORM CLOSE-RC2-FILE.
           IF IO-FG NOT = 0
              STRING
              "BR:" BILNSC-BRANCH
              ": TODAY HAS NOT BEEN OPENED FOR PAYMENT PROCESSING!"
                        DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              PERFORM CLOSE-WINDOW
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LONPF-SCREEN
              GO TO EXIT-PROG.

      *WE FOUND THE 'A1' RC RECORD MAKE SURE THE 'OP' EXISTS
           MOVE NEW-DATA-PATH TO WK-DATA-PATH.
           MOVE WK-MACHINE    TO OPEN-OWN-MACHINE.
           MOVE BILNSC-BRANCH TO OPEN-OWN-BR.
           PERFORM LOAD-OP-OWN-FILE.
           MOVE OPEN-OWN-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              STRING
              "BR:" BILNSC-BRANCH
              ": TODAY HAS NOT BEEN OPENED FOR PAYMENT PROCESSING!"
                        DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              PERFORM CLOSE-WINDOW
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LONPF-SCREEN
              GO TO EXIT-PROG.

           PERFORM OPEN-OP-OWN-FILE.


      *----------------------------------------------------------------
      * IF COMING FROM INTERRUPT, GET MKTEMP TO USE FOR TEMP FILE
      * THERE MAY BE ANOTHER TEMP FILE IN LOAN PROCESSING, BUT
      * PROGBUF WILL NOT BE PASSED BACK SO WE DON'T CARE...
      *----------------------------------------------------------------

           IF LOAN-PROC-FG NOT = "Y"
              MOVE MKTEMP-DEFAULT TO MKTEMP-BUF
              PERFORM MKTEMP-CALL
              MOVE PROCESS-ID-BUF TO USER-PID.


      * MUST SET UP NEW ENVIRONMENT FOR NEW POSTING BRANCH!

           SET ENVIRONMENT "INTERBR" TO BILNSC-BR-ALP.

      * SET EXTERNAL FIL WORKERS TO NOW BE THE OWNING BRANCH THAT
      * YOU ARE GOING TO POST TO;
       
           MOVE NEW-DATA-MACHINE TO EXT-FILPATH-MACHINE.
           MOVE NEW-DATA-DATA    TO EXT-FILPATH-DATA.

           MOVE "Y"    TO INTERRUPT-FG.
           MOVE ACCTNO TO LOAN-KEY.
      * IN A15, THE "F" IS AN INITIAL VALUE IN THE SCREEN SO IT GETS RESET
      * TO "F" EVERYTIME YOU GO IN FROM THE MENU
      * FULL-DISPLAY IS A TRIGGER FOR WHETHER YOU CAN POST ONE PAYMENT AND
      * ARE FORCED OUT, OR CAN CONTINUE POSTING.
           MOVE "F"    TO FULL-DISPLAY.

           MOVE 0 TO PROG-NO.
       LOAD-PRE.
           MOVE PROG-WK TO FORM-NAM.
      *COPY "LIBGB/LOADCO.CPY".
      * NEED THIS TO PASS PROG-BUF AND LPPFBUF
       COPY "LIBGB/LOADCOLP.CPY".
       LOAD-POST.
           IF ERRCD = "E" OR ERRCD = "X"
              PERFORM CLOSE-WINDOW
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LONPF-SCREEN
              GO TO EXIT-PROG.

           MOVE NEW-PROG-SUB TO PROG-NO.

           GO TO LOAD-PRE.

      ******************************************************************
      * LOCAL SCAN SECTION.
      *  ALTERNATIVE TO GLOBAL LOAN SCAN. ALLOWS USER TO ENTER A SINGLE
      *  BRANCH AND THEN BE SENT INTO PAYMENT PROCESSING.
      ******************************************************************
       LOCAL-SCAN SECTION.
      ******************************************************************
           MOVE 5 TO WINDOW-LINES.
           MOVE 60 TO WINDOW-COLUMNS.
           MOVE 18 TO WINDOW-BEGIN-Y.
           MOVE 10 TO WINDOW-BEGIN-X.
           MOVE " " TO WINDOW-FIT-TO.
           MOVE "INBRTR" TO VDU-FORM-NAME.
           MOVE "WI/INBRTR" TO WINDOW-FORM-NAM.
           PERFORM OPEN-WINDOW.
           PERFORM CHECK-SECURITY.
           DISPLAY INBRTR-SCREEN.
           MOVE INBRTR-FORM-FIELDS TO WR-BUF.
           MOVE INBRTR-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

      *    -------------------------------------------------------------
      *    ENTER OWNING BRANCH OF ACCOUNT.
      *    -------------------------------------------------------------
       ENTER-BRANCH.
           MOVE " ENTER BRANCH TO POST A PAYMENT FOR:" TO MESS.
           PERFORM SEND-WSEL-LEGEND.

           MOVE "ENNN040BRNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              PERFORM CLOSE-WINDOW
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY INBRTR-SCREEN
              GO TO EXIT-PROG-BYPASS-REMOVE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-BRANCH.

           MOVE VDUNUM0 TO BILNSC-BRANCH BR-NO.

           PERFORM GET-BR.
           IF IO-FG NOT = 0
              MOVE "BRANCH RECORD NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

           MOVE "S  A040NAME." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

           IF BR-MACHINE NOT = SOURCE-MACHINE
              MOVE 0 TO PASSWDW-SEQ
              MOVE "TO POST ACROSS MACHINES" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "POSTING TO ANOTHER MACHINE IS NOT ALLOWED"
                     TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-BRANCH.

           MOVE " ACCEPT BRANCH (Y/N):" TO MESS.
           PERFORM SEND-WSEL-LEGEND.

           MOVE "ENNA010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-BRANCH.
           IF VDUSTATUS = "1U" GO TO ENTER-BRANCH.
           IF VDUSTATUS = "1>"
              PERFORM CLOSE-WINDOW
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY INBRTR-SCREEN
              GO TO EXIT-PROG-BYPASS-REMOVE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-BRANCH.
           IF VDUBUF NOT = "Y"
              GO TO ENTER-BRANCH.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY INBRTR-SCREEN.
           PERFORM CLOSE-WINDOW.

      * IF LOCAL THEN CLEAR ACCOUNT AND SEQUENCE NUMBER.
      * SCAN CAN BE INDUCED FROM PAYMENT POSTING.

           MOVE 0 TO BILNSC-ACCTNO.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "INBRTR" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE INBRTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE INBRTR-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/INBRTR_EVA.CPY".
              COPY "LIBLP/LONPF_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       MISC SECTION.
      ********************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/UNAME.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/CHKSEC.CPY".
      *COPY "LIBGB/LOGUID.CPY".
      *----------------------------------------------------------------
       GET-BR.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
      *----------------------------------------------------------------
       SEND-WSEL-LEGEND.
           MOVE WSEL-LIST TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.
      *    CALL XFER USING STAT WSEL-LIST MESS.
      *    CALL WRFORM USING STAT WSEL-LIST.
      *----------------------------------------------------------------

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ***********************************************
       IO-ROUTINE SECTION.
      ***********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBRC2RN.CPY".
      *----------------------------------------------------------------
      * OPEN FILE FOR OWNING BRANCH
      *----------------------------------------------------------------
       LOAD-OP-OWN-FILE.
           MOVE EXT-FILPATH-BASE       TO OPEN-OWN-BASE.
       OPEN-OP-OWN-FILE.
           PERFORM LOAD-OP-OWN-FILE.
           PERFORM OPEN-IT.
           MOVE OPEN-OWN-PATH TO E-FILE.
           OPEN I-O OP-OWN-FILE.
           IF IO-FG = 8
              GO TO OPEN-OP-OWN-FILE.
           IF IO-FG = 7
              PERFORM CLOSE-OP-OWN-FILE
              GO TO OPEN-OP-OWN-FILE.
       CLOSE-OP-OWN-FILE.
           MOVE "CLOSE" TO E-MSG.
           CLOSE OP-OWN-FILE.

      *----------------------------------------------------------------
      * OPEN FILE FOR POSTING BRANCH
      *----------------------------------------------------------------
       LOAD-OP-POST-FILE.
           MOVE EXT-FILPATH-BASE       TO OPEN-POST-BASE.
       OPEN-OP-POST-FILE.
           PERFORM LOAD-OP-POST-FILE.
           PERFORM OPEN-IT.
           MOVE OPEN-POST-PATH TO E-FILE.
           OPEN I-O OP-POST-FILE.
           IF IO-FG = 8
              GO TO OPEN-OP-POST-FILE.
           IF IO-FG = 7
              PERFORM CLOSE-OP-POST-FILE
              GO TO OPEN-OP-POST-FILE.
       CLOSE-OP-POST-FILE.
           MOVE "CLOSE" TO E-MSG.
           CLOSE OP-POST-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-ROUTINE.
       LOAD-RESTART.
       EXIT-PROG.

           PERFORM CLOSE-OP-OWN-FILE.

      *----------------------------------------------------------------
      *  RESET DATA PATH TO POSTING BRANCH
      *----------------------------------------------------------------
           MOVE OLD-DATA-PATH TO WK-DATA-PATH.
           PERFORM CLOSE-OP-POST-FILE.

      *----------------------------------------------------------------
      * IF COMING FROM INTERRUPT, REMOVE TEMPORARY WORK FILE
      *----------------------------------------------------------------
           IF LOAN-PROC-FG NOT = "Y"
              MOVE TEMP-PATH TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE.

       EXIT-PROG-BYPASS-REMOVE.

      * MUST RESTORE ENVIRONMENT VARIABLE INTERBR TO 0 UPON EXIT
           SET ENVIRONMENT "INTERBR" TO "0000".
      * THIS IS REALLY GROSS, BUT UP ABOVE WHEN OPEN-WINDOW AND SECURITY
      * IS DONE FOR WI/INBRTR, AND YOU FAIL SECURITY (3S), THAT GETS PUT
      * IN FORM-OBJ, AND THEN YOU ARE SENT TO EXIT-PROG, WHICH IS HERE.
      * NOW YOU ARE TRYING TO CALL CO/CL/LOGUID BUT YOU HAVE 3S/CL/LOGUID
      * AND I CANT USE SAVE-FORM-PATH BECAUSE LIBGB/LOGUID.CPY ALREADY
      * USES IT RIGHT BEFORE THIS CALL, SO I HAD TO CREATE A SEPARATE HOLD
      * FIELD
           MOVE FORM-OBJ TO SAVE-FORM-OBJ.
           MOVE "CO" TO FORM-OBJ.
      *    PERFORM LOGUID-CALL.
           MOVE SAVE-FORM-OBJ TO FORM-OBJ.

      * SET EXTERNAL FIL WORKERS BACK TO ORIGINAL
      *
           MOVE OLD-DATA-MACHINE TO EXT-FILPATH-MACHINE.
           MOVE OLD-DATA-DATA    TO EXT-FILPATH-DATA.
      *

           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPF-SCREEN.
           DESTROY INBRTR-WINDOW-HANDLE.
           MOVE SAVE-TCLP-MAIN-WINDOW-HANDLE TO TCLP-MAIN-WINDOW-HANDLE.

           MOVE "LP/INBRTR" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           EXIT PROGRAM.
