       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPPBAT.
      ******************************************************************
      *
      *  DESC:  EOD BATCH FORM PRINTING
      *
      *=================================================================
      *  REV:
      *  BAH 990723 REMOVED FORMS EXPRESS
      *  KEC 021015 {PR#00000} PARADATA <A15> [JIM ROSENAUER ]
      *             CHANGED "PROG-WK" TO ACCEPT ANY VARATION OF LP/PRT?.
      *
      *  KEC 050801 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *             CHANGED TO ADD PATH OPTION OF 'PCL':
      *               - LONP??
      *               - PCL#??
      *               - PRT0??
      *
      *  KEC 050810 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *             CHANGED IT AGAIN FOR LONP/PCL/PRT.
      *             CHANGED BECAUSE OF FD-OPTION (AKA FD-SMARTPORT).
      *             CHANGED 88-LEVELS OF FD-PROG-PREFIX.
      *             ADDED SMARTPORT-BUF & SMARTPORT-LIST.
      *
      * KEC 111031 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *            ADDED 'PST' (POSTSCRIPT) OPTION.
      *
      * KEC 140612 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *            CHANGED PATH PCL/PRT/PST TO USE THE FP DIRECTORY.
      *
      * KEC 2017.0119 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED FD-SMARTPORT TO FD-OPTION [88 LEVELS].
      * BAH 2022.0622 ONLY ALLOW ADMIN LOGINS "A" F3 ALL BRANCHES, 
      *               DUE TO GLOBAL FILES. ADMINS NEED FOR LONPEX
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LPPBAT.CBL S35 02/01/24-10:06:38 >".

       01  ALL-LIST                PIC X(13)     VALUE "BR BRFG MSEL.".

       01  LEGEND                  PIC X(67)     VALUE SPACES.

       01  LPBATP-LINK             VALUE SPACES.
           03  MKTEMP-BUF              PIC X(35).
           03  FIRST-TIME              PIC X(01).
           03  LPPBAT-WINDOW-HANDLE    PIC X(10).

       01  MSEL-LIST               PIC X(05)     VALUE "MSEL.".


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       78  PROG-DIR-PREFIX-LONP                  VALUE "LP/LONP".
       78  PROG-DIR-PREFIX-PCL                   VALUE "FP/PCL ".
       78  PROG-DIR-PREFIX-PST                   VALUE "FP/PST ".
       78  PROG-DIR-PREFIX-PRT                   VALUE "FP/PRT ".
       01  PROG-WK.
           03  PROG-DIR-NAME       PIC X(07)     VALUE "LP/LONP".
           03  FILLER              REDEFINES PROG-DIR-NAME.
               05  FILLER          PIC X(06).
               05  PROG-DIR-PREFIX PIC X(01).
           03  PROG-NO             PIC X(02)     VALUE SPACES.

       01  PROG-SUB                PIC 9(01)     VALUE ZEROES.

       01  SEL-LIST                PIC X(04)     VALUE "SEL.".


      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01  EXT-EOBUF-MESS REDEFINES MESS PIC X(61).
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBLP/LPIPBUF.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LPPBAT_DEF.CPY".
       COPY "LIBLP/LPPBAT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       LINKAGE SECTION.
       01  FORM-PATHNAME PIC X(22).
       01  EXIT-PATHNAME PIC X(22).

       SCREEN SECTION.
       COPY "LIBLP/LPPBAT_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPPBAT"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE GETENV-BUF TO LOADCO-OPT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LPPBAT" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS LPPBAT-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       GET-BR.

      * PASSING TO LPBATP
           MOVE "Y" TO FIRST-TIME.

           MOVE SPACES TO MKTEMP-BUF.

           MOVE SPACES TO WR-BUF.
           MOVE ALL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "F7-EXIT, ENTER BRANCH NO. (F3-ALL):" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BR." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO GET-BR.

      * IF NOT ADMIN, CAN ONLY ENTER YOUR OWN BRANCH
           IF EXT-CONAME-USER-PROFILE-CD NOT = X"61"
              IF VDUNUM0 NOT = EXT-CONAME-USER-LOC
                 MOVE "CAN ONLY ENTER YOUR BRANCH" TO MESS
                 PERFORM SEND-MESS
                 GO TO GET-BR.

           MOVE EXT-CONAME-USER-LOC TO BRANCH VDUNUM0.
           MOVE VDUNUM0 TO BRANCH.

           IF BRANCH = 0 GO TO GET-BR.

           GO TO GET-PRINT-REPRINT.

      *-----------------------------------------------------------------
       ALL-BR.
      * ONLY ALLOW ADMIN "A" LOGINS F3 ABILITY SINCE GLOBAL FILES
           IF EXT-CONAME-USER-PROFILE-CD = X"61"
              MOVE "S  A040BR." TO VDU
              MOVE "0000" TO VDUBUF
              MOVE 0 TO BRANCH
           ELSE
              MOVE "SNNN040BR." TO VDU
              MOVE EXT-CONAME-USER-LOC TO BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A050BRFG." TO VDU.
           MOVE "-ALL-" TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       GET-PRINT-REPRINT.
           MOVE
           "F1, ^-UP, ENTER 'P'-PRINT NEW FORMS, 'R'-REPRINT FORMS:"
                                                          TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRFG." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO GET-BR.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1U" GO TO GET-BR.

           IF VDUSTATUS NOT = "00" GO TO GET-PRINT-REPRINT.

           MOVE VDUBUF TO IP-PRFG.

           IF IP-PRFG NOT = "P" AND IP-PRFG NOT = "R"
              GO TO GET-PRINT-REPRINT.

           GO TO SETRP.


      *-----------------------------------------------------------------
       SETRP.
           MOVE SPACES TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "F7-CLEAR, ENTER PRINTER NO:" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE LPPBAT-QUEUE-POS TO Q-POS.
           MOVE LPPBAT-COPIES-POS TO C-POS.
           MOVE LPPBAT-PRU-POS TO P-POS.
           MOVE "N" TO Q-BUF.
           MOVE "01" TO C-BUF.

           MOVE "N" TO STAT.

           PERFORM PRU-IN.

           IF STAT = "1>" GO TO GET-BR.
           IF STAT NOT = "00" GO TO SETRP.


      *-----------------------------------------------------------------
       GET-PG.
           MOVE SPACES TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      * LOANS HAVE "L" IN FD-TYPE
      * NOTES HAVE "N" TO FD-TYPE

           MOVE "L" TO IP-FMCODE.
           MOVE 0   TO IP-FMNO.

      * LPPBAT FORM IN A15 HAD SEVERAL FIELDS INITIALIZED
           MOVE "Y" TO BATCH-PR.
           MOVE 0   TO LOAN-KEY BORROWER-KEY.

      * USING THIS FIELD TO IDENTIFY THE 2 FORMS ON THE 1098 FORM. NEEDS
      * TO START OUT AT 1, IT WILL INCREMENT IN PRT053 PRINT PROGRAM

           MOVE 1 TO BATCH.
           MOVE 1 TO PROG-SUB.
           MOVE "LP/LPBATP" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PROG-BUF IP-BUF
                                PRU-PATH LPBATP-LINK EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           GO TO LOAD-POST.


       LOAD-PRE.

           CALL FORM-PROGX USING FORM-PATH PROG-BUF IP-BUF PRU-PATH
                EXIT-PATHNAME ON OVERFLOW GO TO LOAD-OVERFLOW.
           CANCEL FORM-PROGX.

           GO TO LOAD-POST.

       LOAD-OVERFLOW.
           MOVE "CALL ERROR" TO ERRMSG-HEAD.
           MOVE "CA," TO FILE-STAT-DISP.
           CALL "C$CALLERR" USING FILE-STATD2.
           MOVE FILE-STAT-DISP TO ERRMSG-MSG1.
           MOVE FORM-PATH      TO ERRMSG-MSG2.
           MOVE SPACES         TO ERRMSG-MSG3.
           PERFORM CALL-ERRMSG.
           GO TO STOP-RUN.

      *-----------------------------------------------------------------
       LOAD-POST.

           IF ERRCD = "X" GO TO ENDIT.
           IF ERRCD = "E" GO TO ENDIT.
           IF ERRCD = "B" GO TO GET-BR.

           GO TO POST-MENU POST-PRINT
              DEPENDING ON PROG-SUB.

      *-----------------------------------------------------------------
       POST-MENU.

           MOVE SEL-LIST TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.

           IF ( ERRCD = "X" ) GO TO ENDIT.
           IF ( ERRCD = "B" ) GO TO GET-BR.

           IF ( IP-PROG = SPACES ) GO TO GET-PG.

           MOVE 2 TO PROG-SUB.

           MOVE SPACES          TO FD-REC.
           MOVE IP-PROG-PREFIX  TO FD-PROG-PREFIX.
           MOVE IP-FD-OPTION    TO FD-OPTION.
           PERFORM LPFRFD-FIND-PREFIX.

           IF ( LPWSFD-PREFIX-PCL )
              MOVE PROG-DIR-PREFIX-PCL     TO PROG-DIR-NAME
              MOVE IP-PROG-PREFIX          TO PROG-DIR-PREFIX
              MOVE IP-PROG                 TO PROG-NO
           ELSE
           IF ( LPWSFD-PREFIX-PST )
              MOVE PROG-DIR-PREFIX-PST     TO PROG-DIR-NAME
              MOVE IP-PROG-PREFIX          TO PROG-DIR-PREFIX
              MOVE IP-PROG                 TO PROG-NO
           ELSE
           IF ( LPWSFD-PREFIX-PRT )
              MOVE PROG-DIR-PREFIX-PRT     TO PROG-DIR-NAME
              MOVE IP-PROG-PREFIX          TO PROG-DIR-PREFIX
              MOVE IP-PROG                 TO PROG-NO
           ELSE
              MOVE PROG-DIR-PREFIX-LONP    TO PROG-DIR-NAME
              MOVE IP-PROG                 TO PROG-NO.

           MOVE PROG-WK TO FORM-NAM.
           GO TO LOAD-PRE.

      *-----------------------------------------------------------------
       POST-PRINT.
           IF ERRCD = "L" GO TO GET-PG.
           IF ERRCD = "E" GO TO EXIT-PROG.

           MOVE SPACES TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO GET-PG.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPFRFD.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           DISPLAY LPPBAT-SCREEN UPON LPPBAT-WINDOW-HANDLE.
           MOVE LPPBAT-FORM-FIELDS TO WR-BUF.
           MOVE LPPBAT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE LPPBAT-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPPBAT-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LPPBAT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ERRMSG.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SENDMESS.CPY".
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       EXIT-PROG.
       ENDIT.
      *-----------------------------------------------------------------
       LOAD-RESTART.

           PERFORM PRU-OUT.

           MOVE "LP/LPEDMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-FINISH' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPPBAT"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPPBAT-SCREEN.
           DESTROY LPPBAT-WINDOW-HANDLE.
           EXIT PROGRAM.
