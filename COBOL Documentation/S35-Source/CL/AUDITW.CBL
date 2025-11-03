       IDENTIFICATION DIVISION.
       PROGRAM-ID.      AUDITW.
       DATE-WRITTEN.    2022.02.07.
      *******************************************************
      *  DESC:  AUDIT CHANGES CREATE AUFILE
      *  CALLED FROM:   SNMAIN, SEMAIN, SECENT, SECREA, SECOPY
      *                 PWMAIN, PEMAIN
      *
      *         WRITE AN AUDIT RECORD FOR THE CHANGE MADE TO
      *         FILES.
      *
      *  IN:
      *       AUDITW-FILE-NAME      PIC X(7).
      *          SEFILE, SNFILE, ETC.
      *       AUDITW-CHANGE-TYPE    PIC X.
      *          "A" ADD, "D" DELETE, "C" CHANGE
      *       AUDITW-ENTER-TICKET   PIC X      VALUE "Y".
      *          SECOPY REWRITES ENTIRE FILE, ONLY ENTER TICKET ONCE
      *          THEN MOVE "N" TO AUDITW-ENTER-TICKET,
      *       AUDITW-HOLD-TICKET    PIC X(20)  VALUE SPACES.
      *          PASS TICKET # IF USED IN MULITPLE WRITES LIKE SECOPY
      *       AUDITW-ORIG-REC       PIC X(6400).
      *          RECORD BEFORE THE CHANGE (ORSE-REC) NEW & DELETE ARE BLANK
      *       AUDITW-CHG-REC        PIC X(6400).
      *          RECORD AFTER THE CHANGE (CHSE-REC) NEW & DELETE ARE BLANK
      *
      * REV:
      *  20220207 BAH NEW #1542
      *******************************************************
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
           "@(#)CL/AUDITW S35 03/08/22-12:55:49 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBGB/GB01AU.CPY".
       COPY "LIBGB/GB01AU_SQL.CPY".
       COPY "LIBGB/GBWSAU.CPY".

       01  TODAYS-DATE         PIC 9(8).
       01  ERRCD               PIC X     VALUE SPACES.
       01  HOLD-TICKET         PIC X(20) VALUE SPACES.
       01  HOLD-AU1-KEY.
           03  HOLD-AU-FILE-NAME          PIC X(7).
           03  HOLD-AU-DATE               PIC 9(8).
           03  HOLD-AU-SEQ                PIC 9(8).

      * ADD NEW FILES HERE
       COPY "LIBGB/GB01SE.CPY" REPLACING LEADING "SE" BY "ORSE".
       COPY "LIBGB/GB01SE.CPY" REPLACING LEADING "SE" BY "CHSE".
       COPY "LIBGB/GB01SN.CPY" REPLACING LEADING "SN" BY "ORSN".
       COPY "LIBGB/GB01SN.CPY" REPLACING LEADING "SN" BY "CHSN".
       COPY "LIBGB/GB01PW.CPY" REPLACING LEADING "PW" BY "ORPW".
       COPY "LIBGB/GB01PW.CPY" REPLACING LEADING "PW" BY "CHPW".
       COPY "LIBGB/GB01PE.CPY" REPLACING LEADING "PE" BY "ORPE".
       COPY "LIBGB/GB01PE.CPY" REPLACING LEADING "PE" BY "CHPE".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/TICKET_DEF.CPY".
       COPY "LIBWI/TICKET_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".


       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/AUDITWW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/TICKET_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME AUDITW-WORKERS EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           ACCEPT TODAYS-DATE FROM CENTURY-DATE.

           PERFORM OPEN-AU1-FILE.

      * DEFAULTS TO "Y", IF YOU DONT WANT TO ENTER FOR A PROGRAM THAT
      * UPDATES SEVERAL RECORDS FOR 1 TICKET, MUST MOVE "N" TO IT
           IF AUDITW-ENTER-TICKET NOT = "N"
              PERFORM ENTER-TICKET
           ELSE
              MOVE AUDITW-HOLD-TICKET TO HOLD-TICKET.

           IF AUDITW-FILE-NAME = "SEFILE"
              PERFORM UPDATE-AUDIT-SEFILE
           ELSE
           IF AUDITW-FILE-NAME = "SNFILE"
              PERFORM UPDATE-AUDIT-SNFILE
           ELSE
           IF AUDITW-FILE-NAME = "PWFILE"
              PERFORM UPDATE-AUDIT-PWFILE
           ELSE
           IF AUDITW-FILE-NAME = "PEFILE"
              PERFORM UPDATE-AUDIT-PEFILE.

           GO TO END-ROUTINE.


      **************************************************
       UPDATE-AUDIT-SEFILE SECTION.
      **************************************************
           MOVE AUDITW-ORIG-REC TO ORSE-REC.
           MOVE AUDITW-CHG-REC  TO CHSE-REC.

           MOVE AUDITW-CHANGE-TYPE TO AU-ACTION.
           IF AU-ACTION-DELETE
              PERFORM SET-SEFILE
              MOVE "* DELETED *"           TO AU-FIELD-DESC
              PERFORM CREATE-AUDIT-FILE
              GO TO UPDATE-AUDIT-SEFILE-EXIT.
           IF AU-ACTION-ADD
              PERFORM SET-SEFILE
              MOVE "* ADDED *    "         TO AU-FIELD-DESC
              PERFORM CREATE-AUDIT-FILE
              GO TO UPDATE-AUDIT-SEFILE-EXIT.

           IF ORSE-EXP-DATE NOT = CHSE-EXP-DATE
              PERFORM SET-SEFILE
              MOVE "EXPIRATION DATE"   TO AU-FIELD-DESC
              MOVE ORSE-EXP-DATE       TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-ALPDATE
              MOVE ALP-DATE            TO AU-ORIG-VALUE
              MOVE CHSE-EXP-DATE       TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-ALPDATE
              MOVE ALP-DATE            TO AU-NEW-VALUE
              PERFORM CREATE-AUDIT-FILE.
           IF ORSE-PURCHASABLE NOT = CHSE-PURCHASABLE
              PERFORM SET-SEFILE
              MOVE "PURCHASABLE"       TO AU-FIELD-DESC
              MOVE ORSE-PURCHASABLE    TO AU-ORIG-VALUE
              MOVE CHSE-PURCHASABLE    TO AU-NEW-VALUE
              PERFORM CREATE-AUDIT-FILE.
           IF ORSE-PROFILE NOT = CHSE-PROFILE
              PERFORM SET-SEFILE
              MOVE "PROFILES"          TO AU-FIELD-DESC
              MOVE ORSE-PROFILE        TO AU-ORIG-VALUE
              MOVE CHSE-PROFILE        TO AU-NEW-VALUE
              PERFORM CREATE-AUDIT-FILE.

       UPDATE-AUDIT-SEFILE-EXIT.
           EXIT.

      **************************************************
       UPDATE-AUDIT-SNFILE SECTION.
      **************************************************
           MOVE AUDITW-ORIG-REC TO ORSN-REC.
           MOVE AUDITW-CHG-REC  TO CHSN-REC.

           MOVE AUDITW-CHANGE-TYPE TO AU-ACTION.
           IF AU-ACTION-DELETE
              PERFORM SET-SNFILE
              MOVE "* DELETED *"           TO AU-FIELD-DESC
              PERFORM CREATE-AUDIT-FILE
              GO TO UPDATE-AUDIT-SNFILE-EXIT.
           IF AU-ACTION-ADD
              PERFORM SET-SNFILE
              MOVE "* ADDED *    "         TO AU-FIELD-DESC
              PERFORM CREATE-AUDIT-FILE
              GO TO UPDATE-AUDIT-SNFILE-EXIT.

           IF ORSN-SEQ NOT = CHSN-SEQ
              PERFORM SET-SNFILE
              MOVE "SEQUENCE"  TO AU-FIELD-DESC
              MOVE ORSN-SEQ    TO AU-ORIG-VALUE
              MOVE CHSN-SEQ    TO AU-NEW-VALUE
              PERFORM CREATE-AUDIT-FILE.
           IF ORSN-DESC NOT = CHSN-DESC
              PERFORM SET-SNFILE
              MOVE "DESCRIPTION"    TO AU-FIELD-DESC
              MOVE ORSN-DESC        TO AU-ORIG-VALUE
              MOVE CHSN-DESC        TO AU-NEW-VALUE
              PERFORM CREATE-AUDIT-FILE.
           IF ORSN-PURCHASABLE NOT = CHSN-PURCHASABLE
              PERFORM SET-SNFILE
              MOVE "PURCHASABLE"       TO AU-FIELD-DESC
              MOVE ORSN-PURCHASABLE    TO AU-ORIG-VALUE
              MOVE CHSN-PURCHASABLE    TO AU-NEW-VALUE
              PERFORM CREATE-AUDIT-FILE.

       UPDATE-AUDIT-SNFILE-EXIT.
           EXIT.

      **************************************************
       UPDATE-AUDIT-PWFILE SECTION.
      **************************************************
           MOVE AUDITW-ORIG-REC TO ORPW-REC.
           MOVE AUDITW-CHG-REC  TO CHPW-REC.

           MOVE AUDITW-CHANGE-TYPE TO AU-ACTION.
           IF AU-ACTION-DELETE
              PERFORM SET-PWFILE
              MOVE "* DELETED *"           TO AU-FIELD-DESC
              PERFORM CREATE-AUDIT-FILE
              GO TO UPDATE-AUDIT-PWFILE-EXIT.
           IF AU-ACTION-ADD
              PERFORM SET-PWFILE
              MOVE "* ADDED *    "         TO AU-FIELD-DESC
              PERFORM CREATE-AUDIT-FILE
              GO TO UPDATE-AUDIT-PWFILE-EXIT.

           IF ORPW-PASSYN NOT = CHPW-PASSYN
              PERFORM SET-PWFILE
              MOVE "PASSYN"       TO AU-FIELD-DESC
              MOVE ORPW-PASSYN    TO AU-ORIG-VALUE
              MOVE CHPW-PASSYN    TO AU-NEW-VALUE
              PERFORM CREATE-AUDIT-FILE.
           IF ORPW-PASSWORD NOT = CHPW-PASSWORD
              PERFORM SET-PWFILE
              MOVE "PASSWORD"           TO AU-FIELD-DESC
              MOVE ORPW-PASSWORD        TO AU-ORIG-VALUE
              MOVE CHPW-PASSWORD        TO AU-NEW-VALUE
              PERFORM CREATE-AUDIT-FILE.
           IF ORPW-DESC NOT = CHPW-DESC
              PERFORM SET-PWFILE
              MOVE "DESC"           TO AU-FIELD-DESC
              MOVE ORPW-DESC        TO AU-ORIG-VALUE
              MOVE CHPW-DESC        TO AU-NEW-VALUE
              PERFORM CREATE-AUDIT-FILE.
           IF ORPW-USER-PROFILE NOT = CHPW-USER-PROFILE
              PERFORM SET-PWFILE
              MOVE "USER PROFILE"     TO AU-FIELD-DESC
              MOVE ORPW-USER-PROFILE  TO AU-ORIG-VALUE
              MOVE CHPW-USER-PROFILE  TO AU-NEW-VALUE
              PERFORM CREATE-AUDIT-FILE.
           IF ORPW-ASK-FG NOT = CHPW-ASK-FG
              PERFORM SET-PWFILE
              MOVE "ASK FLAG"   TO AU-FIELD-DESC
              MOVE ORPW-ASK-FG  TO AU-ORIG-VALUE
              MOVE CHPW-ASK-FG  TO AU-NEW-VALUE
              PERFORM CREATE-AUDIT-FILE.
           IF ORPW-EXCEPTION-FG NOT = CHPW-EXCEPTION-FG
              PERFORM SET-PWFILE
              MOVE "EXCEPTION FLAG"  TO AU-FIELD-DESC
              MOVE ORPW-EXCEPTION-FG TO AU-ORIG-VALUE
              MOVE CHPW-EXCEPTION-FG TO AU-NEW-VALUE
              PERFORM CREATE-AUDIT-FILE.

       UPDATE-AUDIT-PWFILE-EXIT.
           EXIT.

      **************************************************
       UPDATE-AUDIT-PEFILE SECTION.
      **************************************************
           MOVE AUDITW-ORIG-REC TO ORPE-REC.
           MOVE AUDITW-CHG-REC  TO CHPE-REC.

           MOVE AUDITW-CHANGE-TYPE TO AU-ACTION.
           IF AU-ACTION-DELETE
              PERFORM SET-PEFILE
              MOVE "* DELETED *"           TO AU-FIELD-DESC
              PERFORM CREATE-AUDIT-FILE
              GO TO UPDATE-AUDIT-PEFILE-EXIT.
           IF AU-ACTION-ADD
              PERFORM SET-PEFILE
              MOVE "* ADDED *    "         TO AU-FIELD-DESC
              PERFORM CREATE-AUDIT-FILE
              GO TO UPDATE-AUDIT-PEFILE-EXIT.

       UPDATE-AUDIT-PEFILE-EXIT.
           EXIT.

      ****************************************************
       CREATE-AUDIT-FILE SECTION.
      ****************************************************
           MOVE TODAYS-DATE         TO HOLD-AU-DATE.

           MOVE AUDITW-PROGRAM      TO AU-PROGRAM.
           PERFORM GET-TIME.
           MOVE HH-MM-SS            TO AU-TIME.
           MOVE EXT-CONAME-USER-ID  TO AU-USERID.

           MOVE HOLD-TICKET         TO AU-TICKET.

           MOVE 1  TO HOLD-AU-SEQ.

       UPDATE-AU-FILE-AGAIN.

           MOVE HOLD-AU1-KEY        TO AU1-KEY.
           PERFORM WRITE-AU1-FILE.
           IF IO-BAD
              IF E-CODE NOT = "22"
                 GO TO IO-ERROR
           ELSE
              IF HOLD-AU-SEQ < 99999998
                 ADD 1 TO HOLD-AU-SEQ
                 GO TO UPDATE-AU-FILE-AGAIN.

       CREATE-AU-MEMO-EXIT.
           EXIT.


      *******************************************
       ENTER-TICKET SECTION.
      *******************************************

           MOVE 7           TO WINDOW-LINES.
           MOVE 37          TO WINDOW-COLUMNS.
           MOVE 5           TO WINDOW-BEGIN-Y.
           MOVE 2           TO WINDOW-BEGIN-X.
           MOVE "WI/TICKET" TO WINDOW-FORM-NAM.

           MOVE "TICKET"    TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.

           DISPLAY TICKET-SCREEN.

       ENTER-TICKET-FIELD.
           MOVE "F7-EXIT, ENTER TICKET #" TO WR-BUF.
           MOVE "WSEL."                   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A200TICKET." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              IF VDUBUF = SPACES
                 GO TO ENTER-TICKET-FIELD
              ELSE
                 GO TO EXIT-ENTER-TICKET.
           IF VDUSTATUS NOT = "00" GO TO ENTER-TICKET-FIELD.

           MOVE VDUBUF TO HOLD-TICKET AUDITW-HOLD-TICKET.
           IF HOLD-TICKET = SPACES
              GO TO ENTER-TICKET-FIELD.

       EXIT-ENTER-TICKET.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TICKET-SCREEN.
           PERFORM CLOSE-WINDOW.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE TICKET-HELP--FILE TO HELP-LINK-FILE.
           MOVE TICKET-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/TICKET_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/GBAUCL.CPY".

      **********************
       MISC-ROUTINES SECTION.
      **********************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETDAT.CPY".
       SET-SEFILE.
           PERFORM CLEAR-AU-FILE.
           MOVE "SEFILE"            TO HOLD-AU-FILE-NAME.
           MOVE 0                   TO AU-BRANCH.
           MOVE ORSE1-KEY           TO AU-FILE-KEY.
           MOVE AUDITW-CHANGE-TYPE  TO AU-ACTION.
       SET-SNFILE.
           PERFORM CLEAR-AU-FILE.
           MOVE "SNFILE"            TO HOLD-AU-FILE-NAME.
           MOVE 0                   TO AU-BRANCH.
           MOVE ORSN1-KEY           TO AU-FILE-KEY.
           MOVE AUDITW-CHANGE-TYPE  TO AU-ACTION.
       SET-PWFILE.
           PERFORM CLEAR-AU-FILE.
           MOVE "PWFILE"            TO HOLD-AU-FILE-NAME.
           MOVE 0                   TO AU-BRANCH.
           MOVE ORPW1-KEY           TO AU-FILE-KEY.
           MOVE AUDITW-CHANGE-TYPE  TO AU-ACTION.
       SET-PEFILE.
           PERFORM CLEAR-AU-FILE.
           MOVE "PEFILE"            TO HOLD-AU-FILE-NAME.
           MOVE 0                   TO AU-BRANCH.
           MOVE ORPE1-KEY           TO AU-FILE-KEY.
           MOVE AUDITW-CHANGE-TYPE  TO AU-ACTION.

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-AU1-FILE.
       COPY "LIBGB/GBAUGS_SQL.CPY".
       COPY "LIBGB/GBAU1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
