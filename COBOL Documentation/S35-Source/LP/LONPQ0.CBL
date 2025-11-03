       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPQ0.
       DATE-WRITTEN.    112296
      ******************************************************************
      *
      *                 (LP)
      *  DESCRIPTION:   RESCISSION DRIVER (MENU)
      *
      *  REV:
      *
      *  BLF 051013 ADDED REDEFINITION OF FORM-PATHNAME FOR TEST IN
      *             DECLARATIVES
      *  BLM 101206 CHECK GP-SECURITY-FG & DO NEW SECURITY ROUTINES IF
      *             FLAG IS Y, PR# NEWSECURITY
      *  BLM 111130 ACTIVATE NEW SECURITY ROUTINES IN A15, PR# NEWSECURITY
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      * BLM 170117 REMOVE SEND-MESS WITH SECURITY ERROR, THIS IS IN
      *            CHKSEC SO WE WERE GETTING IT TWICE.  BARB
      * BAH 170630 ACTIVATE 8 DIGIT DATES, PD0006
      * AMM 190412 LOCAL TO GLOBAL CHANGES FOR RX FILE
      * BAH 20190924 REMOVED ACCESS-CALL RXFILE
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPQ0.CBL S35 09/26/22-10:19:34 >".

      *-----------------------------------------------------------------
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01RX.CPY".
       COPY "LIBLP/LP01RX_SQL.CPY".
       COPY "LIBLP/LPWSRX.CPY".

       01  FORM-BUF                    PIC 9(6)   VALUE 0.
       01  MSEL-LIST                   PIC X(5)   VALUE "MSEL.".

       01  TODAY-DATE                  PIC 9(8)   VALUE ZEROES.

      *=================================================================
      *
      *    M E S S A G E - C O U N T E R S
      *
      *=================================================================
       01  MESS-CTR-MESSAGES.
           03  MESSAGE-CTR-1           PIC X(48)  VALUE
               "NUMBER OF ACCOUNTS DUE FOR UPDATE TODAY///////: ".
           03  MESSAGE-CTR-2           PIC X(48)  VALUE
               "NUMBER OF ACCOUNTS DUE FOR UPDATE IN ARREARAGE: ".

       01  MESS-CTR-COUNTERS.
           03  COUNTER-1               PIC 9(3)   VALUE ZEROES.
           03  COUNTER-2               PIC 9(3)   VALUE ZEROES.

       01  MESS-CTR-LISTS.
           03  MESS-CTR-1-LIST         PIC X(12)  VALUE "MESS-CTR1:B.".
           03  MESS-CTR-2-LIST         PIC X(12)  VALUE "MESS-CTR2:B.".

       01  MESS-CTR-BUF                PIC X(51)  VALUE SPACES.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/SCREENW.CPY".

       COPY "LIBLP/LONPQ_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".

       COPY "LIBLP/LONPQ_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".


       LINKAGE SECTION.
       01  FORM-PATHNAME      PIC X(22).
       01  EXIT-PATHNAME      PIC X(22).
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPQ_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPQ" TO VDU-FORM-NAME.

       SETUP-BR-RX.

           PERFORM FORM-RESET.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TODAY-DATE.

           PERFORM GET-GPENV.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO RX-PATH-OVERRIDE.

           MOVE ZEROES TO RX-ACCTNO COUNTER-1 COUNTER-2.
           PERFORM OPEN-RX1-FILE.
           PERFORM START-RX1-FILE.

       READ-NEXT-RX.

           PERFORM READ-RX1-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR ( RX-BRNO NOT = RX-PATH-OWNBR )
              PERFORM CLOSE-RX1-FILE
              GO TO UPDATE-SCREEN-COUNTERS.

      * LOAN IN RESCISSION
           IF ( RX-STATUS = "R" )
              GO TO READ-NEXT-RX.

      * RESCISSION DATE
           IF RX-INTDATE = TODAY-DATE
              ADD 1 TO COUNTER-1
           ELSE
           IF RX-INTDATE < TODAY-DATE
              ADD 1 TO COUNTER-2.

           GO TO READ-NEXT-RX.

       UPDATE-SCREEN-COUNTERS.

           IF ( COUNTER-1 NOT = ZEROES )
              STRING MESSAGE-CTR-1,COUNTER-1
                     DELIMITED BY "  " INTO MESS-CTR-BUF
              INSPECT MESS-CTR-BUF REPLACING ALL "/" BY " "
              MOVE MESS-CTR-BUF TO WR-BUF
              MOVE MESS-CTR-1-LIST TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
              MOVE SPACES TO MESS-CTR-BUF
              MOVE MESS-CTR-BUF TO WR-BUF
              MOVE MESS-CTR-1-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF ( COUNTER-2 NOT = ZEROES )
              STRING MESSAGE-CTR-2,COUNTER-2
                     DELIMITED BY "  " INTO MESS-CTR-BUF
              MOVE MESS-CTR-BUF TO WR-BUF
              MOVE MESS-CTR-2-LIST TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
              MOVE SPACES TO MESS-CTR-BUF
              MOVE MESS-CTR-BUF TO WR-BUF
              MOVE MESS-CTR-2-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       GET-SEL.

           MOVE "ENNN060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LONPQ-SCREEN
              PERFORM MENU-SELECTION
              IF ( MENU-FG = 99 )
                 GO TO GET-SEL
              ELSE
                 GO TO END-ROUTINE.

           IF VDUSTATUS NOT = "00"
              GO TO GET-SEL.

           MOVE VDUNUM0 TO FORM-BUF.

           IF ( FORM-BUF = 0 )
              GO TO GET-SEL.

           IF ( FORM-BUF > 1 )
              GO TO GET-SEL.

           GO TO M01
              DEPENDING ON FORM-BUF.

           GO TO GET-SEL.

      *=================================================================
       M01.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPQ-SCREEN.
           MOVE "LP/LONPQ1" TO FORM-NAM.
           PERFORM TEST-SECURITY.
           GO TO GET-PROG.

      *=================================================================
       GET-PROG.
       COPY "LIBGB/LOADCOLP.CPY".
       LOAD-POST.

           MOVE "LP/LONPQ" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

           PERFORM TEST-SECURITY.
           MOVE "CO" TO FORM-OBJ.

           GO TO SETUP-BR-RX.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPMENU.CPY".

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPQ_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
               WHEN OTHER   MOVE "2A" TO STAT.

      *************************************
       SETUP-LINK-INFO SECTION.
      *************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPQ-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPQ-HELP--DIR TO HELP-LINK-DIR.


      *************************************
       FORM-RESET SECTION.
      *************************************
           MOVE LPLOAN-SYSDATE TO LONPQ-SYSDATE.
           MOVE EXT-CONAME-CONAME TO LONPQ-CONAME.
           DISPLAY LONPQ-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONPQ-FORM-FIELDS TO WR-BUF.
           MOVE LONPQ-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
       MISC SECTION.
      ******************************************************************
       COPY "LIBGB/SYSTEM.CPY".
       TEST-SECURITY.
           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           MOVE "CO"        TO FORM-OBJ
           MOVE "CL/CHKSEC" TO FORM-NAM.
           CALL "CHKSEC" USING CHKSEC-FIELDS ON OVERFLOW
                MOVE CHKSEC-FORM-PATH TO FORM-PATH
                MOVE "3Z" TO FORM-OBJ
                GO TO END-ROUTINE
           END-CALL.
           CANCEL "CHKSEC".
           MOVE CHKSEC-FORM-PATH TO FORM-PATH.
           IF CHKSEC-STAT = "3S"
              MOVE "1>" TO STAT VDUSTATUS
              GO TO END-ROUTINE.
           IF CHKSEC-STAT = "3O"
              MOVE "1>" TO STAT VDUSTATUS
              GO TO END-ROUTINE.
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-RX1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPRXGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPRX1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPQ-SCREEN.
       STOP-RUN.
           EXIT PROGRAM.
