       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPW0.
       DATE-WRITTEN.    032089
      **********************************************************************
      *                 (LP)
      *  DESCRIPTION:   BATCH PAYMENT PROCESSING  MENU
      * REV:
      *  JTG 092597 ADDED BRANCH OPTION BR-BALLOON-LOANS-FG, CASTLE FINANCE
      *  JTG 110598 ADDED BRACH FILE READ FOR LPMENU REVOLVING FIELDS
      *  MJD 990429 ADDED GB/PERUSE TO VIEW REPORTS.  IF MERCURY THEN
      *             ALLOW OPTIONS TO UPDATE CORPORATE BATCH AND VIEW
      *             BATCH OUTPUT.
      *  MJD 991108 ADDED GB/PERUSE TO VIEW REPORTS.  IF REGACC THEN
      *             ALLOW OPTIONS TO UPDATE CORPORATE BATCH AND VIEW
      *             BATCH OUTPUT.
      *  MJD 000420 CHANGED VARIABLE SCREEN OPTIONS TO INCLUDE  LOCK BOX
      *             PROCESSING IF MERCURY FINANCE (MERC PR#227)
      *  BAH 010412 CHANGED VARIABLE SCREEN OPTIONS TO INCLUDE  LOCK BOX
      *             PROCESSING IF REGACC, JUST LIKE MERCURY
      *  JTG 010605 CHANGED VARIABLE SCREEN OPTIONS TO INCLUDE  TRUE PAY/ETC
      *             PROCESSING IF MERCURY FINANCE & REGACC
      *             ADDED GP-TRUE-NEXT-BATCH           (MERC PR#243/251)
      *  JTG 011112 CHANGED VARIABLE SCREEN OPTIONS TO INCLUDE  MONEYGRAM
      *             PROCESSING IF MERCURY FINANCE & REGACC
      *             ADDED MONEYGRAM FILE OPTION                  MERC PR#263
      *  BAH 020821 ADDED GP-I-AM-REGACC TO ANYWHERE GP-I-AM-MERC, KEVIN
      *             GOT REGACC TO USE MERCURYS TRUE PAY LAYOUT...
      *  JTG 021011 ADDED NEW CODE FILE TYPE 'BR' -BATCH PAYMENT REFERENCE CODES
      *             BATCH PAYMENT REVISIONS                         REGACC #0006
      *  JTG 021226 ADDED GP BATCH RANGES FOR DISPLAY               REGACC #0006
      *  JTG 040407 ADDED GP-OTREF-NEXT-BATCH  900-909             CENTRIX #2254
      *  BLF 051013 ADDED REDEFINITION OF FORM-PATHNAME FOR TEST IN
      *             DECLARATIVES
      *  BLM 101202 IF NEW SECURITY (GP-SECURITY-FG = Y), DO NEW SECURITY
      *             CHECKS, PR# NEWSECURITY
      *  BLM 111130 ACTIVATE NEW SECURITY ROUTINES IN A15, PR# NEWSECURITY
      *  BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 2024.0628 REMOVED SELECTIONS 2. BATCH PAYMENT ENTRY (LONPW2),
      *               3. BATCH PAYMENT FILE LISTING (LONPW3) AND
      *               4. BATCH PAYMENT FILE UPDATE (LONPW4) PER KEVIN S35Q-78
      **************************************************************************
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPW0.CBL S34 05/25/21-07:46:33 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  FORM-BUF                    PIC 9(6)  VALUE 0.
       01  FORM-LIST                   PIC X(4)  VALUE "SEL.".
       01  BATCH-BUF.
           03  BATCH-NO                PIC 999   VALUE 0.
           03  BATCH-NO-REFCD          PIC X(5)  VALUE " ".
       01  BATCH-NO-LIST               PIC X(19) VALUE
           "BATCHNO BATCHREFCD.".
       01  MSEL-LIST                   PIC X(5)  VALUE "MSEL.".

       COPY "LIBLP/LPBPPATH.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  ALLOW-SEQ        VALUE 1.

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPW_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".
       COPY "LIBLP/LONPW_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       LINKAGE SECTION.
       01  FORM-PATHNAME      PIC X(22).
       01  EXIT-PATHNAME      PIC X(22).

       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPW_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.

      ********************************
      *  C O N T R O L   M O D U L E *
      ********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPW" TO VDU-FORM-NAME.

           MOVE "FOR BATCH PAYMENT MENU" TO PASSWDW-PROMPT2.
           MOVE ALLOW-SEQ TO PASSWDW-SEQ.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPW-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO GET-SEL
              ELSE
                 GO TO EXIT-PROG.

           PERFORM GET-GPENV.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

       GET-SEL.

           PERFORM FORM-RESET.

           IF BATCH NOT = 0
              MOVE BATCH       TO BATCH-NO
              MOVE BATCH-REFCD TO BATCH-NO-REFCD
              MOVE BATCH-BUF     TO WR-BUF
              MOVE BATCH-NO-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE SPACES            TO WR-BUF.
           MOVE "RANGE07 DESC07." TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACES TO WR-BUF.
           MOVE "RANGE08 DESC08 RANGE09 DESC09 DESC10 DESC11."
                                  TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACES    TO WR-BUF.
           MOVE FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "ENTER SELECTION, F6-VIEW REPORTS, F7-MENU:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LONPW-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO GET-SEL
              ELSE
                 GO TO EXIT-PROG.

           IF VDUSTATUS = "1<"
              MOVE "GB/PERUSE" TO FORM-NAM
              CALL "PERUSE" USING FORM-PATH EXIT-PATHNAME
              CANCEL "PERUSE"
              GO TO GET-SEL.

           IF VDUSTATUS NOT = "00"
              GO TO GET-SEL.
           MOVE VDUNUM0 TO FORM-BUF.
           IF FORM-BUF = 0
              GO TO GET-SEL.

           IF FORM-BUF > 6
              GO TO GET-SEL.

           IF FORM-BUF > 1 AND FORM-BUF < 7
              IF BATCH = 0
                 MOVE "BATCH NUMBER MUST BE ESTABLISHED FIRST!"
                                                             TO MESS
                 PERFORM SEND-MESS
                 GO TO GET-SEL.

           GO TO M01 M02 M03
              DEPENDING ON FORM-BUF.
           GO TO GET-SEL.
      * SET BATCH NO. TO PROCESS
       M01.
           MOVE "LP/LONPW1" TO FORM-NAM.
           GO TO GET-PROG.
      * REFCD        9. REFCD FILE UPDATE (CORP)
       M02.
           MOVE "LP/LONPW9" TO FORM-NAM.
           GO TO GET-PROG.

      * BPOTH       6. OTHER BAL REFCD FILE UPDATE (CORP)
       M03.
           MOVE "LP/LONPWA" TO FORM-NAM.
           GO TO GET-PROG.

       GET-PROG.
           IF FORM-NAM NOT = "LP/LONPW1"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPW-SCREEN.

      **************************************************************
      * THE SECURITY CHECK HERE IS HARD-CODED INSTEAD OF COPYING
      * LIBGB/GETFM.  COPY MEMBER GOES TO EXIT-PROG ON ERROR & THE
      * SECURITY ERROR MSG IS NOT DISPLAYED, EVEN THO IT STOPS
      * YOU FROM RUNNING SELECTION.  NOTE, THE SAME THING HAPPENED
      * IN A15, SECURITY WORKED BUT NO MSG.  I THINK THIS IS A
      * SPECIAL SITUATION SINCE THIS IS CALLED BY LONP.  BARB
      * 1/17/17 REMOVE SEND-MESS WITH SECURITY ERROR, THIS IS IN
      *         CHKSEC SO WE WERE GETTING IT TWICE.  BARB
      **************************************************************
       CHECK-SECURITY.
           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           MOVE "CO"        TO FORM-OBJ.
           MOVE "CL/CHKSEC" TO FORM-NAM.
           CALL "CHKSEC" USING CHKSEC-FIELDS ON OVERFLOW
                MOVE CHKSEC-FORM-PATH TO FORM-PATH
                MOVE "3Z" TO FORM-OBJ
                GO TO EXIT-PROG
           END-CALL.
           CANCEL "CHKSEC".
           MOVE CHKSEC-FORM-PATH TO FORM-PATH.
           IF CHKSEC-STAT = "3S"
              MOVE "1>" TO STAT VDUSTATUS
              GO TO GET-SEL.
           IF CHKSEC-STAT = "3O"
              MOVE "1>" TO STAT VDUSTATUS
              GO TO GET-SEL.

       COPY "LIBGB/LOADCOLP.CPY".

       LOAD-POST.
           MOVE "LP/LONPW" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

           GO TO GET-SEL.

       COPY "LIBLP/LPMENU.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPW_EVA.CPY".
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
              MOVE LONPW-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPW-HELP--DIR TO HELP-LINK-DIR.


      *************************************
       FORM-RESET SECTION.
      *************************************
           MOVE LPLOAN-SYSDATE TO LONPW-SYSDATE.
           MOVE LPLOAN-CONAME TO LONPW-CONAME.
           DISPLAY LONPW-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONPW-FORM-FIELDS TO WR-BUF. 
           MOVE LONPW-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***********************************
       MISC-ROUTINES SECTION.
      ***********************************
       COPY "LIBGB/SYSTEM.CPY".
       VERIFY-PASSWORD.
           MOVE "LONPW0" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD" TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************
       IO-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
            MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
       END-RUN.
       EXIT-PROG.
       LOAD-RESTART.
       STOP-RUN.
            DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
            DESTROY LONPW-SCREEN.
       EXIT-RETURN.
           EXIT PROGRAM.
