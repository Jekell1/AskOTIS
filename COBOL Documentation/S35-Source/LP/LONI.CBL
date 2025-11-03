      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONI.
      **************************************************************************
      *   DIR :  LP
      *   DESC:  LOAN CONVERSION DRIVER
      *          SCREEN  PROGRAM  DESC
      *          LONIA   LONIA0   ENTER TRANSACTION DATE
      *          LONIE   LONIE0   BORROWER ENTRY
      *          EONIE   EONIE0   BORROWER PAGE 2
      *          LONIF   LONIF0   TRANSACTION POSTING
      *                  LONIF1   TRANSACTION LINES
      *          LONIZ   LONIZ0   EARNINGS TESTER        353509
      *                  LONIZ1   EARNINGS TESTER
      *          LONIY   LONIY0   EARNINGS TESTER #2     353508
      *                  LONIY1   EARNINGS TESTER #2
      *          LONIG   LONIG0   MEMO POSTING
      *          LONIJ   LONIJ0   SALES FINANCE ENTRY PG1
      *          LONIK   LONIK0   SALES FINANCE ENTRY PG2
      *          LONIN   LONIN0   DIRECT LOAN ENTRY PG1
      *          XONIN   XONIN0   CALC DIRECT LOAN ENTRY PG1
      *          LONIO   LONIO0   DIRECT LOAN ENTRY PG2
      *          LONIU   LONIU0   S.F. MOD/INQ PG3
      *          LONIV   LONIV0   DIRECT MOD/INQ PG3
      *
      * REV:
      * MJD 110697 EXIT PROGRAM TO BE LP/LPCVMU
      * CS  991001 ADDED FLDVAL ON NEW FIELD 'BKCVFG' TO DETERMINE WHETHER
      *            TO DO A BULK PURCHASE OR ACCOUNT CONVERSION
      * BAH 000911 REMOVE WORK FILE !!  (SAME LOGIC AS LONP.C)
      * BAH 050510 ADDED RESERVATION OF OP-FILE, SO THE DAY DOESNT GET CLOSED
      *            IN LOAN PROCESSING AND CONVERSION LOSES STATS AND GL
      *            REGMGM PL# 488
      * BLF 051013 ADDED REDEFINITION OF FORM-PATHNAME FOR TEST IN
      *            DECLARATIVES
      * BLM 101202 ADD GPENV, IF NEW SECURITY (GP-SECURITY-FG = Y), DO
      *            NEW SECURITY CHECKS, PR# NEWSECURITY
      * BLM 111130 ACTIVATE NEW SECURITY ROUTINES IN A15, PR# NEWSECURITY
      *  CS 170606 ACTIVATE 8 DIGIT DATES
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSOP.CPY".
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDOP.CPY".
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)./LP/LONI.CBL S35 02/27/24-11:52:14 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       01  OP-IFN             PIC X(2)  VALUE "OP".

       01  PROG-WK.
           03  FILLER        PIC X(7)  VALUE "LP/LONI".
           03  PROG-NO       PIC XX    VALUE "A0".
       01  PROG-WKX REDEFINES PROG-WK.
           03  FILLER               PIC X(3).
           03  PROG-WKX-PREFIX-NO.
               05  PROG-WKX-PREFIX  PIC X(4).
               05  PROG-WKX-NO      PIC XX.
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBLP/LPLONIW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

       LINKAGE SECTION.
       01  FORM-PATHNAME PIC X(22).
       01  EXIT-PATHNAME PIC X(22).
       01  BKCVFG-BUF    PIC X.

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME BKCVFG-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              OP-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-BR-FILE.
           CLOSE OP-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **********************************
       MAIN-PROGRAM SECTION.
      **********************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.

      * PRE-LOAD OPTIONS (DEBUG MODE, ETC)
           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LP/LONIA" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       COPY "LIBGB/CHKSEC.CPY".

      * MAKE SURE INITIAL NON DISPLAY BUFFERS CLEARED/SETUP PROPERLY
      * THESE WERE NON DISPLAY BUFFERS, NOW THEY WILL GET PASSED

           MOVE SPACES TO PROG-BUF.
           MOVE 0 TO TRANS-DATE BRANCH LOAN-KEY BORROWER-KEY MONTH-DATE.
           MOVE EXT-CONAME-SYSDATE TO LPLONI-SYSDATE.
           MOVE EXT-CONAME-USER-ID TO LPLONI-USERID.

           MOVE "LONI" TO PROG-WKX-PREFIX.
           MOVE "A0A" TO NEW-PROG-FORM.

           GO TO GET-PROG.
       GET-FORM.
           MOVE NEW-FORM TO PROG-NO.

      *    MOVE "FM" TO FORM-OBJ.

           MOVE PROG-WK TO FORM-NAM.

      *    CALL GTFORM USING STAT FORM-PATH.
      *    IF STAT NOT = "00"
      *       PERFORM MISSING-FORM
      *       GO TO END-ROUTINE.

      * CHECK FOR NEW SECURITY:
           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           MOVE "CO"        TO FORM-OBJ.
           MOVE "CL/CHKSEC" TO FORM-NAM.
           CALL "CHKSEC" USING CHKSEC-FIELDS ON OVERFLOW
                MOVE CHKSEC-FORM-PATH TO FORM-PATH
                MOVE "3Z" TO FORM-OBJ
                GO TO EXIT-PROG
           END-CALL.
           CANCEL "CHKSEC".

           MOVE CHKSEC-STAT TO STAT.
           IF STAT NOT = "00"
              MOVE STAT TO FORM-OBJ
              GO TO EXIT-PROG.

       GET-PROG.
           MOVE NEW-PROG TO PROG-NO.
           MOVE PROG-WK TO FORM-NAM.

      *   SET BULK/CONVERSION FLAG:
           MOVE BKCVFG-BUF TO BULK-CONVERSION-FG.

       COPY "LIBGB/LOADCO_LONI.CPY".
       LOAD-POST.
           IF ERRCD = "X"
              GO TO END-ROUTINE.
           IF ERRCD = "E"
              PERFORM ERR-CONV
              GO TO END-ROUTINE.

           IF PROG-WKX-NO = "A0"
              MOVE BRANCH TO BR-NO
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              IF IO-FG = 9
                 MOVE "BRANCH RECORD CANNOT BE ACCESSED" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-ROUTINE.

           IF ERRCD = "P"
              MOVE NEW-PROG TO PROG-WKX-NO
              PERFORM SELECT-VERSION
              GO TO GET-PROG.

           MOVE NEW-FORM TO PROG-WKX-NO.
           PERFORM SELECT-VERSION.
           GO TO GET-FORM.

      *******************************************
       MISC-ROUTINES SECTION.
      *******************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/RMFILE.CPY".
       FORM-RESET.
       SETUP-LINK-INFO.
       VDU-CONVERT-FIELD.
       SELECT-VERSION.
           MOVE "LONI" TO PROG-WKX-PREFIX.
           IF BR-FRONT-END-FG = "Y"
              IF (PROG-WKX-NO = "N0" OR "N ")
                 MOVE "XONI" TO PROG-WKX-PREFIX
              ELSE
                 MOVE "LONI" TO PROG-WKX-PREFIX.
           IF PROG-WKX-NO = "E0" OR "E "
              IF ERRCD = "2"
                 MOVE "EONI" TO PROG-WKX-PREFIX.
       ERR-CONV.
           MOVE "ERROR IN LOAN CONVERSIONS" TO MESS
           PERFORM SEND-MESS.
           DESTROY ALL CONTROLS.

       COPY "LIBGB/DATER.CPY".

      ******************************************
       IO-ROUTINE SECTION.
      ******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPEDPATHIO.CPY".
       OPEN-OP-FILE.
           PERFORM OPEN-IT.
           MOVE OP-IFN TO E-FILE.
           OPEN I-O OP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OP-FILE.
           IF IO-FG = 7
              PERFORM CLOSE-OP-FILE
              GO TO OPEN-OP-FILE.
       CLOSE-OP-FILE.
           MOVE "CLOSE" TO E-MSG.
           CLOSE OP-FILE.

      ******************************************
       END-ROUTINE SECTION.
      ******************************************
       EXIT-PROG.
           IF USER-PID NOT = " "
              MOVE USER-PID TO TEMP-PID
              MOVE TEMP-PATH TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE.

           DESTROY ALL CONTROLS.

       LOAD-RESTART.
           IF BKCVFG-BUF = "C"
              MOVE "LP/LPCVMU" TO FORM-NAM
           ELSE
              MOVE "LP/LPBKMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           EXIT PROGRAM.
