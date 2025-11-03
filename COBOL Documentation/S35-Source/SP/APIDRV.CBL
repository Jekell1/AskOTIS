       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       APIDRV.
       DATE-WRITTEN. 02/10/2025.
      *************************************************************************
      *  DESC:  API DRIVER PROGRAM 
      *
      *   OPTION 1. CALCULATE, VERIFY, CREATE ASCII OUTPUT FILE WITH
      *             CALCULATION RESULTS AND A LOG
      *   OPTION 2. CALCULATE AND VERIFY AGAIN, CREATE A VISION STAGEFILE
      *             WITH COPY OF LN-REC AND NEXT ACCOUNT # THEN PRINT DOCS
      *   OPTION 3. CUSTOMER ACCEPTS, READ STAGEFILE AND CREATE LOAN
      *             AND DISBURSE TO DEBIT CARD
      *   OPTION 4. OFFER DECLINED, REMOVE STAGEFILE RECORD
      *
      *         $ SIGN RUN, NO SCREENS
      *
      * TO RUN IT;
      *    APIDRV.SH 1,CORRELATION#, BRNO, SSNO, CAN LOAN, DEBITCARD #
      *              ^ PROCESS TO RUN 
      *
      *    APIDRV.SH 0001123456789012001111122333304
      *
      * CREATES LOG AND ASCII FILE IN ../FTP/LOANS/
      *
      *   1 - CALLS APILNS FOR CALCULATIONS AND RESTRICTIONS 
      *             APILNS CALLS CALCZL AND LNVERI
      *             IF SUCCESSFUL, CREATES A LOG AND ASCII FILE WITH CALCS
      *               IN FTP/LOANS
      *                 L.(CORRELATION#).YYMMDD     LOG WITH PASS OR FAIL
      *                 A.(CORRELATION#).YYMMDD     ASCCI FILE WITH CALCS
      *
      *  CUSTOMER ACCEPTS OFFER
      *
      *   2 - CALLS APILNS FOR CALCULATIONS AND RESTRICTIONS AGAIN
      *             APILNS CALLS CALCZL AND LNVERI
      *             IF SUCCESSFUL, CREATES A NEW VISION FILE (STAGE) WITH
      *              PD-REC BURIED IN IT SO WE CAN PASS THAT TO LN-REC AND
      *              PRINT DOCUMENTS IN THE END.
      * 
      *  CUSTOMER HAS SIGNED DOCS
      *
      *   3 - CALLS APILNS TO TURN THE STAGEFILE RECORD INTO A LOAN AND
      *             DISBURSE TO A DEBIT CARD
      *
      * 2025.0210 BAH 
      *************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOG-FILE ASSIGN LOG-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      **************************************************
      *  LOG FILE                      
      *   LG/REFI/(CORRELATION.YYMMDD)
      **************************************************
       FD  LOG-FILE
           LABEL RECORDS ARE STANDARD.
       01  LOG-REC.
      * PASS OR FAIL
           03  LOG-PASS-FAIL     PIC X(4).
           03  FILLER            PIC X.
           03  LOG-STATUS        PIC 9(4).
           03  FILLER            PIC X.
           03  LOG-DATE          PIC 9999/99/99.
           03  FILLER            PIC X.
           03  LOG-TIME          PIC X(8).
           03  FILLER            PIC X.
           03  LOG-BRNO          PIC 9(4).
           03  FILLER            PIC X.
           03  LOG-SSNO          PIC 999/99/9999.
           03  FILLER            PIC X.
           03  LOG-MSG           PIC X(36).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/APIDRV S35 03/14/25-14:04:14 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       01  LOG-PATH.
           03  FILLER             PIC X(01)   VALUE "/".
           03  LOG-BASE           PIC X(09).
           03  LOG-MACHINE        PIC X(02).
           03  LOG-DIRECTORY      PIC X(5) VALUE "/FTP/".
           03  FILLER             PIC X(8)  VALUE "LOANS/L.".
           03  LOG-PATHNAME.
               05  LOG-CORRELATION     PIC X(12).
               05  FILLER              PIC X    VALUE ".".
               05  LOG-PATH-YY    PIC 99.
               05  LOG-PATH-MM    PIC 99.
               05  LOG-PATH-DD    PIC 99.

      *-----------------------------------------------------------------
      * ENVIROMENT: POSTNAME AS IT WAS WHEN USER ENTERED PROGRAM
      *-----------------------------------------------------------------
       01  ORIG-POSTNAME.
           03  ORIG-SECURITY   PIC X.
           03  ORIG-BR         PIC 9(4).
           03  ORIG-NAME       PIC X(5).
           
      *-----------------------------------------------------------------
      * ENVIROMENT: FIL AS IT WAS WHEN USER ENTERED PROGRAM
      *-----------------------------------------------------------------
       01  ORIG-FIL.
           03  ORIG-XXX        PIC X(3).
           03  ORIG-SLASH1     PIC X.
           03  ORIG-XXXX       PIC X(4).
           03  ORIG-SLASH2     PIC X.
           03  ORIG-MACHINE    PIC X(2).
           03  ORIG-SLASH3     PIC X.
           03  ORIG-DATA-PATH  PIC X(6).

       01 WORK-USERID.
          03  WORK-SECURITY        PIC X.
          03  WORK-BR              PIC 9(4).
          03  WORK-NAME            PIC X(5).

       01 HOLD-FIL.
          03  HOLD-XXX             PIC X(3).
          03  HOLD-SLASH1          PIC X.
          03  HOLD-XXXX            PIC X(4).
          03  HOLD-SLASH2          PIC X.
          03  HOLD-MACHINE         PIC X(2).
          03  HOLD-SLASH3          PIC X.
          03  HOLD-DATA-PATH       PIC X(6).

       01  USERID-GROUP-NAME.
           03  USER-GROUP           PIC X(3).
           03  FILLER               PIC X(2).


       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPLOANW.CPY".
      *COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       

       01  INPUT-STRING              PIC X(41).
       01  INPUT-REC.
           03  INPUT-PROCESS-CODE    PIC 9(4).
           03  INPUT-PROCESS-CODEX REDEFINES
                                     INPUT-PROCESS-CODE PIC X(4).
           03  INPUT-CORRELATION     PIC X(12).
           03  INPUT-BRNO            PIC 9(4).
           03  INPUT-BRNOX REDEFINES INPUT-BRNO PIC X(4).
           03  INPUT-SSNO            PIC 9(9).
           03  INPUT-SSNOX REDEFINES INPUT-SSNO PIC X(9).
           03  INPUT-CAN-LOAN        PIC 9(2).
           03  INPUT-CAN-LOANX REDEFINES INPUT-CAN-LOAN PIC X(2).
           03  INPUT-DEBIT-CARD      PIC 9(10).
           03  INPUT-DEBIT-CARDX REDEFINES INPUT-DEBIT-CARD PIC X(10).
       01  RESULT-REC.
           03  RESULT-STATUS          PIC 9(4).
           03  RESULT-MSG             PIC X(40).
 
       01  FORM-PATH.
           03  FILLER         PIC X.
           03  FORM-REL       PIC X(7).
           03  FILLER REDEFINES FORM-REL.
               05  FILLER     PIC X(4).
               05  LOGUID-REL PIC X(3).
           03  FILLER         PIC X.
           03  FORM-OBJ       PIC XX.
           03  FILLER         PIC X.
           03  FORM-NAM.
               05  FILLER     PIC X(3).
               05  FORM-PROGX PIC X(6).
           03  FILLER         PIC X.

       01  FORM-PATHNAME      PIC X(22).
       01  EXIT-PATHNAME      PIC X(22).

       PROCEDURE DIVISION CHAINING INPUT-STRING.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON LOG-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-BR-FILE.
           CLOSE LOG-FILE.
       COPY "LIBGB/DECLAR2.CPY".
 
      ******************************************************************
      *
      *  PROGRAM CONTROL
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.

      * NEED TO GET EXTERNAL VARIABLES SET
           MOVE "CO" TO FORM-OBJ.
           MOVE "GB/SETENV" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

           PERFORM GET-GPENV.

           MOVE 99 TO RESULT-STATUS.

      * NEED HERE FOR CREATE-LOG

           ACCEPT DATE-YYMMDD FROM DATE.
           PERFORM CONVERT-YYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO TRANS-DATE.

           MOVE INPUT-STRING TO INPUT-REC.

           IF INPUT-PROCESS-CODEX NOT NUMERIC
              MOVE 7                               TO RESULT-STATUS
              MOVE "APIDRV: NON NUMERIC PROCESS ID; &&&&" TO RESULT-MSG
              INSPECT RESULT-MSG REPLACING FIRST "&&&&" 
                                              BY INPUT-PROCESS-CODEX
              PERFORM CREATE-LOG
              GO TO END-PROGRAM.

           IF INPUT-BRNOX NOT NUMERIC
              MOVE 2                              TO RESULT-STATUS
              MOVE "APIDRV: INVALID BRANCH; &&&&" TO RESULT-MSG
              INSPECT RESULT-MSG REPLACING FIRST "&&&&" BY INPUT-BRNOX
              PERFORM CREATE-LOG
              GO TO END-PROGRAM.

           IF INPUT-SSNOX NOT NUMERIC
              MOVE 3                                 TO RESULT-STATUS
              MOVE "APIDRV: INVALID SSNO; &&&&&&&&&" TO RESULT-MSG
              INSPECT RESULT-MSG REPLACING FIRST "&&&&&&&&&"
                                                     BY INPUT-SSNOX
              PERFORM CREATE-LOG
              GO TO END-PROGRAM.

           IF INPUT-CAN-LOANX NOT NUMERIC
              MOVE 5                              TO RESULT-STATUS
              MOVE "APIDRV: INVALID CAN-LOAN; &&" TO RESULT-MSG
              INSPECT RESULT-MSG REPLACING FIRST "&&"
                                                  BY INPUT-CAN-LOANX
              PERFORM CREATE-LOG
              GO TO END-PROGRAM.

      D    STOP MESS.
      * FIRST 2 OPTIONS DO NOT USE OR NEED DEBIT CARD
           IF INPUT-PROCESS-CODE = 3
              IF INPUT-DEBIT-CARDX NOT NUMERIC
                 MOVE 34                           TO RESULT-STATUS
                 MOVE "APIDRV: INVALID DEBIT-CARD" TO RESULT-MSG
                 PERFORM CREATE-LOG
                 GO TO END-PROGRAM.

           PERFORM INITIALIZATION.

           MOVE INPUT-BRNO TO BR-KEY.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9 
              MOVE 2                                 TO RESULT-STATUS
              MOVE "APIDRV: BRANCH &&&& NOT ON FILE" TO RESULT-MSG
              INSPECT RESULT-MSG REPLACING FIRST "&&&&" BY INPUT-BRNO
              PERFORM CREATE-LOG
              GO TO END-PROGRAM.

      * ONLY PROCESS FILES FOR THE MACHINE YOU ARE ON

           IF BR-MACHINE NOT = ORIG-MACHINE
              MOVE 4                                   TO RESULT-STATUS
              MOVE "APIDRV: BRANCH NOT ON THIS SERVER" TO RESULT-MSG
              PERFORM CREATE-LOG
              GO TO END-PROGRAM.

      *************************************************************

      * INPUT-PROCESS-CODE 1 = CALCS AND VERIFY, WRITE ASCII OUTPUT & LOG
      * INPUT-PROCESS-CODE 2 = CALCS AND VERIFY, WRITE VISION STAGE FILE
      * INPUT-PROCESS-CODE 3 = TURN STAGE FILE INTO LOAN REC AND DISBURSE

           IF INPUT-PROCESS-CODE = 1 OR 2 OR 3 OR 4
              MOVE "SP/APILNS" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH INPUT-REC RESULT-REC
              CANCEL FORM-PROGX
           ELSE
              MOVE 7                             TO RESULT-STATUS
              MOVE "APIDRV: INVALID PROCESS CODE" TO RESULT-MSG
              PERFORM CREATE-LOG
              GO TO END-PROGRAM.
 
      * LOG EITHER SUCCESSFUL OR THE ERROR

           PERFORM CREATE-LOG.

           MOVE RESULT-STATUS TO RESULT-STATUS.
        
           GO TO END-PROGRAM.


      ******************************************
       CREATE-LOG SECTION.
      ******************************************
      D    STOP MESS.
           CALL "C$TOLOWER" USING LOG-DIRECTORY, VALUE 5.
           MOVE TRANS-DATE     TO DATE-MMDDYY.
           MOVE DATE-MMDDYY-YY TO LOG-PATH-YY.
           MOVE DATE-MMDDYY-MM TO LOG-PATH-MM.
           MOVE DATE-MMDDYY-DD TO LOG-PATH-DD.
           MOVE INPUT-CORRELATION TO LOG-CORRELATION.

           MOVE ORIG-FIL   TO LOG-BASE.
           MOVE BR-MACHINE TO LOG-MACHINE.

           MOVE LOG-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              PERFORM OPEN-LOG-FILE-EXTEND
           ELSE
              PERFORM OPEN-LOG-FILE-OUTPUT.

           IF RESULT-STATUS = 99
              MOVE SPACES TO LOG-PASS-FAIL
           ELSE
           IF RESULT-STATUS = 0
              MOVE "PASS" TO LOG-PASS-FAIL
           ELSE
              MOVE "FAIL" TO LOG-PASS-FAIL.

           MOVE RESULT-MSG         TO LOG-MSG.
           MOVE RESULT-STATUS      TO LOG-STATUS.
           MOVE TRANS-DATE         TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO LOG-DATE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT          TO LOG-TIME.
           MOVE INPUT-BRNO         TO LOG-BRNO.
           MOVE INPUT-SSNO         TO LOG-SSNO.
           INSPECT LOG-SSNO REPLACING ALL "/" BY "-".
           PERFORM WRITE-LOG-FILE.
           PERFORM CLOSE-LOG-FILE.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE INPUT-BRNO TO BRANCH.

           MOVE " " TO ERRCD.
            
           ACCEPT ORIG-FIL      FROM ENVIRONMENT "FIL".
           ACCEPT ORIG-POSTNAME FROM ENVIRONMENT "POSTNAME".
           ACCEPT DATE-YYMMDD FROM DATE.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE.

           PERFORM OPEN-BR-FILE.

       EXIT-INITIALIZE.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".

      *COPY "LIBGB/SCREEN.CPY".

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       OPEN-LOG-FILE-EXTEND.
           OPEN EXTEND LOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-LOG-FILE-EXTEND.
           IF IO-FG = 7
              CLOSE LOG-FILE
              GO TO OPEN-LOG-FILE-EXTEND.

       OPEN-LOG-FILE-OUTPUT.
           OPEN OUTPUT LOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-LOG-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE LOG-FILE
              GO TO OPEN-LOG-FILE-OUTPUT.
       OPEN-LOG-FILE.
           OPEN INPUT LOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-LOG-FILE.
           IF IO-FG = 7
              CLOSE LOG-FILE
              GO TO OPEN-LOG-FILE.
       WRITE-LOG-FILE.
           WRITE LOG-REC.
       CLOSE-LOG-FILE.
           CLOSE LOG-FILE.
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-PROGRAM.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           MOVE RESULT-STATUS TO RETURN-CODE.

           EXIT PROGRAM RETURNING RETURN-CODE.
