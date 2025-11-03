       IDENTIFICATION DIVISION.
       PROGRAM-ID.      APIDOC.
       DATE-WRITTEN.    10/23/2024.
      **************************************************************************
      *                 (SP)
      *  DESCRIPTION:   F3 ALL DOCUMENT PRINT $ SIGN RUN, NO SCREEN
      *                   (FROM LONPM0, MODELED AFTER APIPAY)
      *
      *  FOR PROGRAMMERS TO RUN IT;
      *         . SHH
      *         SH ./APIDOC.SH BRNO/ACCTNO (8 DIGITS, NO 01)
      *                        0011078865
      *
      * RETURN-STATUS = 0 = SUCCESSFUL UPDATE
      *
      *  REV:
      *   2024.1031 BAH #1683 S35Q-142
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * NEED FOR MANIFEST FILE
       COPY "LIBFP/FPFSPST.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
 
       COPY "LIBGB/GBFDPR.CPY".
      * NEED FOR MANIFEST FILE
       COPY "LIBFP/FPFDPST.CPY".
 
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/APIDOC S35 10/31/24-15:59:10 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
      * NEED FOR MANIFEST FILE
       COPY "LIBFP/FPWSPST.CPY".

       01  PR-IFN          PIC X(4) VALUE "B6PR".
 
       01  ERR-NUM             PIC 9(09)  COMP-4.
       01  RETURN-STATUS       PIC 9(6) VALUE 0.
       01  INPUT-STRING.
           03  INPUT-BRANCH PIC 9(4).
           03  INPUT-NUMBER PIC 9(8).

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
       01  ORIG-POSTNAME.
           03  ORIG-SECURITY   PIC X.
           03  ORIG-BR         PIC 9(4).
           03  ORIG-NAME       PIC X(5).

       01 NEW-USERID.
          03  NEW-SECURITY        PIC X.
          03  NEW-BR              PIC 9(4).
          03  NEW-NAME            PIC X(5).
       01  NEW-DIRECTORY       PIC X(22) VALUE SPACES.
      *01  NEW-FIL.
      *    03  NEW-XXX             PIC X(3).
      *    03  NEW-SLASH1          PIC X.
      *    03  NEW-XXXX            PIC X(4).
      *    03  NEW-SLASH2          PIC X.
      *    03  NEW-MACHINE         PIC X(2).
      *    03  NEW-SLASH3          PIC X.
      *    03  NEW-DATA-PATH       PIC X(6).

       01  PRU-PATH-LIST   PIC X(6) VALUE "PPATH.".

       78  PROG-DIR-PREFIX-LONP                  VALUE "LP/LONP".
       78  PROG-DIR-PREFIX-PCL                   VALUE "FP/PCL ".
       78  PROG-DIR-PREFIX-PST                   VALUE "FP/PST ".
       78  PROG-DIR-PREFIX-PRT                   VALUE "FP/PRT ".
       01  PROG-WK.
           03  PROG-DIR-NAME       PIC X(07)     VALUE "LP/LONP".
           03  FILLER              REDEFINES PROG-DIR-NAME.
               05  FILLER          PIC X(06).
               05  PROG-DIR-PREFIX PIC X(01).
           03  PROG-WK-IP          PIC X(02)     VALUE SPACES.

       01  HOLD-OPT-OUT         PIC X.
       01  HOLD-SSNO.
           03  SSNO-FIRST-3     PIC 9(3).
           03  SSNO-MID-2       PIC 9(2).
           03  SSNO-LAST-4      PIC 9(4).
       01  HOLD-MAKER-FNAME     PIC X(16).
       01  HOLD-MAKER-LNAME     PIC X(16).
       01  HOLD-MAKER-DOB       PIC 9(08).
       01  HOLD-COMAKER-FNAME   PIC X(16).
       01  HOLD-COMAKER-LNAME   PIC X(16).
       01  HOLD-COMAKER-OPT-OUT PIC X.

       01  SUB             PIC 9(6) COMP-3.
       01  INSPECT-W       PIC X(86).
       01  ADD-ON          PIC X(25).

      *************************
      *  INTERMEDIATE PROG    *
      *************************
       COPY "LIBLP/LPIPBUF.CPY".

      *COPY "LIBWI/WINDOWSW".
      *COPY "LIBGB/CALLID".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
      *COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
 
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
               05  FORM-DIRX  PIC X(2).
               05  FILLER     PIC X.
               05  FORM-PROGX PIC X(6).
           03  FILLER         PIC X.

       01  FORM-STAT          PIC X(2).
       01  FORM-PROG          PIC X(6) VALUE " ".
       01  FORM-SPANISH       PIC X    VALUE " ".

       01  COCHK-FIELDS.
           03  SAVE-FORM-PATH     PIC X(22).
           03  COCHK-STAT         PIC X(02).
       01  GP-REC-FOR-SEC.
           03  FILLER             PIC X(458).
           03  GP-NEW-SECURITY    PIC X.
           03  FILLER             PIC X(53).

       01  FORM-PATHNAME      PIC X(22).
       01  TEST-FORMPATHNAME REDEFINES FORM-PATHNAME.
           03  FILLER         PIC X(15).
           03  TEST-FORM-1ST4 PIC X(4).
           03  FILLER         PIC X(3).
       01  EXIT-PATHNAME      PIC X(22).
       COPY "LIBLP/LPLOANW.CPY".

       PROCEDURE DIVISION CHAINING INPUT-STRING.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              PR-FILE MANIFEST-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE MANIFEST-FILE.
       COPY "LIBGB/DECLAR2.CPY".
 
      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.

      * NEED TO GET EXTERNAL VARIABLES SET
           MOVE "CO"        TO FORM-OBJ.
           MOVE "GB/SETENV" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE 99 TO RETURN-STATUS.

           PERFORM GET-GPENV.

           IF INPUT-BRANCH = 0 OR INPUT-BRANCH NOT NUMERIC
              MOVE 03 TO RETURN-STATUS
              GO TO END-ROUTINE.

           IF INPUT-NUMBER = 0 OR INPUT-NUMBER NOT NUMERIC
              MOVE 04 TO RETURN-STATUS
              GO TO END-ROUTINE.

           
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF ( IO-FG NOT = ZEROES )
              MOVE 06                       TO RETURN-STATUS
              GO TO END-ROUTINE.

           PERFORM OPEN-BR-FILE.
           MOVE INPUT-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE 03 TO RETURN-STATUS
              GO TO END-ROUTINE
           ELSE
              PERFORM CLOSE-BR-FILE.

           IF BR-ALLOW-F3-ALL-DOCS NOT = "Y"
              MOVE 17 TO RETURN-STATUS
              GO TO END-ROUTINE.

           ACCEPT ORIG-FIL      FROM ENVIRONMENT "FIL".
      * ONLY PROCESS FILES FOR THE MACHINE YOU ARE ON
           IF BR-MACHINE NOT = EXT-FILPATH-MACHINE
              MOVE 14                TO RETURN-STATUS
              GO TO END-ROUTINE.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"              TO BW-PATH-OVERRIDE
                                    LN-PATH-OVERRIDE.

           PERFORM CHANGE-DIR-MODULE.
           IF RETURN-STATUS NOT = 99
              GO TO END-ROUTINE.

           MOVE "N" TO STAT.
           MOVE "PRU"  TO PRU-PATH.

      *
      *    CALL XFER USING STAT PRU-PATH-LIST PRU-PATH.

           MOVE " " TO ROUTE-CD.
           MOVE "S" TO ERRCD.

           MOVE INPUT-NUMBER TO LN-ACCTNO LOAN-KEY.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              MOVE 04                       TO RETURN-STATUS
              GO TO END-ROUTINE.

           IF ( LN-POCD-VOID )
              MOVE 07                       TO RETURN-STATUS
              GO TO END-ROUTINE.

           IF ( LN-POCD-RESCIND )
              MOVE 08                       TO RETURN-STATUS
              GO TO END-ROUTINE.

           PERFORM OPEN-SP1-FILE.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           IF IO-FG NOT = 0
              MOVE 09                       TO RETURN-STATUS
              GO TO END-ROUTINE.
           PERFORM CLOSE-SP1-FILE.

           MOVE SPACES TO HOLD-OPT-OUT
                          HOLD-MAKER-FNAME
                          HOLD-MAKER-LNAME
                          HOLD-COMAKER-FNAME
                          HOLD-COMAKER-LNAME
                          HOLD-COMAKER-OPT-OUT.
           MOVE 0      TO HOLD-MAKER-DOB.

           PERFORM GET-BORROWERS.
           IF RETURN-STATUS = 10
              GO TO END-ROUTINE.


           PERFORM PRINT-ALL-DOCUMENTS.

      * IF YOU COME BACK WITH NO ERRORS IN RETURN-STATUS, RUN WAS GOOD
           IF RETURN-STATUS = 99
              MOVE 0 TO RETURN-STATUS.

           GO TO END-ROUTINE.


      *****************************
       PRINT-ALL-DOCUMENTS SECTION.
      *****************************

           SET ENVIRONMENT "PST_OUTPUT" TO "A".

       PRINT-ALL-DOCUMENTS-ASK.

           PERFORM OPEN-FD1-FILE.
         
           MOVE ORIG-FIL                 TO PST-DATA-PATH.

      * CREATE ORIGINAL MANIFEST WHILE BUILDING DOCUMENTS WITHOUT THE M
      * WHEN THEY ARE ALL DONE, MOVE THE MANIFEST FILE TO HAVE THE M SO
      * THE API KNOWS THEY ARE ALL DONE CREATED
           MOVE PST-FIL-BASE             TO PST-PATH-MAN-FIL
                                            PST-PATH-MAN-D-FIL.
           MOVE LN-OWNBR                 TO PST-PATH-MAN-BRNO
                                            PST-PATH-MAN-D-BRNO.
           MOVE LN-ACCTNO                TO PST-PATH-MAN-ACCTNO
                                            PST-PATH-MAN-D-ACCTNO.

           PERFORM OPEN-MANIFEST-FILE-OUTPUT.
           PERFORM CLOSE-MANIFEST-FILE.
           PERFORM OPEN-MANIFEST-FILE.

           MOVE SPACES TO MANIFEST-FIRST-RECORD
                          MANIFEST-REC.
           MOVE LN-ORGST                 TO MANIFEST-STATE.
           MOVE LN-SSNO(1)               TO HOLD-SSNO.
           MOVE SSNO-LAST-4              TO MANIFEST-LAST-4.
           MOVE LN-OWNBR                 TO MANIFEST-BR-NO.
           MOVE SSNO-MID-2               TO MANIFEST-MID-2.
           MOVE LN-ACCTNO                TO MANIFEST-LN-NUMBER.
           MOVE SSNO-FIRST-3             TO MANIFEST-FIRST-3.
           MOVE LN-CLASS                 TO MANIFEST-CLASS.
           MOVE LN-SUBCLASS              TO MANIFEST-SUBCLASS.
           MOVE LN-LNAMT                 TO MANIFEST-LNAMT.
           MOVE HOLD-OPT-OUT             TO MANIFEST-OPT-OUT.
           MOVE HOLD-MAKER-FNAME         TO MANIFEST-MAKER-FNAME.
           MOVE HOLD-MAKER-LNAME         TO MANIFEST-MAKER-LNAME.
           MOVE HOLD-MAKER-DOB           TO MANIFEST-MAKER-DOB.
           MOVE HOLD-COMAKER-FNAME       TO MANIFEST-COMAKER-FNAME.
           MOVE HOLD-COMAKER-LNAME       TO MANIFEST-COMAKER-LNAME.
           MOVE HOLD-COMAKER-OPT-OUT     TO MANIFEST-COMAKER-OPT-OUT.
           MOVE LN-LOANDATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY            TO MANIFEST-LOANDATE.
           MOVE X"0D"                    TO MANIFEST-FIRST-CR.
           MOVE MANIFEST-FIRST-RECORD    TO MANIFEST-REC.
           PERFORM WRITE-MANIFEST-FILE.
           PERFORM CLOSE-MANIFEST-FILE.

           MOVE 0 TO SUB.

       PRINT-NEXT-DOCUMENT.
           ADD 1 TO SUB.
           IF SUB > 20
              GO TO END-PRINT-ALL-DOCUMENTS.

           IF SP-PRFORM(SUB) = 0
              GO TO PRINT-NEXT-DOCUMENT.

           MOVE SP-PRFORM(SUB) TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG NOT = 0
              GO TO PRINT-NEXT-DOCUMENT.

           IF FD-TYPE-OF-DOC = SPACES
              GO TO PRINT-NEXT-DOCUMENT.

           IF ( FD-PROG = GB-CHECK-PROG )
              GO TO PRINT-NEXT-DOCUMENT.

           PERFORM LPFRFD-FIND-PREFIX.

           IF NOT ( FD-OPTION-PCL-LASER )
              IF NOT ( FD-OPTION-PST-LASER )
                 GO TO PRINT-NEXT-DOCUMENT.

           MOVE FD-PROG       TO IP-PROG.
           MOVE FD-FMNO       TO IP-FMNO.
           MOVE "0"           TO CONT-PRINT.
           MOVE FD-OPTION     TO IP-FD-OPTION.

           ACCEPT DATE-YYYYMMDD FROM CENTURY-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE TO LPLOAN-SYSDATE.

           PERFORM LPFRFD-FIND-PREFIX.
           IF ( LPWSFD-PREFIX-PCL )
              MOVE PROG-DIR-PREFIX-PCL  TO PROG-DIR-NAME
              MOVE FD-PROG-PREFIX       TO PROG-DIR-PREFIX
              MOVE IP-PROG              TO PROG-WK-IP
           ELSE
           IF ( LPWSFD-PREFIX-PST )
              MOVE PROG-DIR-PREFIX-PST  TO PROG-DIR-NAME
              MOVE FD-PROG-PREFIX       TO PROG-DIR-PREFIX
              MOVE IP-PROG              TO PROG-WK-IP.

           MOVE PROG-WK TO FORM-NAM.

           CALL FORM-PROGX USING FORM-PATH PROG-BUF IP-BUF PRU-PATH.
           CANCEL FORM-PROGX.

           GO TO PRINT-NEXT-DOCUMENT.

       END-PRINT-ALL-DOCUMENTS.
      * NOW MOVE THE FILE TO HAVE THE LEADING M SO THEY KNOW THE FORMS
      * ARE DONE AND THEY CAN GRAB THEM
           MOVE PST-PATH-MANIFEST      TO PST-WS-MV-MANI-FILE-1.
           MOVE PST-PATH-MANIFEST-DONE TO PST-WS-MV-MANI-FILE-2.
           MOVE PST-WS-MV-MANIFEST-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE 13 TO RETURN-STATUS
              GO TO EXIT-PRINT-ALL-DOCUMENTS.

       EXIT-PRINT-ALL-DOCUMENTS.
            EXIT.

      *****************************
       GET-BORROWERS SECTION.
      *****************************

           PERFORM OPEN-BW1-FILE.
           MOVE 1 TO SUB.
       GET-NEXT-BW.
           MOVE SPACES       TO BW-LNAME BW-FNAME.
           IF LN-SSNO(SUB) NOT = 0
              MOVE LN-SSNO(SUB) TO BW-SSNO
              PERFORM READ-BW1-FILE
              IF IO-FG = 9
                 MOVE 10 TO RETURN-STATUS
                 GO TO EXIT-GET-BORROWERS
              ELSE
                 PERFORM APPEND-BW-NAMES
                 IF SUB = 2
                    MOVE BW-FNAME           TO HOLD-COMAKER-FNAME
                    MOVE BW-LNAME           TO HOLD-COMAKER-LNAME
                    MOVE BW-PRIVACY-OPT-OUT TO HOLD-COMAKER-OPT-OUT.

           IF SUB = 1
              MOVE BW-PRIVACY-OPT-OUT TO HOLD-OPT-OUT
              MOVE BW-FNAME           TO HOLD-MAKER-FNAME
              MOVE BW-LNAME           TO HOLD-MAKER-LNAME
              MOVE BW-BDATE           TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY
              MOVE DATE-MMDDYYYY      TO HOLD-MAKER-DOB.
           IF SUB < 4
              ADD 1 TO SUB
              GO TO GET-NEXT-BW.

           PERFORM CLOSE-BW1-FILE.

       EXIT-GET-BORROWERS.
           EXIT.

      *********************************
       CHANGE-DIR-MODULE SECTION.
      *********************************
           ACCEPT ORIG-POSTNAME FROM ENVIRONMENT "POSTNAME".

      *  CHANGE 'POSTNAME' TO NEW BRANCH
      *  CHANGE 'FIL'      TO NEW BRANCH
      *  CHANGE CURRENT WORKING DIRECTORY TO /{NEW FIL}/GB

      *    MOVE BR-DATA-PATH TO NEW-DATA-PATH.

      * CHANGE THE EXTERNAL FILPATH TO THAT OF THE CURRENT BRANCH.

           MOVE BR-MACHINE            TO EXT-FILPATH-MACHINE.
           MOVE BR-DATA-PATH          TO EXT-FILPATH-DATA.
           SET ENVIRONMENT "FIL"      TO EXT-FILPATH-FIL.
           MOVE ORIG-POSTNAME         TO NEW-USERID.
           MOVE BR-NO                 TO NEW-BR BRANCH.
           SET ENVIRONMENT "POSTNAME" TO NEW-USERID.

           MOVE "GB/SETENV" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PATH.

       EXIT-CHANGE-DIR.
           EXIT.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
      *SEND-LEGEND.
      *    MOVE "S  A000MSEL." TO VDU.
      *    MOVE MESS TO VDUBUF.
      *    PERFORM SCREENIO.
       APPEND-BW-NAMES.
           MOVE BW-LNAME TO INSPECT-W.
           MOVE BW-FNAME TO ADD-ON.
           PERFORM ADD-IT.
       ADD-IT.
           INSPECT INSPECT-W REPLACING FIRST "  " BY " +".
           INSPECT INSPECT-W REPLACING FIRST
              "+                        " BY ADD-ON.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPFRFD.CPY".
       COPY "LIBLP/LPBAGE.CPY".
      *COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".

      ***************************************
       IO-ROUTINE SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
      * NEED FOR MANIFEST FILE
       COPY "LIBFP/FPIOPST.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
       END-PROG.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           MOVE RETURN-STATUS TO RETURN-CODE.

           EXIT PROGRAM RETURNING RETURN-CODE.
