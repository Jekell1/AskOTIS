      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TWINFO.
      ******************************************************************
      *
      *  DESC:   CREDIT BUREAU INFORMATION FILE FOR METRO 2 HEADER INFO
      *
      * REV:
      *  040428 BAH ADDED TRW-INFO-BRNO-ACCTNO TO INCLUDE THE BRNO IN FRONT
      *             OF ACCOUNT NO. FOR DUPLICATES WITH MULTIPLE BRANCHES
      *  040617 BAH ADDED TRW-INFO-IDENT-BRNO TO PUT BRNO IN FIELD (5)
      *             IDENT NO. POS 21 - 40 FOR LENDMARK PR# 2328
      *             EQUIFAX REQUIRING IT FOR THEM !!
      * 050427 BAH ADDED TRW-YEARS-STOP-REPORTING TO TEST DOFD AND STOP
      *            REPORTING IF MORE YEARS THAN THIS, ACC PR# 2517
      * 050713 BAH ADDED A WINDOW SCAN
      * 160106 BAH ADDED 12. TRW-INVALID-SSNO, WORLD PR#1129, REGMGM PR#4931
      *            "0" FORCE ALL STARTING WITH 9XX- OR 000- TO 999-99-999
      *            "9" FORCE ALL STARTING WITH 990- TO 999-99-9999 (REGMGM)
      * 160808 BAH ADDED 13. TRW-INFO-SCHED-PAY FOR METRO2 TO ALWAYS USE
      *            NO-SCHED-PAYMENT FOR ALL ACCOUNTS WHICH WILL USE THE
      *            PAYMENT SCHEDULE IF IT EXIST. REGACC PR# 383
      * 2020.06.08 BAH REMOVED 11. TRW YEARS STOP REPORTING  #1440
      * 2025.0304 BAH ADDED TRW-INFO-INNOVIS-ID #1702 S35-169
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
      /*****************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/TWINFO.CBL S34 06/21/21-11:19:13 >".

      ******************************************************************
      *
      *     P R O G R A M     W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/SP01TRWINFO.CPY".
       COPY "LIBSP/SP01TRWINFO_SQL.CPY".
       COPY "LIBSP/SPWSTRWINFO.CPY".
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------

       01  ERRCD                PIC X      VALUE " ".
       01  UNAME-NODE           PIC X(10)  VALUE SPACES.

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).

       01  ELE             PIC 9(3).
       01  HOLD-KEY        PIC 9(4).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  ONE             PIC S9.

       01  ORIGINAL-REC      PIC X(TRW-SIZE).
       01  CHANGED-REC       PIC X(TRW-SIZE).

      ******************************************************************
      *     SCREEN DISPLAY BUFFER
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(10).
           03  F02         PIC X(5).
           03  F03         PIC X(10).
           03  F04         PIC X(40).
           03  F05         PIC X(32).
           03  F051        PIC X(32).
           03  F052        PIC X(32).
           03  F06         PIC 999/999/9999.
           03  F07         PIC X(20).
           03  F08         PIC X.
           03  F09         PIC X.
           03  F10         PIC X.
           03  F11         PIC X(10).
           03  F12         PIC X.
           03  F13         PIC X.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(52) VALUE
           "010 020 030 040 050 051 052 060 070 080 090 100 110 ".
           03  FILLER      PIC X(18) VALUE
             "120 130.".

      ******************************************************************
      *     ELEMENT SPECIFICATIONS
      ******************************************************************
       01  MAX             PIC 99   VALUE 13.
       01  SPEC-TABLE.
      * 1. EQUIFAX PROGRAM IDENTIFIER
           03  FILLER      PIC X(8) VALUE "E  A100 ".
      * 2. EXPERIAN PROGRAM IDENTIFIER
           03  FILLER      PIC X(8) VALUE "E  A050 ".
      * 3. TRANS UNION PROGRAM IDENTIFIER
           03  FILLER      PIC X(8) VALUE "E  A100 ".
      * 4. REPORTER NAME
           03  FILLER      PIC X(8) VALUE "E  A400 ".
      * 5. REPORTER ADDRESS
           03  FILLER      PIC X(8) VALUE "E  A320 ".
      * 6. REPORTER PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 7. UNIQUE CREDIT GRANTOR ID
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      * 8. TRANSMIT FILE (CF/LF)
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 9. INCLUDE BRNO WITH ACCTNO
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 10. BRNO IN BASE IDENTO NO. FIELD 5
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 11. INNOVIS PROGRAM IDENTIFIER
           03  FILLER      PIC X(8) VALUE "E  A100 ".
      * 12. INVALID SSNO (9??- OR 000-??)
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 13. USE SCHED PAYMENT AMT
           03  FILLER      PIC X(8) VALUE "E  A010 ".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 13 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.


      /*****************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/TWISCNW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PHON400.CPY".
       01  TWINFO-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/TWINFO_DEF.CPY".
       COPY "LIBSP/TWINFO_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/TWINFO_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N   P R O G R A M    M O D U L E
      *
      ******************************************************************
       A000-MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.
       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE "TWINFO" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS TWINFO-WINDOW-HANDLE.

       A010-MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM B010-REC-TRW-MODULE.

           IF ( IO-TYPE = "END" )
              GO TO END-ROUTINE.

           PERFORM B020-ENTRY-MODULE.

           IF ( IO-TYPE = "RWR" )
              PERFORM B030-WRITE-MODULE.

           IF ( IO-TYPE = "DEL" )
             PERFORM B040-DELETE-MODULE.

           GO TO A010-MAIN-MODULE.

      ******************************************************************
       B010-REC-TRW-MODULE SECTION.
      ******************************************************************
       B010-REC-TRW-MODULE-NO.
           MOVE 0      TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE "F7-EXIT, F6-SCAN, ENTER CREDIT BUREAU ID:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N040ID." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1<" )
              PERFORM TRW-INFO-SCAN
              IF IO-FG NOT = 0
                 GO TO B010-REC-TRW-MODULE-NO
              ELSE
                 MOVE "SN N040ID." TO VDU
                 MOVE TWISCN-TRW-KEY TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE TWISCN-TRW-KEY TO VDUNUM0.

           IF ( VDUSTATUS = "1>" )
             MOVE "END" TO IO-TYPE
             GO TO END-B010-REC-TRW-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO B010-REC-TRW-MODULE-NO.

           IF ( VDUNUM0 = 0 )
              GO TO B010-REC-TRW-MODULE-NO.

           MOVE VDUNUM0 TO TRW-INFO-ID HOLD-KEY.

      *-----------------------------------------------------------------
       READ-TRWFILE.
           PERFORM GET-TRW-INFO.
           IF ( IO-FG = 0 )
              MOVE TRW-INFO-REC TO ORIGINAL-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-B010-REC-TRW-MODULE.
           EXIT.

      ***********************
       TRW-INFO-SCAN SECTION.
      ***********************

           MOVE 0           TO TWISCN-TRW-KEY.
           MOVE "WI/TWISCN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH TWISCN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF TWISCN-TRW-KEY = 0
              MOVE 9 TO IO-FG
           ELSE
              MOVE 0 TO IO-FG.

      ******************************************************************
      *    B 0 2 0 - E N T R Y - M O D U L E
      *
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ******************************************************************
       B020-ENTRY-MODULE SECTION.
           MOVE SPACES  TO IO-TYPE.
           MOVE 1       TO ONE.

       B020-CREATE-ROUTINE.
           IF ( NEW-FLAG NOT = 1 )
              GO TO B020-MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE < 1 )
              MOVE 1 TO ELE ONE.

           IF ( ELE NOT > MAX )
              GO TO B020-ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       B020-MODIFY-ROUTINE.
           MOVE "F1, 'DELETE', ENTER FIELD # :" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-B020-ENTRY-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO B020-MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ( ACTION = SPACES )
              MOVE "RWR" TO IO-TYPE
              GO TO END-B020-ENTRY-MODULE.

           IF ( ACTION = "DELETE" )
              MOVE "DEL" TO IO-TYPE
              GO TO END-B020-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF ( CHNO NOT NUMERIC )
              GO TO B020-MODIFY-ROUTINE.

           IF ( CHNO = 0 )
              GO TO B020-MODIFY-ROUTINE.

           IF ( CHNO > MAX )
              GO TO B020-MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       B020-ENTER-ELE.


           IF ( CHKEY NOT = ZEROS )
              IF ( CHFG (ELE) = " " )
                 IF ( NEW-FLAG NOT = 1 )
                    GO TO B020-MODIFY-ROUTINE.

           IF ( CHFG(ELE) = "N" )
              IF ( NEW-FLAG = 1 )
                 GO TO B020-CREATE-ROUTINE
              ELSE
                 IF ( CHKEY NOT = EXT-CONAME-DAILY-PASSWORD )
                    GO TO B020-MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE 0         TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       B020-TEST-F4.
           IF ( VDUSTATUS = "1U" )
             IF ( NEW-FLAG = 1 )
              MOVE -1 TO ONE
              GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       B020-TEST-F5.
           IF ( VDUSTATUS = "1D" )
              MOVE 1 TO ONE
              GO TO B020-GOTO-FIELD.

      *-----------------------------------------------------------------
       B020-TEST-F6.
           IF ( VDUSTATUS NOT = "1<" )
              GO TO B020-TEST-F1.

      *-----------------------------------------------------------------
       B020-TEST-F1.
           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-B020-ENTRY-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO B020-ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       B020-GOTO-FIELD.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09
                 F-10 F-11 F-12 F-13
                 DEPENDING ON ELE.

           GO TO END-B020-ENTRY-MODULE.
      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS
      *
      *-----------------------------------------------------------------
       F-01.
           MOVE VDUBUF TO TRW-INFO-EQUIFAX-ID.
           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO TRW-INFO-EXPERIAN-ID.
           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO TRW-INFO-TUC-ID.
           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO TRW-INFO-REPORTER-NAME.
           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO TRW-INFO-REPORTER-ADR1.
       F-051.
           MOVE "E  A320051." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO B020-ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-051.
           MOVE VDUBUF TO TRW-INFO-REPORTER-ADR2.
       F-052.
           MOVE "E  A320052." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-051.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-052.
           MOVE VDUBUF TO TRW-INFO-REPORTER-CSZ.

           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
              MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
              GO TO B020-ENTER-ELE.
           MOVE TEL-BUF TO TRW-INFO-REPORTER-PHONE.
           GO TO B020-CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO TRW-INFO-GRANTOR-CODE.
           GO TO B020-CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO TRW-INFO-CR-LF.
           IF TRW-INFO-CR-LF NOT = "Y" AND NOT = "N"
              GO TO B020-ENTER-ELE.
           GO TO B020-CREATE-ROUTINE.
       F-09.
           MOVE VDUBUF TO TRW-INFO-BRNO-ACCTNO.
           IF TRW-INFO-BRNO-ACCTNO NOT = "Y" AND NOT = "N"
              GO TO B020-ENTER-ELE.
           GO TO B020-CREATE-ROUTINE.
       F-10.
           MOVE VDUBUF TO TRW-INFO-IDENT-BRNO.
           IF TRW-INFO-IDENT-BRNO NOT = "Y" AND NOT = "N"
              GO TO B020-ENTER-ELE.
           GO TO B020-CREATE-ROUTINE.
       F-11.
           MOVE VDUBUF TO TRW-INFO-INNOVIS-ID.
           GO TO B020-CREATE-ROUTINE.
       F-12.
           MOVE VDUBUF TO TRW-INVALID-SSNO.
           IF NOT TRW-INVALID-SSNO-VALID
              GO TO B020-ENTER-ELE.
           GO TO B020-CREATE-ROUTINE.
       F-13.
           MOVE VDUBUF TO TRW-INFO-SCHED-PAY.
           IF TRW-INFO-SCHED-PAY NOT = "Y" AND NOT = "N"
              GO TO B020-ENTER-ELE.
           GO TO B020-CREATE-ROUTINE.

       END-B020-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       B030-WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-TRW-INFO-FILE.

           IF ( NEW-FLAG NOT = 0 )
              GO TO B030-WRITE-RECORD.

           MOVE TRW-INFO-REC TO CHANGED-REC.

           IF ( ORIGINAL-REC = CHANGED-REC )
              GO TO END-B030-WRITE-MODULE.

           PERFORM READ-TRW-INFO-FILE.
           IF ( IO-FG NOT = 0 )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-B030-WRITE-MODULE.

           IF ( TRW-INFO-REC NOT = ORIGINAL-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-B030-WRITE-MODULE.

           MOVE CHANGED-REC TO TRW-INFO-REC.
           PERFORM REWRITE-TRW-INFO-FILE.
           GO TO END-B030-WRITE-MODULE.

      *-----------------------------------------------------------------
       B030-WRITE-RECORD.
           PERFORM WRITE-TRW-INFO-FILE.

           IF ( IO-FG NOT = 0 )
              PERFORM ALREADY-CREATED.

       END-B030-WRITE-MODULE.
           PERFORM CLOSE-TRW-INFO-FILE.

      ******************************************************************
       B040-DELETE-MODULE SECTION.
      ******************************************************************
           IF ( NEW-FLAG NOT = 0 )
              GO TO END-B040-DELETE-MODULE.

           PERFORM OPEN-TRW-INFO-FILE.
           PERFORM READ-TRW-INFO-FILE.
           IF ( IO-FG NOT = 0 )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-B040-DELETE-MODULE.

           IF ( TRW-INFO-REC NOT = ORIGINAL-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-B040-DELETE-MODULE.

           PERFORM DELETE-TRW-INFO-FILE.

       END-B040-DELETE-MODULE.
           PERFORM CLOSE-TRW-INFO-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1                     TO NEW-FLAG.
           MOVE "S  A000NEW."         TO VDU.
           MOVE "*** NEW RECORD ***"  TO VDUBUF.
           PERFORM SCREENIO.

           MOVE SPACES    TO TRW-INFO-REC.

           MOVE HOLD-KEY  TO TRW-INFO-ID.
           MOVE 0         TO ELE.

           ACCEPT UNAME-NODE FROM ENVIRONMENT "UNAME_NODE".
           CALL "C$TOUPPER" USING UNAME-NODE, VALUE 10.
           MOVE "S  A200070." TO VDU.
           MOVE UNAME-NODE TO TRW-INFO-GRANTOR-CODE VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES               TO DISPLAY-BUF.
           MOVE TRW-INFO-EQUIFAX-ID        TO F01.
           MOVE TRW-INFO-EXPERIAN-ID       TO F02.
           MOVE TRW-INFO-TUC-ID            TO F03.
           MOVE TRW-INFO-REPORTER-NAME     TO F04.
           MOVE TRW-INFO-REPORTER-ADR1     TO F05.
           MOVE TRW-INFO-REPORTER-ADR2     TO F051.
           MOVE TRW-INFO-REPORTER-CSZ      TO F052.
           IF TRW-INFO-REPORTER-PHONE NOT = 0
              MOVE TRW-INFO-REPORTER-PHONE TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT TO F06.
           MOVE TRW-INFO-GRANTOR-CODE      TO F07.
           MOVE TRW-INFO-CR-LF             TO F08.
           MOVE TRW-INFO-BRNO-ACCTNO       TO F09.
           MOVE TRW-INFO-IDENT-BRNO        TO F10.
           MOVE TRW-INFO-INNOVIS-ID        TO F11.
           MOVE TRW-INVALID-SSNO           TO F12.
           MOVE TRW-INFO-SCHED-PAY         TO F13.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
      *
      *     C O P Y   L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************

           MOVE EXT-CONAME-CONAME   TO TWINFO-CONAME.
           MOVE EXT-CONAME-SYSDATE  TO TWINFO-SYSDATE.
           DISPLAY SPACES UPON TWINFO-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TWINFO-SCREEN UPON TWINFO-WINDOW-HANDLE.
           MOVE TWINFO-FORM-FIELDS TO WR-BUF.
           MOVE TWINFO-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE TWINFO-HELP--FILE TO HELP-LINK-FILE.
           MOVE TWINFO-HELP--DIR TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************

           MOVE "00"                 TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.

           EVALUATE TRUE
                    COPY "LIBSP/TWINFO_EVA.CPY".
                    WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/PHONEIO.CPY".

       GET-TRW-INFO.
           PERFORM OPEN-TRW-INFO-FILE.
           PERFORM READ-TRW-INFO-FILE.
           PERFORM CLOSE-TRW-INFO-FILE.
       SEND-LEGEND.
           MOVE "S  A400MSEL."  TO VDU.
           MOVE MESS            TO VDUBUF.
           PERFORM SCREENIO.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
       COPY "LIBSP/SPTRWINFOGS_SQL.CPY".
       COPY "LIBSP/SPTRWINFOIN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "SP/TWMENU" TO FORM-NAM.
           MOVE FORM-PATH   TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TWINFO-SCREEN.
           DESTROY TWINFO-WINDOW-HANDLE.
           EXIT PROGRAM.
