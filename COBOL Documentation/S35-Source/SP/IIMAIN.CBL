       IDENTIFICATION DIVISION.
       PROGRAM-ID.      IIMAIN.
      ******************************************************************
      *
      *                 (SP)
      *
      *  DESCRIPTION:   INTERNAL REVENUE IDENTIFICATION MAINTENANCE
      *
      *=================================================================
      * REV:
      * 012799 BAH CHANGES PER IRS...REMOVED TRANSMITTOR FIELDS, SPLIT
      *            OUT PAYER CITY, STATE, AND ZIP
      *            ADDED PHONE AND CONTACT
      * 021217 BAH ADDED 14. CONTACT E-MAIL: AS PER NEW IRS FIELD
      * 2016/12/13 ADDED 3 NEW FIELDS FOR ABILITY TO PRINT THE 1098 FORM
      *
      * KEC 2016.1223 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED II-FILE FROM RELATIVE TO INDEXED.
      * BAH 20190731 ADDED FUTURE FIELDS, CLEAR ALL NUMERICS
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/IIMAIN.CBL S34 06/19/21-12:30:15 >".
      *******************************
      *       PROGRAM WORKERS       *
      *******************************
       COPY "LIBSP/SP01II.CPY".
       COPY "LIBSP/SP01II_SQL.CPY".
       COPY "LIBSP/SPWSII.CPY".

       01  ERRCD           PIC X        VALUE SPACES.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

       01  ORIGINAL-REC    PIC X(II-SIZE)   VALUE SPACES.
       01  CHANGED-REC     PIC X(II-SIZE)   VALUE SPACES.

      ******************************
      *    SCREEN DISPLAY BUFFER   *
      ******************************
       01  TRANSFER-BUF                  PIC X(32).
       01  TRANSFER-LIST                 PIC X(10) VALUE "ADDRESS-1.".

       01  TRANSFER-2-BUF.
           03  TRANSFER-CITY           PIC X(32).
           03  TRANSFER-STATE          PIC X(32).
           03  TRANSFER-ZIP            PIC X(32).
           03  TRANSFER-PHONE          PIC X(32).
       01  TRANSFER-2-LIST               PIC X(21) VALUE
           "CITY STATE ZIP PHONE.".

       01  AGENT-BUF                     PIC X(32).
       01  AGENT-LIST                    PIC X(12) VALUE "TRANS-AGENT.".

       01  DISPLAY-BUF     VALUE SPACES.
      * 1. II-EMPLOYER-ID
           03  F01         PIC 9(9).
      * 2. II-NAME-CONTROL-NO
           03  F02         PIC X(4).
      * 3. II-TRANS-CONTROL-CODE
           03  F03         PIC X(5).
      * 4. II-FOREIGN-CORP
           03  F04         PIC X.
      * 5. II-TRANSFER-AGENT-FG
           03  F05         PIC X.
      * 6. II-PAYER-NAME
           03  F06         PIC X(40).
      * 7. II-SECOND-PAYER
           03  F07         PIC X(40).
      * 8. II-PAYER-ADDRESS
           03  F08         PIC X(40).
      * 9. II-PAYER-CITY
           03  F09         PIC X(29).
      * 10. II-PAYER-STATE
           03  F10         PIC X(2).
      * 11. II-PAYER-ZIP
           03  F11         PIC X(9).
      * 12. II-PAYER-PHONE
           03  F12         PIC X(12).
           03  F121        PIC X(5).
      * 13. II-CONTACT
           03  F13         PIC X(40).
      * 14. II-CONTACT
           03  F14         PIC X(45).
      * 15. FORM PRINT FIELDS
           03  F15         PIC 9(4).
           03  F151        PIC X.
           03  F152        PIC X.


       01  DISPLAY-LIST.
           03  FILLER      PIC X(32) VALUE
           "010 020 030 040 050 060 070 080 ".
           03  FILLER      PIC X(40) VALUE
           "090 100 110 120 121 130 140 150 151 152.".

      ********************************
      *    ELEMENT SPECIFICATIONS    *
      ********************************
       01  MAX             PIC 99   VALUE 15.
       01  SPEC-TABLE.
      * 1  II-EMPLOYER-ID
           03  FILLER      PIC X(8) VALUE "EN N090 ".
      * 2  II-NAME-CONTROL-NO
           03  FILLER      PIC X(8) VALUE "E  A040 ".
      * 3  II-TRANS-CONTROL-CODE
           03  FILLER      PIC X(8) VALUE "E  A050 ".
      * 4  II-FOREIGN-CORP
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 5  II-TRANSFER-AGENT-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 6  II-PAYER-NAME
           03  FILLER      PIC X(8) VALUE "E  A400 ".
      * 7  II-SECOND-PAYER
           03  FILLER      PIC X(8) VALUE "E  A400 ".
      * 8  II-PAYER-ADDRESS
           03  FILLER      PIC X(8) VALUE "E  A400 ".
      * 9  II-PAYER-CITY
           03  FILLER      PIC X(8) VALUE "E  A290 ".
      *10  II-PAYER-STATE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      *11  II-PAYER-ZIP
           03  FILLER      PIC X(8) VALUE "E  A090 ".
      *12  II-PAYER-PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      *13  II-CONTACT
           03  FILLER      PIC X(8) VALUE "E  A400 ".
      *14  II-CONTACT-EMAIL
           03  FILLER      PIC X(8) VALUE "E  A450 ".
      *15  II-TAX-YEAR
           03  FILLER      PIC X(8) VALUE "ENNN040 ".


       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 15 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  IIMAIN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/IIMAIN_DEF.CPY".
       COPY "LIBSP/IIMAIN_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/IIMAIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ********************************
      *      MAIN PROGRAM MODULE     *
      ********************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
       COPY "LIBGB/CHKSEC.CPY".
           MOVE "IIMAIN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS IIMAIN-WINDOW-HANDLE.

           IF II-TRANSFER-AGENT-FG = "Y"
              MOVE "TRANSFER AGENT NAME            :" TO AGENT-BUF
              MOVE "TRANSFER AGENT ADDRESS         :" TO TRANSFER-BUF
              MOVE "TRANSFER AGENT CITY            :" TO TRANSFER-CITY
              MOVE "TRANSFER AGENT STATE           :" TO TRANSFER-STATE
              MOVE "TRANSFER AGENT ZIPCODE         :" TO TRANSFER-ZIP
              MOVE "TRANSFER AGENT PHONE           :" TO TRANSFER-PHONE
           ELSE
              MOVE "SECOND PAYER NAME              :" TO AGENT-BUF
              MOVE "PAYER ADDRESS                  :" TO TRANSFER-BUF
              MOVE "PAYER AGENT CITY               :" TO TRANSFER-CITY
              MOVE "PAYER AGENT STATE              :" TO TRANSFER-STATE
              MOVE "PAYER AGENT ZIPCODE            :" TO TRANSFER-ZIP
              MOVE "PAYER AGENT PHONE              :" TO TRANSFER-PHONE.

           MOVE AGENT-BUF TO WR-BUF.
           MOVE AGENT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE TRANSFER-BUF TO WR-BUF.
           MOVE TRANSFER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE TRANSFER-2-BUF TO WR-BUF.
           MOVE TRANSFER-2-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-II-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           GO TO MAIN-MODULE.

      ******************************
       REC-II-MODULE SECTION.
      ******************************
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.
       READ-IIFILE.
           MOVE 01 TO II-KEY.
           PERFORM OPEN-II-FILE.
           PERFORM READ-II-FILE.
           PERFORM CLOSE-II-FILE.
           IF IO-FG = 0
              MOVE II-REC TO ORIGINAL-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-LP-MODULE.
           EXIT.

      *************************************************
      *  ENTRY-MODULE
      *  NEW-FLAG = 0 - MODIFY EXISTING RECORD        *
      *             1 - INITIAL NEW RECORD ENTRY      *
      *             2 - MODIFY NEW RECORD             *
      *************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                    GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                MOVE -1 TO ONE
                GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
       TEST-F7.
           IF VDUSTATUS = "1>" AND ELE = 1 AND NEW-FLAG = 1
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
      **************************************
      *    SET RECORD ELEMENTS             *
      **************************************
       F-01.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           MOVE VDUNUM0 TO II-EMPLOYER-ID.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO II-NAME-CONTROL-NO.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO II-TRANS-CONTROL-CODE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO II-FOREIGN-CORP.
           IF II-FOREIGN-CORP NOT = "Y" AND
              II-FOREIGN-CORP NOT = "N"
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO II-TRANSFER-AGENT-FG.
           IF II-TRANSFER-AGENT-FG NOT = "Y" AND
              II-TRANSFER-AGENT-FG NOT = "N"
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF II-TRANSFER-AGENT-FG = "Y"
              MOVE "TRANSFER AGENT NAME            :" TO AGENT-BUF
              MOVE "TRANSFER AGENT ADDRESS         :" TO TRANSFER-BUF
              MOVE "TRANSFER AGENT CITY            :" TO TRANSFER-CITY
              MOVE "TRANSFER AGENT STATE           :" TO TRANSFER-STATE
              MOVE "TRANSFER AGENT ZIPCODE         :" TO TRANSFER-ZIP
              MOVE "TRANSFER AGENT PHONE           :" TO TRANSFER-PHONE
           ELSE
              MOVE "SECOND PAYER NAME              :" TO AGENT-BUF
              MOVE "PAYER ADDRESS                  :" TO TRANSFER-BUF
              MOVE "PAYER AGENT CITY               :" TO TRANSFER-CITY
              MOVE "PAYER AGENT STATE              :" TO TRANSFER-STATE
              MOVE "PAYER AGENT ZIPCODE            :" TO TRANSFER-ZIP
              MOVE "PAYER AGENT PHONE              :" TO TRANSFER-PHONE.
           MOVE AGENT-BUF TO WR-BUF.
           MOVE AGENT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE TRANSFER-BUF TO WR-BUF.
           MOVE TRANSFER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE TRANSFER-2-BUF TO WR-BUF.
           MOVE TRANSFER-2-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO II-PAYER-NAME.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO II-SECOND-PAYER.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO II-PAYER-ADDRESS.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUBUF TO II-PAYER-CITY.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUBUF TO II-PAYER-STATE.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUBUF TO II-PAYER-ZIP.
           GO TO CREATE-ROUTINE.
       F-12.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO II-PAYER-PHONE.
       F-121.
           MOVE "E  A050121." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           MOVE VDUBUF TO II-PAYER-EXT.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUBUF TO II-CONTACT.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUBUF TO II-CONTACT-EMAIL.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUNUM0 TO II-TAX-YEAR.
           IF II-TAX-YEAR < 2014
              MOVE "INVALID YEAR" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       F-151.
           MOVE "E  A010151." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           MOVE VDUBUF TO II-CURRENT-LAST.
           IF II-CURRENT-LAST NOT = "C" AND NOT = "L"
              MOVE "ENTER 'C' FOR CURRENT OR 'L' FOR LAST" TO MESS
              PERFORM SEND-MESS
              GO TO F-151.

       F-152.
           MOVE "E  A010152." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-151.
           MOVE VDUBUF TO II-LASER-FORM.
           IF II-LASER-FORM NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO F-152.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      *************************************
       WRITE-MODULE SECTION.
      *************************************
           PERFORM OPEN-II-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE II-REC TO CHANGED-REC.
           IF ORIGINAL-REC = CHANGED-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-II-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF II-REC NOT = ORIGINAL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-REC TO II-REC.
           PERFORM REWRITE-II-FILE.

           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-II-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-II-FILE.

      **************************************
       NEW-RECORD-SETUP SECTION.
      **************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-II-FILE.

           MOVE 01 TO II-KEY.

           MOVE 0 TO ELE.

      *********************************
       DISPLAY-FIELDS SECTION.
      *********************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE II-EMPLOYER-ID TO F01.
           MOVE II-NAME-CONTROL-NO TO F02.
           MOVE II-TRANS-CONTROL-CODE TO F03.
           MOVE II-FOREIGN-CORP TO F04.
           MOVE II-TRANSFER-AGENT-FG TO F05.
           MOVE II-PAYER-NAME TO F06.
           MOVE II-SECOND-PAYER TO F07.
           MOVE II-PAYER-ADDRESS TO F08.
           MOVE II-PAYER-CITY TO F09.
           MOVE II-PAYER-STATE TO F10.
           MOVE II-PAYER-ZIP TO F11.
           IF II-PAYER-PHONE NOT = 0
              MOVE II-PAYER-PHONE TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT TO F12
              MOVE II-PAYER-EXT TO F121.
           MOVE II-CONTACT TO F13.
           MOVE II-CONTACT-EMAIL TO F14.
           MOVE II-TAX-YEAR      TO F15.
           MOVE II-CURRENT-LAST  TO F151.
           MOVE II-LASER-FORM    TO F152.


       DISPLAY-FIELDS-EXIT.
           IF II-TRANSFER-AGENT-FG = "Y"
              MOVE "TRANSFER AGENT NAME            :" TO AGENT-BUF
              MOVE "TRANSFER AGENT ADDRESS         :" TO TRANSFER-BUF
              MOVE "TRANSFER AGENT CITY            :" TO TRANSFER-CITY
              MOVE "TRANSFER AGENT STATE           :" TO TRANSFER-STATE
              MOVE "TRANSFER AGENT ZIPCODE         :" TO TRANSFER-ZIP
              MOVE "TRANSFER AGENT PHONE           :" TO TRANSFER-PHONE
           ELSE
              MOVE "SECOND PAYER NAME              :" TO AGENT-BUF
              MOVE "PAYER ADDRESS                  :" TO TRANSFER-BUF
              MOVE "PAYER AGENT CITY               :" TO TRANSFER-CITY
              MOVE "PAYER AGENT STATE              :" TO TRANSFER-STATE
              MOVE "PAYER AGENT ZIPCODE            :" TO TRANSFER-ZIP
              MOVE "PAYER AGENT PHONE              :" TO TRANSFER-PHONE.
           MOVE AGENT-BUF TO WR-BUF.
           MOVE AGENT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE TRANSFER-BUF TO WR-BUF.
           MOVE TRANSFER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE TRANSFER-2-BUF TO WR-BUF.
           MOVE TRANSFER-2-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBSP/SPIICL.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************

           MOVE EXT-CONAME-CONAME   TO IIMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE  TO IIMAIN-SYSDATE.
           DISPLAY SPACES UPON IIMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY IIMAIN-SCREEN UPON IIMAIN-WINDOW-HANDLE.
           MOVE IIMAIN-FORM-FIELDS TO WR-BUF.
           MOVE IIMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE IIMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IIMAIN-HELP--DIR TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************

           MOVE "00"                 TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.

           EVALUATE TRUE
                    COPY "LIBSP/IIMAIN_EVA.CPY".
                    WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ****************************
       MISC-PARAGRAPHS SECTION.
      ****************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/PHONEIO.CPY".
       SEND-LEGEND.
           PERFORM CALL-TMMSG.
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

      *****************************
       IO-ROUTINES SECTION.
      *****************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-II-FILE.
       COPY "LIBSP/SPIIGS_SQL.CPY".
       COPY "LIBSP/SPIIIN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *****************************
       END-ROUTINE SECTION.
      *****************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "SP/IRMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY IIMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
