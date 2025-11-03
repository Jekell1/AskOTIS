       IDENTIFICATION DIVISION.
       PROGRAM-ID.      AQSMAN.
       DATE-WRITTEN.   040519.
      ******************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   AUDITOR QUEUE HEADER FILE MAINTENANCE
      *
      * REV:
      *  AMM 190405 NEW PROGRAM, SPLIT FROM AQ FILE
      *  BAH 20190723 CLEAR-AQS-FILE, FUTURE FIELDS WERE NOT CLEARED
      *  BAH 20200214 FIXED THE SCANKEY ENTRY
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/AQSMAN.CBL S34 06/03/21-14:49:38 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBSP/SP01AQS.CPY".
       COPY "LIBSP/SP01AQS_SQL.CPY".
       COPY "LIBSP/SPWSAQS.CPY".
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ORIGINAL LOAN RECORD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBSP/SP01AQS.CPY" REPLACING
            LEADING "AQS" BY "AQSOR".
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHANGED LOAN RECORD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBSP/SP01AQS.CPY" REPLACING
            LEADING "AQS" BY "AQSCH".

      ******************************************************************
      *
      *    L M C - W O R K E R S
      *
      *=================================================================
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-BRNO                PIC 9(04).
           03  FILLER                  PIC X(01).
           03  LMC-FIELD               PIC X(03).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(09).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(16).
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-ORG-DATE            REDEFINES LMC-ORG-TEXT
                                       PIC 99/99/99.
           03  LMC-ORG-DLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-ORG-GLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-ORG-PHONE           REDEFINES LMC-ORG-TEXT
                                       PIC 999/999/9999.
           03  LMC-ORG-SSNO            REDEFINES LMC-ORG-TEXT
                                       PIC 999/99/9999.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(16).
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-CHG-DATE            REDEFINES LMC-CHG-TEXT
                                       PIC 99/99/99.
           03  LMC-CHG-DLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-CHG-GLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-CHG-PHONE           REDEFINES LMC-CHG-TEXT
                                       PIC 999/999/9999.
           03  LMC-CHG-SSNO            REDEFINES LMC-CHG-TEXT
                                       PIC 999/99/9999.

      ******************************************************************
       01  ERRCD             PIC X    VALUE SPACES.

       01  ONE               PIC S9.
       01  ELE               PIC 99    VALUE 0.
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  IO-TYPE           PIC X(3).
       01  NEW-FLAG          PIC 9.
       01  FIRST-SCAN        PIC 9     VALUE 0.
       01  HOLD-AQS1-KEY      PIC 9(04).

       01  ORIGINAL-REC      PIC X(AQS-SIZE).
       01  CHANGED-REC       PIC X(AQS-SIZE).


      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF       VALUE SPACES.
           03  F01           PIC ZZZZZ9.
           03  F02           PIC ZZZZZ9.
           03  F03           PIC ZZZZZ9.

       01  DISPLAY-LIST.
           03  FILLER        PIC X(12) VALUE
           "010 020 030.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX               PIC 9(2)    VALUE 3.
       01  SPEC-TABLE.
      * BRANCH
           03  FILLER        PIC X(8) VALUE "ENNN060 ".
           03  FILLER        PIC X(8) VALUE "ENNN060 ".
           03  FILLER        PIC X(8) VALUE "ENNN060 ".
       01  SPEC-TAB          REDEFINES SPEC-TABLE.
           03  SPEC-REC      OCCURS 3 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       01  AQSMAN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/AQSMAN_DEF.CPY".
       COPY "LIBMN/AQSMAN_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/AQSMAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      *******************************************
      *      MAIN PROGRAM MODULE
      *******************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "AQSMAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS AQSMAN-WINDOW-HANDLE.

       MAIN-MODULE.
      D    STOP MESS.
           PERFORM FORM-RESET.

           PERFORM REC-AQS-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      *****************************************
       REC-AQS-MODULE SECTION.
      *****************************************
       REC-AQS-MODULE-BRNO.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.
           MOVE "F7-EXIT, ENTER BRANCH NUMBER:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N040BRNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-AQS-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-AQS-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-AQS-MODULE-BRNO.

           MOVE VDUNUM0 TO AQS-BRNO.
       READ-AQSFILE.
           PERFORM OPEN-AQS1-FILE.
           PERFORM READ-AQS1-FILE.
           PERFORM CLOSE-AQS1-FILE.
           IF IO-FG = 0
              MOVE AQS-REC TO ORIGINAL-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              MOVE AQS1-KEY TO HOLD-AQS1-KEY
              PERFORM NEW-RECORD-SETUP.
       END-REC-AQS-MODULE.
           EXIT.

      ************************************************
       ENTER-SCANKEY SECTION.
      ************************************************
           MOVE "F1-RESET, F6-CONT SCAN, ENTER KEY:" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-AQS1-FILE.

           MOVE 0 TO AQS-BRNO.
           MOVE 0 TO FIRST-SCAN.
       ENTER-SCANKEY-KEY.
           MOVE "EN N040SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCANKEY-CLOSE.
           IF VDUSTATUS = "1<"
              GO TO ENTER-SCANKEY-READ-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCANKEY-KEY.
           IF VDUNUM0 = 0
              IF FIRST-SCAN NOT = 0
                 GO TO ENTER-SCANKEY-CLOSE
              ELSE
                 GO TO ENTER-SCANKEY-KEY.

           MOVE VDUNUM0 TO AQS-BRNO.

       ENTER-SCANKEY-READ-NEXT.
           IF FIRST-SCAN = 0
              PERFORM START-AQS1-FILE
              MOVE 1 TO FIRST-SCAN.

           PERFORM READ-AQS1-FILE-NEXT.
           IF IO-FG = 9 OR AQS-BRNO NOT = AQS-PATH-OWNBR
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCANKEY-CLOSE.

           GO TO ENTER-SCANKEY-READ-NEXT.

           MOVE AQS-REC TO ORIGINAL-REC.
           PERFORM ENTER-SCANKEY-DISPLAY.
           PERFORM DISPLAY-FIELDS.

           GO TO ENTER-SCANKEY-KEY.

       ENTER-SCANKEY-DISPLAY.
           MOVE "SN N040BRNO." TO VDU.
           MOVE AQS-BRNO TO VDUNUM0.
           PERFORM SCREENIO.

       ENTER-SCANKEY-CLOSE.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

       ENTER-SCANKEY-END.
           EXIT.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
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
           MOVE "F1, 'DELETE', ENTER FIELD #:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
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
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUNUM0 TO AQS-NEXT-SEQNO.
           IF AQS-NEXT-SEQNO = 0
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM0 TO AQS-TOT-REC.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUNUM0 TO AQS-WORKED-REC.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      **********************************
       WRITE-MODULE SECTION.
      **********************************
           PERFORM OPEN-AQS1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE AQS-REC TO CHANGED-REC.
           IF ORIGINAL-REC = CHANGED-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-AQS1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF AQS-REC NOT = ORIGINAL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE CHANGED-REC TO AQS-REC.
           PERFORM REWRITE-AQS1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-AQS1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-AQS1-FILE.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE ORIGINAL-REC               TO AQSOR-REC.
           MOVE CHANGED-REC                TO AQSCH-REC.

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE AQSOR-BRNO                 TO LMC-BRNO.
           MOVE " TO "                     TO LMC-TO-X.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF AQSOR-NEXT-SEQNO NOT = AQSCH-NEXT-SEQNO
             MOVE "01 "                   TO LMC-FIELD
             MOVE "NXTSEQNO"              TO LMC-DESC
             MOVE AQSOR-NEXT-SEQNO        TO LMC-ORG-NUM
             MOVE AQSCH-NEXT-SEQNO        TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF AQSOR-TOT-REC NOT = AQSCH-TOT-REC
             MOVE "02 "                   TO LMC-FIELD
             MOVE "TOT REC "              TO LMC-DESC
             MOVE AQSOR-TOT-REC           TO LMC-ORG-NUM
             MOVE AQSCH-TOT-REC           TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF AQSOR-WORKED-REC NOT = AQSCH-WORKED-REC
             MOVE "03 "                   TO LMC-FIELD
             MOVE "WORK REC"              TO LMC-DESC
             MOVE AQSOR-WORKED-REC        TO LMC-ORG-NUM
             MOVE AQSCH-WORKED-REC        TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

       LOG-MAINTENANCE-CHGS-EXIT.
           EXIT.

      ******************************************************************
       LOG-FIELD-CHANGE SECTION.
      ******************************************************************

           MOVE "F"           TO ERRLOGW-ACTION.
           MOVE LMC-DETAIL    TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       LOG-FIELD-CHANGE-EXIT.
           EXIT.

      *********************************
       DELETE-MODULE SECTION.
      *********************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.
           PERFORM OPEN-AQS1-FILE.
           PERFORM READ-AQS1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF AQS-REC NOT = ORIGINAL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-AQS1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-AQS1-FILE.

      *****************************
       NEW-RECORD-SETUP SECTION.
      *****************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       CLEAR-NEW-RECORD.
           PERFORM CLEAR-AQS-FILE.

           MOVE HOLD-AQS1-KEY TO AQS1-KEY.

       NEW-RECORD-EXIT.
           EXIT.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE " " TO DISPLAY-BUF.
           MOVE AQS-NEXT-SEQNO TO F01.
           MOVE AQS-TOT-REC TO F02.
           MOVE AQS-WORKED-REC TO F03.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBSP/SPAQSCL.CPY".

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO AQSMAN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO AQSMAN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY AQSMAN-SCREEN UPON AQSMAN-WINDOW-HANDLE.
           MOVE AQSMAN-FORM-FIELDS TO WR-BUF.
           MOVE AQSMAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE AQSMAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE AQSMAN-HELP--DIR TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/AQSMAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************
       MISC-PARAGRAPHS SECTION.
      *********************************************
       COPY "LIBGB/ACCESS.CPY".
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
       SEND-LEGEND.
           MOVE "S  A070MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      *********************************************
       IO-ROUTINES SECTION.
      *********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-AQS1-FILE.
       COPY "LIBSP/SPAQSGS_SQL.CPY".
       COPY "LIBSP/SPAQS1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *********************************************
       END-ROUTINE SECTION.
      *********************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/CPMNMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN  
           DESTROY AQSMAN-SCREEN.       
           DESTROY AQSMAN-WINDOW-HANDLE.
           EXIT PROGRAM.
