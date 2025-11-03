       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LEMAIN.
       DATE-WRITTEN.    091884.
      ******************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   ENDORSER FILE MAINTENANCE
      *  REVS:
      * MJD 131119 A30 CONVERSION COMPLETED
      *  CS 160511 REMOVED REDEF LE-FLAG/LE-SSNO AND MADE LE-FLAG PIC X
      *  CS 170424 ACTIVATE 8 DIGIT DATES
      * KEC 2017.0425 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          FIXED 'LE' FILE TO HAVE SUB-PREFIX OF 'E' OR 'R'.
      * BAH 180531 ADDED ERRLOG
      * BAH 181030 GLOBAL LEFILE
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
           "@(#)MN/LEMAIN.CBL S35 02/21/24-12:31:03 >".

      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01LE.CPY".
       COPY "LIBLP/LP01LE_SQL.CPY".
       COPY "LIBLP/LPWSLE.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

       01 LEMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       01  ERRCD           PIC X VALUE " ".
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

       01  HOLD-LE1-KEY                PIC X(31)     VALUE SPACES.
       01  FILLER            REDEFINES HOLD-LE1-KEY.
           03  HOLD-LE-BRNO            PIC 9(4).
           03  HOLD-LE-ACCTNO          PIC 9(8).
           03  HOLD-LE-TYPE            PIC X.
           03  HOLD-LE-FLAG            PIC X.
           03  HOLD-LE-SSNO            PIC 9(9).
           03  HOLD-LE-OSACCTNO        PIC 9(8).

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ORIGINAL RECORD LEOR
      *    CHANGED  RECORD LECH
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBLP/LP01LE.CPY" REPLACING LEADING "LE" BY "LEOR".
       COPY "LIBLP/LP01LE.CPY" REPLACING LEADING "LE" BY "LECH".

       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-NUMBER       PIC Z(7).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(8).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(13).
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(9).99-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(13).
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(9).99-.

       01  NAME-WK         PIC X(40).
       01  ADD-ON          PIC X(16).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC 99/99/99    BLANK ZERO.
           03  F02         PIC X.
           03  F03         PIC Z9.9999     BLANK ZERO.
           03  F04         PIC ZZZ,ZZ9.99- BLANK ZERO.
       01  DISPLAY-LISTS.
           03  FILLER      PIC X(16) VALUE
           "010 020 030 040.".
       01  DISPLAY-LIST REDEFINES DISPLAY-LISTS
                           PIC X(16).

      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX             PIC 99   VALUE 4.
       01  SPEC-TABLE.
      * 1. OSDATE
           03  FILLER      PIC X(8) VALUE "ERNM060N".
      * 2. OSMAKERCD
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 3. OSRATE
           03  FILLER      PIC X(8) VALUE "EN N033N".
      * 4. OSAMT
           03  FILLER      PIC X(8) VALUE "E  N062N".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 4 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LEMAIN_DEF.CPY".
       COPY "LIBMN/LEMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/LEMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ************************************************
       MAIN-PROGRAM SECTION.
      ************************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".
           MOVE "LEMAIN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS LEMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-LE1-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ********************************************
       REC-LE1-MODULE SECTION.
      ********************************************
       REC-LE1-MODULE-NO.

           MOVE LE-PATH-OWNBR TO LE-BRNO HOLD-LE-BRNO.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE "EN N060LN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-LE1-MODULE.
           IF VDUSTATUS NOT = "00"
             GO TO REC-LE1-MODULE-NO.
           MOVE VDUNUM0 TO LN-ACCTNO LE-ACCTNO HOLD-LE-ACCTNO.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              MOVE "INVALID LOAN #" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LE1-MODULE-NO.
           PERFORM CLOSE-LN1-FILE.

           MOVE "S  A000LNDATE." TO VDU.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY   TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "SN N062LNAMT." TO VDU.
           MOVE LN-LNAMT TO VDUNUM2.
           PERFORM SCREENIO.

       ENTER-LE1-SSNO.
           MOVE "EN N090BW." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LE1-MODULE.
           IF VDUSTATUS = "1U"
              GO TO REC-LE1-MODULE-NO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-LE1-SSNO.
           MOVE VDUNUM0 TO LE-SSNO BW-SSNO HOLD-LE-SSNO.

           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE "INVALID BORROWER #" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LE1-SSNO.
           PERFORM CLOSE-BW1-FILE.

           IF LN-SSNO(1) NOT = BW-SSNO
              IF LN-SSNO(2) NOT = BW-SSNO
                 IF LN-SSNO(3) NOT = BW-SSNO
                    IF LN-SSNO(4) NOT = BW-SSNO
                       MOVE "SS# DOES NOT MATCH LOAN SS#" TO MESS
                       PERFORM SEND-MESS
                       GO TO ENTER-LE1-SSNO.

           MOVE BW-LNAME TO NAME-WK.
           MOVE BW-FNAME TO ADD-ON.
           INSPECT NAME-WK REPLACING FIRST "   " BY ", +".
           INSPECT NAME-WK REPLACING FIRST "+               " BY ADD-ON.
           MOVE "S  A000BWX." TO VDU.
           MOVE NAME-WK TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-LE1-OSNUM.
           MOVE "EN N060LN2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-LE1-MODULE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LE1-SSNO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-LE1-OSNUM.
           MOVE VDUNUM0 TO LN-ACCTNO.

           MOVE LN-ACCTNO TO LE-OSACCTNO HOLD-LE-OSACCTNO.
       READ-LEFILE.
           MOVE "E" TO LE-TYPE HOLD-LE-TYPE.
           MOVE SPACES TO LE-FLAG  HOLD-LE-FLAG.
           PERFORM OPEN-LE1-FILE.
           PERFORM READ-LE1-FILE.
           PERFORM CLOSE-LE1-FILE.
           IF IO-FG = 0
              MOVE LE-REC TO LEOR-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-LE1-MODULE.
           EXIT.

      ***********************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
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
           IF NEW-FLAG = 0
              IF CHFG(ELE) = "N"
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
           GO TO F-01 F-02 F-03 F-04
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDU-DATE-YYYYMMDD TO LE-E-OSDATE.
           GO TO CREATE-ROUTINE.
       F-02.
           IF VDUBUF NOT = "M"
              IF VDUBUF NOT = "J"
                 IF VDUBUF NOT = "E"
                    MOVE "M-MAKER, J-JOINT, E-ENDORSER" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.
           MOVE VDUBUF TO LE-E-OSMAKERCD.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUNUM3 TO LE-E-OSRATE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM2 TO LE-E-OSAMT.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      ***********************************************
       WRITE-MODULE SECTION.
      ***********************************************
           PERFORM OPEN-LE1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE LE-REC TO LECH-REC.

           IF LEOR-REC = LECH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-LE1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF LE-REC NOT = LEOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE LECH-REC TO LE-REC.
           PERFORM REWRITE-LE1-FILE.

           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-LE1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-LE1-FILE.

      ********************************************
       DELETE-MODULE SECTION.
      ********************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LE1-FILE.
           PERFORM READ-LE1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF LE-REC NOT = LEOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-LE1-FILE.

      *WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY
           STRING LE1-KEY, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-DELETE-MODULE.
           PERFORM CLOSE-LE1-FILE.

      ***********************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ***********************************************
           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LE-BRNO   TO WORK-ERRLOG-BR.
           MOVE LE-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF LEOR-E-OSDATE NOT = LECH-E-OSDATE
             MOVE "01 "                     TO WORK-ERRLOG-FIELD
             MOVE "OTHER LOANDATE"          TO WORK-ERRLOG-DESC
             MOVE LEOR-E-OSDATE             TO WORK-ERRLOG-ORG-N0
             MOVE LECH-E-OSDATE             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LEOR-E-OSMAKERCD NOT = LECH-E-OSMAKERCD
             MOVE "02 "                     TO WORK-ERRLOG-FIELD
             MOVE "OTHER MAKERCD"           TO WORK-ERRLOG-DESC
             MOVE LEOR-E-OSMAKERCD          TO WORK-ERRLOG-ORG-ALP
             MOVE LECH-E-OSMAKERCD          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LEOR-E-OSRATE NOT = LECH-E-OSRATE
             MOVE "03 "                  TO WORK-ERRLOG-FIELD
             MOVE "OTHER RATE"           TO WORK-ERRLOG-DESC
             MOVE LEOR-E-OSRATE          TO WORK-ERRLOG-ORG-N2
             MOVE LECH-E-OSRATE          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LEOR-E-OSAMT NOT = LECH-E-OSAMT
             MOVE "04 "                 TO WORK-ERRLOG-FIELD
             MOVE "OTHER BAL"           TO WORK-ERRLOG-DESC
             MOVE LEOR-E-OSAMT          TO WORK-ERRLOG-ORG-N2
             MOVE LECH-E-OSAMT          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.


      *************************************
       LOG-FIELD-CHANGE SECTION.
      *************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      **********************************
       NEW-RECORD-SETUP SECTION.
      **********************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LE-FILE.  

           MOVE HOLD-LE1-KEY TO LE1-KEY.
           MOVE 0 TO ELE.

      **************************************
       DISPLAY-FIELDS SECTION.
      **************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE LE-E-OSDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F01.
           MOVE LE-E-OSMAKERCD TO F02.
           MOVE LE-E-OSRATE TO F03.
           MOVE LE-E-OSAMT TO F04.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO LEMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LEMAIN-SYSDATE.
           DISPLAY SPACES UPON LEMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LEMAIN-SCREEN UPON LEMAIN-WINDOW-HANDLE.
           MOVE LEMAIN-FORM-FIELDS TO WR-BUF.
           MOVE LEMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE LEMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE LEMAIN-HELP--DIR TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LEMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLECL.CPY".
       COPY "LIBGB/DATER.CPY".

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
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      *********************************************
       IO-ROUTINES SECTION.
      *********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LE1-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLEGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPLE1IN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *********************************************
       END-ROUTINE SECTION.
      *********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY LEMAIN-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
