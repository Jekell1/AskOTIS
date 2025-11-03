       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RMMAIN.
       DATE-WRITTEN.    102389.
      ******************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   RESTRICTED MONEY FILE MAINTENANCE
      *  REVS:
      *   JTG 102490 ADDED LE-BALANCE
      *  MJD 131120 A30 CONVERSION COMPLETED
      *   CS 160511 REMOVED REDEF OF LE-FLAG/LE-SSNO AND MADE SEPARATE FIELDS
      *
      * KEC 2017.0425 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          FIXED 'LE' FILE TO HAVE SUB-PREFIX OF 'E' OR 'R'.
      *  CS 170505 ACTIVATE 8 DIGIT DATES
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
           "@(#)MN/RMMAIN.CBL S35 02/21/24-13:17:14 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01LE.CPY".
       COPY "LIBLP/LP01LE_SQL.CPY".
       COPY "LIBLP/LPWSLE.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

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

       01  ORIGINAL-LE-REC    PIC X(LE-SIZE).
       01  CHANGED-LE-REC     PIC X(LE-SIZE).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC ZZ/ZZ/ZZ.
           03  F02         PIC ZZZ,ZZ9.99 BLANK ZERO.
           03  F03         PIC ZZ/ZZ/ZZ.
           03  F04         PIC ZZZ,ZZ9.99- BLANK ZERO.
           03  F05         PIC ZZZ,ZZ9.99- BLANK ZERO.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(20) VALUE
           "010 020 030 040 050.".
      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX             PIC 99   VALUE 5.
       01  SPEC-TABLE.
      * 1. LOANDATE
           03  FILLER      PIC X(8) VALUE "ERNM060N".
      * 2. LNAMT
           03  FILLER      PIC X(8) VALUE "E  N062N".
      * 3. POFFDATE
           03  FILLER      PIC X(8) VALUE "ERNM060N".
      * 4. RESTRICTED-MONEY
           03  FILLER      PIC X(8) VALUE "E  N062N".
      * 5. BALANCE...NET
           03  FILLER      PIC X(8) VALUE "E  N062N".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 5 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       01 RMMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/RMMAIN_DEF.CPY".
       COPY "LIBMN/RMMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/RMMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      *****************************************
       MAIN-PROGRAM SECTION.
      *****************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "RMMAIN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS RMMAIN-WINDOW-HANDLE.

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

      *****************************************
       REC-LE1-MODULE SECTION.
      *****************************************
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
           MOVE VDUNUM0 TO LN-ACCTNO.

           MOVE LN-ACCTNO TO LE-ACCTNO HOLD-LE-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              MOVE "INVALID LOAN #" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LE1-MODULE-NO.

       ENTER-LE-FLAG.
           MOVE "R RA010FL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LE1-MODULE.
           IF VDUSTATUS = "1U"
              GO TO REC-LE1-MODULE-NO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-LE-FLAG.
           MOVE VDUBUF TO LE-FLAG.
           IF LE-FLAG NOT = "D" AND
                      NOT = "P" AND
                      NOT = "R"
              MOVE "MUST BE D, P, OR R" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LE-FLAG.
           MOVE LE-FLAG TO HOLD-LE-FLAG.
       ENTER-LE1-OSNUM.
           MOVE "EN N060LN2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LE1-MODULE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LE-FLAG.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-LE1-OSNUM.
           MOVE VDUNUM0 TO LN-ACCTNO LE-OSACCTNO HOLD-LE-OSACCTNO.
       READ-LEFILE.
           MOVE "R" TO LE-TYPE HOLD-LE-TYPE.
           MOVE ZEROS  TO LE-SSNO  HOLD-LE-SSNO.
           PERFORM OPEN-LE1-FILE.
           PERFORM READ-LE1-FILE.
           PERFORM CLOSE-LE1-FILE.
           IF IO-FG = 0
              MOVE LE-REC TO ORIGINAL-LE-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.
       END-REC-LE1-MODULE.
           EXIT.

      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      *************************************************
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

           IF ELE = 5
              IF LE-FLAG = "D"
                 MOVE "ENTER DUAL LOAN BALANCE" TO MESS
                 PERFORM SEND-MESS
              ELSE
              IF LE-FLAG = "P"
                 MOVE "ENTER 0" TO MESS
                 PERFORM SEND-MESS
              ELSE
              IF LE-FLAG = "R"
                 MOVE "ENTER NET RENEWAL AMOUNT" TO MESS
                 PERFORM SEND-MESS.

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
           GO TO F-01 F-02 F-03 F-04 F-05
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
       F-01.
           MOVE VDU-DATE-YYYYMMDD TO LE-R-LOANDATE.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM2 TO LE-R-LNAMT.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDU-DATE-YYYYMMDD TO LE-R-POFFDATE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM2 TO LE-R-RESTRICTED-MONEY.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO LE-R-BALANCE.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      *****************************************
       WRITE-MODULE SECTION.
      *****************************************
           PERFORM OPEN-LE1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
           MOVE LE-REC TO CHANGED-LE-REC.
           IF ORIGINAL-LE-REC = CHANGED-LE-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-LE1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF LE-REC NOT = ORIGINAL-LE-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-LE-REC TO LE-REC.
           PERFORM REWRITE-LE1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-LE1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-LE1-FILE.

      *****************************************
       DELETE-MODULE SECTION.
      *****************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LE1-FILE.
           PERFORM READ-LE1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF LE-REC NOT = ORIGINAL-LE-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-LE1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-LE1-FILE.

      *****************************************
       NEW-RECORD-SETUP SECTION.
      *****************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       
           PERFORM CLEAR-LE-FILE.

           MOVE HOLD-LE1-KEY TO LE1-KEY.
           MOVE 0 TO ELE.

      *****************************************
       DISPLAY-FIELDS SECTION.
      *****************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE LE-R-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO F01.

           MOVE LE-R-LNAMT TO F02.
           MOVE LE-R-POFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO F03.

           MOVE LE-R-RESTRICTED-MONEY TO F04.
           MOVE LE-R-BALANCE TO F05.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************
       FORM-RESET SECTION.
      *****************************************
           MOVE EXT-CONAME-CONAME TO RMMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO RMMAIN-SYSDATE.
           DISPLAY SPACES UPON RMMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY RMMAIN-SCREEN UPON RMMAIN-WINDOW-HANDLE.
           MOVE RMMAIN-FORM-FIELDS TO WR-BUF.
           MOVE RMMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************
       SETUP-LINK-INFO SECTION.
      *****************************************
           MOVE RMMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE RMMAIN-HELP--DIR TO HELP-LINK-DIR.

      *****************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/RMMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************
       COPY "LIBGB/ACCESS.CPY".
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLECL.CPY".

      *****************************************
       IO-ROUTINES SECTION.
      *****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LE1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLEGS_SQL.CPY".
       COPY "LIBLP/LPLE1IN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *****************************************
       END-ROUTINE SECTION.
      *****************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY RMMAIN-WINDOW-HANDLE
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
