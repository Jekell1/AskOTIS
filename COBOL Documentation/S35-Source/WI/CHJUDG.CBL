       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHJUDG.
       DATE-WRITTEN.  04/03/2012.
      *****************************************************
      *                 (WI)
      *
      *  DESCRIPTION:   CHANGE JUDGMENT STATUS
      *                 CALLED FROM F9 INTERRUPT
      *
      *  REV:
      *  BAH 120403 NEW FOR CITIZENS, IA PR# 3990
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *  BAH 170412 ACTIVATE 8 DIGIT DATES, PD0006
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/CHJUDG A32 11/13/18-08:22:37 >".

       COPY "LIBLP/LP01LSJ.CPY".
       COPY "LIBLP/LP01LSJ_SQL.CPY".
       COPY "LIBLP/LPWSLSJ.CPY".

       01  CHJUDG-LINES              PIC 99  VALUE 22.
       01  CHJUDG-COLUMNS            PIC 99  VALUE 78.
       01  CHJUDG-BEGIN-Y            PIC 99  VALUE 2.
       01  CHJUDG-BEGIN-X            PIC 99  VALUE 2.

       01  LEGEND          PIC X(70).
       01  LEGEND-01.
           03  FILLER       PIC X(37)  VALUE
           "F1-RESET, F7-UPDATE, ENTER FIELD #:".

       01  ERRCD           PIC X     VALUE SPACES.
       01  HOLD-SYS-DATE   PIC 9(8).

      *-----------------------------------------------------------------
       01  CHAN              PIC X(4)  VALUE "CHAN".
       01  ONE               PIC S9.
       01  ELE               PIC 99    VALUE 0.
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  IO-TYPE           PIC X(3).

       01  NEW-FLAG          PIC 9.

       01  HOLD-LSJ1-KEY.
           03  HOLD-BRNO     PIC 9(4).
           03  HOLD-ACCTNO   PIC 9(8).
           03  HOLD-SEQNO    PIC 99.

      * ORIGINAL RECORD
           COPY "LIBLP/LP01LSJ.CPY" REPLACING LEADING "LSJ" BY "LSJ-OR".
      * CHANGED RECORD
           COPY "LIBLP/LP01LSJ.CPY" REPLACING LEADING "LSJ" BY "LSJ-CH".

       01  DISPLAY-BUF      VALUE SPACES.
           03  D-01        PIC 99/99/99.
           03  D-02        PIC X(15).
           03  D-03        PIC Z(7).99.
           03  D-04        PIC X(25).
           03  D-05        PIC X(25).
           03  D-06        PIC X(25).
           03  D-07        PIC X(15).
           03  D-08        PIC X(2).
           03  D-081       PIC X(10).
           03  D-09        PIC X(12).
           03  D-10        PIC Z(7).99.
           03  D-11        PIC Z(7).99.
           03  D-12        PIC Z(7).99.
           03  D-13        PIC 99/99/99.
           03  D-14        PIC X(25).
           03  D-15        PIC X(25).
           03  D-16        PIC X(25).
           03  D-17        PIC X(15).
           03  D-18        PIC X(2).
           03  D-181       PIC X(10).
           03  D-19        PIC 99/99/99.
           03  D-20        PIC X.
           03  D-21        PIC X.
           03  D-22        PIC X(25).
           03  D-23        PIC X(25).
           03  D-24        PIC X(15).
           03  D-25        PIC X(2).
           03  D-251       PIC X(10).
           03  D-26        PIC X(12).


       01  DISPLAY-LIST.
           03  FILLER      PIC X(36) VALUE
           "010 020 030 040 050 060 070 080 081 ".
           03  FILLER      PIC X(36) VALUE
           "090 100 110 120 130 140 150 160 170 ".
           03  FILLER      PIC X(36) VALUE
           "180 181 190 200 210 220 230 240 250 ".
           03  FILLER      PIC X(8) VALUE
           "251 260.".


       01  MAX           PIC 99   VALUE 26.

      ******************************************************************
      *      ELEMENT SPECIFICATIONS
      ******************************************************************
       01  SPEC-TABLE.
      * 1  JDDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 2  CASE#
           03  FILLER      PIC X(8) VALUE "E NA150 ".
      * 3  AMOUNT
           03  FILLER      PIC X(8) VALUE "ENNN072 ".
      * 4  COUNTY
           03  FILLER      PIC X(8) VALUE "E NA250 ".
      * 5  CLERK
           03  FILLER      PIC X(8) VALUE "E NA250 ".
           03  FILLER      PIC X(8) VALUE "E NA250 ".
           03  FILLER      PIC X(8) VALUE "E NA150 ".
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      * 9 CLERK PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 10 SHERRIFF FEES
           03  FILLER      PIC X(8) VALUE "ENNN072 ".
           03  FILLER      PIC X(8) VALUE "ENNN072 ".
           03  FILLER      PIC X(8) VALUE "ENNN072 ".
      * 13. DISMISSAL DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
           03  FILLER      PIC X(8) VALUE "E  A250 ".
      * 15. EMPLYR
           03  FILLER      PIC X(8) VALUE "E NA250 ".
           03  FILLER      PIC X(8) VALUE "E NA250 ".
           03  FILLER      PIC X(8) VALUE "E NA150 ".
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      * 19. GARN DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
           03  FILLER      PIC X(8) VALUE "E NA010 ".
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 22. ATTORNEY
           03  FILLER      PIC X(8) VALUE "E NA250 ".
           03  FILLER      PIC X(8) VALUE "E NA250 ".
           03  FILLER      PIC X(8) VALUE "E NA150 ".
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      * 26 ATTY PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".


       01  SPEC-TAB1      REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 26 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.


       COPY "LIBCL/CLMENUW_EXT.CPY".

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/PASSWDW.CPY".

       01  CHJUDG-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CHJUDG_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/CHJUDG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CHJUDG_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E    D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.

           MOVE " " TO VDU-WIMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CHJUDG" TO VDU-FORM-NAME.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYS-DATE.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE CHJUDG-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  CHJUDG-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    CHJUDG-COLUMNS + VDU-WIN-W-MOD
                      LINES   CHJUDG-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS CHJUDG-WINDOW-HANDLE.

           MOVE "S  A240TITLE:FR." TO VDU.
           MOVE "JUDGMENT INFORMATION" TO VDUBUF.
           PERFORM SCREENIO.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-LS-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE
              GO TO END-ROUTINE.

           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE
             GO TO END-ROUTINE.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      ****************************************
       REC-LS-MODULE SECTION.
      ****************************************
           MOVE SPACES TO IO-TYPE LSJ-OR-REC.
           MOVE 0 TO NEW-FLAG.

           MOVE EXT-WI-CL-OWNBR  TO LSJ-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LSJ-ACCTNO.
           MOVE 1                TO LSJ-SEQNO.
           MOVE LSJ1-KEY         TO HOLD-LSJ1-KEY.

           PERFORM OPEN-LSJ1-FILE.
           PERFORM READ-LSJ1-FILE.
           PERFORM CLOSE-LSJ1-FILE.
           IF IO-FG = 0
              MOVE LSJ-REC TO LSJ-OR-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
              GO TO END-REC-LS-MODULE.

           PERFORM NEW-RECORD-SETUP.

       END-REC-LS-MODULE.
           EXIT.

      ******************************************************************
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.
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
           MOVE LEGEND-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL " " BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.

           MOVE "F1-CANCEL, ENTER FIELD INFORMATION:" TO LEGEND.
           PERFORM SEND-LEGEND.

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

       TEST-F7.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15
                 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25
                 F-26
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.


      ****************************************
      *      SET RECORD ELEMENTS
      ****************************************
       F-01.
           MOVE VDU-DATE-YYYYMMDD TO LSJ-FILEDATE.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO LSJ-CASE-NO.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUNUM2 TO LSJ-AMT.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO LSJ-COUNTY.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO LSJ-CLERK-NAME.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO LSJ-CLERK-ADDR.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO LSJ-CLERK-CITY.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO LSJ-CLERK-STATE.
       F-081.
           MOVE "E  A100081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-081.
           MOVE VDUBUF TO LSJ-CLERK-ZIP.
           GO TO CREATE-ROUTINE.
       F-09.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO LSJ-CLERK-PHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120090." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUNUM2 TO LSJ-SHERIFF-FEE.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUNUM2 TO LSJ-COURT-FEE.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM2 TO LSJ-ATTY-FEE.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDU-DATE-YYYYMMDD TO LSJ-DISMISS-DATE.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUBUF TO LSJ-GARN-INFO.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUBUF TO LSJ-EMPLOY-NAME.
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDUBUF TO LSJ-EMPLOY-ADDR.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUBUF TO LSJ-EMPLOY-CITY.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUBUF TO LSJ-EMPLOY-STATE.
       F-181.
           MOVE "E  A100181." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-181.
           MOVE VDUBUF TO LSJ-EMPLOY-ZIP.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDU-DATE-YYYYMMDD TO LSJ-GARN-DATE.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUBUF TO LSJ-JD-FILED-ON.
           IF LSJ-JD-FILED-ON NOT = "M" AND NOT
                                    = "S" AND NOT
                                     = "B" AND NOT
                                      = " "
              MOVE "ENTER: 'M', 'S', 'B', OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUBUF TO LSJ-GARN-FILED-ON.
           IF LSJ-GARN-FILED-ON NOT = "M" AND NOT
                                      = "S" AND NOT
                                       = "B" AND NOT
                                        = " "
              MOVE "ENTER: 'M', 'S', 'B', OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUBUF TO LSJ-ATTY-NAME.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUBUF TO LSJ-ATTY-ADDR.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUBUF TO LSJ-ATTY-CITY.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE VDUBUF TO LSJ-ATTY-STATE.
       F-251.
           MOVE "E  A100251." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-251.
           MOVE VDUBUF TO LSJ-ATTY-ZIP.
           GO TO CREATE-ROUTINE.

       F-26.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO LSJ-ATTY-PHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120260." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      *************************************************
       WRITE-MODULE SECTION.
      *************************************************
           PERFORM OPEN-LSJ1-FILE.

           MOVE LSJ-REC TO LSJ-CH-REC.

           IF LSJ-OR-REC = LSJ-CH-REC
              GO TO END-WRITE-MODULE.

           IF LSJ-OR-REC = SPACES
              GO TO WRITE-RECORD.


           PERFORM READ-LSJ1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF LSJ-REC NOT = LSJ-OR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE LSJ-CH-REC TO LSJ-REC.

           PERFORM REWRITE-LSJ1-FILE.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LSJ1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.


       END-WRITE-MODULE.

      **********************************************
       DELETE-MODULE SECTION.
      **********************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LSJ1-FILE.
           PERFORM READ-LSJ1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF LSJ-REC NOT = LSJ-OR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-LSJ1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-LSJ1-FILE.

      *****************************************
       NEW-RECORD-SETUP SECTION.
      *****************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LSJ-FILE.

           MOVE HOLD-LSJ1-KEY TO LSJ1-KEY.

       NEW-RECORD-EXIT.
           EXIT.

      ****************************************
       DISPLAY-FIELDS SECTION.
      ****************************************
           MOVE SPACES               TO DISPLAY-BUF.

           MOVE LSJ-FILEDATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO D-01.
           MOVE LSJ-CASE-NO        TO D-02.
           MOVE LSJ-AMT            TO D-03.
           MOVE LSJ-COUNTY         TO D-04.
           MOVE LSJ-CLERK-NAME     TO D-05.
           MOVE LSJ-CLERK-ADDR     TO D-06.
           MOVE LSJ-CLERK-CITY     TO D-07.
           MOVE LSJ-CLERK-STATE    TO D-08.
           MOVE LSJ-CLERK-ZIP      TO D-081.
           MOVE LSJ-CLERK-PHONE    TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO D-09.

           MOVE LSJ-SHERIFF-FEE    TO D-10.
           MOVE LSJ-COURT-FEE      TO D-11.
           MOVE LSJ-ATTY-FEE       TO D-12.

           MOVE LSJ-DISMISS-DATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO D-13.
           MOVE LSJ-GARN-INFO      TO D-14.
           MOVE LSJ-EMPLOY-NAME    TO D-15.
           MOVE LSJ-EMPLOY-ADDR    TO D-16.
           MOVE LSJ-EMPLOY-CITY    TO D-17.
           MOVE LSJ-EMPLOY-STATE   TO D-18.
           MOVE LSJ-EMPLOY-ZIP     TO D-181.
           MOVE LSJ-GARN-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO D-19.

           MOVE LSJ-JD-FILED-ON    TO D-20.
           MOVE LSJ-GARN-FILED-ON  TO D-21.
           MOVE LSJ-ATTY-NAME      TO D-22.
           MOVE LSJ-ATTY-ADDR      TO D-23.
           MOVE LSJ-ATTY-CITY      TO D-24.
           MOVE LSJ-ATTY-STATE     TO D-25.
           MOVE LSJ-ATTY-ZIP       TO D-251.
           MOVE LSJ-ATTY-PHONE    TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT             TO D-26.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLSJCL.CPY".


      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON CHJUDG-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY CHJUDG-SCREEN UPON CHJUDG-WINDOW-HANDLE.
           MOVE CHJUDG-FORM-FIELDS TO WR-BUF.
           MOVE CHJUDG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE CHJUDG-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHJUDG-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CHJUDG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************
       MISC-PARAGRAPHS SECTION.
      *********************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A700WSEL:D." TO VDU.
           MOVE LEGEND TO VDUBUF.
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
           PERFORM CLOSE-LSJ1-FILE.
       COPY "LIBLP/LPLSJGS_SQL.CPY".
       COPY "LIBLP/LPLSJ1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHJUDG-SCREEN.
           DESTROY CHJUDG-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
