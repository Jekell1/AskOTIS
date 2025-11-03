       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CIINQ.
       DATE-WRITTEN. 01/31/2011.
      ******************************************************************
      *
      *                 (WI)
      *  DESCRIPTION:   CUSTOMER INFORMATION WINDOW
      *                 CALLED FROM F9 INTERRUPT AND CISCAN
      *
      *         WRITTEN FOR WORLD
      *
      * REV:
      * BAH 050822 WORLD PR# 739
      * KEC 2016.1003 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED CI-DATE FIELDS TO BE IN CCYYMMDD FORMAT.
      * BAH 170412 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181009 GLOBAL CIFILE
      * BAH 20191007 REMOVED ACCESS-CALL CIFILE
      * BAH 2024.0417 REMOVED CIINQW, CALL PROGRAM INSTEAD OF WINDOW, NOTHING
      *               TO PASS
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)WI/CIINQ.CBL S35 02/22/24-12:50:27 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01CI.CPY".
       COPY "LIBLP/LP01CI_SQL.CPY".
       COPY "LIBLP/LPWSCI.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".


       01  CIINQ-LINES              PIC 99  VALUE 22.
       01  CIINQ-COLUMNS            PIC 99  VALUE 78.
       01  CIINQ-BEGIN-Y            PIC 99  VALUE 2.
       01  CIINQ-BEGIN-X            PIC 99  VALUE 2.

       01  ERRCD           PIC X       VALUE SPACES.
       01  LEGEND          PIC X(70).
       01  LEGEND-01.
           03  FILLER       PIC X(37)  VALUE
           "F7-EXIT:".

       01  IO-TYPE           PIC X(3).
       01  WORK-AMT          PIC S9(7) VALUE 0.

       01  DISPLAY-BUF      VALUE SPACES.
           03  D-NUMBER         PIC ZZZZZ9.
      * SSNO
           03  D-01             PIC 999/99/9999.
           03  D-NAME-1         PIC X(32).
      * SSNO #2
           03  D-02             PIC 999/99/9999 BLANK ZERO.
           03  D-NAME-2         PIC X(32).
      * DATE OF INFORMATION
           03  D-03             PIC 99/99/99.
      * AMOUNT REQUESTED
           03  D-04             PIC Z(7)-.
      * MARITAL STATUS
           03  D-05             PIC X.
      * YEARS IN AREA
           03  D-06             PIC Z9.
      * DATE AT PRESENT ADR
           03  D-07             PIC 99/99/99 BLANK ZERO.
      * MONTHLY RENT/MTG
           03  D-08             PIC Z(7)-.
      * FAIR MARKET VALUE
           03  D-09             PIC Z(7)-.
      * MORTGAGE BALANCE
           03  D-10             PIC Z(7)-.
      * CHECKING Y/N
           03  D-11             PIC X.
      * SAVINGS Y/N
           03  D-12             PIC X.
      * DATE OF HIRE (1)
           03  D-13             PIC 99/99/99 BLANK ZERO.
      * POSITION CODE (1)
           03  D-14             PIC X(5).
      * GROSS MO. INCOME (1)
           03  D-15             PIC Z(7)-.
      * DATE OF HIRE (2)
           03  D-16             PIC 99/99/99 BLANK ZERO.
      * POSITION CODE (2)
           03  D-17             PIC X(5).
      * GROSS MO. INCOME (2)
           03  D-18             PIC Z(7)-.
      * TOT MONTHLY INCOME
           03  D-19             PIC Z(7)-.
      * LESS MONTHLY PAY
           03  D-20             PIC Z(7)-.
      * LESS RENT
           03  D-21             PIC Z(7)-.
      * FREE INCOME
           03  D-22             PIC Z(7)-.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(55) VALUE
           "ACCTNO 010 NAME1 020 NAME2 030 040 050 060 070 080 090 ".
           03  FILLER      PIC X(52) VALUE
           "100 110 120 130 140 150 160 170 180 190 200 210 220.".

       COPY "LIBCL/CLMENUW_EXT.CPY".

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".

       01  CIINQ-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CIINQ_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/CIINQ_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CIINQ_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ************************************
      *   P R O G R A M   C O N T R O L  *
      ************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-WIMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CIINQ" TO VDU-FORM-NAME.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE CIINQ-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  CIINQ-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    CIINQ-COLUMNS + VDU-WIN-W-MOD
                      LINES   CIINQ-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS CIINQ-WINDOW-HANDLE.

           MOVE "S  A250TITLE:FR." TO VDU.
           MOVE "LOAN BORROWER INFO INQUIRY" TO VDUBUF.
           PERFORM SCREENIO.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-CI-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           GO TO MAIN-MODULE.

      **************************
       REC-CI-MODULE SECTION.
      **************************
           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

           PERFORM OPEN-CI1-FILE.
           MOVE LN-OWNBR  TO CI-BRNO.
           MOVE LN-ACCTNO TO CI-ACCTNO.
           PERFORM READ-CI1-FILE.
           PERFORM CLOSE-CI1-FILE.
           IF IO-FG = 0
              PERFORM DISPLAY-FIELDS
           ELSE
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CI-MODULE.

       MODIFY-ROUTINE.
           MOVE LEGEND-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CI-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           GO TO MODIFY-ROUTINE.

       END-REC-CI-MODULE.
           EXIT.

      ***************************
       DISPLAY-FIELDS SECTION.
      ***************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE LN-ACCTNO TO D-NUMBER.

           PERFORM OPEN-BW1-FILE.
           MOVE CI-SSNO TO D-01 BW-SSNO.
           INSPECT D-01 REPLACING ALL "/" BY "-".
           PERFORM READ-BW1-FILE.
           MOVE SPACES TO D-NAME-1.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                                         INTO D-NAME-1.

           IF CI-SSNO-2 NOT = 0
              MOVE CI-SSNO-2 TO D-02 BW-SSNO
              INSPECT D-02 REPLACING ALL "/" BY "-"
              PERFORM READ-BW1-FILE
              MOVE SPACES TO D-NAME-2
              STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                  INTO D-NAME-2.
           PERFORM CLOSE-BW1-FILE.

           MOVE CI-DATE-UPDATED        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO D-03.

           MOVE CI-AMT-REQUESTED       TO D-04.
           MOVE CI-MARITAL-STAT        TO D-05.
           MOVE CI-YEARS-IN-AREA       TO D-06.

           MOVE CI-DATE-AT-PRESENT-ADR TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO D-07.

           MOVE CI-MONTHLY-RENT-MTG    TO D-08.
           MOVE CI-FAIR-MARKET-VALUE   TO D-09.
           MOVE CI-MTG-BALANCE         TO D-10.
           MOVE CI-CHECKING-ACCOUNT    TO D-11.
           MOVE CI-SAVINGS-ACCOUNT     TO D-12.

           MOVE CI-DATE-HIRE-1         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO D-13.

           MOVE CI-POSITION-CODE-1     TO D-14.
           MOVE CI-GROSS-MO-INCOME-1   TO D-15.

           MOVE CI-DATE-HIRE-2         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO D-16.

           MOVE CI-POSITION-CODE-2     TO D-17.
           MOVE CI-GROSS-MO-INCOME-2   TO D-18.

           MOVE CI-TOTAL-MO-INCOME     TO D-19.
           MOVE CI-LESS-MO-PAY         TO D-20.
           MOVE CI-MONTHLY-RENT-MTG    TO D-21.
           COMPUTE WORK-AMT = CI-TOTAL-MO-INCOME
                            - CI-LESS-MO-PAY
                            - CI-MONTHLY-RENT-MTG.
           MOVE WORK-AMT               TO D-22.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON CIINQ-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY CIINQ-SCREEN UPON CIINQ-WINDOW-HANDLE.
           MOVE CIINQ-FORM-FIELDS TO WR-BUF.
           MOVE CIINQ-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE CIINQ-HELP--FILE TO HELP-LINK-FILE.
           MOVE CIINQ-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CIINQ_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************
       MISC-PARAGRAPHS SECTION.
      ****************************
       COPY "LIBGB/ACCESS.CPY".
      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A700WSEL:D." TO VDU.
           MOVE LEGEND TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CI1-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCIGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPCI1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CIINQ-SCREEN.
           DESTROY CIINQ-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
