       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PMTSCH.
       DATE-WRITTEN.    011096.
      ******************************************************************
      *   DIR :  WI
      *   DESC:  LOAN PAYMENT SCHEDULE VIEW
      * CALLED:  FROM F9 CLMENU TO VIEW PAYMENT SCHEDULE.
      *
      * REV:
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
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
           "@(#)WI/PMTSCH.CBL S34 05/21/21-13:59:10 >".
      *-----------------------------------------------------------------
      *           PROGRAM WORKERS
      *-----------------------------------------------------------------
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

       01  PMTSCH-LINES               PIC 99  VALUE 18.
       01  PMTSCH-COLUMNS             PIC 99  VALUE 65.
       01  PMTSCH-BEGIN-Y             PIC 99  VALUE 5.
       01  PMTSCH-BEGIN-X             PIC 99  VALUE 5.

       01  LEGEND            PIC X(55).
       01  LEGEND-LIST       PIC X(8)   VALUE "WSEL:D.".

       01  ERRCD             PIC X VALUE " ".
      *-----------------------------------------------------------------
      *           SCREEN DISPLAY BUFFER
      *-----------------------------------------------------------------
       01  DISPLAY-BUF    VALUE SPACES.
           03  F010       PIC ZZZ.
           03  F011       PIC ZZZZZZ.ZZ.
           03  F020       PIC ZZZ.
           03  F021       PIC ZZZZZZ.ZZ.
           03  F030       PIC ZZZ.
           03  F031       PIC ZZZZZZ.ZZ.
           03  F040       PIC ZZZ.
           03  F041       PIC ZZZZZZ.ZZ.
           03  F050       PIC ZZZ.
           03  F051       PIC ZZZZZZ.ZZ.
           03  F060       PIC ZZZ.
           03  F061       PIC ZZZZZZ.ZZ.
           03  F070       PIC ZZZ.
           03  F071       PIC ZZZZZZ.ZZ.
           03  F080       PIC ZZZ.
           03  F081       PIC ZZZZZZ.ZZ.
           03  F090       PIC ZZZ.
           03  F091       PIC ZZZZZZ.ZZ.
           03  F100       PIC ZZZ.
           03  F101       PIC ZZZZZZ.ZZ.
           03  F110       PIC ZZZ.
           03  F111       PIC ZZZZZZ.ZZ.
           03  F120       PIC ZZZ.
           03  F121       PIC ZZZZZZ.ZZ.
           03  F130       PIC ZZZ.
           03  F131       PIC ZZZZZZ.ZZ.
           03  F140       PIC ZZZ.
           03  F141       PIC ZZZZZZ.ZZ.
           03  F150       PIC ZZZ.
           03  F151       PIC ZZZZZZ.ZZ.
           03  F160       PIC ZZZ.
           03  F161       PIC ZZZZZZ.ZZ.
           03  F170       PIC ZZZ.
           03  F171       PIC ZZZZZZ.ZZ.

       01  DISPLAY-LIST.
           03  FILLER     PIC X(32) VALUE
               "010 011 020 021 030 031 040 041 ".
           03  FILLER     PIC X(32) VALUE
               "050 051 060 061 070 071 080 081 ".
           03  FILLER     PIC X(32) VALUE
               "090 091 100 101 110 111 120 121 ".
           03  FILLER     PIC X(32) VALUE
               "130 131 140 141 150 151 160 161 ".
           03  FILLER     PIC X(8) VALUE
               "170 171.".

      *-----------------------------------------------------------------
      *           MISC WORKERS
      *-----------------------------------------------------------------
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01  PMTSCH-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/PMTSCH_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/PMTSCH_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/PMTSCH_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ***************************************************
       MAIN-PROGRAM SECTION.
      ***************************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE SPACES        TO VDU-WIMENU-SELECTION.
           MOVE "PMTSCH"      TO VDU-FORM-NAME.

           MOVE SPACES TO ERRCD.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE PMTSCH-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  PMTSCH-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    PMTSCH-COLUMNS + VDU-WIN-W-MOD
                      LINES   PMTSCH-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS PMTSCH-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE "LOAN RECORD NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           MOVE LN-OWNBR  TO LTP-BRNO.
           MOVE LN-ACCTNO TO LTP-ACCTNO.
           MOVE 1         TO LTP-SEQNO.

           PERFORM OPEN-LTP1-FILE.
           PERFORM READ-LTP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           PERFORM DISPLAY-FIELDS.

           MOVE " F7 - EXIT :" TO LEGEND.
           PERFORM SEND-LEGEND.

       ENTER-AGAIN.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           GO TO ENTER-AGAIN.

       END-REC-LT-MODULE.
           EXIT.

      *************************************
       DISPLAY-FIELDS SECTION.
      *************************************
           MOVE LTP-SCHLD-TERM(1)    TO F010.
           MOVE LTP-SCHLD-PAYMNT(1)  TO F011.
           MOVE LTP-SCHLD-TERM(2)    TO F020.
           MOVE LTP-SCHLD-PAYMNT(2)  TO F021.
           MOVE LTP-SCHLD-TERM(3)    TO F030.
           MOVE LTP-SCHLD-PAYMNT(3)  TO F031.
           MOVE LTP-SCHLD-TERM(4)    TO F040.
           MOVE LTP-SCHLD-PAYMNT(4)  TO F041.
           MOVE LTP-SCHLD-TERM(5)    TO F050.
           MOVE LTP-SCHLD-PAYMNT(5)  TO F051.
           MOVE LTP-SCHLD-TERM(6)    TO F060.
           MOVE LTP-SCHLD-PAYMNT(6)  TO F061.
           MOVE LTP-SCHLD-TERM(7)    TO F070.
           MOVE LTP-SCHLD-PAYMNT(7)  TO F071.
           MOVE LTP-SCHLD-TERM(8)    TO F080.
           MOVE LTP-SCHLD-PAYMNT(8)  TO F081.
           MOVE LTP-SCHLD-TERM(9)    TO F090.
           MOVE LTP-SCHLD-PAYMNT(9)  TO F091.
           MOVE LTP-SCHLD-TERM(10)   TO F100.
           MOVE LTP-SCHLD-PAYMNT(10) TO F101.
           MOVE LTP-SCHLD-TERM(11)   TO F110.
           MOVE LTP-SCHLD-PAYMNT(11) TO F111.
           MOVE LTP-SCHLD-TERM(12)   TO F120.
           MOVE LTP-SCHLD-PAYMNT(12) TO F121.
           MOVE LTP-SCHLD-TERM(13)   TO F130.
           MOVE LTP-SCHLD-PAYMNT(13) TO F131.
           MOVE LTP-SCHLD-TERM(14)   TO F140.
           MOVE LTP-SCHLD-PAYMNT(14) TO F141.
           MOVE LTP-SCHLD-TERM(15)   TO F150.
           MOVE LTP-SCHLD-PAYMNT(15) TO F151.
           MOVE LTP-SCHLD-TERM(16)   TO F160.
           MOVE LTP-SCHLD-PAYMNT(16) TO F161.
           MOVE LTP-SCHLD-TERM(17)   TO F170.
           MOVE LTP-SCHLD-PAYMNT(17) TO F171.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON PMTSCH-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY PMTSCH-SCREEN UPON PMTSCH-WINDOW-HANDLE.
           MOVE PMTSCH-FORM-FIELDS TO WR-BUF.
           MOVE PMTSCH-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE PMTSCH-HELP--FILE TO HELP-LINK-FILE.
           MOVE PMTSCH-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/PMTSCH_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************
       IO-ROUTINES SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN1-FILE.
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTP1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ****************************************
       END-ROUTINE SECTION.
      ****************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PMTSCH-SCREEN.
           DESTROY PMTSCH-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
