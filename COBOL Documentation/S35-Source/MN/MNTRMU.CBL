       IDENTIFICATION DIVISION.
       PROGRAM-ID.      MNTRMU.
       DATE-WRITTEN.    090694.
      *****************************************************************
      *
      *  DESC:  TRANSACTION FILE MAINTENANCE MENU
      *
      *=================================================================
      *
      * ***NOTE*** MN/MNTRMU.CBL (A30) IS MN/TRMNMU (A15)
      * ***NOTE*** DUPLICATE IN NP WITHIN A15
      *
      *=================================================================
      * REV.
      * 092795 KEC ADDED OPTION FOR BRANCH (BULK) TRANSFERS
      * 102396 JFF ADDED OPTION FOR CLASS TRANSFERS.
      * MJD 110596 ADDED OPTION FOR CONVERSION MAINTENANCE.
      * MJD 000222 ADDED TRWMAN (CASH BALANCING TRANSACTION) (WORLD PR#219)
      * MJD 010104 ADDED TRGMIN (EARNINGS TRANSACTION) (PR#1375)
      * BLV 011227 CHANGED MENU SELECTION FOR XF TO SAY SERVICE
      * BLF 050127 STARRED OFF SEND-LEGEND, WAS MESSING UP DISPLAY-BUF,
      *            NO EXPLANATION, JUST LIKE IN WIMENU!!!
      * KEC 050907 {PR#00419} WORLD <A15> [JIM ROSENAUER]
      *            ADDED THREE MORE LINES SO THAT I COULD ADD
      *            TRANSACTION INSURANCE (TRI) MAINTENANCE.
      * KEC 2015.1118 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         RENAMED MN/TRMNMU (A15) TO BE MN/MNTRMU (A30)
      *         DUE TO DUPLICATE NAME IN THE NP DIRECTORY.
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING HOLD-KEY.
      * BLM 161223 REMOVE TRGMIN, EARNINGS, NOT USED
      *  CS 170109 CHANGED CANCEL FORM-PROGX TO CANCEL PROGRAM NAME IN
      *            SALEFIN-MAINT,XFER-MAINT,BRANCH-XFER,CLASS-XFER,
      *            CONVERSION-MAINT,PAYMENT-MAINT,DEPOSIT MAINT,CASHBAL
      *            LOAN MAINT.  WHEN EXITING TO MENU AND GOING BACK IN,
      *            GOING TO PAGE2 OF A MAINTENANCE, IT TOOK YOU TO
      *            TRMNMU.
      * BAH 2024.0520 INSURANCE CLAIMS MAINT WAS CALLING TRIMIN INSTEAD
      *               OF TRIMAN S35Q-17
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/MNTRMU.CBL S35 05/20/24-14:08:06 >".

      *****************************************************************
      *     P R O G R A M   W O R K E R S              *
      *****************************************************************

       01  EXT-EOBUF-MESS          PIC XX.
       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-STAT             PIC XX.

       01  FLDSEL-LIST.
           03  FILLER PIC X(20)  VALUE "010 020 030 040 050 ".
           03  FILLER PIC X(20)  VALUE "060 070 080 090 100 ".
           03  FILLER PIC X(20)  VALUE "110 120 130 140 150.".

       01  FLDSEL-BUF.
           05  FILLER              PIC X.
           05  FLDSEL-KEY          PIC XX.
           05  FLDSEL-NAME         PIC X(26).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "150.".
       01  LEGEND                  PIC X(30) VALUE SPACES.
       01  SUB                     PIC 99.

       01  DISPLAY-LIST.
           03  FILLER PIC X(20)  VALUE "010 020 030 040 050 ".
           03  FILLER PIC X(20)  VALUE "060 070 080 090 100 ".
           03  FILLER PIC X(20)  VALUE "110 120 130 140 150 ".
           03  FILLER PIC X(16)  VALUE "PGNO ENDMORE:SF.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 15 TIMES.
               05  FILLER          PIC X.
               05  D-KEY           PIC XX.
               05  D-NAME          PIC X(26).
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).


      *****************************************************************
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       01 MNTRMU-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/MNTRMU_DEF.CPY".
       COPY "LIBMN/MNTRMU_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/MNTRMU_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      /****************************************************************
      *
      *   M A I N    P R O G R A M    M O D U L E
      *
      *****************************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           PERFORM SETUP-WINDOW.

       SELECT-FIELD-FROM-DISPLAY.
           MOVE "INTERRUPT-OPTIONS" TO WR-BUF.
           MOVE "TITLE." TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE " F7-EXIT, SELECT OPTION(RTN):" TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY.

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.
           IF NOT (FLDSEL-STAT = "00")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

      *---------------------------------------------------------------
       CHECK-SELECTION.
     
           IF FLDSEL-KEY = "CH"
              PERFORM CHECK-MAINT
           ELSE
           IF FLDSEL-KEY = "DE"
              PERFORM DEPOSIT-MAINT
           ELSE
           IF FLDSEL-KEY = "CA"
              PERFORM CASHBAL-MAINT
           ELSE
           IF FLDSEL-KEY = "LO"
              PERFORM LOAN-MAINT
           ELSE
           IF FLDSEL-KEY = "PA"
              PERFORM PAYMENT-MAINT
           ELSE
           IF FLDSEL-KEY = "SA"
              PERFORM SALEFIN-MAINT
           ELSE
           IF FLDSEL-KEY = "XF"
              PERFORM XFER-MAINT
           ELSE
           IF FLDSEL-KEY = "BR"
              PERFORM BRANCH-XFER-MAINT
           ELSE
           IF FLDSEL-KEY = "CL"
              PERFORM CLASS-XFER-MAINT
           ELSE
           IF FLDSEL-KEY = "CO"
              PERFORM CONVERSION-MAINT
           ELSE
           IF FLDSEL-KEY = "IN"
              PERFORM TR-I-MAINTENANCE.

           IF FLDSEL-KEY NOT = "SC"
              GO TO CONTINUE-SELECT.

           PERFORM TRANSACTION-SCAN.
      *    IF SCAN-STATUS NOT = "00"
              GO TO CONTINUE-SELECT.

      *    GO TO CHECK-SELECTION.

       CONTINUE-SELECT.
           GO TO SELECT-FIELD-FROM-DISPLAY.

      *****************************************************************
       TRANSACTION-SCAN SECTION.
      *****************************************************************
           MOVE "WI/TRSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      *****************************************************************
       CHECK-MAINT SECTION.
      *****************************************************************
           MOVE "MN/TRCMAN"  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL "TRCMAN".

      *****************************************************************
       DEPOSIT-MAINT SECTION.
      *****************************************************************
           MOVE "MN/TRDMAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL "TRDMAN".

      *****************************************************************
       CASHBAL-MAINT SECTION.
      *****************************************************************
           MOVE "MN/TRWMAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL "TRWMAN".

      *****************************************************************
       LOAN-MAINT SECTION.
      *****************************************************************
           MOVE "MN/TRLMIN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL "TRLMIN".        

      *****************************************************************
       PAYMENT-MAINT SECTION.
      *****************************************************************
           MOVE "MN/TRPMIN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL "TRPMIN".

      *****************************************************************
       SALEFIN-MAINT SECTION.
      *****************************************************************
           MOVE "MN/TRSMIN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL "TRSMIN".

      *****************************************************************
       XFER-MAINT SECTION.
      *****************************************************************
           MOVE "MN/TRXMIN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL "TRXMIN".

      *****************************************************************
       BRANCH-XFER-MAINT SECTION.
      *****************************************************************
           MOVE "MN/TRBMIN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL "TRBMIN".

      *****************************************************************
       CLASS-XFER-MAINT SECTION.
      *****************************************************************
           MOVE "MN/TRJMIN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL "TRJMIN".

      *****************************************************************
       CONVERSION-MAINT SECTION.
      *****************************************************************
           MOVE "MN/TRKMIN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL "TRKMIN".

      *****************************************************************
       TR-I-MAINTENANCE SECTION.
      *****************************************************************
           MOVE "MN/TRIMAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL "TRIMAN".

      *****************************************************************
       SETUP-WINDOW SECTION.
      *****************************************************************

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "MNTRMU" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 02 * VDU-WIN-L-OFFSET
                   SCREEN COL 23 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 33 + VDU-WIN-W-MOD
                   LINES 19 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS MNTRMU-WINDOW-HANDLE.

           MOVE SPACES TO DISPLAY-BUF.

           MOVE "SC"  TO D-KEY(1).
           MOVE "AN TRANSACTIONS  "           TO D-NAME(1).
           MOVE "CH"  TO D-KEY(2).
           MOVE "ECK/CASH TRANSACTION MAINT"  TO D-NAME(2).
           MOVE "DE"  TO D-KEY(3).
           MOVE "POSIT TRANSACTION MAINT"     TO D-NAME(3).
           MOVE "LO"  TO D-KEY(4).
           MOVE "AN TRANSACTION MAINT"        TO D-NAME(4).
           MOVE "PA"  TO D-KEY(5).
           MOVE "YMENT TRANSACTION MAINT"     TO D-NAME(5).
           MOVE "SA"  TO D-KEY(6).
           MOVE "LES FINANCE TRANS MAINT"     TO D-NAME(6).
           MOVE "XF"  TO D-KEY(7).
           MOVE "ER TO SERVICE TRANS MAINT"   TO D-NAME(7).
           MOVE "BR"  TO D-KEY(8).
           MOVE "ANCH XFER TRANSACTION MAINT" TO D-NAME(8).
           MOVE "CL"  TO D-KEY(9).
           MOVE "ASS XFER TRANSACTION MAINT"  TO D-NAME(9).
           MOVE "CO"  TO D-KEY(10).
           MOVE "NVERSION TRANSACTION MAINT"  TO D-NAME(10).
           MOVE "CA"  TO D-KEY(11).
           MOVE "SH BAL TRANSACTION MAINT  "  TO D-NAME(11).
           MOVE "IN"  TO D-KEY(12).
           MOVE "SURANCE CLAIMS MAINTENANCE " TO D-NAME(12).
           MOVE "  "  TO D-KEY(13).
           MOVE "                           " TO D-NAME(13).
           MOVE "  "  TO D-KEY(14).
           MOVE "                           " TO D-NAME(14).

       DISPLAY-PAGE.
           MOVE "* END *" TO D-END-MORE.
           MOVE 1 TO D-PAGENO.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           DISPLAY SPACES UPON MNTRMU-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY MNTRMU-SCREEN UPON MNTRMU-WINDOW-HANDLE.
           MOVE MNTRMU-FORM-FIELDS TO WR-BUF.
           MOVE MNTRMU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE MNTRMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE MNTRMU-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/MNTRMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *****************************************************************
       MISC-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       ALARM.
           DISPLAY OMITTED WITH BEEP.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE "WSEL:D." TO WR-LIST.
           PERFORM CALL-WRFORM.

       END-ROUTINE.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY MNTRMU-SCREEN.
           DESTROY MNTRMU-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
