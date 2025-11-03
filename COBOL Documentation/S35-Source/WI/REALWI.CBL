       IDENTIFICATION DIVISION.
       PROGRAM-ID.    REALWI.
       AUTHOR.        MG.
       DATE-WRITTEN.  10-15-2001.
      ******************************************************************
      *
      * IDENTIFICATION:  WI/REALWI.C
      *
      * DESCRIPTION   :  AMORTIZED INTEREST PAYMENT INFO
      *
      * CALLED FROM   :  WI/RVLINE.C
      *
      *-----------------------------------------------------------------
      * REV:
      * MG  2001.1015 {PR#00000} PARADATA FINANCIAL [JOHN]
      *                ***NEW***
      * BAH 160128 REMOVED LTFILE, PD0003
      * BAH 160928 ADDED REALWI-BRNO AND CHANGED REALWI-SEQNO TO 9(5)
      * BAH 170421 ACTIVATE 8 DIGIT DATES, PD0006
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/REALWI.CBL S34 05/21/21-10:19:34 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ERRCD                       PIC X(01)     VALUE SPACES.
       01  LEGEND                      PIC X(75)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

       01  LEGEND-OPTIONS.
           03  FILLER PIC X(30) VALUE " F7-EXIT                      ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(10) VALUE "          ".

       01  LXZ-FOUND       PIC X(01)  VALUE "Y".

      ******************************************************************
      *
      *    D I S P L A Y - W O R K E R S
      *
      ******************************************************************

       01  DISPLAY-BUF.
           03  D-PAYDATE          PIC 99/99/99     BLANK ZERO.
           03  D-TRCD             PIC X(2).
           03  D-REFNO            PIC X(5).
           03  D-TRAMT            PIC Z(6)9.99-    BLANK ZERO.

           03  D-POST-REGPY       PIC Z(6)9.99-    BLANK ZERO.
           03  D-POST-EXTRA-PRIN  PIC Z(6)9.99-    BLANK ZERO.
           03  D-POST-OTH         PIC Z(6)9.99-    BLANK ZERO.
           03  D-POST-LCHG        PIC Z(6)9.99-    BLANK ZERO.
           03  D-TOTPMT           PIC Z(6)9.99-    BLANK ZERO.

           03  D-PDUE-APINT       PIC Z(6)9.99-    BLANK ZERO.
           03  D-PDUE-APPRIN      PIC Z(6)9.99-    BLANK ZERO.
           03  D-CUR-APINT        PIC Z(6)9.99-    BLANK ZERO.
           03  D-CUR-APPRIN       PIC Z(6)9.99-    BLANK ZERO.
           03  D-AP-EXTRA-PRIN    PIC Z(6)9.99-    BLANK ZERO.
           03  D-APOTH            PIC Z(6)9.99-    BLANK ZERO.
           03  D-APLC             PIC Z(6)9.99-    BLANK ZERO.
           03  D-TOTAPP           PIC Z(6)9.99-    BLANK ZERO.

           03  D-PDTH             PIC 99/99/99.
           03  D-LCPDTH           PIC 99/99/99.
           03  D-OTHBAL           PIC Z(6)9.99-    BLANK ZERO.
           03  D-LCBAL            PIC Z(6)9.99-    BLANK ZERO.
           03  D-INTBAL           PIC Z(6)9.99-    BLANK ZERO.
           03  D-CURBAL           PIC Z(6)9.99-    BLANK ZERO.

           03  D-INT-SINCE-LFP    PIC Z(6)9.99-    BLANK ZERO.
           03  D-PRIN-SINCE-LFP   PIC Z(6)9.99-    BLANK ZERO.

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER                   PIC X(39) VALUE
           "010 020 030 040 050 060 070 080 TOTPAY ".
           03  FILLER                   PIC X(35) VALUE
           "090 100 110 120 130 140 150 TOTAPP ".
           03  FILLER                   PIC X(39) VALUE
           "PDTH LCPDTH OTHBAL LCBAL INTBAL CURBAL ".
           03  FILLER                   PIC X(15) VALUE
           "INTLFP PRINLFP.".
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".

       01 REALWI-WINDOW-HANDLE     PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/REALWI_DEF.CPY".
       COPY "LIBWI/REALWI_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      *-----------------------------------------------------------------
       COPY "LIBGB/GETFMW.CPY".
       01  REALWI-BRNO                PIC 9(04).
       01  REALWI-ACCTNO              PIC 9(08).
       01  REALWI-SEQNO               PIC 9(05).

       SCREEN SECTION.
       COPY "LIBWI/REALWI_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME REALWI-BRNO
                               REALWI-ACCTNO REALWI-SEQNO EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "REALWI" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT SCREEN LINE 02 * VDU-WIN-L-OFFSET
               AT SCREEN COL 02 * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               SIZE 78 + VDU-WIN-W-MOD
               LINES 20 + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS REALWI-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE SPACES TO ERRCD.
           MOVE SPACES TO VDU-WIMENU-SELECTION.
           MOVE "Y"    TO LXZ-FOUND.


           PERFORM PRIMARY-DRIVER.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       PRIMARY-DRIVER SECTION.
      ******************************************************************

           MOVE REALWI-BRNO   TO LP-BRNO.
           MOVE REALWI-ACCTNO TO LP-ACCTNO.
           MOVE REALWI-SEQNO  TO LP-SEQNO.
           PERFORM GET-LP1-RECORD.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE LP-BRNO   TO LX-BRNO.
           MOVE LP-ACCTNO TO LX-ACCTNO.
           MOVE LP-SEQNO  TO LX-SEQNO.
           PERFORM OPEN-LX1-FILE.
           PERFORM READ-LX1-FILE.
           PERFORM CLOSE-LX1-FILE.
           IF ( IO-BAD )
              MOVE ZEROES TO LXZ-POST-REGPY  LXZ-POST-EXTRA-PRIN
                             LXZ-POST-OTH    LXZ-POST-LCHG
                             LXZ-PDUE-APINT  LXZ-PDUE-APPRIN
                             LXZ-CUR-APINT   LXZ-CUR-APPRIN
              MOVE "N" TO LXZ-FOUND.

           PERFORM DISPLAY-RECORD.

           MOVE LEGEND-OPTIONS TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       PRIMARY-DRIVER-OPTIONS.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF NOT ( F7-KEY )
              GO TO PRIMARY-DRIVER-OPTIONS.

       PRIMARY-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-RECORD SECTION.
      ******************************************************************
           MOVE SPACES                     TO DISPLAY-BUF.

           MOVE LP-PAYDATE                 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO D-PAYDATE.
           MOVE LP-TRCD                    TO D-TRCD.
           MOVE LP-REFNO                   TO D-REFNO.
           MOVE LP-TRAMT                   TO D-TRAMT.
           MOVE LXZ-POST-REGPY             TO D-POST-REGPY.
           MOVE LXZ-POST-EXTRA-PRIN        TO D-POST-EXTRA-PRIN.
           MOVE LXZ-POST-OTH               TO D-POST-OTH.
           MOVE LXZ-POST-LCHG              TO D-POST-LCHG.
           COMPUTE D-TOTPMT = LXZ-POST-REGPY + LXZ-POST-EXTRA-PRIN
                            + LXZ-POST-OTH   + LXZ-POST-LCHG.
           MOVE LXZ-PDUE-APINT             TO D-PDUE-APINT.
           MOVE LXZ-PDUE-APPRIN            TO D-PDUE-APPRIN.
           MOVE LXZ-CUR-APINT              TO D-CUR-APINT.
           MOVE LXZ-CUR-APPRIN             TO D-CUR-APPRIN.
           MOVE LP-EXTRA-PRIN-LFP          TO D-AP-EXTRA-PRIN.
           MOVE LP-APOTH                   TO D-APOTH.
           MOVE LP-APLC                    TO D-APLC.
           COMPUTE D-TOTAPP = LXZ-PDUE-APINT    + LXZ-PDUE-APPRIN
                            + LXZ-CUR-APINT     + LXZ-CUR-APPRIN
                            + LP-EXTRA-PRIN-LFP + LP-APOTH
                            + LP-APLC.
           MOVE LP-PDTH-DATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO D-PDTH.
           MOVE LP-LCPDTH-DATE             TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO D-LCPDTH.
           MOVE LP-OTHBAL                  TO D-OTHBAL.
           MOVE LP-LCBAL                   TO D-LCBAL.
           MOVE LP-INTBAL                  TO D-INTBAL.
           MOVE LP-CURBAL                  TO D-CURBAL.
           MOVE LP-REMAIN-INT-SINCE-LFP    TO D-INT-SINCE-LFP.
           MOVE LP-REMAIN-PRIN-SINCE-LFP   TO D-PRIN-SINCE-LFP.

           PERFORM SEND-DISPLAY.

           IF ( LXZ-FOUND = "N" )
              MOVE "NO LXZ-RECORD FOUND!!!" TO MESS
              PERFORM SEND-MESS.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ********************************************
       FORM-RESET SECTION.
      ********************************************
           DISPLAY SPACES UPON REALWI-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY REALWI-SCREEN UPON REALWI-WINDOW-HANDLE.
           MOVE REALWI-FORM-FIELDS TO WR-BUF.
           MOVE REALWI-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ********************************************
       SETUP-LINK-INFO SECTION.
      ********************************************
           MOVE REALWI-HELP--FILE TO HELP-LINK-FILE.
           MOVE REALWI-HELP--DIR TO HELP-LINK-DIR.

      ********************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/REALWI_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
      *-----------------------------------------------------------------
       GET-LP1-RECORD.

           PERFORM OPEN-LP1-FILE.
           PERFORM READ-LP1-FILE.
           PERFORM CLOSE-LP1-FILE.

      *-----------------------------------------------------------------
       SEND-DISPLAY.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LX1-FILE.
           PERFORM CLOSE-LP1-FILE.
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLX1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY REALWI-SCREEN.
           DESTROY REALWI-WINDOW-HANDLE.
           EXIT PROGRAM.
