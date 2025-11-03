       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CLMENU.
       DATE-WRITTEN.    082989.
      ******************************************************************
      *
      *  DESC:  COLLECTORS WINDOWS MAIN SELECTION MENU
      *              F9-INTERRUPT
      *
      * REV:
      *  042895 BAH ADDED DEALER PAYOFF INQUIRY OPTION
      *  051795 JTG ADDED REPOSESSION INQUIRY OPTION
      *  071495 MJD ADDED SFMAST - SALES FINANCE MASTER INQUIRY,
      *               AND THE LOGIC TO CHOOSE BETWEEN DIRECT LOAN/S.F.
      *               WHEN CALLING INQ.
      *  071795 MJD CHANGED LNFINC TO LNMAST.
      *  071196 BAH ADDED "AUTO INSURANCE INFO" FROM REL A50,
      *             INSTANT, PR#353
      *  080896 BAH ADDED LINES 15 AND 16, RESIZED THE WINDOW
      *  121296 JTG ADDED OPTION VDU-CLMENU-SELECTION = "R", RESCISSION
      *  011097 MJD ADDED PAYMENT SCHEDULE OPTION/ EXT-WI-CL-PAY-SCHLD-FG.
      *  KEC 013197 REVISED TO:
      *              --- READ THE LN-FILE;
      *              --- ACCESS THE LOAN APPLICATION MODULE.
      *  JTG 022897 ADDED GEORGIA INQUIRY (GAINQ) MERCURY AUDITORS
      *  JTG 052797 CHANGED TO DISPLAY BORROWERS NAMES
      *  JTG 020598 ADDED TEST FOR CAN LOANS ONLY AND IF SO STOPPED
      *             OPTION FOR RENEWAL FOR BALANCE (RN)
      *  MG  990927 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BLV 020628 ADDED LOGIC TO CORRESPOND WITH THE F2-NEXT ACCOUNT
      *             LOGIC IN GAINQY.C
      *  BLV 020924 ADDED WINDOW TO CHANGE LOAN FOLLOWUP DATE AND
      *             REASON, REGACC PR# 5.  EXPANDED POSSIBLE WINDOW
      *             SELECTIONS FROM 20 TO 24
      *  KEC 030424 {PR#00026} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *             ADDED 'TRIAD-ENTRY' AND 'TRIAD INFORMATION'.
      *  BAH 040514 ADDED POOLID DESCRIPTION FOR CENTRIX, PR# 2261
      *  BAH 050406 ADDED LONPP1 FOR THRIFT INVESTMENT HARD COPY, PR# 2507
      *  BAH 050824 ADDED CHDEFC FOR DEFICIENCY INFORMATION WINDOW,
      *             REGACC PR# 112
      *  BAH 050915 ADDED PTHMOD FOR PAID THRU MODIFICATION LENDMARK PR# 2568
      *  BAH 070313 ADDED FOLEXP FOR EXPANDED FOLLOWUP TRITON PR# 2937
      *  BAH 070715 ADDED FSBMP FOR REGACC PR# 154
      *   CS 070905 ADDED REINSTATEMENT OPTION REGACC PR#180
      *  BLF 070911 IF EXT-WI-CL-ARCHIVE-FG = "Y", MOVE "AR" TO IP-BUF
      *             & XFER SO LONPP0 CAN SET UP ARCHIVE PATHS,
      *             WORLD PR# 547
      *  BLF 070912 IF EXT-WI-CL-ARCHIVE-FG = "Y", ONLY ALLOW SPECIFIC
      *             SELECTIONS WHICH HAVE ARCHIVE LOGIC OR DON'T USE
      *             ARCHIVED FILES (LIKE BORROWER INQUIRIES, WORLD PR# 547
      *  BLF 070913 CLEAN UP "IF" STATEMENTS ON FLDSEL - JUST CHECK
      *             FLDSEL-BUF, PUT IN ALPHABETICAL ORDER
      *  BLF 070913 EXPAND # OF SELECTIONS FROM 24 TO 32, CURRENTLY
      *             COULD HAVE MAX OF 27 ITEMS IF ALL 4 BORROWER SLOTS
      *             USED & ALL FLAGS ON
      *  BLF 070918 ADD ARCH-LN-FILE PATH LOGIC TO READ INSTEAD OF LN-FILE
      *             IF EXT-WI-CL-ARCHIVE-FG = "Y", WORLD PR# 547
      *  BAH 081125 ADDED MORE VARIABLES TO REINST-BUF , REGACC PR# 228
      *  BAH 091020 ADDED HIGH CREDIT FOR WORLD PR# 659
      *  BAH 100617 ADDED "MISC FIELDS" LSMISC FOR UNITED PR# 3615
      *  KEC 110111 {PR#00693} WORLD <A15> [JIM ROSENAUER]
      *             ADDED OPTION TO ACCESS THE SPANISH LOAN APPLICATION
      *             (LA/SIPGRM) IF IT IS A SPANISH BRANCH OTHERWISE
      *             USE THE ENGLISH LOAN APPLICATION (LA/LAMENU).
      *  BAH 110201 ADDED LOAN BORROWER INFO FOR WORLD PR# 739
      *  BAH 110204 ADDED GPS INFORMATION FOR PCCREDIT PR# 3699
      *  BLM 110207 ONLY DO LOAN BORROWER INFO IF I-AM-WORLD, WORLD PR# 739
      *  BAH 110223 ADDED EXPANDED LEGAL INFO FOR UNITED ACC PR# 3707
      *  BAH 110228 ADDED SCRA(SERVICE MEMBER CIVIL RELIEF ACT) REGACC PR# 289
      *  BAH 110425 ADDED SOME GP-I-AM-REGACC TO NOT DISPLAY SOME SELECTIONS
      *             BECAUSE THE ABOVE CHANGE PUT THEM OVER THE 32 MAX ENTRIES
      *             AND LOCKED THEM UP. NEED TO EXPAND THIS WINDOW !
      *  BAH 110427 CHANGED TO ALLOW FOR 48 SELECTIONS, CHANGED LINES FROM
      *             X(25) TO X(24), ADDED 3RD COLUMN, FORM MUST GO !
      *  BLM 120224 ADD TRIAD INFO TO LIST OF VALID ARCHIVE SELECTIONS,
      *             REGACC PR# 304
      *  BAH 120404 ADDED GP-JUDGMENT-WINDOW CHJUDG FOR CITIZEN PR# 3990
      *  MJD 140604 ADDED EXT- WORKERS FOR WINDOW CONTROL
      *  MJD 140926 ADDED CLMENU_HLD.CPY.  WORKER USED TO HOLD EXT-WI-CL-WORKER
      *             SO VALUES CAN BE RESTORED AFTER CALL
      *  KEC 2016.0502 REMOVED USE OF LIBCL/CLMENUW_HLD.CPY AND NOW USING
      *                EXT-WI-CL-SIZE (NEW 78; 200 BYTES).
      *  KEC 2016.0510 CHANGED USE OF APPNO TO APP-NUMBER FOR MA1-KEY USAGE.
      * KEC 2018-0822 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      * BAH 181203 REMOVED ARCHIVE CODE, LNFILE
      * BAH 20190709 REMOVED TRIAD, PT0151
      * BAH 20220421 ADDED ADDITIONAL LOAN FIELDS SECURITIZATION #1551
      * BAH 20230504 REMOVED INTERIM COUPONS CHCOUP KM
      *                      GPS INFO CHGPS
      * BAH 20250317 PUT PRINT HARD COPY (LONPP0) BACK S35Q-180
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)CL/CLMENU S35 04/29/25-11:17:35 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

      *********************************************************
      *   LNFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LN-ARCHIVE-PATH.
      *    03  FILLER                     PIC X     VALUE "/".
      *    03  LN-ARCHIVE-PATH1.
      *        05  LN-ARCHIVE-DIR1        PIC X(3).
      *        05  FILLER                 PIC X     VALUE "/".
      *        05  LN-ARCHIVE-DIRECTORY   PIC X(4).
      *        05  FILLER                 PIC X     VALUE "/".
      *        05  LN-ARCHIVE-MACHINE     PIC XX.
      *        05  FILLER                 PIC X     VALUE "/".
      *        05  LN-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                     PIC X(10) VALUE
      *        "/LP/LNFILE".

      *-----------------------------------------------------------------
       01  FIRST-FIELD             PIC X(4)    VALUE "010.".
       01  LAST-FIELD              PIC X(4)    VALUE "240.".

       01  GET-NEXT-FG             PIC X(02) VALUE "  ".

       COPY "LIBLP/LPIPBUF.CPY".
       COPY "LIBLP/LPLOANW.CPY".

       01  SAVE-WI-CL-SSNO-NO      PIC 9.
       01  SUB                     PIC 99.
       01  SUBX                    PIC 9.

       01  FLDSEL-BUF.
           05  FLDSEL-KEY.
               07  FLDSEL-KEYN            PIC 9.
           05  FLDSEL-NAME                PIC X(22).
           05  FILLER                    REDEFINES FLDSEL-NAME.
               07  FILLER                 PIC X(19).
               07  FILLER                 PIC X.
               07  FLDSEL-MAKERCD         PIC 9.
               07  FILLER                 PIC X.

       01  FLDSEL-LIST.
           03  FILLER                     PIC X(40) VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER                     PIC X(40) VALUE
               "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER                     PIC X(40) VALUE
               "210 220 230 240 250 260 270 280 290 300 ".
           03  FILLER                     PIC X(40) VALUE
               "310 320 330 340 350 360 370 380 390 400 ".
           03  FILLER                     PIC X(40) VALUE
               "410 420 430 440 450 460 470 480.".

       01  FLDSEL-TABLE                  REDEFINES FLDSEL-LIST.
           03  FLDSEL-TAB                 OCCURS 48 TIMES.
               05  FILLER                 PIC X(3).
               05  FLDSEL-PERIOD          PIC X.

       01  FLDSEL-FIELD                   PIC X(4)   VALUE "010.".
       01  FLDSEL-STAT                    PIC XX.

       01  TEST-FLDSEL-BUF                PIC X(24).
           88  VALID-ARCHIVE-SELECTION    VALUES
               "BANKRUPT STATUS        "
               "COLLATERAL             "
               "CREDIT BUREAU          "
               "GEORGIA INFORMATION    "
               "LOAN MASTER            ".

       01  CLMENU-WINDOW-HANDLE   PIC X(10).
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 48 TIMES.
               05  D-KEY.
                   07  D-KEYN      PIC 9.
               05  D-NAME          PIC X(22).
               05  FILLER          REDEFINES D-NAME.
                   07  D-PERIOD    PIC XX.
                   07  D-BWNAME    PIC X(16).
                   07  D-FILLER    PIC X.
                   07  D-LEFT      PIC X.
                   07  D-MAKERCD   PIC X.
                   07  D-RIGHT     PIC X.
               05  FILLER          PIC X.
           03  D-PRINTER           PIC ZZ.
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  DISPLAY-LIST.
           03  FILLER              PIC X(40)  VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER              PIC X(40)  VALUE
               "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER              PIC X(40) VALUE
               "210 220 230 240 250 260 270 280 290 300 ".
           03  FILLER              PIC X(40)  VALUE
               "310 320 330 340 350 360 370 380 390 400 ".
           03  FILLER              PIC X(52)  VALUE
               "410 420 430 440 450 460 470 480 PGNO ENDMORE:SF.".

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBCL/CLMENUW_EXT.CPY".
       01  HLD-WI-CL-WORKER        PIC X(EXT-WI-CL-SIZE)   VALUE SPACES.
      *-----------------------------------------------------------------

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CLMENU_DEF.CPY".
       COPY "LIBWI/CLMENU_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBWI/LNRNEWW.CPY".
       COPY "LIBWI/LNPBRNW.CPY".
       COPY "LIBWI/SFINFOW.CPY".
       COPY "LIBWI/GAINQW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  CLMENU-SELECTION    PIC XX.

       SCREEN SECTION.
       COPY "LIBWI/CLMENU_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E    D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME CLMENU-SELECTION EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE "CLMENU" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE 03 * VDU-WIN-L-OFFSET
                   AT COL 02 * VDU-WIN-C-OFFSET
                   SIZE 78 + VDU-WIN-W-MOD
                   LINES 20 + VDU-WIN-H-MOD
                   CELL SIZE IS LABEL FONT
                   COLOR IS VDU-WCOLOR-FWIN
                   CONTROL VALUE IS VDU-WCTRL-FWIN
                   BLANK SCREEN
                   HANDLE  IS CLMENU-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".

           PERFORM GET-GPENV.
           PERFORM INITIALIZATION.

      * BACK UP WORKER AS VALUES MAY CHANGE IN CALLED PROGRAM AND WE DO NOT
      * WANT IT TO AFFECT PROGRAM YOU JUST INTERRUPED OUT OF.
           MOVE EXT-WI-CL-WORKER TO HLD-WI-CL-WORKER.

           PERFORM PROCESS-WINDOW-MENU.

      * RESTORE  UP WORKER AS VALUES MAY CHANGE IN CALLED PROGRAM AND WE DO NOT
      * WANT IT TO AFFECT PROGRAM YOU JUST INTERRUPED OUT OF.
           MOVE HLD-WI-CL-WORKER TO EXT-WI-CL-WORKER.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ****************************
       INITIALIZATION SECTION.
      ****************************

           MOVE FORM-PATHNAME TO FORM-PATH.

      *    IF EXT-WI-CL-ARCHIVE-FG = "Y"
      *       PERFORM SETUP-LN-ARCHIVE-PATH.

           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

           PERFORM SETUP-WINDOW.

       INITIALIZATION-EXIT.
           EXIT.

      *******************************
       PROCESS-WINDOW-MENU SECTION.
      *******************************
       SELECT-FIELD-FROM-DISPLAY.

           MOVE " F7, SELECT OPTION (RTN):" TO MESS.
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

           IF ( FLDSEL-STAT = "1>" )
              GO TO PROCESS-WINDOW-MENU-EXIT.

           IF NOT (FLDSEL-STAT = "00")
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF ( FLDSEL-KEY = "1" OR "2" OR "3" OR "4")
              MOVE EXT-WI-CL-SSNO-NO TO SAVE-WI-CL-SSNO-NO
              MOVE FLDSEL-KEYN TO EXT-WI-CL-SSNO-NO
              PERFORM CHBORR-SCAN
              MOVE SAVE-WI-CL-SSNO-NO TO EXT-WI-CL-SSNO-NO
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF EXT-WI-CL-ARCHIVE-FG = "Y"
              MOVE FLDSEL-BUF TO TEST-FLDSEL-BUF
              IF NOT VALID-ARCHIVE-SELECTION
                 MOVE "SELECTION NOT VALID FOR ARCHIVED LOAN" TO MESS
                 PERFORM SEND-MESS
                 GO TO SELECT-FIELD-FROM-DISPLAY.

           IF FLDSEL-BUF = "ADDITIONAL LOAN FIELDS"
              PERFORM ADDITIONAL-DISPLAY
           ELSE
              IF FLDSEL-BUF = "AUTO INSURANCE INFO"
                 PERFORM AUTOIN-SCAN
           ELSE
              IF FLDSEL-BUF = "BANKRUPT STATUS"
                 PERFORM CHBANK-SCAN
           ELSE
              IF FLDSEL-BUF = "COLLATERAL"
                 PERFORM LTSCAN-SCAN
           ELSE
              IF FLDSEL-BUF = "CREDIT BUREAU"
                 PERFORM CREDIT-BUREAU-REPORTING
           ELSE
              IF FLDSEL-BUF = "DEFICIENCY INFO"
                 PERFORM DEFICIENCY-INFO
           ELSE
              IF FLDSEL-BUF = "FOLLOW UP INFO"
                 PERFORM FOLLOWUP-ENTRY
           ELSE
              IF FLDSEL-BUF = "GEORGIA INFORMATION"
                 PERFORM GEORGIA-SCAN
                 IF GAINQ-STAT NOT = SPACES
                    MOVE GAINQ-STAT TO GET-NEXT-FG
                    GO TO END-PROGRAM
                 END-IF
           ELSE
              IF FLDSEL-BUF = "LEGAL INFORMATION"
                 PERFORM LEGAL-ENTRY
           ELSE
              IF FLDSEL-BUF = "HIGH CREDIT"
                 PERFORM CHANGE-HIGH-CREDIT
           ELSE
              IF FLDSEL-BUF = "JUDGMENT INFO"
                 PERFORM JUDGMENT-INFO
           ELSE
              IF FLDSEL-BUF = "CUSTOMER INFORMATION"
                 PERFORM CUSTOMER-INFO
           ELSE
              IF FLDSEL-BUF = "LOAN MASTER"
                 IF ( EXT-WI-CL-SALEFIN = "Y" AND
                       EXT-WI-CL-ARCHIVE-FG NOT = "Y")
                    PERFORM SFMAST-SCAN
                 ELSE
                    PERFORM LNMAST-SCAN
                 END-IF
           ELSE
              IF FLDSEL-BUF = "MISCELLANEOUS FIELDS"
                 PERFORM MISC-FIELDS
           ELSE
              IF FLDSEL-BUF = "PAID THRU MODIFICATION"
                 PERFORM PAID-THRU-MOD
           ELSE
              IF FLDSEL-BUF = "PAYMENT SCHEDULE"
                 PERFORM PMTSCH-SCAN
           ELSE
              IF FLDSEL-BUF = "PRINT HARD COPY"
                 PERFORM PRINT-HARD-COPY
           ELSE
              IF FLDSEL-BUF = "RENEWAL FOR CASH"
                 PERFORM LNPBRN-SCAN
           ELSE
              IF FLDSEL-BUF = "RENEWAL NET BALANCE"
                 PERFORM LNRNEW-SCAN
           ELSE
              IF FLDSEL-BUF = "REPOSSESSION STATUS"
                 PERFORM CHREPO-SCAN
           ELSE
              IF FLDSEL-BUF = "SALES FINANCE INFO"
                 PERFORM SFINFO-SCAN
           ELSE
              IF FLDSEL-BUF = "SCRA INFORMATION"
                 PERFORM SCRA-INFO.

           GO TO SELECT-FIELD-FROM-DISPLAY.

       PROCESS-WINDOW-MENU-EXIT.
           EXIT.

      ***************************
       ADDITIONAL-DISPLAY SECTION.
      ***************************
           MOVE "WI/ADDTL" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      ***********************
       SFINFO-SCAN SECTION.
      ***********************
           MOVE "Y"              TO SFINFO-DLPOFF-FG.
           MOVE "WI/SFINFO" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH SFINFO-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      ***********************
       LTSCAN-SCAN SECTION.
      ***********************
           MOVE "WI/LTSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      *********************
       SFMAST-SCAN SECTION.
      *********************
           MOVE "WI/SFMAST" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      **********************
       MISC-FIELDS SECTION.
      **********************
           MOVE "WI/LSMISC" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      **********************
       LNMAST-SCAN SECTION.
      **********************
           MOVE "WI/LNMAST" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      *********************
       LNRNEW-SCAN SECTION.
      *********************
           MOVE "WI/LNRNEW" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LNRNEW-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      *********************
       LNPBRN-SCAN SECTION.
      *********************
           MOVE "WI/LNPBRN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LNPBRN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      **********************
       CHBORR-SCAN SECTION.
      **********************
           MOVE "WI/CHBORR" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      *********************
       CHREPO-SCAN SECTION.
      *********************
           MOVE "WI/CHREPO" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      *************************
       PAID-THRU-MOD SECTION.
      *************************
           MOVE "WI/PTHMOD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      *************************
       CUSTOMER-INFO SECTION.
      *************************
           MOVE "WI/CIINQ" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      *************************
       DEFICIENCY-INFO SECTION.
      *************************
           MOVE "WI/CHDEFC" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      *************************
       LEGAL-ENTRY SECTION.
      *************************
           MOVE "WI/CHLEGL" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      *************************
       SCRA-INFO SECTION.
      *************************
           MOVE "WI/CHSCRA" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      *********************
       CHBANK-SCAN SECTION.
      *********************
           MOVE "WI/CHBANK" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      **********************
       GEORGIA-SCAN SECTION.
      **********************
           MOVE EXT-WI-CL-ACCTNO TO GAINQ-ACCTNO.
           MOVE "WI/GAINQY" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GAINQ-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      *******************************
       CHANGE-HIGH-CREDIT SECTION.
      *******************************
           MOVE "WI/CHCRED" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      **************************
       JUDGMENT-INFO SECTION.
      **************************
           MOVE "WI/CHJUDG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      *************************
       PRINT-HARD-COPY SECTION.
      *************************

      * WI- FIELDS ARE EXTERNAL, LONPP0 WILL GRAB THEM

           MOVE "CL" TO IP-PROG.

           MOVE "LP/LONPP0" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH IP-BUF PROG-BUF.
           CANCEL FORM-PROGX.

      *********************
       AUTOIN-SCAN SECTION.
      *********************
           MOVE "WI/AUTOIN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      *********************
       PMTSCH-SCAN SECTION.
      *********************
           MOVE "WI/PMTSCH" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      **********************************
       CREDIT-BUREAU-REPORTING SECTION.
      **********************************
           MOVE "WI/CHTRWF" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      ************************
       SET-BORR-D-KEY SECTION.
      ************************
           MOVE SUBX TO D-KEYN(SUB).
           MOVE "." TO D-PERIOD(SUB).
           MOVE LN-SSNO(SUBX) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           MOVE SPACES TO D-BWNAME(SUB)
                          D-FILLER(SUB).
           STRING BW-LNAME, ", ", BW-FNAME DELIMITED BY "   "
                  INTO D-BWNAME(SUB).
           MOVE "("  TO D-LEFT(SUB).
           MOVE EXT-WI-CL-MAKERCD(SUBX) TO D-MAKERCD(SUB).
           MOVE ")"  TO D-RIGHT(SUB).

      *************************
       FOLLOWUP-ENTRY SECTION.
      *************************
           MOVE "WI/FOLLOW" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "CL/CLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      **********************
       SETUP-WINDOW SECTION.
      **********************

           PERFORM FORM-RESET.

           IF ( CLMENU-SELECTION = "F" OR "R" )
              MOVE "  ACCOUNT OPTIONS" TO WR-BUF
              MOVE "TITLE."            TO WR-LIST
              PERFORM CALL-WRFORM.

       BUILD-PAGENO.

           MOVE SPACES TO DISPLAY-BUF.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 48
                   MOVE SPACES TO FLDSEL-PERIOD(SUB)
           END-PERFORM.

           MOVE ZEROES TO SUB.

           ADD 1              TO SUB.
           MOVE EXT-WI-CL-SSNO-NO TO SUBX.
           PERFORM SET-BORR-D-KEY.

           IF ( EXT-WI-CL-SSNO(1) NOT = ZEROES ) AND
              ( EXT-WI-CL-SSNO-NO NOT = 1      )
              ADD  1 TO SUB
              MOVE 1 TO SUBX
              PERFORM SET-BORR-D-KEY.

           IF ( EXT-WI-CL-SSNO(2) NOT = ZEROES ) AND
              ( EXT-WI-CL-SSNO-NO NOT = 2      )
              ADD  1 TO SUB
              MOVE 2 TO SUBX
              PERFORM SET-BORR-D-KEY.

           IF ( EXT-WI-CL-SSNO(3) NOT = ZEROES ) AND
              ( EXT-WI-CL-SSNO-NO NOT = 3      )
              ADD  1 TO SUB
              MOVE 3 TO SUBX
              PERFORM SET-BORR-D-KEY.

           IF ( EXT-WI-CL-SSNO(4) NOT = ZEROES ) AND
              ( EXT-WI-CL-SSNO-NO NOT = 4      )
              ADD  1 TO SUB
              MOVE 4 TO SUBX
              PERFORM SET-BORR-D-KEY.

      
           ADD  1                          TO SUB.
           MOVE "A"                        TO D-KEY(SUB).
           MOVE "DDITIONAL LOAN FIELDS"    TO D-NAME(SUB).
      
           ADD  1                          TO SUB.
           MOVE "A"                        TO D-KEY(SUB).
           MOVE "UTO INSURANCE INFO"       TO D-NAME(SUB).

           IF ( CLMENU-SELECTION NOT = "R" )
              ADD  1                       TO SUB
              MOVE "B"                     TO D-KEY(SUB)
              MOVE "ANKRUPT STATUS"        TO D-NAME(SUB).

           ADD  1                          TO SUB.
           MOVE "C"                        TO D-KEY(SUB).
           MOVE "OLLATERAL"                TO D-NAME(SUB).

           ADD  1                          TO SUB.
           MOVE "C"                        TO D-KEY(SUB).
           MOVE "REDIT BUREAU"             TO D-NAME(SUB).

           ADD  1                       TO SUB.
           MOVE "C"                     TO D-KEY(SUB).
           MOVE "USTOMER INFORMATION"   TO D-NAME(SUB).
      
           ADD  1                          TO SUB.
           MOVE "D"                        TO D-KEY(SUB).
           MOVE "EFICIENCY INFO"           TO D-NAME(SUB).

           ADD  1                       TO SUB
           MOVE "F"                     TO D-KEY(SUB)
           MOVE "OLLOW UP INFO"         TO D-NAME(SUB).

           IF ( LN-ORGST = "GA" )
              ADD 1                        TO SUB
              MOVE "G"                     TO D-KEY(SUB)
              MOVE "EORGIA INFORMATION"    TO D-NAME(SUB).

            ADD 1                        TO SUB
            MOVE "H"                     TO D-KEY(SUB)
            MOVE "IGH CREDIT"            TO D-NAME(SUB).

           IF ( GP-JUDGMENT-WINDOW = "Y" )
              ADD 1                        TO SUB
              MOVE "J"                     TO D-KEY(SUB)
              MOVE "UDGMENT INFO"          TO D-NAME(SUB).

           IF ( GP-EXPANDED-LEGAL-WINDOW = "Y" )
              ADD  1                       TO SUB
              MOVE "L"                     TO D-KEY(SUB)
              MOVE "EGAL INFORMATION"      TO D-NAME(SUB).
      
           ADD  1                          TO SUB.
           MOVE "L"                        TO D-KEY(SUB).
           MOVE "OAN MASTER"               TO D-NAME(SUB).

           ADD  1                          TO SUB.
           MOVE "M"                        TO D-KEY(SUB).
           MOVE "ISCELLANEOUS FIELDS"      TO D-NAME(SUB).
      
           IF ( GP-DEFERMENT-APPROVAL = "Y" )
              ADD 1                        TO SUB
              MOVE "P"                     TO D-KEY(SUB)
              MOVE "AID THRU MODIFICATION" TO D-NAME(SUB).
      
           IF ( EXT-WI-CL-PAY-SCHLD-FG = "Y" )
               ADD  1                      TO SUB
               MOVE "P"                    TO D-KEY(SUB)
               MOVE "AYMENT SCHEDULE"      TO D-NAME(SUB).

           ADD  1                          TO SUB.
           MOVE "P"                        TO D-KEY(SUB).
           MOVE "RINT HARD COPY"           TO D-NAME(SUB).

           IF ( CLMENU-SELECTION NOT = "R" )
              ADD  1                       TO SUB
              MOVE "R"                     TO D-KEY(SUB)
              MOVE "ENEWAL FOR CASH"       TO D-NAME(SUB).

           IF ( CLMENU-SELECTION NOT = "R" ) AND
              ( EXT-CONAME-CAN-LOANS-FG NOT = "O" )
              ADD  1                       TO SUB
              MOVE "R"                     TO D-KEY(SUB)
              MOVE "ENEWAL NET BALANCE"    TO D-NAME(SUB).

           IF ( CLMENU-SELECTION NOT = "R" )
              ADD 1                        TO SUB
              MOVE "R"                     TO D-KEY(SUB)
              MOVE "EPOSSESSION STATUS"    TO D-NAME(SUB).
      
           IF ( EXT-WI-CL-SALEFIN = "Y" )
              ADD  1                       TO SUB
              MOVE "S"                     TO D-KEY(SUB)
              MOVE "ALES FINANCE INFO"     TO D-NAME(SUB).

           IF ( GP-USE-SCRA = "Y" )
              ADD  1                       TO SUB
              MOVE "S"                     TO D-KEY(SUB)
              MOVE "CRA INFORMATION"       TO D-NAME(SUB).

           IF SUB NOT > 48
              MOVE "." TO FLDSEL-PERIOD(SUB)
              MOVE FLDSEL-TAB(SUB) TO LAST-FIELD.

       DISPLAY-PAGE.
           MOVE "* END *" TO D-END-MORE.
           MOVE 1         TO D-PAGENO.
           MOVE 0         TO D-PRINTER.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       EXIT-SETUP-WINDOW.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      ***************************************************************
       FORM-RESET SECTION.
      ***************************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CLMENU-SCREEN.
           DISPLAY CLMENU-SCREEN UPON CLMENU-WINDOW-HANDLE.
           MOVE CLMENU-FORM-FIELDS TO WR-BUF..
           MOVE CLMENU-FORM-LIST TO WR-LIST..
           PERFORM CALL-WRFORM..

       FORM-RESET-EXIT.
           EXIT.

      ***************************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************************
           MOVE CLMENU-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLMENU-HELP--DIR  TO HELP-LINK-DIR.

      ***************************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CLMENU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************************************
       MISC-ROUTINES SECTION.
      ***************************************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "MSEL:D." TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.

      ***************************************************************
       IO-ROUTINES SECTION.
      ***************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".

      ***************************************************************
       IO-ERROR SECTION.
      ***************************************************************
           MOVE "WINDOW ERROR " TO MESS.
           PERFORM SEND-MESS.

      ***************************************************************
       END-PROGRAM SECTION.
      ***************************************************************
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CLMENU-WINDOW-HANDLE.
           DESTROY CLMENU-SCREEN.

       STOP-RUN.
           EXIT PROGRAM.
