       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RVLINE.
       AUTHOR.          JTG.
       DATE-WRITTEN.    12-17-1998.
      ******************************************************************
      *
      * IDENTIFICATION:  WI/RVLINE.C
      *                  CTRL B    CONTROL B
      *
      * DESCRIPTION   :  BALANCE TRANSACTION HISTORY WINDOW
      *                  PAYMENT (LP) FILE WINDOW SCAN
      *
      *-----------------------------------------------------------------
      * REV:
      * JTG 121798 **NEW**
      * MJD 990224 ADDED STINFO WINDOW
      * JTG 990706 ADDED 'WI/AVINFO' WINDOW RE: DIRECT LOAN ADVANCES
      * JTG 990730 ADDED LOGIC TO CORRECTLY DISPLAY FOR OTHER CHARGES
      * JTG 990809 ADDED LOGIC FOR REVOLVING 'PE' PAYMENT EXCEPTION
      * JTG 991018 ADDED LOGIC FOR REVOLVING 'AP' PAYMENT EXCEPTION
      * KEC 000720 ADDED 'WI/LPINFO'.
      * MG  000724 ADDED PASSWORD LOGIC ON F5-KEY (WI/LPINFO).
      * KEC 000919 ADDED LOGIC TO SKIP THE CHECK OF THE GROUP FILE TO
      *             SEE IF TO REQUIRE THE PASSWORD.
      * MG  010709 MADE THE SCAN GO TO THE LATEST TRANSACTION
      * MG  011010 PUT A '*' IN BETWEEN THE CODE AND THE REFERENCE
      *            FOR REVERSED TRANSACTIONS.
      * MG  011012 ADDED A WINDOW FOR ADDON INFORMATION (LT).
      * MG  011017 ADDED A WINDOW FOR AMORTIZED INFO (LX-Z).
      * JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99     LENDMARK PR#J021
      * BAH 021014 ADDED NEW TRANSACTION CODE OF "IN" INSURANCE LOSS, EXACTLY
      *            LIKE "PE", REGACC PR# 0007
      *  CS 060720 ADDED TOGGLE OF LATE CHARGES/INTEREST/BOTH PR#2811 SUPPORT
      *  CS 070111 FIXED F2-TOGGLE. TOGGLE-AMOUNT-TABLE WORKERS WERE NOT RESET
      *            TO ZERO,ADDED A MOVE OF ZERO TO THE TOGGLE WORKERS IN
      *            START-BUILD-PAGENO, PR#569 REGWAM
      *  CS 070115 F2-TOGGLE WHEN CHANGING PAGES. IF TOGGLE WAS TO LATE CHARGE,
      *            NEW PAGE WAS GOING TO DEFAULT OF COMBINED LC/INT SO YOU HAD
      *            TO TOGGLE AGAIN PR#569 REGWAN
      * BLF 071004 FIXED MANY NON-NUMERICS WHEN HAPPENED TWICE EVERY TIME
      *            ACCOUNT WAS CALLED UP.  DID QUITE A BIT OF REVISION...
      *            DIDN'T KNOW WHY EVERYTHING WAS GOING TO A LN-BUF THEN
      *            GETTING MOVED TO D-LINE(SUB), PLUS ANOTHER SEPARATE
      *            DISPLAY BUFFER WITH JUST THE TOGGLE AMTS FOR LC & INT.
      *            CHANGED HEADING ON TOGGLE TO JUST SAY INT OR LC INSTEAD OF
      *            DASHES ON TOP.  CHANGED APPEAR OK BUT I'M NOT SURE IF
      *            I TESTED ALL THE OPTIONS & COULDN'T TEST REVOLVING!
      *            LN2-BUF WAS 81 BYTES & IT REDEFINED LN-BUF WHICH WAS 78
      *            BYTES; I GOT RID OF 1 BYTE OF FILLER & SHORTENED THE REF
      *            TO 3...NOT SURE WHAT ELSE TO DO....
      * BLF 071008 DO A FLDVAL FOR VALUE OF RVLINE-ARCHIVE-FG; IF "Y",
      *            PASSED ACCT.
      *            IS IN ARCHIVE DIRECTORY, SO SET UP ARCHIVE PATHS FOR LN,
      *            LP, LT, RL, RT., WORLD PR# 547
      * BLM 080811 INCREASE MAX-PAGES & PAGE-KEY TABLE FROM 50 TO 300,
      *            FLATIRON (CENTRIX), PR# 71241
      * BAH 090204 FIX LARGE APOTH ON "AP" AND ALSO ON PAYMENTS,REGACC PL# 649
      * BLM 140218 FIX NON-NUMERIC ERROR IN ACU_TCLP.  ROUTINE TO CHECK
      *            PASSWORD WAS USING A FIELD CALLED EP-BRANCH, WHICH WAS
      *            AN UNUSED FIELD, I GUESS, IT WAS 2020.  ONLY GOT TO THE
      *            SPOT CAUSING NON-NUMERICS IF USER HAD A VALID GROUP,
      *            E.G. USER = A9990SC1 & THERE IS A VALID GROUP SC1.
      *            THIS PROBLEM WAS REPORTED UNDER PR# 950 & ALSO UNDER
      *            PL# 825
      *  CS 140627 DISPLAY WAS MESSED UP WHEN CTRLB BECAUSE THE REDEFINE  
      *            DID NOT LINE UP CORRECTLY, CHANGED DISPLAY-LIST-TBL TO
      *            FILLER(PIC X4),DISPLAY-LIST-ATT X(2) AND FILLER OF 2
      * BLM 140925 REMOVE FS, FD, WS, & IO FOR PWFILE & REMOVE PWFILE FROM
      *            DECLARATIVES, NOT NEEDED.
      * BAH 160125 SPLIT OUT LTFILE, PD0003
      *  CS 160613 CHANGE LPSEQ,PAGE-LPSEQNO  TO 9(5)
      * BLM 160616 REMOVE RVLINE-CYCLE-DAY, NOT USED
      * BAH 161004 ADDED READ OF LTR FOR FIELDS REMOVED FROM LOAN
      *  CS 170109 ADDED SET OF 1 TO LTR-SEQNO IN SETUP-FOR-RVBAL2 SECTION 
      * BAH 170406 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 190103 GLOBAL LPFILE
      * BAH 20190308 CHANGED LP-ADDON TO USE LPWSLP-LP-ADDON, MOVED LP-REFNO
      *              REDEFINITIONS TO LPWSLP COPY MEMBER
      * BAH 20220803 HARD CODED START
      **************************************************************************
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
           "@(#)WI/RVLINE.CBL S35 02/22/24-09:57:49 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

      *********************************************************
      *   LNFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LN-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE "/LP/LNFILE".

      *********************************************************
      *   LPFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LP-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LP-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LP-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LP-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LP-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE "/GB/LPFILE".

      *********************************************************
      *   LTIFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LTI-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTI-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTI-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTI-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTI-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(11) VALUE "/LP/LTIFILE".

      *********************************************************
      *   RLFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  RL-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  RL-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  RL-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  RL-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  RL-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE "/LP/RLFILE".

      *********************************************************
      *   RTFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  RT-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  RT-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  RT-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  RT-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  RT-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE "/LP/RTFILE".

      *-----------------------------------------------------------------
       01  CASE-BUF                    PIC X(04).
       01  USER-GROUP                  PIC X(3).

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  DISP-REV-BAL                PIC X(01)     VALUE "Y".
       01  EOF-FG                      PIC X(01)     VALUE SPACES.
       01  ERRCD                       PIC X(01)     VALUE SPACES.
       01  LN-WK                       PIC 9(03)     COMP-3 VALUE ZEROS.
       01  HOLD-SYSDATE-YYYYMMDD       PIC 9(8).

       01  LPSEQNO-BUF.
           03  LPSEQNO-TAB             OCCURS 08 TIMES.
               05  LPSEQNO             PIC 9(05).

       01  ONE                         PIC S9(1)     VALUE 1.

       01  PAGE-WORKERS.
           03  CURRENT-PAGE            PIC S9(3)     VALUE ZEROES.
           03  MAX-PAGES               PIC 9(03)     VALUE 300.
           03  PAGE-COUNT              PIC 9(03)     VALUE ZEROES.
           03  PAGE-KEY.
               05  PAGE-LPSEQNO        OCCURS 300
                                       PIC 9(05)     VALUE ZEROES.

       01  SUBSCRIPTS-WORKERS.
           03  SUB                     PIC 9(02)     VALUE ZEROES.
           03  FSUB                    PIC 9(02)     VALUE ZEROES.
           03  CSUB                    PIC 9(2)      VALUE ZEROS.

       01  WS-MESSAGES.
           03  CHANGE-BRANCH-MESS      PIC X(56)      VALUE
              "PASSWORD NEEDED TO VIEW TRANSACTION RECORD!!!          ".

       01  FIRST-TIME-PASSWORD         PIC X(01)     VALUE "Y".

       01  REALWI-FLAG                 PIC X(01)     VALUE "Y".

       01  FLDSEL-BUF.
           05  FILLER                  PIC X(07).
           05  FLDSEL-KEY              PIC X(02).
           05  FILLER                  PIC X(69).

       01  FLDSEL-FIELD                PIC X(04)     VALUE "010.".
       01  FILLER                      REDEFINES FLDSEL-FIELD.
           03  FIELD-NO                PIC 9(02).
           03  FILLER                  PIC X(02).
       01  FILLER                      REDEFINES FLDSEL-FIELD.
           03  FLDSEL-FIELD-SUB        PIC 9(02).
           03  FILLER                  PIC X(02).

       01  FLDSEL-LIST.
           03  FILLER     PIC X(20)    VALUE "010 020 030 040 050 ".
           03  FILLER     PIC X(12)    VALUE "060 070 080.".

       01  FLDSEL-STAT                 PIC X(02)     VALUE SPACES.
           88  FLDSEL-F1-KEY                         VALUE "10".
           88  FLDSEL-F2-KEY                         VALUE "12".
           88  FLDSEL-F3-KEY                         VALUE "16".
           88  FLDSEL-F4-KEY                         VALUE "18".
           88  FLDSEL-F5-KEY                         VALUE "1:".
           88  FLDSEL-F6-KEY                         VALUE "1<".
           88  FLDSEL-F7-KEY                         VALUE "1>".
           88  FLDSEL-F8-KEY                         VALUE "1 ".
           88  FLDSEL-F9-KEY                         VALUE "1(".
           88  FLDSEL-F10-KEY                        VALUE "1)".
           88  FLDSEL-F11-KEY                        VALUE "1!".
           88  FLDSEL-F12-KEY                        VALUE "1#".
           88  FLDSEL-DOWN-KEY                       VALUE "1D".
           88  FLDSEL-ENTER-KEY                      VALUE "00".
           88  FLDSEL-HOME-KEY                       VALUE "1H".
           88  FLDSEL-LEFT-KEY                       VALUE "1L".
           88  FLDSEL-PAGE-UP-KEY                    VALUE "1P".
           88  FLDSEL-PAGE-DOWN-KEY                  VALUE "1N".
           88  FLDSEL-RIGHT-KEY                      VALUE "1R".
           88  FLDSEL-UP-KEY                         VALUE "1U".

      *-----------------------------------------------------------------
       01  FIRST-FIELD                 PIC X(04)     VALUE "010.".
       01  LAST-FIELD                  PIC X(04)     VALUE "080.".
       01  MAX-FIELDS                  PIC 9(02)     VALUE 8.

       01  DISPLAY-HEAD-BUF            PIC X(78).
       01  DISPLAY-HEAD-LIST           PIC X(12) VALUE "HEAD1 HEAD2.".

       01  DISPLAY-BUF.
           03  D-LINE                      OCCURS 8 TIMES.
               05  L-010                   PIC 9(6).
               05  L-DATE-SIGNAL           PIC X(1).
               05  L-020                   PIC X(2).
               05  L-STAR                  PIC X(1).
               05  L-030                   PIC X(5).
      * GROUP SIZE = 24
               05  L-WK.
                   07  L-040               PIC Z(5).99- BLANK ZEROES.
                   07  L-040RATE           REDEFINES L-040
                                           PIC Z(4).999-.
                   07  FILLER              REDEFINES L-040.
                       09  L-040D          PIC X(1).
                       09  L-040DLNO       PIC 9(4).99-.

                   07  L-050               PIC Z(3).99- BLANK ZEROES.
                   07  L-060               PIC Z(4).99- BLANK ZEROES.

      *- - - - - - - - - - - - - - - - - - -{ USED FOR AP TRANSACTIONS }
               05  FILLER            REDEFINES L-WK.
                   07  FILLER              PIC X(7).
                   07  L-050-Z5            PIC Z(5).99- BLANK ZEROES.
                   07  FILLER              PIC X(8).

      *- - - - - - - - { USED FOR LARGE APOTH TRANSACTIONS }
               05  FILLER                  REDEFINES L-WK.
                   07  FILLER              PIC X(9).
                   07  L-050-SH            PIC Z(4).99- BLANK ZEROES.
                   07  L-060-SH            PIC Z(3).99- BLANK ZEROES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  L-070                   PIC Z(5).99- BLANK ZEROES.
               05 L-080-MO             PIC 9(2).
               05 L-080-DAYR           PIC 9(2).
               05  L-090                   PIC Z(5).99- BLANK ZEROES.
               05  L-100                   PIC Z(4).99- BLANK ZEROES.
               05  L-110                   PIC Z(5)9.99.
               05  FILLER                  REDEFINES L-110.
                   07  FILLER              PIC X(8).
                   07  L-120               PIC X(1).

      *-----------------------------------------------------------------
      *    DISPLAY BUFFER USED IF PRINCIPAL IS OVER 99,999.99
      *    LEFT OUT PAID THRU, DOESNT MATTER ON A PAYOFF
      *-----------------------------------------------------------------
           03  LN2-BUF                 REDEFINES D-LINE OCCURS 8.
               05  L2-010                  PIC 9(06).
               05  L2-DATE-SIGNAL          PIC X(01).
               05  L2-020                  PIC X(02).
               05  L2-STAR                 PIC X(01).
               05  L2-030                  PIC X(03).
               05  L2-040                  PIC Z(07).99- BLANK ZEROES.
               05  L2-040RATE             REDEFINES L2-040
                                       PIC Z(06).999-.
               05  FILLER                 REDEFINES L2-040.
                   07  FILLER              PIC X(01).
                   07  L2-040D             PIC X(01).
                   07  FILLER              PIC X(01).
                   07  L2-040DLNO          PIC 9(04).99-.
               05  L2-050                  PIC Z(04).99- BLANK ZEROES.
               05  L2-060                  PIC Z(05).99- BLANK ZEROES.
               05  L2-070                  PIC Z(06).99- BLANK ZEROES.
               05  L2-090                  PIC Z(05).99- BLANK ZEROES.
               05  L2-100                  PIC Z(04).99- BLANK ZEROES.
               05  L2-110                  PIC Z(05)9.99.
               05  L2-120                  PIC X(01).

           03  D-END-MORE              PIC X(07).
           03  D-PAGENO                PIC ZZ9.

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  DISPLAY-LIST-TABLE.
               05  FILLER     PIC X(8)     VALUE "010    ".
               05  FILLER     PIC X(8)     VALUE "020    ".
               05  FILLER     PIC X(8)     VALUE "030    ".
               05  FILLER     PIC X(8)     VALUE "040    ".
               05  FILLER     PIC X(8)     VALUE "050    ".
               05  FILLER     PIC X(8)     VALUE "060    ".
               05  FILLER     PIC X(8)     VALUE "070    ".
               05  FILLER     PIC X(8)     VALUE "080    ".
           03  FILLER                   REDEFINES DISPLAY-LIST-TABLE.
               05  DISPLAY-LIST-TBL     OCCURS 8 TIMES.
                   07  FILLER           PIC X(4).
                   07  DISPLAY-LIST-ATT PIC X(2).
                   07  FILLER           PIC X(2).

           03  FILLER         PIC X(15)     VALUE "ENDMORE:F PGNO.".

       01  TOGGLE-DATE                        PIC X(01)    VALUE "P".
           88  TOGGLE-DATE-PAYDATE                         VALUE "P".
           88  TOGGLE-DATE-TRANS-DATE                      VALUE "T".

       01  TOGGLE-LC-INT-AMT-BUFFER           VALUE ZEROES.
           03  TOGGLE-AMOUNT-TABLE            OCCURS 8.
               05  TOGGLE-LC-INT              PIC 9(06)V99.
               05  TOGGLE-LC                  PIC 9(06)V99.
               05  TOGGLE-INT                 PIC 9(6)V99.
               05  TOGGLE-LC-INT-OWING        PIC 9(6)V99.
               05  TOGGLE-LC-OWING            PIC 9(6)V99.
               05  TOGGLE-INT-OWING           PIC 9(6)V99.

       01  TOGGLE-LATE-INTEREST               PIC 9(01)    VALUE 1.
           88  TOGGLE-BOTH                                 VALUE 1.
           88  TOGGLE-LATE-CHARGE                          VALUE 2.
           88  TOGGLE-INTEREST                             VALUE 3.


       01  TOGGLE-LATE-INT-FIRST-TIME         PIC X(01)    VALUE "Y".
           88  TOGGLE-LATE-INT-FIRST-TIME-N                VALUE "N".
           88  TOGGLE-LATE-INT-FIRST-TIME-Y                VALUE "Y".

       01  TOGGLE-LC-INT-COUNT                PIC 9        VALUE 1.

       01  DISPLAY-HEAD.
           03  FILLER                  PIC X(32) VALUE
           " DATE  CD REF    AMOUNT  OTHER  ".
           03  H-INT-LC                PIC X(6)  VALUE
           "INT/LC".
           03  FILLER                  PIC X(24) VALUE
           "     PRIN PDTH   OTHER  ".
           03  H-INT-LC-BAL            PIC X(6)  VALUE
           "INT/LC".
           03  FILLER                  PIC X(10) VALUE
           "      PRIN".

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBCL/CLMENUW_EXT.CPY".

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/FLDSELW.CPY".

       COPY "LIBWI/RVLINEW.CPY".
       COPY "LIBWI/LPINFOW.CPY".

       01  RVLINE-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/RVLINE_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/RVLINE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  CLMENU-SELECTION            PIC X(02).
       01  RVLINE-CTRL-B-BUF           PIC X(50).

       SCREEN SECTION.
       COPY "LIBWI/RVLINE_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME CLMENU-SELECTION RVLINE-CTRL-B-BUF
                  EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE RVLINE-CTRL-B-BUF TO RVLINE-BUF.
           MOVE "RVLINE" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT SCREEN LINE 10 * VDU-WIN-L-OFFSET
                      AT SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE 80 + VDU-WIN-W-MOD
                      LINES 13 + VDU-WIN-H-MOD
                      BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                      HANDLE IS RVLINE-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           DISPLAY RVLINE-SCREEN.
           MOVE RVLINE-FORM-FIELDS TO WR-BUF.
           MOVE RVLINE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACES           TO VDU-WIMENU-SELECTION.
           MOVE CLMENU-SELECTION TO VDU-CLMENU-SELECTION.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE-YYYYMMDD.

      * TEST ENVIRONMENT VARIABLE "RVLINES" TO DETERMINE WHETHER
      * OR NOT TO DISPLAY BALANCES ON REVERSALS.
           MOVE "RVLINES" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF ( STAT-GOOD )
              MOVE "N" TO DISP-REV-BAL
           ELSE
              MOVE "Y" TO DISP-REV-BAL.


           PERFORM PRIMARY-DRIVER.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       PRIMARY-DRIVER SECTION.
      ******************************************************************

      * PASSED FROM CALLING PROGRAM
      *    IF RVLINE-ARCHIVE-FG = "Y"
      *       PERFORM SETUP-ARCHIVE-PATHS
      *       IF IO-BAD
      *          GO TO END-PROGRAM.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1           TO PAGE-COUNT ONE.
           MOVE ZEROES      TO CURRENT-PAGE.

           PERFORM OPEN-LP1-FILE.
           PERFORM LOAD-LAST-PAGE.

       START-BUILD-PAGENO.
           PERFORM VARYING CSUB FROM 1 BY 1 UNTIL CSUB > 8
              MOVE ZEROS TO TOGGLE-LC-INT(CSUB)
                   TOGGLE-LC(CSUB)
                   TOGGLE-INT(CSUB)
                   TOGGLE-LC-INT-OWING(CSUB)
                   TOGGLE-LC-OWING(CSUB)
                   TOGGLE-INT-OWING(CSUB)
           END-PERFORM.

           ADD ONE TO CURRENT-PAGE.

           IF ( CURRENT-PAGE < 1 )
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           IF ( CURRENT-PAGE > PAGE-COUNT )
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           MOVE SPACES TO DISPLAY-BUF
                          FLDSEL-BUF.
           MOVE DISPLAY-HEAD TO DISPLAY-HEAD-BUF.

           PERFORM VARYING FSUB FROM 1 BY 1 UNTIL FSUB > 8
                   MOVE ZEROES TO LPSEQNO-TAB(FSUB)
           END-PERFORM.

           MOVE SPACES TO EOF-FG.
           MOVE 1      TO FSUB.

      *- - - - - - - - - - - - - - - - - - - - - - -{ START 'LP1' KEY  }

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE RVLINE-ACCTNO  TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE PAGE-LPSEQNO(CURRENT-PAGE) TO LP-SEQNO
                                              QLP1-WBEG-SEQNO.
           MOVE ALL "9"       TO QLP1-WEND-SEQNO.

           PERFORM START-LP1-FILE.

       BUILD-PAGENO.

           PERFORM READ-LP1-FILE-NEXT.
           IF ( IO-BAD ) OR
              ( LP-BRNO   NOT = RVLINE-BRNO ) OR
              ( LP-ACCTNO NOT = RVLINE-ACCTNO )
              MOVE "Y" TO EOF-FG
              GO TO DISPLAY-PAGE.

           IF ( FSUB > MAX-FIELDS )
              GO TO DISPLAY-PAGE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FSUB     TO LN-WK.
           PERFORM SETUP-LINE.
           MOVE LN-WK    TO SUB.

           MOVE LP-SEQNO TO LPSEQNO(FSUB).

           IF ( LP-CURBAL < ZEROES ) AND
              ( DISP-REV-BAL = "Y" )
              MOVE ":R"  TO DISPLAY-LIST-ATT(LN-WK)
           ELSE
              MOVE " "   TO DISPLAY-LIST-ATT(LN-WK).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ADD 1 TO FSUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.

           IF ( PAGE-COUNT = 1 )
              IF ( FSUB = 1 )
                 MOVE DISPLAY-HEAD-BUF TO WR-BUF
                 MOVE DISPLAY-HEAD-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 MOVE "NO PAYMENT RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 GO TO PRIMARY-DRIVER-EXIT.

           MOVE "MORE..." TO D-END-MORE.

           IF ( CURRENT-PAGE = PAGE-COUNT )
              IF ( EOF-FG = SPACES )
                 IF ( PAGE-COUNT < MAX-PAGES )
                    ADD 1 TO PAGE-COUNT
                    MOVE LP-SEQNO TO PAGE-LPSEQNO(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.

           MOVE CURRENT-PAGE TO D-PAGENO.
           PERFORM DISPLAY-ALL-LINES.

       SELECT-FIELD-FROM-DISPLAY.

           PERFORM GET-SP-RECORD
           IF ( SP-CAL-RATETYPE(1) = "Z" )
              MOVE " F7-EXIT, F4-MORTGAGE PY, F5-TRANSACTION:" TO MESS
           ELSE
              MOVE
            " F7-EXIT, F4-ADDON, F5-TRANSACTION F2-TOGGLE LC/INT/BOTH"
                                                         TO MESS.

           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ TEST: F2-KEY }
           IF ( FLDSEL-F2-KEY)
               ADD 1 TO TOGGLE-LC-INT-COUNT
               PERFORM TOGGLE-ON-LC-INT
               GO TO SELECT-FIELD-FROM-DISPLAY.


      *- - - - - - - - - - - - - - - - - - - - - - - - -{ TEST: F3-KEY }
           IF ( FLDSEL-F3-KEY )
              GO TO SELECT-FIELD-FROM-DISPLAY.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ TEST: F4-KEY }
           IF ( FLDSEL-F4-KEY )
              PERFORM SETUP-FOR-REALWI
              IF ( REALWI-FLAG = "Y" )
                 GO TO SELECT-FIELD-FROM-DISPLAY.

           IF ( FLDSEL-F4-KEY )
              PERFORM SETUP-FOR-LTINFO
              GO TO SELECT-FIELD-FROM-DISPLAY.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ TEST: F5-KEY }
           IF ( FLDSEL-F5-KEY )
              IF ( FIRST-TIME-PASSWORD NOT = "Y" )
                 PERFORM SETUP-FOR-LPINFO
              ELSE
                 MOVE "OK" TO PASSWDW-STAT
                 PERFORM CHECK-FOR-PASSWORD
                 IF ( PASSWDW-STAT NOT = "OK" )
                    GO TO SELECT-FIELD-FROM-DISPLAY
                 ELSE
                    MOVE "N" TO FIRST-TIME-PASSWORD
                    PERFORM SETUP-FOR-LPINFO
                    GO TO SELECT-FIELD-FROM-DISPLAY.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ TEST: F7-KEY }
           IF ( FLDSEL-F7-KEY )
              GO TO PRIMARY-DRIVER-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ TEST: UP-KEY }
           IF ( FLDSEL-UP-KEY )
              IF ( CURRENT-PAGE = 1 )
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      *- - - - - - - - - - - - - - - - - - - - - - - -{ TEST: PAGE-UP  }
           IF ( FLDSEL-PAGE-UP-KEY )
              IF ( CURRENT-PAGE = 1 )
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      *- - - - - - - - - - - - - - - - - -{ TEST: DOWN-KEY & PAGE-DOWN }
           IF ( FLDSEL-DOWN-KEY      ) OR
              ( FLDSEL-PAGE-DOWN-KEY )
              IF ( CURRENT-PAGE NOT < PAGE-COUNT ) OR
                 ( EOF-FG = "Y"                  )
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      *- - - - - - - - - - - - - - - - - - - - - - - -{ TEST: HOME-KEY }
           IF ( FLDSEL-HOME-KEY )
              MOVE ZEROES      TO CURRENT-PAGE
              MOVE 1           TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

      *- - - - - - - - - - - - - - - - - -{ INVALID SELECTION; RETURN  }
           PERFORM ALARM.
           GO TO SELECT-FIELD-FROM-DISPLAY.

       PRIMARY-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       CHECK-FOR-PASSWORD SECTION.
      ******************************************************************

      * CHECKING TO SEE IF LOGIN ID IS A GROUP MEMBER

           MOVE EXT-CONAME-USER-GROUP TO CASE-BUF.
           CALL "C$TOUPPER" USING CASE-BUF, VALUE 4.
           MOVE CASE-BUF TO USER-GROUP WGR-WORD.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > 0 )
              GO TO PASSWORD-REQUIRED-BR.

      * CHECKING TO SEE IF BRANCH ENTERED IS WITHIN THE MEMBER.

           PERFORM OPEN-GR1-FILE.
           MOVE USER-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.
           PERFORM OPEN-LN1-FILE.
           MOVE RVLINE-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-GOOD
              MOVE LN-OWNBR TO WGR-SRESULT
           ELSE
              MOVE 0 TO WGR-SRESULT.
           PERFORM SEARCH-GR-VERIFY.

           IF ( WGR-ERR > 0 )
              GO TO PASSWORD-REQUIRED-BR.

           GO TO EXIT-CHECK-FOR-PASSWORD.

      * LOGIN ID OR BRANCH ENTERED IS NOT VALID,
      * MUST ENTER PASSWORD TO MAKE CHANGE TO LIMIT.

       PASSWORD-REQUIRED-BR.

           PERFORM VERIFY-PASSWORD.

           IF ( PASSWDW-STAT NOT = "OK" )
              MOVE CHANGE-BRANCH-MESS TO MESS
              PERFORM SEND-MESS.

       EXIT-CHECK-FOR-PASSWORD.
           EXIT.

      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "RVLINE"                     TO PASSWDW-PGM.
           MOVE 1                            TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO VIEW THIS BRANCH:"       TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD"                  TO FORM-NAM.

           CALL "PASSWD" USING FORM-PATH PASSWDW-BUF.
           CANCEL "PASSWD".

       EXIT-VERIFY-PASSWORD.
           EXIT.

      ******************************************************************
       SETUP-LINE SECTION.
      ******************************************************************
           MOVE SPACES TO D-LINE(FSUB).

           IF LP-APCUR > 99999.99 OR LP-APCUR < -99999.99
              PERFORM SETUP-LINE2
              GO TO SETUP-LINE-EXIT.

           IF ( TOGGLE-DATE-PAYDATE )
              MOVE LP-PAYDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO L-010(FSUB)
           ELSE
              MOVE LP-TRDATE   TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO L-010(FSUB).

           IF LP-PAYDATE NOT = LP-TRDATE
              IF ( TOGGLE-DATE-PAYDATE )
                 MOVE "."          TO L-DATE-SIGNAL(FSUB)
              ELSE
      * LOWER T
                 MOVE X'74' TO L-DATE-SIGNAL(FSUB).

           MOVE LP-TRCD  TO L-020(FSUB).
           IF ( LP-REV = "Y" )
              MOVE "*"   TO L-STAR(FSUB).

           MOVE LP-REFNO TO L-030(FSUB) LPWSLP-LP-REFNO.

           IF LP-TRCD = "AP"
              MOVE LP-APOTH TO L-050-Z5(FSUB)
           ELSE
              IF LP-TRCD = "JD" OR "AC" OR "IR"
                 MOVE LP-NEWSTAT-RATE TO L-040RATE(FSUB)
              ELSE
                 IF (LP-TRCD = "P4" OR "P5")
                          AND (LP-DLNO NOT= 0)
                    MOVE "D" TO L-040D(FSUB)
                    MOVE LP-DLNO TO L-040DLNO(FSUB)
                 ELSE
                    MOVE LP-TRAMT TO L-040(FSUB)
                    IF ((LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" 
                                                        OR "ST") OR
                           (LP-TRCD = "RV"
                                   AND (LPWSLP-LP-ADDON-TEXT = "ADD"
                                                            OR "ADJ"
                                                            OR "CNL")
                            )
                       )
                       IF LP-IBPC = "P"
                          MOVE LP-APCUR TO L-040(FSUB)
                       END-IF
                    END-IF
                 END-IF
              END-IF
              IF (  (LP-TRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
                    OR ( (LP-TRCD = "RV") AND
                          (LP-REVTRCD = "PB" OR "RN" OR "SC"
                                               OR "RB" OR "RO")
                       )
                 )  AND (LP-APOT2 NOT = 0)
                 ADD LP-APOTH LP-APOT2 GIVING L-050(FSUB)
                 INSPECT L-050(FSUB) REPLACING TRAILING " " BY "*"
              ELSE
                 MOVE LP-APOTH TO L-050(FSUB).

           ADD  LP-APLC LP-APINT   GIVING TOGGLE-LC-INT(FSUB).
           MOVE LP-APLC            TO TOGGLE-LC(FSUB).
           MOVE LP-APINT           TO TOGGLE-INT(FSUB).
           ADD  LP-LCBAL LP-INTBAL GIVING TOGGLE-LC-INT-OWING(FSUB).
           MOVE LP-LCBAL           TO TOGGLE-LC-OWING(FSUB).
           MOVE LP-INTBAL          TO TOGGLE-INT-OWING(FSUB).

           IF TOGGLE-BOTH
              MOVE TOGGLE-LC-INT(FSUB)       TO L-060(FSUB)
              MOVE TOGGLE-LC-INT-OWING(FSUB) TO L-100(FSUB)
           ELSE
           IF TOGGLE-LATE-CHARGE
              MOVE TOGGLE-LC(FSUB)          TO L-060(FSUB)
              MOVE TOGGLE-LC-OWING(FSUB)    TO L-100(FSUB)
           ELSE
              MOVE TOGGLE-INT(FSUB)         TO L-060(FSUB)
              MOVE TOGGLE-INT-OWING(FSUB)   TO L-100(FSUB).

           MOVE LP-APCUR TO L-070(FSUB).

           IF LP-TRCD = "AP"
              MOVE LP-APINTOWE TO L-050-Z5(FSUB)
              ADD LP-LCPARTIALS LP-INTDUE GIVING L-060(FSUB)
              COMPUTE L-070(FSUB) = 0 - LP-APINTOWE
                                      - LP-INTDUE
                                      - LP-LCPARTIALS
           ELSE
              IF ( (LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST")
                       OR (LP-TRCD = "RV"
                             AND (LPWSLP-LP-ADDON-TEXT = "ADD"
                                                      OR "ADJ"
                                                      OR "CNL")
                          )
                 )
                 IF LP-IBPC = "P"
                    MOVE LP-TRAMT TO L-070(FSUB).

      * REGACC PL# 649  LARGE APOTH GETTING TRUNCATED
           IF LP-TRCD NOT = "AP"
              IF (LP-APOTH > 999.99 OR < -999.99)
                 IF  (LP-APINT + LP-APLC) < 1000.00
                    MOVE LP-APOTH TO L-050-SH(FSUB)
                    ADD LP-APLC LP-APINT GIVING L-060-SH(FSUB).

      * 5/9/97 BAH ALWAYS DISPLAY BALANCE, #487
      * 8/11/98 SUPPRESS BALANCE IF ENV 'RVLINES' IS SET(MJD AS PER JTG)
           IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
               NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0) AND
                   DISP-REV-BAL = "N"
              MOVE "*" TO L-120(FSUB)
           ELSE
              MOVE LP-PDTH-DATE TO DATE-YYYYMMDD
              MOVE DATE-YYYYMMDD-MM TO L-080-MO(FSUB)
              IF RVLINE-UNITPER-CD = "D" OR "S" OR "W" OR "B"
                 MOVE DATE-YYYYMMDD-DD TO L-080-DAYR(FSUB)
              ELSE
                 MOVE DATE-YYYYMMDD-YY TO L-080-DAYR(FSUB)
              END-IF
              COMPUTE L-090(FSUB) = LP-OTHBAL + LP-OT2BAL
              MOVE LP-CURBAL TO L-110(FSUB).

       SETUP-LINE-EXIT.
           EXIT.

      ******************************************************************
       SETUP-LINE2 SECTION.
      ******************************************************************

           IF ( TOGGLE-DATE-PAYDATE )
              MOVE LP-PAYDATE  TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO L2-010(FSUB)
           ELSE
              MOVE LP-TRDATE   TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO L2-010(FSUB).

           IF LP-PAYDATE NOT = LP-TRDATE
              IF ( TOGGLE-DATE-PAYDATE )
                 MOVE "."          TO L2-DATE-SIGNAL(FSUB)
              ELSE
      * LOWER T
                 MOVE X'74' TO L2-DATE-SIGNAL(FSUB).

           MOVE LP-TRCD  TO L2-020(FSUB).
           IF ( LP-REV = "Y" )
              MOVE "*"   TO L2-STAR(FSUB).

           MOVE LP-REFNO TO L2-030(FSUB) LPWSLP-LP-REFNO.

           IF ( LP-TRCD = "JD" OR "AC" OR "IR" )
              MOVE LP-NEWSTAT-RATE TO L2-040RATE(FSUB)
           ELSE
              IF (LP-TRCD = "P4" OR "P5")
                       AND (LP-DLNO NOT = 0)
                 MOVE "D" TO L2-040D(FSUB)
                 MOVE LP-DLNO TO L2-040DLNO(FSUB)
              ELSE
                 MOVE LP-TRAMT TO L2-040(FSUB)
                 IF ( (LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST")
                          OR (LP-TRCD = "RV"
                                AND (LPWSLP-LP-ADDON-TEXT = "ADD"
                                                         OR "ADJ"
                                                         OR "CNL")
                             )
                    )
                    IF LP-IBPC = "P"
                       MOVE LP-APCUR TO L2-040(FSUB).

           IF (  (LP-TRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
                 OR ( (LP-TRCD = "RV") AND
                       (LP-REVTRCD = "PB" OR "RN" OR "SC"
                                            OR "RB" OR "RO")
                    )
              )  AND (LP-APOT2 NOT = 0)
              ADD LP-APOTH LP-APOT2 GIVING L2-050(FSUB)
              INSPECT L2-050(FSUB) REPLACING TRAILING " " BY "*"
           ELSE
              MOVE LP-APOTH TO L2-050(FSUB).

           ADD LP-APINT LP-APLC GIVING L2-060(FSUB).
           MOVE LP-APCUR TO L2-070(FSUB).

           IF ( LP-TRCD = "AP" )
              MOVE LP-APINTOWE TO L2-050(FSUB)
              ADD LP-LCPARTIALS LP-INTDUE GIVING L2-060(FSUB)
              COMPUTE L2-070(FSUB) = 0 - LP-APINTOWE
                                       - LP-INTDUE
                                       - LP-LCPARTIALS
           ELSE
              IF ( (LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST")
                       OR (LP-TRCD = "RV"
                             AND (LPWSLP-LP-ADDON-TEXT = "ADD"
                                                      OR "ADJ"
                                                      OR "CNL")
                          )
                 )
                 IF LP-IBPC = "P"
                    MOVE LP-TRAMT TO L2-070(FSUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * 6/19/97 MJD ALWAYS DISPLAY BALANCE
      * 8/11/98 SUPPRESS BALANCE IF ENV 'RVLINES' IS SET(MJD AS PER JTG)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
               NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0) AND
                   DISP-REV-BAL = "N"
              MOVE "*" TO L2-120(FSUB)
           ELSE
              COMPUTE L2-090(FSUB) = LP-OTHBAL + LP-OT2BAL
              ADD LP-INTBAL LP-LCBAL GIVING L2-100(FSUB)
              MOVE LP-CURBAL TO L2-110(FSUB)
              IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
                             NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0)
                 MOVE "*" TO L2-120(FSUB).

       SETUP-LINE2-EXIT.
           EXIT.

      ******************************************************************
       TOGGLE-ON-LC-INT SECTION.
      ******************************************************************

           IF TOGGLE-LC-INT-COUNT > 3
              MOVE 1 TO TOGGLE-LC-INT-COUNT.

           MOVE TOGGLE-LC-INT-COUNT TO TOGGLE-LATE-INTEREST.

           MOVE ZEROES TO SUB.

       TOGGLE-ON-LC-INT-NEXT.

           ADD 1 TO SUB.

           IF ( SUB > 8 )
              GO TO TOGGLE-ON-LC-INT-END.

           IF TOGGLE-LC-INT(SUB) NOT NUMERIC
              GO TO TOGGLE-ON-LC-INT-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TOGGLE-BOTH
              MOVE TOGGLE-LC-INT(SUB)       TO L-060(SUB)
              MOVE TOGGLE-LC-INT-OWING(SUB) TO L-100(SUB)
           ELSE
           IF TOGGLE-LATE-CHARGE
              MOVE TOGGLE-LC(SUB)          TO L-060(SUB)
              MOVE TOGGLE-LC-OWING(SUB)    TO L-100(SUB)
           ELSE
              MOVE TOGGLE-INT(SUB)         TO L-060(SUB)
              MOVE TOGGLE-INT-OWING(SUB)   TO L-100(SUB).

           GO TO TOGGLE-ON-LC-INT-NEXT.

       TOGGLE-ON-LC-INT-END.

           IF TOGGLE-BOTH
              MOVE "INT+LC" TO H-INT-LC H-INT-LC-BAL
           ELSE
           IF TOGGLE-LATE-CHARGE
              MOVE "    LC" TO H-INT-LC H-INT-LC-BAL
           ELSE
              MOVE "   INT" TO H-INT-LC H-INT-LC-BAL.

           MOVE DISPLAY-HEAD TO DISPLAY-HEAD-BUF.

           PERFORM DISPLAY-ALL-LINES.

       TOGGLE-ON-LC-INT-EXIT.
           EXIT.


      ******************************************************************
       SETUP-FOR-LPINFO SECTION.
      ******************************************************************

           MOVE RVLINE-BRNO       TO LP-BRNO   LPINFO-LP-BRNO.
           MOVE RVLINE-ACCTNO     TO LP-ACCTNO LPINFO-LP-ACCTNO.
           MOVE LPSEQNO(FIELD-NO) TO LP-SEQNO  LPINFO-LP-SEQNO.
           MOVE RVLINE-ARCHIVE-FG TO LPINFO-ARCHIVE-FG.
           MOVE "WI/LPINFO " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LPINFO-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       SETUP-FOR-LPINFO-EXIT.
           EXIT.

      ******************************************************************
       SETUP-FOR-LTINFO SECTION.
      ******************************************************************

           MOVE RVLINE-BRNO       TO LP-BRNO.
           MOVE RVLINE-ACCTNO     TO LP-ACCTNO.
           MOVE LPSEQNO(FIELD-NO) TO LP-SEQNO.
           PERFORM READ-LP1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE LP-BRNO           TO LTI-BRNO.
           MOVE LP-ACCTNO         TO LTI-ACCTNO.
           MOVE LP-LT-SEQNO       TO LTI-SEQNO.
           PERFORM OPEN-LTI1-FILE.
           PERFORM READ-LTI1-FILE.
           IF ( IO-BAD )
              GO TO SETUP-FOR-LTINFO-END.

           MOVE "WI/LTINFO"       TO FORM-NAM.
           CALL "LTINFO" USING FORM-PATH LTI-BRNO LTI-ACCTNO LTI-SEQNO
                                         RVLINE-ARCHIVE-FG EXIT-PATHNAME.
           CANCEL "LTINFO".

       SETUP-FOR-LTINFO-END.
           PERFORM CLOSE-LTI1-FILE.

       SETUP-FOR-LTINFO-EXIT.
           EXIT.

      ******************************************************************
       SETUP-FOR-REALWI SECTION.
      ******************************************************************

           MOVE RVLINE-BRNO       TO LP-BRNO.
           MOVE RVLINE-ACCTNO     TO LP-ACCTNO.
           MOVE LPSEQNO(FIELD-NO) TO LP-SEQNO.
           PERFORM READ-LP1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE LP-BRNO           TO LN-OWNBR.
           MOVE LP-ACCTNO         TO LN-ACCTNO.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE LN-ORGST          TO SP-ORGST.
           MOVE LN-SPRCLASS       TO SP-SPRCLASS.
           MOVE LN-SUBCLASS       TO SP-SUBCLASS.
           MOVE LN-LAWCODE        TO SP-LAWCODE.

           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           IF ( SP-CAL-RATETYPE(1) = "Z"  ) AND
              ( LP-TRCD            = "PY" OR "RV" )
              MOVE "WI/REALWI"       TO FORM-NAM
              CALL "REALWI" USING FORM-PATH LP-BRNO LP-ACCTNO LP-SEQNO
                                  EXIT-PATHNAME
              CANCEL "REALWI"
           ELSE
              MOVE "N"               TO REALWI-FLAG.

       SETUP-FOR-REALWI-EXIT.
           EXIT.

      ******************************************************************
       LOAD-LAST-PAGE SECTION.
      ******************************************************************

           ADD ONE     TO CURRENT-PAGE.
           MOVE 1      TO SUB.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE RVLINE-ACCTNO TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE PAGE-LPSEQNO(CURRENT-PAGE) TO LP-SEQNO
                                              QLP1-WBEG-SEQNO.
           MOVE ALL "9"       TO QLP1-WEND-SEQNO.
           PERFORM START-LP1-FILE.

       LOAD-LAST-PAGE-LOOP.

           PERFORM READ-LP1-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR
              ( RVLINE-ACCTNO NOT = LP-ACCTNO ) OR
              ( RVLINE-BRNO NOT = LP-BRNO)
              GO TO LOAD-LAST-PAGE-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHECK FOR MAX NUMBER OF LINES REACHED ON PAGE; START NEW PAGE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SUB > MAX-FIELDS )
              IF ( PAGE-COUNT < MAX-PAGES )
                 MOVE 1 TO SUB
                 ADD  1 TO CURRENT-PAGE
                 ADD  1 TO PAGE-COUNT
                 MOVE LP-SEQNO TO PAGE-LPSEQNO(PAGE-COUNT)
              ELSE
                 GO TO LOAD-LAST-PAGE-END.

      *- - - - - - - - - - - - -{ POINT TO NEXT LINE; GO GET NEXT REC. }
           ADD 1 TO SUB
           GO TO LOAD-LAST-PAGE-LOOP.

       LOAD-LAST-PAGE-END.

           COMPUTE FLDSEL-FIELD-SUB = SUB - 1.
           MOVE 0 TO SUB.
           MOVE 0 TO ONE.

       LOAD-LAST-PAGE-EXIT.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

       COPY "LIBGB/FLDSEL.CPY".

      ********************************************************
       FORM-RESET SECTION.
      ********************************************************
           DISPLAY RVLINE-SCREEN UPON RVLINE-WINDOW-HANDLE.
           MOVE RVLINE-FORM-FIELDS TO WR-BUF.
           MOVE RVLINE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      ********************************************************
       SETUP-LINK-INFO SECTION.
      ********************************************************
            MOVE RVLINE-HELP--FILE TO HELP-LINK-FILE
            MOVE RVLINE-HELP--DIR  TO HELP-LINK-DIR.

      ********************************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/RVLINE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
      *-----------------------------------------------------------------
       DISPLAY-ALL-LINES.
           MOVE DISPLAY-HEAD-BUF TO WR-BUF.
           MOVE DISPLAY-HEAD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       GET-LN.

           IF ( RVLINE-ACCTNO NOT = LN-ACCTNO )
              MOVE RVLINE-ACCTNO TO LN-ACCTNO
              PERFORM OPEN-LN1-FILE
              PERFORM READ-LN1-FILE
              PERFORM CLOSE-LN1-FILE.

      *-----------------------------------------------------------------
       GET-SP-RECORD.
           MOVE RVLINE-ACCTNO     TO LN-ACCTNO.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE LN-ORGST          TO SP-ORGST.
           MOVE LN-SPRCLASS       TO SP-SPRCLASS.
           MOVE LN-SUBCLASS       TO SP-SUBCLASS.
           MOVE LN-LAWCODE        TO SP-LAWCODE.

           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

      *-----------------------------------------------------------------
       SEND-LEGEND.

           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS             TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY RVLINE-SCREEN.
           DESTROY RVLINE-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
