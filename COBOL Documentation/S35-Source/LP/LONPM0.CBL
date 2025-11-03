       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPM0.
       DATE-WRITTEN.    012384.
      ******************************************************************
      *                 (LP)
      *  DESCRIPTION:   LOAN PROCESSING FORM PRINT DRIVER
      *                 #7 DOCUMENT PRINT
      *
      *  NOTE: F3-ALL WILL BUILD ALL LASER FORMS (IF PRODUCT SOLD) ALONG
      *              WITH A MANIFEST IN FTP/OUT/DOCS DIRECTORY
      *        B2-ALLOW-F3-ALL-DOCS MUST BE "Y"
      *        FD-TYPE-OF-DOC MUST HAVE ANY VALUE IN IT TO BE INCLUDED
      *        SET ENVIRONMENT "PST_OUTPUT" TO "A", THIS SIGNIFIES TO PST
      *              PROGRAMS TO WRITE THE MANIFEST
      *
      *  REV:
      *  KEC 112596 CHANGED LN-POCD FOR RESCISSION.
      *  KEC 120396 ADDED CHECK FOR IF PRINTING 'CHECKS' WHILE
      *             LOAN IS IN RESCISSION.
      *  MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER READ OF BW-FILE.
      *  010397 BAH ADDED FD-PROG-PREFIX TO ALLOW MORE CUSTOM
      *             PRINT PROGRAMS, USES LP/PRT(FD-PROG-PREFIX)(FD-PROG)
      *             INSTEAD OF LP/LONP(FD-PROG)
      *  050197 BAH ADDED SAVED LOAN PRINT FOR CAVALIER FROM A50
      *  121897 KEC ADDED CHECK TO SEE IF PRINTING A STANDARD PRINTER
      *              FORM OR A FORMSXPRESS FORM.
      *  042198 BAH CHANGED NEW-PROG-FORM TO NEW-FORM SO LOAN-KEY DOESNT PASS
      *             BACK TO PAYMENT POSTING AND AUTOMATICALLY PULL UP
      *             THE LOAN
      *  JTG 101298 SET PDSCAN-REVOLVING = 'N', REGENCY REVOL
      *  BAH 990723 CHANGED FXPRESS TO SMART PORT
      *  BAH 010222 IF FD RECORD DOES NOT EXIST, DONT GO TO IO-ERROR, SEND
      *             MESSAGE AND GO TO ENTER A NEW FORM #, WORLD
      *  CS  010415 ADDED F5-TOGGLE FOR ADDITIONAL 10 SPR FORMS, REGACC PR# 1441
      *  KEC 021015 CHANGED "PROG-WK" TO ACCEPT ANY VARATION OF LP/PRT?.
      *  BLV 030617 FIX REGENCY PROBLEM, CHECK IS LONP01, CONTRACT IS PRT001;
      *             WOULDN'T ALLOW INS AUTH PRINT FOR RESCISSION LOANS, SO
      *             SKIP RECISSION CHECK IF PROG = 01 & PREFIX = 0 
      *  KEC 050801 CHANGED TO ADD PATH OPTION OF 'PCL':
      *               - LONP??
      *               - PCL#??
      *               - PRT0??
      *  KEC 050810 CHANGED TO ADD FD-OPTION (AKA FD-SMARTPORT) AND
      *             NEW ARRANGMENT FOR FD-PROG-PREFIX.
      *  KEC 060425 WAS HAVING A PROBLEM PRINTING A LASER FORM (PCL)
      *             FOR REGENCY.  THE GIVEN ACCOUNT WAS IN RESCISSION
      *             AND DID NOT ALLOW PRINTING OF THE GIVEN FORM.
      *             TURNED OUT THAT THE CHECK PROGRAM (GB-CHECK-PROG)
      *             EQUALED THE FD-PROG EVENTHOUGH IT IS A LASER FORM
      *             (PCL).  SO ADDED A CHECK TO NOT STOP IF IT IS A
      *             LASER FORM PROGRAM:   IF NOT ( FD-OPTION-PCL-LASER )
      *  KEC 111003 CHANGED TO ADD PATH OPTION OF 'PST':  - PST#??
      *  KEC 140612 CHANGED PATH PCL/PRT/PST TO USE THE FP DIRECTORY.
      *  BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *  KEC 2017.0119 CHANGED FD-SMARTPORT TO FD-OPTION [88 LEVELS].
      *  BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      *  BAH 2023.1110 ADDED B2-ALLOW-F3-ALL-DOCS TO ALLOW F3-ALL WHICH WILL
      *                BUILD ALL LASER FORMS (IF PRODUCT SOLD) ALONG WITH
      *                A MANIFEST IN FTP/OUT/DOCS DIRECTORY
      *                B2-ALLOW-F3-ALL-DOCS MUST BE "Y"
      *                FD-TYPE-OF-DOC MUST HAVE ANY VALUE IN IT TO BE INCLUDED
      *                #1630
      *  BAH 2023.1205 ADDED MANIFEST-MAKER-DOB, MADE FILE VARYING  #1630A
      *  BAH 2024.0226 ADDED MANIFEST-COMAKER-OPT-OUT #1638A
      *  BAH 2024.0821 ADDED MANIFEST-LOANDATE #1674 S35Q-119
      *  BAH 2024.1031 DISPLAY "PREPARING DOCUMENTS" IF F3-ALL #1683 S35Q-142
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * NEED FOR MANIFEST FILE
       COPY "LIBFP/FPFSPST.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".
      * NEED FOR MANIFEST FILE
       COPY "LIBFP/FPFDPST.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/LONPM0 S35 11/13/23-13:28:23 >".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
      * NEED FOR MANIFEST FILE
       COPY "LIBFP/FPWSPST.CPY".
       01  PR-IFN          PIC X(4) VALUE "B6PR".

       01  FORM-LEVEL      PIC 99  VALUE  0.

       78  PROG-DIR-PREFIX-LONP                  VALUE "LP/LONP".
       78  PROG-DIR-PREFIX-PCL                   VALUE "FP/PCL ".
       78  PROG-DIR-PREFIX-PST                   VALUE "FP/PST ".
       78  PROG-DIR-PREFIX-PRT                   VALUE "FP/PRT ".
       01  PROG-WK.
           03  PROG-DIR-NAME       PIC X(07)     VALUE "LP/LONP".
           03  FILLER              REDEFINES PROG-DIR-NAME.
               05  FILLER          PIC X(06).
               05  PROG-DIR-PREFIX PIC X(01).
           03  PROG-WK-IP          PIC X(02)     VALUE SPACES.

       01  HOLD-OPT-OUT         PIC X.
       01  HOLD-SSNO.
           03  SSNO-FIRST-3     PIC 9(3).
           03  SSNO-MID-2       PIC 9(2).
           03  SSNO-LAST-4      PIC 9(4).
       01  HOLD-MAKER-FNAME     PIC X(16).
       01  HOLD-MAKER-LNAME     PIC X(16).
       01  HOLD-MAKER-DOB       PIC 9(08).
       01  HOLD-COMAKER-FNAME   PIC X(16).
       01  HOLD-COMAKER-LNAME   PIC X(16).
       01  HOLD-COMAKER-OPT-OUT PIC X.
       01  HOLD-PST-ENV         PIC X.

       01  SYSDATE-MMDDYY              PIC 9(06)     VALUE ZEROES.
       01  FILLER                      REDEFINES SYSDATE-MMDDYY.
           03  SYSDATE-MMDDYY-MM       PIC 9(02).
           03  SYSDATE-MMDDYY-DD       PIC 9(02).
           03  SYSDATE-MMDDYY-YY       PIC 9(02).

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  SUB             PIC 9(6) COMP-3.
       01  S1              PIC 9(2).
       01  S2              PIC 9(2).
       01  INSPECT-W       PIC X(86).
       01  ADD-ON          PIC X(25).

       01  MESSAGE-RESCISSION        PIC X(49)     VALUE
              "CHECKS CANNOT BE PRINTED WHILE LOAN IN RESCISSION".

      *************************
      *  INTERMEDIATE PROG    *
      *************************
       COPY "LIBLP/LPIPBUF.CPY".

      *=================================================================
      *    NOTE: CONTCUT-FMNO NEEDS TO BE 9(4) BUT LONPM0 FORM/SCREEN
      *          NEEDS TO BE CHANGED AT FUTURE DATE.      KEC 2016.0211
      *=================================================================
       01  CONTCUT-BUF              VALUE SPACES.
           03  CONTCUT-DISPLAYED.
               05  CONTCUT-DESC     PIC X(30) OCCURS 10.
               05  CONTCUT-TYPE     PIC X(10) OCCURS 10.
               05  CONTCUT-DOC-TYPE PIC XX    OCCURS 10.
               05  CONTCUT-TOGGLE-1 PIC X(15).
               05  CONTCUT-TOGGLE-2 PIC X(15).


           03  CONTCUT-NOT-DISPLAYED.
               05  CONTCUT-PROG     PIC XX    OCCURS 10.
               05  CONTCUT-FMNO     PIC 999   OCCURS 10.
               05  CONTCUT-PRCONT   PIC X     OCCURS 10.

       01  CONTCUT-LIST.
           03  FILLER               PIC X(40) VALUE
           "01X 02X 03X 04X 05X 06X 07X 08X 09X 10X ".
           03  FILLER               PIC X(40) VALUE
           "011 021 031 041 051 061 071 081 091 101 ".
           03  FILLER               PIC X(40) VALUE
           "012 022 032 042 052 062 072 082 092 102 ".
           03  CONTCUT-PRFORM-LIST.
               05  CONTCUT-TOGGLE-ATTR.
                   07  FILLER          PIC X(09)  VALUE "PRFORM-10".
                   07  CONTCUT10-ATTR  PIC X(04)  VALUE ":RF ".
                   07  FILLER          PIC X(09)  VALUE "PRFORM-20".
                   07  CONTCUT20-ATTR  PIC X(04)  VALUE "    ".
                   07  FILLER          PIC X(09)  VALUE ".".

       01  ALP-BW.
           03  ALP-BW123   PIC X(3).
           03  ALP-HYPHEN1 PIC X.
           03  ALP-BW45    PIC X(2).
           03  ALP-HYPHEN2 PIC X.
           03  ALP-BW6789  PIC X(4).
       01  NUM-BW          PIC 9(9).
       01  NUM-BWX REDEFINES NUM-BW.
           03  NUM-BW123   PIC X(3).
           03  NUM-BW45    PIC X(2).
           03  NUM-BW6789  PIC X(4).

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-BUF         VALUE " ".
           03  D-LOAN-NUM      PIC X(16).
           03  D-LCNO          PIC 99.
           03  D-LC-DESC       PIC X(15).
           03  D-LNAMT         PIC ZZZ,ZZ9.99.
           03  D-010-040       OCCURS 4.
               05  D-010       PIC X(11).
               05  FILLER      PIC X.
               05  D-011       PIC X(33).
               05  FILLER      PIC X.
               05  D-012       PIC X(1).

       01  DISPLAY-LISTS.
           03  FILLER          PIC X(38)  VALUE
           "LNNO LNSEQ LC LCX AMT 010 020 030 040.".
       01  DISPLAY-LIST REDEFINES DISPLAY-LISTS
                               PIC X(38).

       COPY "LIBWI/PDSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPM_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBLP/LONPM_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".

       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPM_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".


       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              PR-FILE MANIFEST-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE MANIFEST-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPM" TO VDU-FORM-NAME.
      D    STOP MESS.

           PERFORM FORM-RESET.

           PERFORM GET-GPENV.

           PERFORM OPEN-GB-FILE.
           MOVE BRANCH TO GB-BRNO.
           MOVE 1      TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

           MOVE BRANCH TO BR-NO.
           PERFORM GET-BR-RECORD.

           MOVE "S  A020PRU." TO VDU.
           MOVE BR-PRINTER(1) TO VDUBUF.
           PERFORM SCREENIO.
       SETRP.
           MOVE "F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE 0 TO Q-POS C-POS.
           MOVE LONPM-PRU-POS TO P-POS.

           MOVE "1" TO C-BUF.
           MOVE 00 TO P-BUF.

           MOVE "N" TO STAT Q-BUF.

           PERFORM PRU-IN.
           IF STAT = "1>"
              PERFORM PRU-OUT
              PERFORM DESTROY-LONPM-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO SETRP
              ELSE
                 MOVE "R" TO ROUTE-CD
                 MOVE 0 TO LOAN-KEY
                 GO TO END-PROG.
           IF STAT NOT = "00"
              GO TO SETRP.


      *    PRINTER RESERVED AT THIS POINT.
      *    IF A NEW PROGRAM IS SELECTED VIA THE MENU,
      *    MOVE "R" TO ROUTE-CD
      *    ROUTE-CD REMAINS " " FOR PRINTING.
      *    UPON EXITING THIS PROGRAM ERRCD SHOULD
      *    ALWAYS BE "S" (TO USE SPECIAL TCL SUB-ROUTINES).

           MOVE " " TO ROUTE-CD.
           MOVE "S" TO ERRCD.


       CHECK-LOAN-KEY.
           IF LOAN-KEY NOT = 0
              MOVE LOAN-KEY TO LN-ACCTNO
              GO TO READ-LOAN-FILE..

           MOVE "F7-MENU, F3-SAVED LOANS,  ENTER LOAN #:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "EN N140LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO SETRP.

           IF VDUSTATUS = "1>"
              PERFORM PRU-OUT
              PERFORM DESTROY-LONPM-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO CHECK-LOAN-KEY
              ELSE
                 MOVE "R" TO ROUTE-CD
                 MOVE 0 TO LOAN-KEY
                 GO TO END-PROG.

      * F3-SCAN SAVED LOANS
           IF VDUSTATUS = "16"
              MOVE "L"    TO PDSCAN-ID
              MOVE BRANCH TO PDSCAN-BRANCH
              MOVE SPACES TO PDSCAN-VERIFY
              MOVE 0      TO PDSCAN-NO
              MOVE "WI/PDSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH PDSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF PDSCAN-STATUS NOT = "00" OR PDSCAN-NO = 0
                 GO TO CHECK-LOAN-KEY
              ELSE
                PERFORM GET-PD1
                IF IO-FG = 9
                   GO TO CHECK-LOAN-KEY
                ELSE
                   MOVE 0 TO LOAN-KEY
                   MOVE PD-RECORD  TO LN-REC
                   MOVE PD-SOC-SEC TO BORROWER-KEY
                   GO TO SKIP-READ-LOAN.

           IF VDUSTATUS NOT = "00"
              GO TO CHECK-LOAN-KEY.

           MOVE BRANCH  TO LN-OWNBR.
           MOVE VDUNUM0 TO LN-ACCTNO.
       READ-LOAN-FILE.
           MOVE LN-ACCTNO TO LOAN-KEY.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              MOVE "INVALID ACCOUNT NUMBER" TO MESS
              PERFORM SEND-MESS
              MOVE 0 TO LOAN-KEY
              GO TO CHECK-LOAN-KEY.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              MOVE 0 TO LOAN-KEY
              GO TO CHECK-LOAN-KEY.

           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              MOVE 0 TO LOAN-KEY
              GO TO CHECK-LOAN-KEY.

       SKIP-READ-LOAN.
           PERFORM GET-SP-RECORD.
           MOVE LN-OWNBR TO BR-NO.
           PERFORM GET-BR-RECORD.
           PERFORM DISPLAY-FIELDS.

       ENTER-FORM.
           IF BR-ALLOW-F3-ALL-DOCS = "Y"
              MOVE "F1-CLR,F3-ALL,F7-MENU,^-LOAN#,F5-FORMS,ENTER #:"
                    TO MESS
           ELSE
              MOVE "F1-CLR, F7-MENU, ^-LOAN #, F5-FORMS, ENTER #:"
                    TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF BR-ALLOW-F3-ALL-DOCS = "Y"
              IF VDUSTATUS = "16"
                 PERFORM PRINT-ALL-DOCUMENTS
                 SET ENVIRONMENT "PST_OUTPUT" TO HOLD-PST-ENV
                 GO TO ENTER-FORM.

           IF VDUSTATUS = "10"
              GO TO SETRP.

           IF VDUSTATUS = "1>"
              PERFORM PRU-OUT
              PERFORM DESTROY-LONPM-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-FORM
               ELSE
                 MOVE "R" TO ROUTE-CD
                 IF NEW-FORM = "F"
                    MOVE 0 TO LOAN-KEY
                    GO TO END-PROG
                  ELSE
                    GO TO END-PROG.
           IF VDUSTATUS = "1U"
              MOVE 0 TO LOAN-KEY
              GO TO CHECK-LOAN-KEY.

      *CS 04/15/01 ADDITIONAL 10 FORMS
           IF VDUSTATUS = "1:"
              PERFORM TOGGLE-FORMS
              GO TO ENTER-FORM.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FORM.

           MOVE VDUBUF TO ACTION.
           INSPECT ACTION REPLACING ALL " " BY "0".
           IF CHNO NOT NUMERIC
              GO TO ENTER-FORM.
           IF CHNO = 0
              GO TO ENTER-FORM.
           IF CHNO > 10
              GO TO ENTER-FORM.

           IF FORM-LEVEL = 10
              ADD 10 TO CHNO.

           IF SP-PRFORM(CHNO) = 0
              GO TO ENTER-FORM.

       SET-FORM-LENGTH.
           MOVE SP-PRFORM(CHNO) TO FD-FMNO.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID FORM NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST FOR LOAN IN RESCISSION AND IF 'CHECK' PROGRAM SELECTED
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * BLV 030617 SKIP RECISSION CHECK IF PROG=01 & PREFIX = 0 (PRT001)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM LPFRFD-FIND-PREFIX.
           IF ( LPWSFD-PREFIX-PRT     ) AND
              ( FD-PROG-PREFIX = "0"  ) AND
              ( FD-PROG        = "01" )
              MOVE STAT---GOOD TO STAT
           ELSE
              MOVE STAT---BAD  TO STAT.

           IF ( LN-POCD-INRESCISSION )
              IF ( FD-PROG = GB-CHECK-PROG )
      *KC------->IF NOT (FD-PROG = "01" AND FD-PROG-PREFIX = "0")
                 IF NOT ( FD-OPTION-PCL-LASER )
                    IF NOT ( FD-OPTION-PST-LASER )
                       IF ( STAT-BAD )
                          MOVE MESSAGE-RESCISSION TO MESS
                          PERFORM SEND-MESS
                          GO TO ENTER-FORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST FOR CUT SHEET AND CENTRONIC AUTOFEED:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF FORM-LEVEL = 10
              SUBTRACT 10 FROM CHNO.


       FORM-LOADED-YN.
           IF CONTCUT-PRCONT(CHNO) NOT = "Y"
              MOVE "^-FORM #,LOAD FORM;'A'-ALIGN,'P'-PRINT:" TO MESS
           ELSE
              MOVE "^-FORM #, 'A'-ALIGN, 'P'-PRINT        :" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-FORM.
           IF VDUSTATUS NOT = "00"
              GO TO FORM-LOADED-YN.
           IF VDUBUF = "     A"
              MOVE "MC" TO IP-PROG
              MOVE "N" TO IP-AOK
              MOVE CONTCUT-FMNO(CHNO) TO IP-FMNO
      *       CALL XFER USING STAT IP-LIST IP-BUF
      *       CALL XFER USING STAT PROG-LIST PROG-BUF
              GO TO PROGRAM-LOAD.

           IF VDUBUF NOT = "     P"
              GO TO FORM-LOADED-YN.

           MOVE "Y" TO IP-AOK.
           MOVE CONTCUT-PROG(CHNO) TO IP-PROG.
           MOVE CONTCUT-FMNO(CHNO) TO IP-FMNO.
           IF CONTCUT-PRCONT(CHNO) = "Y"
              MOVE "1" TO CONT-PRINT
            ELSE
              MOVE "0" TO CONT-PRINT.


       PROGRAM-LOAD.

           PERFORM LPFRFD-FIND-PREFIX.
           IF ( LPWSFD-PREFIX-PCL )
              MOVE PROG-DIR-PREFIX-PCL  TO PROG-DIR-NAME
              MOVE FD-PROG-PREFIX       TO PROG-DIR-PREFIX
              MOVE IP-PROG              TO PROG-WK-IP
           ELSE
           IF ( LPWSFD-PREFIX-PST )
              MOVE PROG-DIR-PREFIX-PST  TO PROG-DIR-NAME
              MOVE FD-PROG-PREFIX       TO PROG-DIR-PREFIX
              MOVE IP-PROG              TO PROG-WK-IP
           ELSE
           IF ( LPWSFD-PREFIX-PRT )
              MOVE PROG-DIR-PREFIX-PRT  TO PROG-DIR-NAME
              MOVE FD-PROG-PREFIX       TO PROG-DIR-PREFIX
              MOVE IP-PROG              TO PROG-WK-IP
           ELSE
              MOVE PROG-DIR-PREFIX-LONP TO PROG-DIR-NAME
              MOVE IP-PROG              TO PROG-WK-IP.

           MOVE PROG-WK TO FORM-NAM.

      * THIS IS CALLING THE PRINT PROGRAM, EXAMPLE: LONPM2, PRT041, ETC.
           CALL FORM-PROGX USING FORM-PATH PROG-BUF IP-BUF PRU-PATH.
           CANCEL FORM-PROGX.

           IF ( ROUTE-CD = "R" )
              GO TO END-ROUTINE.

           PERFORM SETUP-FORM-TITLES.
           GO TO ENTER-FORM.

      *****************************
       PRINT-ALL-DOCUMENTS SECTION.
      *****************************
      D    STOP MESS.
           MOVE "PST_OUTPUT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-GOOD 
              MOVE GETENV-BUF TO HOLD-PST-ENV
           ELSE
              MOVE " "        TO HOLD-PST-ENV.

           SET ENVIRONMENT "PST_OUTPUT" TO "A".

       PRINT-ALL-DOCUMENTS-ASK.

           MOVE "^-FORM #, 'P'- TO PRINT ALL  :" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO EXIT-PRINT-ALL-DOCUMENTS.
           IF VDUBUF NOT = "     P"
              GO TO PRINT-ALL-DOCUMENTS-ASK.

           MOVE "PREPARING DOCUMENTS" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-FD1-FILE.
         
           MOVE "FIL"                    TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF               TO PST-DATA-PATH.

      * CREATE ORIGINAL MANIFEST WHILE BUILDING DOCUMENTS WITHOUT THE M
      * WHEN THEY ARE ALL DONE, MOVE THE MANIFEST FILE TO HAVE THE M SO
      * THE API KNOWS THEY ARE ALL DONE CREATED
           MOVE PST-FIL-BASE             TO PST-PATH-MAN-FIL
                                            PST-PATH-MAN-D-FIL.
           MOVE LN-OWNBR                 TO PST-PATH-MAN-BRNO
                                            PST-PATH-MAN-D-BRNO.
           MOVE LN-ACCTNO                TO PST-PATH-MAN-ACCTNO
                                            PST-PATH-MAN-D-ACCTNO.

           PERFORM OPEN-MANIFEST-FILE-OUTPUT.
           PERFORM CLOSE-MANIFEST-FILE.
           PERFORM OPEN-MANIFEST-FILE.

           MOVE SPACES TO MANIFEST-FIRST-RECORD
                          MANIFEST-REC.
           MOVE LN-ORGST                 TO MANIFEST-STATE.
           MOVE LN-SSNO(1)               TO HOLD-SSNO.
           MOVE SSNO-LAST-4              TO MANIFEST-LAST-4.
           MOVE LN-OWNBR                 TO MANIFEST-BR-NO.
           MOVE SSNO-MID-2               TO MANIFEST-MID-2.
           MOVE LN-ACCTNO                TO MANIFEST-LN-NUMBER.
           MOVE SSNO-FIRST-3             TO MANIFEST-FIRST-3.
           MOVE LN-CLASS                 TO MANIFEST-CLASS.
           MOVE LN-SUBCLASS              TO MANIFEST-SUBCLASS.
           MOVE LN-LNAMT                 TO MANIFEST-LNAMT.
           MOVE HOLD-OPT-OUT             TO MANIFEST-OPT-OUT.
           MOVE HOLD-MAKER-FNAME         TO MANIFEST-MAKER-FNAME.
           MOVE HOLD-MAKER-LNAME         TO MANIFEST-MAKER-LNAME.
      * MMDDYYYY
           MOVE HOLD-MAKER-DOB           TO MANIFEST-MAKER-DOB.
           MOVE HOLD-COMAKER-FNAME       TO MANIFEST-COMAKER-FNAME.
           MOVE HOLD-COMAKER-LNAME       TO MANIFEST-COMAKER-LNAME.
           MOVE HOLD-COMAKER-OPT-OUT     TO MANIFEST-COMAKER-OPT-OUT.
           MOVE LN-LOANDATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY            TO MANIFEST-LOANDATE.
           MOVE X"0D"                    TO MANIFEST-FIRST-CR.
           MOVE MANIFEST-FIRST-RECORD    TO MANIFEST-REC.
           PERFORM WRITE-MANIFEST-FILE.
           PERFORM CLOSE-MANIFEST-FILE.

           MOVE 0 TO CHNO.

       PRINT-NEXT-DOCUMENT.
           ADD 1 TO CHNO.
           IF CHNO > 20
              GO TO END-PRINT-ALL-DOCUMENTS.

           IF SP-PRFORM(CHNO) = 0
              GO TO PRINT-NEXT-DOCUMENT.

           MOVE SP-PRFORM(CHNO) TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID FORM NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO PRINT-NEXT-DOCUMENT.

           IF FD-TYPE-OF-DOC = SPACES
              GO TO PRINT-NEXT-DOCUMENT.

           IF ( FD-PROG = GB-CHECK-PROG )
              GO TO PRINT-NEXT-DOCUMENT.

           PERFORM LPFRFD-FIND-PREFIX.

           IF NOT ( FD-OPTION-PCL-LASER )
              IF NOT ( FD-OPTION-PST-LASER )
                 GO TO PRINT-NEXT-DOCUMENT.

           MOVE FD-PROG       TO IP-PROG.
           MOVE FD-FMNO       TO IP-FMNO.
           MOVE "0"           TO CONT-PRINT.
           MOVE FD-OPTION     TO IP-FD-OPTION.

           PERFORM LPFRFD-FIND-PREFIX.
           IF ( LPWSFD-PREFIX-PCL )
              MOVE PROG-DIR-PREFIX-PCL  TO PROG-DIR-NAME
              MOVE FD-PROG-PREFIX       TO PROG-DIR-PREFIX
              MOVE IP-PROG              TO PROG-WK-IP
           ELSE
           IF ( LPWSFD-PREFIX-PST )
              MOVE PROG-DIR-PREFIX-PST  TO PROG-DIR-NAME
              MOVE FD-PROG-PREFIX       TO PROG-DIR-PREFIX
              MOVE IP-PROG              TO PROG-WK-IP.

           MOVE PROG-WK TO FORM-NAM.

      * THIS IS CALLING THE PRINT PROGRAM, EXAMPLE: LONPM2, PRT041, ETC.
           CALL FORM-PROGX USING FORM-PATH PROG-BUF IP-BUF PRU-PATH.
           CANCEL FORM-PROGX.

           IF ( ROUTE-CD = "R" )
              GO TO END-ROUTINE.

           GO TO PRINT-NEXT-DOCUMENT.

       END-PRINT-ALL-DOCUMENTS.
      * NOW MOVE THE FILE TO HAVE THE LEADING M SO THEY KNOW THE FORMS
      * ARE DONE AND THEY CAN GRAB THEM
      D    STOP MESS.
            MOVE PST-PATH-MANIFEST      TO PST-WS-MV-MANI-FILE-1.
            MOVE PST-PATH-MANIFEST-DONE TO PST-WS-MV-MANI-FILE-2.
            MOVE PST-WS-MV-MANIFEST-COMMAND TO SYSTEM-BUF.
            PERFORM SYSTEM-CALL.
            IF STAT NOT = "00"
               MOVE "PROBLEM RENAMING MANIFEST FILE" TO MESS
               PERFORM SEND-MESS.

       EXIT-PRINT-ALL-DOCUMENTS.
            EXIT.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE SPACES TO DISPLAY-BUF
                          HOLD-OPT-OUT
                          HOLD-MAKER-FNAME
                          HOLD-MAKER-LNAME
                          HOLD-COMAKER-FNAME
                          HOLD-COMAKER-LNAME
                          HOLD-COMAKER-OPT-OUT.
           MOVE 0      TO HOLD-MAKER-DOB.

           MOVE LN-CLASS TO LC-CODE D-LCNO.
           PERFORM GET-LC.
           MOVE LC-DESC TO D-LC-DESC.
           MOVE LN-SUM-LNAMT TO D-LNAMT.

           PERFORM OPEN-BW1-FILE.
           MOVE 1 TO SUB.
       DISP-NEXT-BW.
           MOVE LN-SSNO(SUB) TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF NUM-BW NOT = 0
              MOVE NUM-BW TO BW-SSNO
              PERFORM GET-BW1
              MOVE ALP-BW TO D-010(SUB)
              MOVE INSPECT-W TO D-011(SUB)
              MOVE LN-MAKERCD(SUB) TO D-012(SUB)
              IF SUB = 2
                 MOVE BW-FNAME           TO HOLD-COMAKER-FNAME
                 MOVE BW-LNAME           TO HOLD-COMAKER-LNAME
                 MOVE BW-PRIVACY-OPT-OUT TO HOLD-COMAKER-OPT-OUT.
              
           IF SUB = 1
              MOVE BW-PRIVACY-OPT-OUT TO HOLD-OPT-OUT
              MOVE BW-FNAME           TO HOLD-MAKER-FNAME
              MOVE BW-LNAME           TO HOLD-MAKER-LNAME
              MOVE BW-BDATE           TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY
              MOVE DATE-MMDDYYYY      TO HOLD-MAKER-DOB
              PERFORM LPSTAT-DISPLAY
              MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           IF SUB < 4
              ADD 1 TO SUB
              GO TO DISP-NEXT-BW.

           PERFORM CLOSE-BW1-FILE.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM SETUP-FORM-TITLES.


      *********************************************
       SETUP-FORM-TITLES SECTION.
      *********************************************

           PERFORM OPEN-FD1-FILE.
           MOVE SPACES TO CONTCUT-DISPLAYED.
           MOVE ZEROS TO CONTCUT-NOT-DISPLAYED.
           MOVE FORM-LEVEL TO S2.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 10
              ADD 1 TO S2
              IF SP-PRFORM(S2)  NOT = 0
                 MOVE SP-PRFORM(S2) TO FD-FMNO
                 PERFORM READ-FD1-FILE
                 IF IO-FG = 0
                    MOVE FD-FMNO   TO CONTCUT-FMNO(S1)
                    MOVE FD-PROG   TO CONTCUT-PROG(S1)
                    MOVE FD-DESC   TO CONTCUT-DESC(S1)
                    MOVE FD-TYPE-OF-DOC TO CONTCUT-DOC-TYPE(S1)
                    MOVE FD-PRCONT TO CONTCUT-PRCONT(S1)
                    IF FD-PRCONT = "Y"
                       MOVE "CONTINUOUS" TO CONTCUT-TYPE(S1)
                    ELSE
                       MOVE "CUT-SHEET" TO CONTCUT-TYPE(S1)
                    END-IF
                 END-IF
              END-IF
           END-PERFORM.

           IF FORM-LEVEL = 0
              MOVE ":RF " TO CONTCUT10-ATTR
              MOVE SPACES TO CONTCUT20-ATTR
           ELSE
              MOVE SPACES TO CONTCUT10-ATTR
              MOVE ":RF "  TO CONTCUT20-ATTR.

           MOVE "FORMS 1 ~ 10"  TO CONTCUT-TOGGLE-1.
           MOVE "FORMS 11 ~ 20" TO CONTCUT-TOGGLE-2.

           MOVE CONTCUT-BUF TO WR-BUF.
           MOVE CONTCUT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CLOSE-FD1-FILE.


      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE LPLOAN-SYSDATE TO LONPM-SYSDATE.
      * NEED TO CLEAR SCREEN ON F1
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONPM-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONPM-FORM-FIELDS TO WR-BUF.
           MOVE LONPM-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.
           
       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPM-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPM-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPM_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       DESTROY-LONPM-SCREEN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPM-SCREEN.
       GET-PD1.
           MOVE "L"       TO PD-ID.
           MOVE BRANCH    TO PD-BRANCH.
           MOVE PDSCAN-NO TO PD-SOC-SEC.
           PERFORM OPEN-PD1-FILE.
           PERFORM READ-PD1-FILE.
           PERFORM CLOSE-PD1-FILE.
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       GET-BR-RECORD.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR
           ELSE
              PERFORM CLOSE-BR-FILE.
       GET-SP-RECORD.
           PERFORM OPEN-SP1-FILE.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID STATE PARAMETER RECORD" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR
           ELSE
              PERFORM CLOSE-SP1-FILE.
       GET-LC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG = 9
              MOVE "INVALID" TO LC-DESC.
       GET-BW1.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG = 9
              IF PDSCAN-BWNAME NOT = SPACES
                 MOVE PDSCAN-BWNAME TO BW-LNAME
                 MOVE SPACES TO BW-FNAME ALP-BW
                 MOVE 0 TO BW-SSNO
              ELSE
                 MOVE " " TO BW-FNAME
                 MOVE "NOT FOUND" TO BW-LNAME.
           PERFORM APPEND-BW-NAMES.
       APPEND-BW-NAMES.
           MOVE BW-LNAME TO INSPECT-W.
           MOVE BW-FNAME TO ADD-ON.
           PERFORM ADD-IT.
       ADD-IT.
           INSPECT INSPECT-W REPLACING FIRST "  " BY " +".
           INSPECT INSPECT-W REPLACING FIRST
              "+                        " BY ADD-ON.
       NUMBW-TO-ALPBW.
           MOVE " " TO ALP-BW.
           IF NUM-BW NOT = 0
              MOVE NUM-BW123 TO ALP-BW123
              MOVE NUM-BW45 TO ALP-BW45
              MOVE NUM-BW6789 TO ALP-BW6789
              MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.

       TOGGLE-FORMS.
      *FORM-LEVEL = 0 FORMS 1 - 10, FORM-LEVEL = 10, FORMS 11 - 20
           IF FORM-LEVEL  = 0
               MOVE 10  TO FORM-LEVEL
           ELSE
               MOVE 0 TO FORM-LEVEL.

           PERFORM SETUP-FORM-TITLES.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPFRFD.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPMENU.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".

      ***************************************
       IO-ROUTINE SECTION.
      ***************************************
      * NEED FOR MANIFEST FILE
       COPY "LIBFP/FPIOPST.CPY".
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPPD1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
           PERFORM PRU-OUT.
       END-PROG.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           PERFORM DESTROY-LONPM-SCREEN.
           EXIT PROGRAM.
