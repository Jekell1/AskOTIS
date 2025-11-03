       IDENTIFICATION DIVISION.
       PROGRAM-ID.       IRPREP.
       DATE-WRITTEN.   11/21/88.
      *****************************************************
      *                      (SP)
      *       DESCRIPTION:  PREPARES IRS FILES
      *
      *  HERE ARE SOME NOTES FROM THE IRMENU HELP:
      *
      *  2- IRS LOAN (1098) EXTRACTION - THIS PROGRAM WILL ALLOW
      *     YOU TO EXTRACT LOAN ACCOUNTS THAT HAVE PAID INTEREST
      *     WHICH NEEDS TO BE REPORTED TO THE IRS ON FORM 1098.
      *     THIS EXTRACTION READS THE LOAN FILE AND EXTRACTS EITHER
      *     THE CURRENT YEAR-TO-DATE INTEREST OR LAST YEAR-TO-DATE
      *     INTEREST & WRITES THE AMOUNT TO THE 1ST AMOUNT IN THE OUTPUT
      *     FILE, WHICH IS /USR/DATA/R1/SP/ILFILE.  THIS AMOUNT
      *     CORRESPONDS TO BOX 1 ON THE 1098, WHICH IS "MORTGAGE
      *     INTEREST RECEIVED FROM PAYER(S)/BORROWER(S)".
      *     ALSO WRITES 2ND AMOUNT IN THE OUTPUT FILE AS OUTSTANDING
      *     MORTGAGE PRINCIPLE
      *
      *
      *  5- IRS (1099-C) EXTRACTION - THIS PROGRAM ALLOWS YOU TO EXTRACT
      *     CHARGED-OFF ACCOUNTS TO REPORT AS CANCELLATION OF DEBT. THIS
      *     PROGRAM WILL EXTRACT CHARGED-OFF ACCOUNTS MATCHING THE
      *     PARAMETERS IF THEIR BALANCE LESS UNEARNED IS $600 OR MORE.
      *     IF AN ACCOUNT IS SELECTED, A LOAN MEMO IS WRITTEN AND THE
      *     ACCOUNT IS FLAGGED FOR THE 1099C SO IT CANNOT BE EXTRACTED
      *     AGAIN.  THE AMOUNT UPDATED TO BOX 2 (AMOUNT OF DEBT CANCELED)
      *     IS THE BALANCE LESS THE UNEARNED.
      *
      *  6- PREPARE FOR IRS - THIS PROGRAM COMBINES THE OUTPUT FROM
      *     THE PREVIOUS 4 EXTRACTIONS AND CREATES A FILE TO BE SUBMITTED
      *     TO THE IRS.  THE OUTPUT FILE IS /USR/DATA/R1/SP/IRSTAX.
      *
      *     THE FIRST RECORD IN THE IRSTAX FILE IS THE IDENTIFICATION
      *     RECORD, BUILT WITH INFORMATION INPUT THROUGH THE IRS
      *     IDENTIFICATION MAINTENANCE.  FOR ALL THE RECORD TYPES IN
      *     THIS FILE, THE FIRST CHARACTER IS THE TYPE, AND THIS IS
      *     TYPE "T", TRANSMITTOR RECORD.
      *
      *     FOR EACH TYPE OF 1098 OR 1099 FILES WHICH EXIST, THIS PROGRAM
      *     WILL CREATE A TYPE "A" RECORD WITH PAYEE INTO, THEN MULTIPLE
      *     TYPE "B" RECORDS FOR EACH INDIVIDUAL TO BE REPORTED WITH THE
      *     AMOUNT, THEN A TYPE "C" RECORD WITH THE TOTAL AMOUNT FOR THAT
      *     GROUP AND THE NUMBER OF RECORDS REPORTED IN THAT GROUP.
      *
      *     AT THE END IS A TYPE "F" RECORD WITH THE TOTAL NUMBER OF
      *     TYPE "A" RECORDS IN THE FILE.
      *
      *     SO IF YOU HAVE ALL 4 TYPES (1098 & 3 TYPES OF 1099'S) THE
      *     FILE WOULD HAVE THESE TYPES:
      *
      *     1 TYPE T
      *     1 TYPE A
      *       TYPE B'S FOR EACH LOAN REPORTING INTEREST
      *     1 TYPE C
      *     1 TYPE A
      *       TYPE B'S FOR EACH CANCELLED LOAN REPORTED
      *     1 TYPE C
      *     1 TYPE A
      *       TYPE B'S FOR EACH INVESTOR (OR NOTE) REPORTING INTEREST
      *     1 TYPE C
      *     1 TYPE A
      *       TYPE B'S FOR EACH VENDOR REPORTING RENT, NONEMPLOYEE 
      *                                   COMPENSATION OR OTHER INCOME
      *     1 TYPE C
      *     1 TYPE F
      *
      *  REVS:
      *   012789 SLC  PAY YEAR FOR NOTES RECORD "A" WAS NOT
      *               GETTING FILLED.
      *   013189 BAH  PRINT IV-SSNO CORRECT WITH HIGH-ORDER-9
      *   022393 BAH  ACU COBOL DIFFERS FROM MF COBOL IN FILE
      *               TYPES, IMFILE (IRSTAX.001) MUST HAVE
      *               A 0D0A ON THE END OF EACH RECORD. WE
      *               WILL USE A RELATIVE FILE WHICH IN MF COBOL
      *               PUT A 0A ON THE END AND WE FORCED A 0D.
      *               IN ACU, WE MUST FORCE BOTH THE 0A AND 0D.
      *   010598 BAH CHANGED TO WORK IN A60, PUT FILES IN GLOBAL SP
      *   012799 BAH  CHANGES AS PER IRS, YEAR 2000, RECORD EXPANDED
      *               FROM 420 TO 750, DATES EXPANDED...
      *   012999 BAH  ADDED TEST AND REPLACEMENT FLAGS
      *   MJD 991213 CHANGED TO PRINT TOTALS FOR ALL AMOUNTS 1-12
      *   BLV 000111 CHANGED TO PRINT CITY STATE ZIP, ALSO FOR NOTES
      *              PRINT FORFEIT AMT & WITHHELD AMT IF NOT ZERO
      *   BLV 000221 CHANGED AMT-INDICATORS FROM "1" TO "124" FOR
      *              "A" RECORD FOR 1099, NOTES NOW REPORT PENALTIES
      *              & WITHHOLDING AS WELL AS INTEREST
      *   MJD 001102 ADDED LOGIC FOR ACOUNTS PAYABLE (IPFILE).
      *              (WORLD PR#245)
      *   BLV 010125 COPY IN SPWSII, DELETE II-PATH FROM WORKING
      *              STORAGE (DOESN'T MAKE ANY DIFFERENCE, JUST
      *              CONFUSING)
      *   BAH 020104 ADDED VENDOR INFORMATION FIELDS FROM THE BOOK
      *   BAH 021217 NEW IRS CHANGES
      *   BLF 050728 ADDED ICFILE AND UPDATE OF ICFILE FOR 1099-C'S,
      *              WORLD, PR# 411
      *   BLF 051202 ADDED SPLIT-FILE-FG TO DETERMINE WHETHER TO SPLIT
      *              IM-FILE OUT INTO FILES THAT WILL EACH FIT ON A
      *              DISKETTE, OR LEAVE A 1 BIG FILE.  WORLD (2005
      *              TAX YEAR LAST YEAR FOR DISKETTE)A.  IF YOU ANSWER
      *              "N" TO SPLIT, FILE WILL JUST BE IRSTAX WITH NO
      *              SUFFIX.  ALSO, FIX THE RIRS.CMD FILE - IT HAD
      *              AN OLD A48 FILE PATH SO THE OLD IRSTAX FILES
      *              DID NOT GET REMOVED AT THE BEGINNING OF THIS
      *              PROCESS.
      *   BLF 060110 FIX ADDRESS ON VENDOR CONTACT TO CEPI DRIVE
      *   BAH/BLF 070117 CHANGED FOR REPORTING YEAR 2006, WORLD PR# 518
      *   BLF 080227 FIX "IN PROGRESS" MESSAGE LOCKING UP SCREEN IF
      *              RUNNING PIGGY-BACK, REGMGM
      *   BAH 110119 IM-A-RETURN-TYPE IS NOW 2 CHARACTERS, REMOVED THE FIELD
      *              ABOVE IT (IM-A-RED-STA-COMBO-CD) AND NOW RETURN-TYPE
      *              STARTS IN POSITION 26 INSTEAD OF 27 AND IS LEFT JUSTIFIED
      *              SPACE FILLED PR# 3718
      *   BLM 110329 FIXED AP RENTS (BUCKET 7) NOT PRINTING AMT ON REPORT,
      *              FIXED 1099C'S PRINTING AMT TWICE WITH SECOND LINE
      *              DESC OF "FORFEIT". REMOVED SPLIT FILE OPTION SINCE
      *              IRS NO LONGER TAKES DISKETTES.  ADDED DISPLAY OF
      *              INPUT FILES WITH SIZE, DATE & TIME SO YOU CAN SEE
      *              INPUT BEFORE ACCEPTING.  PL# 720
      *   CLS 110817 WORLD WANTS EXTRACT FILE THAT CONTAINS THE DATA FOR
      *              1099S. PRINT SHOP IS GOING TO CREATE THE FORM AND PRINT
      *              FROM THE EXTRACT. ADDED WITHIN THE MERGE-AP-B-REC
      *              SECTION  WORLD PR#750
      *   BLM 111027 CHANGE FORMAT OF WORLD PRINT SHOP FILE, WRITE CITY,
      *              STATE & ZIP AS SEPARATE FIELDS, ADD READ OF VENDOR
      *              TO GET 2 SEPARATE ADDRESS LINES, WORLD PR# 773
      *   BAH 150828 CHANGED CONTACT ADDRESS DUE TO MOVE, PR#4844
      *   BLM 161111 REPORT BUCKET 3, OTHER INCOME, FOR ACCTS PAYABLE 1099,
      *              WORLD PR# 1203A
      *   BAH 161122 REPORT AMOUNT IN BUCKET 6, OUTSTANDING MORTGAGE PRINICPLE
      *              AMOUNT CODE 6, REGENCY PR# 5214
      * KEC 2016.1223 CHANGED II-FILE FROM RELATIVE TO INDEXED.
      * BAH 2016.1230 AMT-INDICATORS AND PAYAMT NOW OCCUR 16 TIMES, NOT 12
      * BLM 2017.0123 FIX WS-AMT-INDICATORS FOR BUCKET 3 THAT I ADDED FOR AP
      *               1099 (PR#1203), MOVE "137" INSTEAD OF "17", 
      *               WORLD PR# 1235
      * BAH 2017.0926 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2017.1221 ADDED IM-B-NO-MORTGAGED-PROP, REGENCY PR#5332
      * BAH 2022.0111 AMT-INDICATORS, PAYAMT, IM-C-TOTALS NOW OCCUR 18
      *               TIMES, NOT 16                                   #1537
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBSP/SPFSIL.CPY".
       COPY "LIBSP/SPFSIM.CPY".
       COPY "LIBSP/SPFSIC.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBSP/SPFDIL.CPY".
       COPY "LIBSP/SPFDIM.CPY".
       COPY "LIBSP/SPFDIC.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/IRPREP.CBL S35 01/11/22-13:12:18 >".
       COPY "LIBSP/SPWSIL.CPY".
       COPY "LIBSP/SPWSIM.CPY".
       COPY "LIBSP/SP01II.CPY".
       COPY "LIBSP/SP01II_SQL.CPY".
       COPY "LIBSP/SPWSII.CPY".
       COPY "LIBSP/SPWSIC.CPY".

       01  REMOVE-PATH.
           03  REMOVE-PATH-BASE     PIC X(09).
           03  FILLER               PIC X(01)   VALUE "/".
           03  REMOVE-PATH-MACHINE  PIC X(02).
           03  FILLER               PIC X(8) VALUE "/SP/IRS*".



       01  IL-KEY               PIC 9(6)     VALUE 1.
       01  IM-KEY               PIC 9(6)     VALUE 0.
       01  IC-KEY               PIC 9(6)     VALUE 1.

       01  C-S-Z                PIC X(85).
       01  STATE                PIC X(2).
       01  ZIP                  PIC X(10).

       01  WS-ENV-FIL           PIC X(19).

       01  LINE-FD              PIC X VALUE X"0A".
       01  CARRAGE-RET          PIC X VALUE X"0D".
       01  SUB                  PIC 9(6)     COMP-3 VALUE 0.
       01  WS-NO-A              PIC 9(8)     COMP-3 VALUE 0.
       01  WS-AMT-TOTALS        OCCURS 18
                                PIC 9(13)V99 COMP-3 VALUE 0.
       01  WS-NO-PAYEES         PIC 9(7)     COMP-3 VALUE 0.
       01  WS-NO-RECS           PIC 9(7)     COMP-3 VALUE 0.
       01  WS-TOTAL-RECS        PIC 9(8)     COMP-3 VALUE 0.
       01  SPLIT-FILE-FG        PIC X        VALUE "N".
       01  RECORD-COUNT         PIC 9(6).
       01  TIME-WK.
           03  FILLER           PIC 99.
           03  TIME-HH          PIC 99.
           03  TIME-MM          PIC 99.
           03  TIME-SS          PIC 99.
           03  TIME-HU          PIC 99.
       01  TIME-WORK REDEFINES TIME-WK PIC 9(10).
       01  FILE-INFO-DISPLAY-BUF.
           03  DISPLAY-BUF      OCCURS 4.
               05  DISP-DESC    PIC X(20).
               05  FILLER       PIC XX.
               05  DISP-PATH.
                   07  DISP-FIL   PIC X(13).
                   07  DISP-FILE  PIC X(9).
               05  FILLER       PIC X.
               05  DISP-COUNT   PIC ZZZZZ9.
               05  FILLER       PIC XX.
               05  DISP-DATE    PIC 99/99/99.
               05  FILLER       PIC X.
               05  DISP-TIME.
                   07  DISP-HH  PIC 99.
                   07  DISP-COL PIC X.
                   07  DISP-MM  PIC 99.
                   07  FILLER   PIC X.
                   07  DISP-AP  PIC XX.
       01  FILE-INFO-DISPLAY-LIST.
           03  FILLER           PIC X(40) VALUE
               "FILEINFO1 FILEINFO2 FILEINFO3 FILEINFO4.".

      ******************************************************************
      * WS-RETURN-TYPE AND WS-INDICATORS DEFINED:
      *-----------------------------------------------------------------
      * RETURN TYPE "3 " = FORM 1098
      * AMOUNT INDICATORS (CODES):
      *   > 1 - MORTGAGE INTEREST RECEIVED FROM PAYER(S)/BORROWER(S)
      *     2 - POINTS PAID ON PURCHASES OF PRINCIPLE INTEREST.
      *     3 - REFUND (OR CREDIT) OF OVERPAID INTEREST.
      *     4 - MORTGAGE INSURANCE PREMIUMS
      *     5 - BLANK (FILER'S USE)
      *   > 6 - OUTSTANDING MORTGAGE PRINCIPLE
      *-----------------------------------------------------------------
      * RETURN TYPE "5 " = FORM 1099-C (CANCELLATION OF DEBT)
      * AMOUNT INDICATORS (CODES):
      *   > 2 - AMOUNT OF DEBT CANCELED
      *     3 - INTEREST, IF INCLUDED IN AMOUNT CODE 2
      *     7 - FAIR MARKET VALUE OF PROPERTY
      *-----------------------------------------------------------------
      * RETURN TYPE "6 " = FORM 1099-INT
      * AMOUNT INDICATORS (CODES):
      *   > 1 - INTEREST INCOME NOT INCLUDED IN AMOUNT CODE 3.
      *   > 2 - EARLY WITHDRAWL PENALTY.
      *     3 - INTEREST ON U.S. SAVINGS BONDS AND TREASURY OBLIGATIONS.
      *   > 4 - FEDERAL INCOME TAX WITHHELD (BACKUP WITHHOLDING).
      *     5 - FOREIGN TAX PAID.
      *-----------------------------------------------------------------
      * RETURN TYPE "A " = FORM 1099-MISC
      * AMOUNT INDICATORS (CODES):
      *   > 1 - RENTS.
      *     2 - ROYALTIES.
      *   > 3 - OTHER INCOME.
      *     4 - FEDERAL INCOME TAX WITHHELD (BACKUP WITHHOLDING ON
      *         PAYMENTS OF INDIAN GAMMING PROFITS.
      *     5 - FISHING BOAT PROCEEDS.
      *     6 - MEDICAL AND HEALTH CARE PAYMENTS.
      *   > 7 - NONEMPLOYEE COMPENSATION.
      *     8 - SUBSTITUTE PAYMENTS IN LIEU OF DIVIDENDS OR INTEREST.
      *     A - CROP INSURANCE PROCEEDS.
      *     B - EXCESS GOLDEN PARACHUTE PAYMENTS.
      *     C - GROSS PROCEEDS PAID TO AN ATTORNEY IN CONNECTION
      *         WITH LEGAL SERVICES.
      *
      *    NOTE: AS FAR AS I WAS TOLD, WORLD WILL ONLY DO 1 AND 7
      *-----------------------------------------------------------------
      * SOURCE: "SPECIFICATIONS FOR FILING FORMS 1098, 1099, 5498, AND
      *          AND W-2G MAGNETICALLY OR ELECTRONICALLY",
      *          REV.PROC. 98-35, PP 24-32.
      ******************************************************************
       01  WS-RETURN-TYPE       PIC XX               VALUE SPACE.
       01  WS-AMT-INDICATORS    PIC X(18)            VALUE SPACES.

       01  HOLD-YEAR            PIC 9(4) VALUE 0.

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  ACTION          PIC X.

       01  MESS-LIST           PIC X(5) VALUE "MESS.".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(41)    VALUE
           " - - - - IRS MEDIA REPORT - - - - ".
           03  FILLER           PIC X(45)    VALUE " ".

       01  HEAD3.
           03  FILLER           PIC X(20) VALUE "  ACCOUNT NUMBER    ".
           03  FILLER           PIC X(7)  VALUE SPACE.
           03  FILLER           PIC X(20) VALUE "NAME / ADDRESS INFO ".
           03  FILLER           PIC X(11) VALUE SPACE.
           03  FILLER           PIC X(7)  VALUE "PAY YR.".
           03  FILLER           PIC X(14) VALUE "  NAME CTRL.  ".
           03  FILLER           PIC X(18) VALUE "TIN TYPE   NUMBER ".
           03  FILLER           PIC X(25)
               VALUE "        REPORTED AMOUNT  ".
           03  FILLER           PIC X(20) VALUE "  FOREIGN".

       01  DETAIL-LINE          PIC X(132)   VALUE SPACE.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-ACCT           PIC X(20).
           03  D-NAME           PIC X(40).
           03  D-PAY-YEAR       PIC 9(4).
           03  FILLER           PIC X(7).
           03  D-NAME-CONTROL   PIC X(4).
           03  FILLER           PIC X(8).
           03  D-TIN-TYPE       PIC X.
           03  FILLER           PIC X(6).
           03  D-TIN            PIC 999/99/9999.
           03  D-EMPIDX REDEFINES D-TIN.
               05  FILLER       PIC X.
               05  D-EMPID      PIC 99/9999999.
           03  FILLER           PIC X(5).
           03  D-AMT            PIC Z(6)9.99-.
           03  FILLER           PIC X(12).
           03  D-FOREIGN        PIC X.

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(20).
           03  D-ADDR           PIC X(40).
           03  D-CSZ            PIC X(46).
           03  D-AMT2           PIC Z(6)9.99-.
           03  D-AMT2-DESC      PIC X(10).

       01  HOLD-TOTAL-LINE.
           03  HOLD-TITLE       PIC X(30)    OCCURS 18.
           03  HOLD-AMOUNT      PIC 9(13)V99 OCCURS 18.

       01  SUB-TOTAL-LINE.
           03  S-TITLE          PIC X(30).
           03  S-AMOUNT         PIC $,$$$,$$$,$$$,$$$.99.
           03  FILLER           PIC X(83) VALUE SPACE.

       01  SUB-TOTAL-LINE-2.
           03  S-TITLE-2        PIC X(30).
           03  S-AMOUNT-2       PIC ZZZ,ZZ9.
           03  FILLER           PIC X(97) VALUE SPACE.

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       01  IRPREP-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/IRPREP_DEF.CPY".
       COPY "LIBSP/IRPREP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/IRPREP_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               IL-FILE IM-FILE PR-FILE IC-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-II-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               IL-FILE IM-FILE IC-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************
       MAIN-MODULE SECTION.
      ******************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "IRPREP" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS IRPREP-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE EXT-FILPATH TO WS-ENV-FIL.

      D    STOP MESS.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM PREPARE-FILES.

           GO TO END-PROGRAM.

       EXIT-MAIN-MODULE.
           EXIT.

      *******************************************************************
       PREPARE-FILES SECTION.
      *******************************************************************

           IF EXT-ROUTE-BUF = "00"
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE " PREPARATION OF IRS FILES IN PROGRESS.."
                                      TO WR-BUF
                 MOVE MESS-LIST TO WR-LIST
                 PERFORM CALL-WRFORM.

           MOVE 01 TO II-KEY.
           PERFORM READ-II-FILE.
           IF IO-FG NOT = 0
              MOVE "MUST FIRST CREATE AN IRS IDENTIFICATION RECORD"
                 TO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.

      * FIRST WILL CONTAIN "T" TRANSMITTER RECORD

           PERFORM BUILD-REC-T.

           PERFORM READ-IL-FILE.
           IF IO-FG = 0
      * 3 = 1098
              MOVE "3 "                 TO WS-RETURN-TYPE
              MOVE "16                " TO WS-AMT-INDICATORS

              MOVE SPACES TO IM-REC

              MOVE IL-B-PAY-YEAR TO IM-A-PAY-YEAR
              PERFORM BUILD-REC-A
              PERFORM MERGE-LOAN-B-RECS UNTIL IO-FG NOT = 0

              MOVE "INTEREST REPORTED FROM LOANS :" TO HOLD-TITLE(1)
              MOVE SPACES                           TO HOLD-TITLE(2)
              MOVE SPACES                           TO HOLD-TITLE(3)
              MOVE SPACES                           TO HOLD-TITLE(4)
              MOVE SPACES                           TO HOLD-TITLE(5)
              MOVE "OUTSTANDING MORTGAGE PRINCIPL:" TO HOLD-TITLE(6)
              MOVE SPACES                           TO HOLD-TITLE(7)
              MOVE SPACES                           TO HOLD-TITLE(8)
              MOVE SPACES                           TO HOLD-TITLE(9)
              MOVE SPACES                           TO HOLD-TITLE(10)
              MOVE SPACES                           TO HOLD-TITLE(11)
              MOVE SPACES                           TO HOLD-TITLE(12)
              MOVE SPACES                           TO HOLD-TITLE(13)
              MOVE SPACES                           TO HOLD-TITLE(14)
              MOVE SPACES                           TO HOLD-TITLE(15)
              MOVE SPACES                           TO HOLD-TITLE(16)
              MOVE SPACES                           TO HOLD-TITLE(17)
              MOVE SPACES                           TO HOLD-TITLE(18)
              PERFORM BUILD-REC-C
              MOVE 99 TO LN-COUNT.

           PERFORM READ-IC-FILE.
           IF IO-FG = 0
              MOVE "5 " TO WS-RETURN-TYPE
              MOVE "2                 " TO WS-AMT-INDICATORS
              MOVE SPACES TO IM-REC
              MOVE IC-B-PAY-YEAR TO IM-A-PAY-YEAR
              PERFORM BUILD-REC-A
              PERFORM MERGE-LOAN-1099C-B-RECS
                 UNTIL IO-FG NOT = 0
              MOVE SPACES                           TO HOLD-TITLE(1)
              MOVE "DEBTS CANCELLED FROM LOANS   :" TO HOLD-TITLE(2)
              MOVE SPACES                           TO HOLD-TITLE(3)
              MOVE SPACES                           TO HOLD-TITLE(4)
              MOVE SPACES                           TO HOLD-TITLE(5)
              MOVE SPACES                           TO HOLD-TITLE(6)
              MOVE SPACES                           TO HOLD-TITLE(7)
              MOVE SPACES                           TO HOLD-TITLE(8)
              MOVE SPACES                           TO HOLD-TITLE(9)
              MOVE SPACES                           TO HOLD-TITLE(10)
              MOVE SPACES                           TO HOLD-TITLE(11)
              MOVE SPACES                           TO HOLD-TITLE(12)
              MOVE SPACES                           TO HOLD-TITLE(13)
              MOVE SPACES                           TO HOLD-TITLE(14)
              MOVE SPACES                           TO HOLD-TITLE(15)
              MOVE SPACES                           TO HOLD-TITLE(16)
              MOVE SPACES                           TO HOLD-TITLE(17)
              MOVE SPACES                           TO HOLD-TITLE(18)
              PERFORM BUILD-REC-C.

           PERFORM BUILD-REC-F.

           PERFORM REBUILD-REC-T.

       EXIT-PREPARE-FILES.
           EXIT.

      *******************************************************************
       INITIALIZATION SECTION.
      *******************************************************************

           MOVE SPACES TO FILE-INFO-DISPLAY-BUF.

           MOVE 1 TO SUB.

           PERFORM LOAD-IL-FILE.
           MOVE IL-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE "SP/ILFILE"    TO DISP-FILE(SUB)
              MOVE "LOANS (1098)" TO DISP-DESC(SUB)
              PERFORM FORMAT-FILE-INFO
              ADD 1 TO SUB.

           PERFORM LOAD-IC-FILE.
           MOVE IC-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE "SP/ICFILE"            TO DISP-FILE(SUB)
              MOVE "DEBT CANCEL (1099-C)" TO DISP-DESC(SUB)
              PERFORM FORMAT-FILE-INFO
              ADD 1 TO SUB.

           MOVE FILE-INFO-DISPLAY-BUF TO WR-BUF.
           MOVE FILE-INFO-DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SETRP.
           MOVE IRPREP-QUEUE-POS TO Q-POS.
           MOVE IRPREP-COPIES-POS TO C-POS.
           MOVE IRPREP-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.

           MOVE SPACE TO ERRCD.
       ACCPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ACCPT.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "N" GO TO ENDIT.
           IF ACTION NOT = "Y" AND NOT = "T"
              GO TO ACCPT.

           MOVE EXT-FILPATH-BASE    TO REMOVE-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO REMOVE-PATH-MACHINE.
           MOVE "RM"                       TO RM-BUF.
           CALL "C$TOLOWER" USING RM-BUF, VALUE 2.
           MOVE REMOVE-PATH         TO RM-FILE.
           PERFORM SYSTEM-CALL.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-IL-FILE.
           PERFORM OPEN-IC-FILE.
           PERFORM OPEN-II-FILE.
           IF SPLIT-FILE-FG = "Y"
              MOVE 1 TO IM-PATH-FILE-COUNT
           ELSE
              MOVE SPACES TO IM-PATH-SUFFIX.
           PERFORM OPEN-IM-FILE-OUTPUT.

           MOVE EXT-CONAME-USER-ID TO H-USERID.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ****************************************
       REBUILD-REC-T SECTION.
      ****************************************

      * OPEN FIRST FILE AGAIN IRSTAX.001, AND REWRITE WITH RECORD COUNT

           PERFORM CLOSE-IM-FILE.

           IF SPLIT-FILE-FG = "Y"
              MOVE 1 TO IM-PATH-FILE-COUNT
           ELSE
              MOVE SPACES TO IM-PATH-SUFFIX.

           MOVE 1 TO IM-KEY.
           PERFORM OPEN-IM-FILE.
           PERFORM READ-IM-FILE.

           MOVE HOLD-YEAR TO IM-T-PAY-YEAR.
           MOVE WS-NO-PAYEES TO IM-T-NO-B-RECS.

           PERFORM REWRITE-IM-FILE.

      *****************************************
       BUILD-REC-T SECTION.
      *****************************************

      *  TRANSMITTER "T" RECORD - ALWAYS THE FIRST RECORD

           MOVE SPACES TO IM-T-REC.

           MOVE "T" TO IM-TYPE.
      * IM-T-PAY-YEAR GETS SET IN REBUILD-T, DONT HAVE YEAR YET
           MOVE 0 TO IM-T-PAY-YEAR.

      * ENTER "P" ONLY IF REPORTING PRIOR YEAR DATA; OTHERWISE, ENTER
      * BLANK
           MOVE SPACES TO IM-T-PRIOR-YEAR.

      * TRANSMITTER'S TIN - REQUIRED
           MOVE II-EMPLOYER-ID TO IM-T-FED-EIN.
      * TRANSMITTER CONTROL CODE - REQUIRED
           MOVE II-TRANS-CONTROL-CODE TO IM-T-TCC.

           IF ACTION = "T"
              MOVE "T" TO IM-T-TEST.
           IF II-FOREIGN-CORP = "Y"
              MOVE "1" TO IM-T-FOREIGN-ENTITY.

      * TRANSMITTER NAME - REQUIRED, ENTER THE NAME OF THE TRANSMITTER IN
      * THE MANNER WHICH IT IS USED IN NORMAL BUSINESS. LEFT JUSTIFY
           MOVE II-PAYER-NAME TO IM-T-TRANS-NAME.
           MOVE II-PAYER-NAME TO IM-T-COMPANY-NAME.
      * MUST BE SAME ADDRESS AS IN BOX 5 OF FORM 4804.
           MOVE II-PAYER-ADDRESS TO IM-T-COMPANY-ADDR.
           MOVE II-PAYER-CITY TO IM-T-COMPANY-CITY.
           MOVE II-PAYER-STATE TO IM-T-COMPANY-STATE.
           MOVE II-PAYER-ZIP TO IM-T-COMPANY-ZIPCODE.

      * TOTAL NUMBER OF PAYEES "B" RECORDS, GETS UPDATED IN REWRITE-T
           MOVE 0 TO IM-T-NO-B-RECS.

           MOVE II-CONTACT TO IM-T-CONTACT.
           MOVE II-CONTACT-EMAIL TO IM-T-CONTACT-EMAIL.
           MOVE II-PAYER-PHONE TO IM-T-CONTACT-PHONE.
           MOVE II-PAYER-EXT TO IM-T-CONTACT-EXT.

      * REQUIRED. ENTER THE NUMBER OF THE RECORD AS IT APPEARS WITHIN YOUR
      * FILE. THE "T" RECORD WILL ALWAYS BE 1.
           MOVE 1 TO IM-T-RECORD-SEQ-NO.

           MOVE "V" TO IM-T-VENDOR-FLAG.
           MOVE "PARADATA FINANCIAL SYSTEMS" TO IM-T-VENDOR-NAME.
           MOVE "15933 CLAYTON ROAD, SUITE 300" TO IM-T-VENDOR-ADDR.
           MOVE "BALLWIN" TO IM-T-VENDOR-CITY.
           MOVE "MO" TO IM-T-VENDOR-STATE.
           MOVE "63011" TO IM-T-VENDOR-ZIP.
           MOVE "KEVIN GROSS" TO IM-T-VENDOR-CONTACT.
           MOVE "6362203120" TO IM-T-VENDOR-PHONE.

           MOVE CARRAGE-RET TO IM-CARR-RTN.
           MOVE LINE-FD TO IM-LINE-FEED.

           ADD 1 TO IM-KEY WS-NO-RECS WS-TOTAL-RECS.
           PERFORM WRITE-IM-FILE.

      ***********************************************
       BUILD-REC-A SECTION.
      ***********************************************

      * PAYER "A" RECORD

           ADD 1 TO WS-NO-A.

           MOVE "A"                TO IM-TYPE.

      * PAYER'S TAXPAYER IDENTIFICATION NUMBER (TIN)
           MOVE II-EMPLOYER-ID     TO IM-A-FED-EIN.
           MOVE II-NAME-CONTROL-NO TO IM-A-NAME-CONTROL-NO.

           MOVE WS-RETURN-TYPE     TO IM-A-RETURN-TYPE.
           MOVE WS-AMT-INDICATORS  TO IM-A-AMT-INDICATORS.

      * FIELD WENT AWAY FOR 2016, NOT SURE WHAT TO DO WITH IT
      *    MOVE "1"                TO IM-A-ORIGINAL.
      *    IF ACTION = "R"
      *       MOVE " "             TO IM-A-ORIGINAL
      *       MOVE "1"             TO IM-A-REPLACEMENT.
           IF II-FOREIGN-CORP = "Y"
              MOVE "1" TO IM-A-FOREIGN-ENTITY.

           MOVE II-PAYER-NAME    TO IM-A-PAYER-NAME.
           MOVE II-SECOND-PAYER  TO IM-A-2ND-PAYER-NAME.
           IF II-TRANSFER-AGENT-FG = "Y"
              MOVE "1"           TO IM-A-TRANSFER-AGENT-CD
           ELSE
              MOVE "0"           TO IM-A-TRANSFER-AGENT-CD.
           MOVE II-PAYER-ADDRESS TO IM-A-PAYER-ADDR.
           MOVE II-PAYER-CITY    TO IM-A-PAYER-CITY.
           MOVE II-PAYER-STATE   TO IM-A-PAYER-STATE.
           MOVE II-PAYER-ZIP     TO IM-A-PAYER-ZIP.
           MOVE II-PAYER-PHONE   TO IM-A-PAYER-PHONE.
           MOVE II-PAYER-EXT     TO IM-A-PAYER-EXT.

           ADD 1 TO IM-KEY WS-NO-RECS WS-TOTAL-RECS.
           MOVE WS-TOTAL-RECS TO IM-A-RECORD-SEQ-NO.

           MOVE CARRAGE-RET TO IM-CARR-RTN.
           MOVE LINE-FD TO IM-LINE-FEED.

           PERFORM WRITE-IM-FILE.

       EXIT-BUILD-REC-A.
           EXIT.

      *****************************************
       MERGE-LOAN-B-RECS SECTION.
      *****************************************

           MOVE 0 TO WS-NO-PAYEES
                     WS-AMT-TOTALS(01) WS-AMT-TOTALS(02)
                     WS-AMT-TOTALS(03) WS-AMT-TOTALS(04)
                     WS-AMT-TOTALS(05) WS-AMT-TOTALS(06)
                     WS-AMT-TOTALS(07) WS-AMT-TOTALS(08)
                     WS-AMT-TOTALS(09) WS-AMT-TOTALS(10)
                     WS-AMT-TOTALS(11) WS-AMT-TOTALS(12)
                     WS-AMT-TOTALS(13) WS-AMT-TOTALS(14)
                     WS-AMT-TOTALS(15) WS-AMT-TOTALS(16)
                     WS-AMT-TOTALS(17) WS-AMT-TOTALS(18).

       LOOP-REC-L-B.

           MOVE IL-REC TO IM-REC.

           ADD 1 TO WS-NO-PAYEES IM-KEY IL-KEY WS-NO-RECS WS-TOTAL-RECS.

      * "1" MORTGAGE INTEREST RECEIVED FROM BORROWER
           ADD IL-B-PAYAMT(1)  TO WS-AMT-TOTALS(1).

      * "6" OUTSTANDING MORTGAGE PRINICPLE
           ADD IL-B-PAYAMT(6)  TO WS-AMT-TOTALS(6).

           MOVE WS-TOTAL-RECS TO IM-B-RECORD-SEQ-NO.
           MOVE CARRAGE-RET   TO IM-CARR-RTN.
           MOVE LINE-FD       TO IM-LINE-FEED.

           PERFORM WRITE-IM-FILE.
           PERFORM PRINT-REPORT.

           IF SPLIT-FILE-FG = "Y"
              IF WS-NO-RECS > 1860
                 MOVE 0 TO WS-NO-RECS
                 PERFORM NEXT-FILE-PLEASE.

           PERFORM READ-IL-FILE.
           IF IO-FG = 0
              GO TO LOOP-REC-L-B.

       EXIT-MERGE-LOAN-B-RECS.
           EXIT.

      **************************************************
       MERGE-LOAN-1099C-B-RECS SECTION.
      **************************************************

      * MERGE LOAN 1099-C RECORDS

           MOVE 0 TO WS-NO-PAYEES
                     WS-AMT-TOTALS(01) WS-AMT-TOTALS(02)
                     WS-AMT-TOTALS(03) WS-AMT-TOTALS(04)
                     WS-AMT-TOTALS(05) WS-AMT-TOTALS(06)
                     WS-AMT-TOTALS(07) WS-AMT-TOTALS(08)
                     WS-AMT-TOTALS(09) WS-AMT-TOTALS(10)
                     WS-AMT-TOTALS(11) WS-AMT-TOTALS(12)
                     WS-AMT-TOTALS(13) WS-AMT-TOTALS(14)
                     WS-AMT-TOTALS(15) WS-AMT-TOTALS(16)
                     WS-AMT-TOTALS(17) WS-AMT-TOTALS(18).
 
       LOOP-REC-C-B.
           MOVE IC-REC TO IM-REC.
           ADD 1 TO WS-NO-PAYEES IM-KEY IC-KEY WS-NO-RECS WS-TOTAL-RECS.

      *    AMT(2) IS AMT OF DEBT CANCELLED:
           ADD IC-B-PAYAMT(2)  TO WS-AMT-TOTALS(2).

      *    AMT(3) IS INTEREST, IF INCLUDED IN AMT(2),
      *    WE ARE NOT REPORTING FOR NOW
      *--->ADD IC-B-PAYAMT(3)  TO WS-AMT-TOTALS(3).

      *    AMT(7) IS FAIR MARKET VALUE OF PROPERTY;
      *    WE ARE NOT REPORTING FOR NOW - AMT(7) IS ONLY
      *    USED IF COMBINED 1099-C & 1099-A ARE BEING FILED!
      *--->ADD IC-B-PAYAMT(7)  TO WS-AMT-TOTALS(7).

           MOVE WS-TOTAL-RECS TO IM-B-RECORD-SEQ-NO.
           MOVE CARRAGE-RET TO IM-CARR-RTN.
           MOVE LINE-FD TO IM-LINE-FEED.

           PERFORM WRITE-IM-FILE.
           PERFORM PRINT-REPORT.
           IF SPLIT-FILE-FG = "Y"
              IF WS-NO-RECS > 1860
                 MOVE 0 TO WS-NO-RECS
                 PERFORM NEXT-FILE-PLEASE.
           PERFORM READ-IC-FILE.
           IF IO-FG = 0
              GO TO LOOP-REC-C-B.

       EXIT-MERGE-LOAN-1099C-B-RECS.
           EXIT.

      **************************************************
       BUILD-REC-C SECTION.
      **************************************************

      * END OF PAYER "C" RECORD - TOTAL NUMBER OF PAYEES AND THE TOTAL
      * OF THE PAYMENT AMOUNT FIELDS FILED FOR EACH PAYER
      * "C" RECORD MUST FOLLOW THE LAST "B" RECORD FOR EACH TYPE OF RETURN.

           MOVE SPACE TO IM-REC.

           MOVE "C" TO IM-TYPE.

           MOVE WS-NO-PAYEES TO IM-C-NO-PAYEES S-AMOUNT-2.

           MOVE "NUMBER OF PAYEE RECORDS: " TO S-TITLE-2.
           MOVE WS-AMT-TOTALS(1)  TO IM-C-TOTAL(1)  HOLD-AMOUNT(1).
           MOVE WS-AMT-TOTALS(2)  TO IM-C-TOTAL(2)  HOLD-AMOUNT(2).
           MOVE WS-AMT-TOTALS(3)  TO IM-C-TOTAL(3)  HOLD-AMOUNT(3).
           MOVE WS-AMT-TOTALS(4)  TO IM-C-TOTAL(4)  HOLD-AMOUNT(4).
           MOVE WS-AMT-TOTALS(5)  TO IM-C-TOTAL(5)  HOLD-AMOUNT(5).
           MOVE WS-AMT-TOTALS(6)  TO IM-C-TOTAL(6)  HOLD-AMOUNT(6).
           MOVE WS-AMT-TOTALS(7)  TO IM-C-TOTAL(7)  HOLD-AMOUNT(7).
           MOVE WS-AMT-TOTALS(8)  TO IM-C-TOTAL(8)  HOLD-AMOUNT(8).
           MOVE WS-AMT-TOTALS(9)  TO IM-C-TOTAL(9)  HOLD-AMOUNT(9).
           MOVE WS-AMT-TOTALS(10) TO IM-C-TOTAL(10) HOLD-AMOUNT(10).
           MOVE WS-AMT-TOTALS(11) TO IM-C-TOTAL(11) HOLD-AMOUNT(11).
           MOVE WS-AMT-TOTALS(12) TO IM-C-TOTAL(12) HOLD-AMOUNT(12).
           MOVE WS-AMT-TOTALS(13) TO IM-C-TOTAL(13) HOLD-AMOUNT(13).
           MOVE WS-AMT-TOTALS(14) TO IM-C-TOTAL(14) HOLD-AMOUNT(14).
           MOVE WS-AMT-TOTALS(15) TO IM-C-TOTAL(15) HOLD-AMOUNT(15).
           MOVE WS-AMT-TOTALS(16) TO IM-C-TOTAL(16) HOLD-AMOUNT(16).
           MOVE WS-AMT-TOTALS(17) TO IM-C-TOTAL(17) HOLD-AMOUNT(17).
           MOVE WS-AMT-TOTALS(18) TO IM-C-TOTAL(18) HOLD-AMOUNT(18).

           ADD 1 TO IM-KEY WS-TOTAL-RECS.

           MOVE WS-TOTAL-RECS TO IM-C-RECORD-SEQ-NO.

           MOVE CARRAGE-RET TO IM-CARR-RTN.
           MOVE LINE-FD TO IM-LINE-FEED.

           PERFORM WRITE-IM-FILE.

           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE SUB-TOTAL-LINE-2 TO PR-REC.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HOLD-TITLE(1) TO S-TITLE.
           MOVE HOLD-AMOUNT(1) TO S-AMOUNT.
           MOVE SUB-TOTAL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

      *    ------------------------------------------------------------
      *    IF OTHER AMOUNTS ARE BEING ACCUMULATED, THEN PRINT THEM HERE
      *    ------------------------------------------------------------
           MOVE SPACES TO PR-REC.
           PERFORM VARYING SUB FROM 2 BY 1 UNTIL SUB > 18
              IF HOLD-TITLE(SUB) NOT = SPACES
                 MOVE HOLD-TITLE(SUB)  TO S-TITLE
                 MOVE HOLD-AMOUNT(SUB) TO S-AMOUNT
                 MOVE SUB-TOTAL-LINE TO PR-REC
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-PR-REC
              END-IF
           END-PERFORM.

           MOVE SPACES TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD 4 TO LN-COUNT.

       EXIT-BUILD-REC-C.
           EXIT.

      **************************************************
       BUILD-REC-F SECTION.
      **************************************************

      * END OF TRANSMISSION "F" RECORD - IS A SUMMARY OF THE NUMBER OF
      * PAYERS IN THE ENTIRE FILE.

           MOVE SPACES TO IM-REC.

           MOVE "F" TO IM-TYPE.

           MOVE "000000000000000000000" TO IM-F-ZEROS.

           MOVE WS-NO-A TO IM-F-NO-A-RECS.

           ADD 1 TO IM-KEY WS-TOTAL-RECS.

           MOVE WS-TOTAL-RECS TO IM-F-RECORD-SEQ-NO.
           MOVE CARRAGE-RET TO IM-CARR-RTN.
           MOVE LINE-FD TO IM-LINE-FEED.
           PERFORM WRITE-IM-FILE.

       EXIT-BUILD-REC-F.
           EXIT.

      ************************************
       PRINT-REPORT SECTION.
      ************************************
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE SPACE TO DETAIL-LINE.

           MOVE IM-B-ACCTNO       TO D-ACCT.
           MOVE IM-B-PAYEE-NAME   TO D-NAME.
           MOVE IM-B-PAY-YEAR     TO D-PAY-YEAR HOLD-YEAR.
           MOVE IM-B-NAME-CONTROL TO D-NAME-CONTROL.
           MOVE IM-B-TIN-TYPE-CD  TO D-TIN-TYPE.
      * HIGH ORDER 9 INDICATES EMPLOYER ID#
           IF IM-B-TIN-TYPE-CD = "2"
              MOVE IM-B-TIN       TO D-TIN
              INSPECT D-TIN REPLACING ALL "/" BY "-"
           ELSE
              MOVE IM-B-TIN       TO D-EMPID
              INSPECT D-EMPID REPLACING ALL "/" BY "-".

           MOVE IM-B-PAYAMT(1) TO D-AMT.


      * RETURN TYPE 5 IS 1099C, AMT IS IN BUCKET 2:
           IF WS-RETURN-TYPE = "5 "
              MOVE IM-B-PAYAMT(2) TO D-AMT2
              MOVE "CANC DEBT" TO D-AMT2-DESC.

      * RETURN TYPE A IS 1099MISC, AMT IS IN BUCKET 1, 3 OR 7:
           IF WS-RETURN-TYPE = "A "
              IF IM-B-PAYAMT(1) NOT = 0
                 MOVE IM-B-PAYAMT(1) TO D-AMT2
                 MOVE "RENTS " TO D-AMT2-DESC
              ELSE
              IF IM-B-PAYAMT(3) NOT = 0
                 MOVE IM-B-PAYAMT(3) TO D-AMT2
                 MOVE "OTH INCOME" TO D-AMT2-DESC
              ELSE
              IF IM-B-PAYAMT(7) NOT = 0
                 MOVE IM-B-PAYAMT(7) TO D-AMT2
                 MOVE "NONEMP COM" TO D-AMT2-DESC.

           MOVE IM-B-FOREIGN-CD TO D-FOREIGN.
           MOVE DETAIL-LINE TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACE TO DETAIL-LINE.
           MOVE IM-B-PAYEE-ADDR TO D-ADDR.
           MOVE IM-B-PAYEE-CITY TO D-CSZ.
           INSPECT D-CSZ REPLACING FIRST "     " BY " ++ +".
           INSPECT D-CSZ REPLACING FIRST "++" BY IM-B-PAYEE-STATE.
           INSPECT D-CSZ REPLACING FIRST "+        "
                                              BY IM-B-PAYEE-ZIP.

      * RETURN TYPE 3 IS 1098, AMT6 IS MORTGAGE PRINCIPLE BALANCE
           IF WS-RETURN-TYPE = "3 "
              MOVE IM-B-PAYAMT(6) TO D-AMT2
              MOVE "MTG BALANC" TO D-AMT2-DESC
              MOVE IM-B-PROPERTY-SECURING TO D-FOREIGN.

      * RETURN TYPE 6 IS 1099-INT (NOTES), AMT IS IN BUCKET 1 BUT
      * FORFEITS ARE IN BUCKET 2 & WITHHELD IS BUCKET 4:
           IF WS-RETURN-TYPE = "6 "
              IF IM-B-PAYAMT(2) NOT = 0
                 MOVE IM-B-PAYAMT(2) TO D-AMT2
                 MOVE "FORFEIT" TO D-AMT2-DESC
              ELSE
                 IF IM-B-PAYAMT(4) NOT = 0
                    MOVE IM-B-PAYAMT(4) TO D-AMT2
                    MOVE "WITHHELD" TO D-AMT2-DESC.

           MOVE DETAIL-LINE TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF WS-RETURN-TYPE = "6 "
              IF IM-B-PAYAMT(2) NOT = 0
                 IF IM-B-PAYAMT(4) NOT = 0
                    MOVE SPACES TO DETAIL-LINE
                    MOVE IM-B-PAYAMT(4) TO D-AMT2
                    MOVE "WITHHELD" TO D-AMT2-DESC
                    MOVE DETAIL-LINE TO PR-REC
                    MOVE 1 TO LN-FEED
                    ADD 1 TO LN-COUNT
                    PERFORM WRITE-PR-REC.

           ADD 3 TO LN-COUNT.

       EXIT-PRINT-REPORT.
           EXIT.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-CONAME TO H-NAME.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           MOVE 5 TO LN-COUNT.
 
      **********************************************
       FORMAT-FILE-INFO SECTION.
      **********************************************
           MOVE WS-ENV-FIL       TO DISP-FIL(SUB).
           COMPUTE RECORD-COUNT = ACCESS-SIZE / 750.
           MOVE RECORD-COUNT     TO DISP-COUNT(SUB).
           MOVE ACCESS-FILE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO DISP-DATE(SUB).
           MOVE ACCESS-FILE-TIME TO TIME-WORK.
           IF TIME-HH <= 11
              MOVE "AM"        TO DISP-AP(SUB)
           ELSE
              MOVE "PM"        TO DISP-AP(SUB).
           IF TIME-HH >= 13
              SUBTRACT 12 FROM TIME-HH.
           MOVE TIME-HH        TO DISP-HH(SUB).
           MOVE ":"            TO DISP-COL(SUB).
           MOVE TIME-MM        TO DISP-MM(SUB).

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO IRPREP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IRPREP-SYSDATE.
           DISPLAY IRPREP-SCREEN UPON IRPREP-WINDOW-HANDLE.
           MOVE IRPREP-FORM-FIELDS TO WR-BUF.
           MOVE IRPREP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE IRPREP-HELP--FILE TO HELP-LINK-FILE.
           MOVE IRPREP-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/IRPREP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

     *****************************
       MISC-ROUTINE SECTION.
     *****************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".

       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.

       NEXT-FILE-PLEASE.
           PERFORM CLOSE-IM-FILE.
           ADD 1 TO IM-PATH-FILE-COUNT.
           MOVE 0 TO IM-KEY.
           PERFORM OPEN-IM-FILE-OUTPUT.

      **************************************
       IO-ROUTINE SECTION.
      **************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPIIGS_SQL.CPY".
       COPY "LIBSP/SPIIRN.CPY".
       COPY "LIBSP/SPILI.CPY".
       COPY "LIBSP/SPIMI.CPY".
       COPY "LIBSP/SPICI.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *************************************
       END-PROGRAM SECTION.
      *************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "SP/IRMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY IRPREP-WINDOW-HANDLE.
           EXIT PROGRAM.
