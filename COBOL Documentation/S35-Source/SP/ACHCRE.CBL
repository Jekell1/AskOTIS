       IDENTIFICATION DIVISION.
       PROGRAM-ID.       ACHCRE.
       DATE-WRITTEN.   02/05/94.
      *****************************************************************
      *                      (SP)
      *
      * DESCRIPTION:  CREATES /USR/DATA/R1/SP/ACFILE
      *               TRANSMISSION OF DEPOSIT INFORMATION IN ACH FORMAT.
      *
      * NOTE: CHANGES FOR CAROLINA FIRST, THEY WILL NOW FTP FILE
      *       & TRANSMIT FROM WEBSITE.  SINCE NOT RENAMING FILE
      *       AFTER TRANSMISSION, WILL CONTINUE TO CREATE FILE IN
      *       SP WITH NAMING CONVENTION OF ACFILE.YYMMDD.R?
      *       IN ADDITIONAL, FROM R2 WE'LL REMOTE COPY FILE TO
      *       /USR/DATA/R1/FTP/ACH/WORLD-R?-YYMMDD.  FILES WILL BE
      *       CLEARED FROM THIS DIRECTORY EACH TIME THE
      *       EXTRACTION IS RUN, SO THERE SHOULDN'T BE TOO MUCH
      *       CONFUSION WHEN FTP'ING THE FILE TO THE BANK.
      *                                       9/29/03  BARB
      *
      *
      *     NOTE (TO WHOM IT MAY CONCERN):
      *     IF MODEM PROBLEM AT WORLD, FOR EX RISC 2 MODEM IS DOWN:
      *     FILE NAMES WILL BE FROM PRIOR BUSINESS DAY:  IF TODAY IS
      *     10/22/99, FILE WILL BE /USR/DATA/R1/SP/ACFILE.991021.
      *     IN THIS EXAMPLE, YOU WILL ALSO NEED TO TRANSMIT NATIONS
      *     CHECK REC (POSITIVE PAY) FROM RISC1; THIS PROCEDURE IS
      *     IDENTICAL TO DEPOSIT EXCEPT THE FILE NAMES ARE
      *     NB INSTEAD OF AC, AND THERE IS NO TRANSMISSION HEADER
      *     SO YOU DON'T HAVE TO MOVE IN A DIFFERENT LOGIN HEADER.
      *
      *     TELL ANGELA TO COMPLETE PROCESS ON GOOD MACHINE (RISC1
      *     IN THIS EXAMPLE), INCLUDING TRANSMITTING FILE. TELL HER
      *     TO PREPARE FILE ON BAD MACHINE (RISC2) AND THEN CALL YOU.
      *     FIRST MOVE FILE ON RISC1:
      *        CD /USR/DATA/R1/SP
      *        MV ACSENT.991021 ACSENT.991021.R1
      *     CHANGE FILE ACCESSES:
      *        CHMOD 777 *991021*
      *        CHGRP STAFF *991021*
      *        CHOWN INSTALL *991021*
      *     THEN COPY TRANSMISSION HEADER FOR RISC2 INTO HEADER:
      *        CD /USR/DATA/R1/SP
      *        CP ACHEAD.R2 ACHEAD
      *     CALL ANGELA & TELL HER TO TRANSMIT FILES & CALL YOU BACK.
      *     AFTER TRANSMISSION IS DONE:
      *     ****IMPORTANT*** COPY TRANSMISSION HEADER FOR RISC1 BACK IN:
      *        CD /USR/DATA/R1/SP
      *        CP ACHEAD.R1 ACHEAD
      *     GO TO RISC2 & MOVE FILE TO SENT STATUS:
      *        CD /USR/DATA/R1/SP
      *        MV ACFILE.991021 ACSENT.991021
      *        MV NBFILE.991021 NBSENT.991021
      *     I USUALLY COPY BACK THE FILE I SAVED AT THE BEGINNING OF
      *     YOU WILL ALSO NEED TO DO THIS FOR THEIR NATIONS BANK
      *     CHECK REC (THIS IS ONLY A RISC2 TRANSMISSION, SO YOU
      *     ONLY HAVE TO MESS WITH IT IF RISC2 MODEM IS DOWN).  IF
      *     YOU DO NEED TO DO NATIONS ON RISC1, THE PROCEDURE IS
      *     IDENTICAL TO THS ONE ABOVE EXCEPT THE FILE NAMES ARE
      *     NB INSTEAD OF AC, AND THERE IS NO TRANSMISSION HEADER
      *     SO YOU DON'T HAVE TO DO THAT PART.
      *                         SINCERELY YOURS, BARB
      *
      * REV:
      *  MJD 062895 ADDED COPY MEMBERS LIBLP/POSTDATE AND POSTDATEW.
      *             SET RC-FUNDS-DATE AND RC-FUNDS-TIME.
      *  MJD 071095 ADDED REPORT ONLY OPTION, DATE SUFFIX TO DATE,
      *             RANGE-LINES, AND DATE & BRANCH RANGE LOGIC.
      *  BLL 080295 MAKE EFFECTIVE DATE YYMMDD, CHANGE ORIGIN FROM
      *             BANK ROUTING # TO FED TAX ID
      *  BLL 080395 PRINT TOTAL, DON'T CREATE DETAIL TRANSMISSION
      *             IF DEPOSIT AMT IS 0
      *  BLL 080495 IF RC-DEPOSIT IS NEGATIVE, USE TRAN CODE OF 22
      *             TO CREDIT BRANCH BANK; ADD TRACING NUMBERS,
      *             PRINT BATCH COUNT, RECORD COUNT
      *  MJD 080395 ADDED PERFORM OPEN-GR1-FILE AND SEARCH-GR-INT TO
      *               INITIALIZE ROUTINE.
      *  BAH 070296 CHANGED LIBSP/ACH.LOGIN TO HAVE AN * IN THE FIRST
      *             POSITION OF THE LOGIN TO OPEN A WINDOW ANYTIME.(MIDLAND)
      *  BLV 101096 FIXED BLOCK COUNT IN TRAILER; WAS 1 TOO HIGH USUALLY
      *  BLV 101096 DON'T TRANSMIT DEPOSIT OR UPDATE RC-STATUS IF
      *             BR-BANK-ACCTNO = SPACES
      *  BLV 110796 UPDATE RC-STATUS IF BLANK ACCT & 0 DEPOSIT; SKIP
      *             UPDATING STATUS IF BLANK ACCT & NON-ZERO DEPOSIT
      *  BLV 111296 DON'T ALLOW OVERWRITE OF EXISTING FILE FOR ENDING DATE
      *  BLV 031197 ALWAYS PRINT TOTALS IF REPORT ONLY = "Y"
      *  MJD 082897 ADDED LOGIC FOR WORLD. BASED ON GP-I-AM-WORLD.
      *             MOVED READ OF GPFILE TO TOP OF PROGRAM
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *             MOVED READ OF GPFILE TO TOP OF PROGRAM
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  MJD 032098 ADDED GETENV(W) FOR USE IN LIBLP/POSTDATE
      *  BAH 040898 CANT GO TO CLOSING-ROUTINES IF BRANCH > END-BRANCH,
      *             THE LAST DAY OF THE MONTH STATUS IS I1 - EOM UPDATED,
      *             THE FIRST DAY OF NEW MONTH WILL BE BEFORE THE I1
      *             RECORD !! RAC KEVIN
      *  BLV 990514 MERCURY CHANGING FROM BANK ONE TO LASALLE BANK; PUT
      *             LASALLE IN ORIGIN NAME, DON'T CREATE LOGIN TRANS-
      *             MITTER CONTROL; SUPPRESS TRACE NUMBERS
      *  BLV 990520 PUT TRACE NUMBERS BACK BECAUSE EVAN IS FULL OF CRAP,
      *             PUT "MERCURY FINANCE" IN ORIGIN-NAME,  PUT SYSDATE
      *             IN COMPANY-DESC-DATE,  DON'T DO OFFSETTING DEPOSIT
      *  BLV 990810 FIX HEADING FOR CHECKS/DEPOSITS BASED ON I-AM-WRLD
      *  BLV 000417 REGENCY FINANCE WILL NOW USE THIS EXTRACTION, CHECK
      *             GP-I-AM-REGENCY FOR CUSTOM FIELDS, REGWAN PR # 1141
      *  BLV 000525 ADD R1 OR R2 SUFFIX TO ACFILE IF WORLD & REMOTE COPY
      *             FILE TO RISC1 IF I-AM-WORLD & RUNNING FROM RISC2
      *  BLV 030813 CHANGES FOR CAROLINA FIRST, WORLD; CHANGE
      *             AC-DESTINATION-NAME, DO OFFSETTING DEPOSIT
      *  BLV 030924 CHANGES FOR CAROLINA FIRST, WORLD; IF I-AM-WORLD
      *             PUT ROUTING # IN AC-ORIGIN INSTEAD OF FED TAX ID
      *  BLV 030929 CHANGES FOR CAROLINA FIRST, WORLD; SINCE SENDING
      *             FROM WEBSITE, WILL COPY FILES FOR EACH RUN TO
      *             ../../TFP/ACH/WORLD-1.YYMMDD (OR WORLD-2-YYMMDD),
      *             THIS IS AN EXTRA COPY OF FILE.  THE FILES WILL
      *             BE CLEARED FROM THIS DIRECTORY AT THE TIME OF
      *             EACH RUN, SO THERE SHOULD ONLY BE 1 DAYS WORTH
      *             THERE
      *  BLF 060915 IF I-AM-WORLD REPORT PRINTS & ADDS CHECK TOTAL
      *             FROM RC-FILE.  FIX TO NOT ADD CHECK TOTAL IF
      *             BRANCH BANK ACCT# IS BLANK, SINCE RECORD WILL
      *             KEEP APPEARING UNTIL ACCT# IS FIXED, WORLD PR# 491
      *  BLF 061127 FIX TO PREVIOUS CHANGE - I HAD PROBLEM WHERE BANK
      *             ACCT WAS BLANK & CHECK TOTAL INCLUDED THOSE BRANCHES
      *             EVERY DAY UNTIL BANK ACCT WAS FIXED.  BUT THEN IF
      *             BANK ACCT WAS BLANK BUT RC-DEPOSIT WAS 0, RC WAS
      *             REWRITTEN WITH K1 STATUS & CHECKS FOR DAYS WITH
      *             0 DEPOSIT DIDN'T GET ADDED IN.  FIXED TO ADD CKS
      *             IF BLANK ACCT# AND 0 DEPOSIT. WORLD PR# 491
      *  BLM 110221 CHANGES FOR CAROLINA FIRST, WORLD; STOP DOING
      *             OFFSETTING DEPOSIT, WORLD PR# 747
      *  BLM 110802 CHANGES FOR CAROLINA FIRST, WORLD; IN COMPANY
      *             BATCH HEADER & BATCH TRAILER, CHANGE SERVICE
      *             CLASS FROM 200 TO 225, WORLD PR# 762
      *  BAH 140516 REMOVED _CMD AND PUT COMMAND IN WORKING STORAGE WITH
      *             C$TOLOWER AND FILLING IN $REL, VERYANT
      *  BAH 140401 MOVE "A" TO FILE-MODIFIER IF GP-I-AM-REGENCY, PR#4508
      *  BLM 150304 CHANGES FOR WELL FARGO, MOVE TAX-ID TO ORIGIN
      *             (INSTEAD OF ROUTING), REMOVE CUSTOM I-AM-WORLD
      *             MOVES ON DESTINATION & ORIGIN NAMES, JUST
      *             SET DEFAULTS:
      *                MOVE GP-BANK-NAME TO AC-DESTINATION-NAME.
      *                MOVE GP-CORP-NAME TO AC-ORIGIN-NAME.
      *             WORLD PR# 1056
      *
      * KEC 2015.1125 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      * BAH 170323 ACTIVATE 8 DIGIT DATES, PD0006
      * BLM 180312 FIX INVALID DEFAULT DATE & UPDATE OF EFFECTIVE DATE
      *            AND FIX DATES IN RANGE PRINT
      * BAH 20200424 SET NOACCEPT-BUF TO "Y" SO IT DOESN'T ENTER ACCEPT
      *              BUF IN EOINIT, IT ENTERS IT IN THIS PROGRAM, AFTER
      *              OTHER TASKS.
      * BAH 20200831 REMOVED RESET ON BR1 
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBSP/SPFSAC.CPY".
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBSP/SPFDAC.CPY".
       COPY "LIBGB/GBFDPR.CPY".
      /
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/ACHCRE.CBL S35 04/01/24-10:54:54 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBSP/SPWSAC.CPY".
       01  PR-IFN               PIC X(2)    VALUE "PR".
       01  AC-KEY               PIC 9(5)    VALUE 1.

       01  REC-COUNT            PIC 9(5) VALUE 0.

       01  WK-TIME.
           03  WK-HH            PIC 99.
           03  WK-MN            PIC 99.

       01  WK-RECS-REM          PIC 99.
       01  WK-TRACE-NUMBER      PIC 9(7) COMP-3.
       01  WK-BATCH-NO          PIC 9(5) COMP-3.
       01  WK-GR-TOT-DEPOSIT    PIC S9(11)V99 COMP-3.
       01  WK-GR-TOT-CHECKS     PIC S9(11)V99 COMP-3.
       01  WK-GR-TOT-DEBIT      PIC S9(11)V99 COMP-3.
       01  WK-GR-TOT-CREDIT     PIC S9(11)V99 COMP-3.
       01  WK-TOT-DEPOSIT       PIC S9(11)V99 COMP-3.
       01  WK-TOT-DEBIT         PIC S9(11)V99 COMP-3.
       01  WK-TOT-CREDIT        PIC S9(11)V99 COMP-3.
       01  WK-NO-SIGN           PIC 9(11)V99 COMP-3.
       01  WK-BATCH-COUNT       PIC 9(5) COMP-3.
       01  WK-TOT-BATCH-COUNT   PIC 9(5) COMP-3.
       01  WK-BIG-HASH.
           03  WK-PRE-HASH      PIC 9(4)    VALUE 0.
           03  WK-HASH          PIC 9(10)   VALUE 0.
       01  WK-X-HASH REDEFINES WK-BIG-HASH
                                PIC 9(14).

       01  WK-T-HASH            PIC 9(14)   VALUE 0.
       01  WK-ROUTING-NO        PIC 9(9).
       01  WK-ROUTINGX REDEFINES WK-ROUTING-NO.
           03  WK-ROUTE1        PIC 9(8).
           03  WK-CHECK-DIGIT   PIC 9.

       01  RECORDS-FOUND        PIC X         VALUE " ".
           88  NO-RECORDS-FOUND               VALUE "N".
       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  WARNING-LIST         PIC X(8)      VALUE "WARNING.".
       01  LEGEND               PIC X(70)     VALUE SPACES.
       01  REPORT-FG            PIC X.

      *****************************************************************
      *   LINE WORKERS     *
      *****************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  HOLD-RC2-KEY.
           03  HOLD-RC-STATUS                  PIC X(02).
           03  HOLD-RC-BRNO                    PIC 9(04).
           03  HOLD-RC-TRANS-DATE              PIC 9(08).
           03  FILLER                REDEFINES HOLD-RC-TRANS-DATE.
               05  HOLD-RC-TRANS-DATE-CC       PIC 9(02).
               05  HOLD-RC-TRANS-DATE-YY       PIC 9(02).
               05  HOLD-RC-TRANS-DATE-MM       PIC 9(02).
               05  HOLD-RC-TRANS-DATE-DD       PIC 9(02).

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(5)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(11)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(22) VALUE
               "BRANCH DEPOSIT LISTING".

       01  HEAD3.
           03  FILLER           PIC X(33)    VALUE
               "BR #  BRANCH NAME".
           03  FILLER           PIC X(46)    VALUE
               "  DATE    STATUS          DEPOSIT             ".

       01  HEAD3-WORLD.
           03  FILLER           PIC X(33)    VALUE
               "BR #  BRANCH NAME".
           03  FILLER           PIC X(46)    VALUE
               "  DATE    STATUS          CHECKS       DEPOSIT".

       01  DETAIL-LINE          VALUE SPACES.
           03  FILLER           PIC X(132).

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO              PIC Z999.
           03  FILLER              PIC X(2).
           03  D-BRNAME            PIC X(25).
           03  FILLER              PIC XX.
           03  D-DATE              PIC 99/99/99.
           03  FILLER              PIC XX.
           03  D-STATUS            PIC X(9).
           03  D-LINE1-REDEF-AREA  PIC X(68).
           03  D-LINE1-STD REDEFINES D-LINE1-REDEF-AREA.
               05  D-DEPOSIT       PIC ZZ,ZZZ,ZZ9.99-.
               05  FILLER          PIC X(54).
           03  D-LINE1-WORLD REDEFINES D-LINE1-REDEF-AREA.
               05  D-WORLD-CHECKS  PIC ZZ,ZZZ,ZZ9.99-.
               05  D-WORLD-DEPOSIT PIC ZZ,ZZZ,ZZ9.99-.
               05  D-WORLD-MSG     PIC X(40).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  FILLER       PIC XX.
               04  R-NUM        PIC 9(8).
           03  R-DATE REDEFINES RANGE-SELECTION
                                PIC 99/99/99.
           03  R-FLAGX REDEFINES RANGE-SELECTION.
               04  FILLER       PIC X(7).
               04  R-FLAG       PIC X(3).

      *****************************************************************
      *     S C R E E N   F I E L D S   *
      *****************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH            PIC 9(4).
           03  END-BRANCH            PIC 9(4).
           03  BEG-DATE              PIC 9(8).
           03  END-DATE              PIC 9(8).
           03  EFFECTIVE-DATE-YYMMDD PIC 9(6).
           03  GROUP-FLAG            PIC X.
           03  ENT-GROUP             PIC X(4).

       01 VERSION-CONTROL       PIC X VALUE "B".

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBCL/CKSTATW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBWI/RCINQW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".

      * "/$REL/SH/EC /$REL/SH/ACH_CP.SH ".
       01  ACHCRE-CP-COMMAND.
           03  FILLER            PIC X    VALUE "/".
           03  ACHCRE-CP-REL     PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  ACHCRE-CP-EC      PIC X(3) VALUE "EC ".
           03  FILLER            PIC X    VALUE "/".
           03  ACHCRE-CP-REL2    PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  ACHCRE-CP-SHELL   PIC X(10) VALUE "ACH_CP.SH ".
           03  ACHCRE-CP-NAME    PIC X(25).
           03  FILLER            PIC X    VALUE " ".
           03  ACHCRE-CP-DATA    PIC X(8) VALUE SPACES.

      *  "/$REL/SH/EC /$REL/SH/ACH_REMOVE1.SH ".
       01  ACHCRE-REMOVE1-COMMAND.
           03  FILLER            PIC X    VALUE "/".
           03  ACHCRE-RM1-REL    PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  ACHCRE-RM1-EC     PIC X(3) VALUE "EC ".
           03  FILLER            PIC X    VALUE "/".
           03  ACHCRE-RM1-REL2   PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  ACHCRE-RM1-SHELL  PIC X(15) VALUE "ACH_REMOVE1.SH ".
           03  ACHCRE-RM1-DATA   PIC X(8) VALUE SPACES.


       COPY "LIBLP/RANGE_CMD.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/ACHCRE_DEF.CPY".
       COPY "LIBSP/ACHCRE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/ACHCRE_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                AC-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
                AC-FILE PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      **************************************************
       MAIN-MODULE SECTION.
      **************************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
       MAIN-ROUTINE.
      D    STOP MESS.
           MOVE "ACHCRE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

      * FIX ALL WORKING STORAGE COMMANDS TO BE IN CORRECT FORMAT
           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO ACHCRE-CP-REL ACHCRE-CP-REL2
                              ACHCRE-RM1-REL ACHCRE-RM1-REL2.

           CALL "C$TOLOWER" USING ACHCRE-CP-EC, VALUE 3.
           CALL "C$TOLOWER" USING ACHCRE-CP-SHELL, VALUE 10.
           CALL "C$TOLOWER" USING ACHCRE-RM1-EC, VALUE 3.
           CALL "C$TOLOWER" USING ACHCRE-RM1-SHELL, VALUE 11.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-RC.
           PERFORM READ-RC2-FILE-NEXT.
           IF IO-FG = 9
              GO TO CLOSING-ROUTINES.

           IF RC-STATUS > "J1"
              GO TO CLOSING-ROUTINES.

      * 4/8/98
      * CANT GO TO CLOSING-ROUTINES IF BRANCH > END-BRANCH BECAUSE THE
      * LAST DAY OF THE MONTH STATUS IS I1 - EOM UPDATED, AND THE FIRST
      * DAY OF NEW MONTH WILL BE BEFORE THE I1 RECORD !! BAH RAC KEVIN
           IF RC-BRNO > END-BRANCH
      *       GO TO CLOSING-ROUTINES.
              GO TO NEXT-RC.

           IF RC-BRNO < BEG-BRANCH
              GO TO NEXT-RC.

           IF RC-TRANS-DATE < BEG-DATE
              GO TO NEXT-RC.

           IF RC-TRANS-DATE > END-DATE
              GO TO NEXT-RC.

           MOVE RC2-KEY TO HOLD-RC2-KEY.

           IF GROUP-FLAG = "N"
              GO TO READ-BR.

           MOVE RC-BRNO TO WGR-SRESULT.
           PERFORM SEARCH-GR-VERIFY.
           IF WGR-ERR > 0
              GO TO NEXT-RC.

       READ-BR.
           MOVE RC-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

      * 0 DEPOSIT - DON'T TRANSMIT, BUT PRINT & UPDATE RC-STATUS.
           IF RC-DEPOSIT NOT = 0
              IF BR-BANK-ACCTNO NOT = SPACES
                 PERFORM 6-ENTRY-DETAIL.

           PERFORM DETAIL-LINE-ROUTINE.
           PERFORM UPDATE-RC-STATUS.

           GO TO NEXT-RC.

       CLOSING-ROUTINES.
           IF REPORT-FG NOT = "Y"
              IF REC-COUNT < 3
                 PERFORM PRINT-TOTAL-ROUTINE
                 PERFORM PRINT-RANGE-LINES
                 MOVE "N" TO RECORDS-FOUND
                 GO TO END-PROGRAM
              ELSE
                 MOVE "Y" TO RECORDS-FOUND.

      *    -------------------------------------------------------------
      *    WORLD: FIRST UNION MAKES OFFSETTING DEPOSIT, SO DON'T !!!
      *    5/20/99 - LASALLE (MERC) DOESN'T WANT OFFSET EITHER - ONLY
      *    BANK ONE DID, SO STAR OFF FOR EVERYONE FOR NOW
      *    4/18/00 - NEED OFFSET FOR NAT'L CITY, REGWAN
      *    2/21/11 - WORLD, CAROLINA FIRST NO LONGER WANTS OFFSET
      *    -------------------------------------------------------------
      *    IF GP-I-AM-REGENCY
      *       PERFORM 6-OFFSETTING-DEPOSIT.

           PERFORM 8-COMPANY-BATCH-CONTROL.
           PERFORM 9-FILE-CONTROL.
           IF WK-RECS-REM NOT = 0
              COMPUTE WK-RECS-REM = 10 - WK-RECS-REM
              PERFORM PAD-FILE WK-RECS-REM TIMES.

           PERFORM PRINT-TOTAL-ROUTINE.

           PERFORM PRINT-RANGE-LINES.

           PERFORM CP-TO-R1.

           GO TO END-PROGRAM.

      ***************************************************
       6-ENTRY-DETAIL SECTION.
      ***************************************************

           MOVE SPACES TO AC-REC.

           MOVE 6      TO AC-REC-TYPE.

      *  IF NEGATIVE DEPOSIT, TRAN-CODE 22 WILL PUT MONEY BACK
      *  INTO ACCOUNT (NOTE AC-DEPOSIT-AMOUNT IS UNSIGNED
           IF RC-DEPOSIT < 0
              MOVE 22         TO AC-TRAN-CODE
              MOVE RC-DEPOSIT TO WK-NO-SIGN
              ADD WK-NO-SIGN  TO WK-TOT-CREDIT
           ELSE
              MOVE 27         TO AC-TRAN-CODE
              ADD RC-DEPOSIT  TO WK-TOT-DEBIT.

           MOVE BR-BANK-ROUTING-NO TO AC-BANK-RT-NUMBER.
           ADD AC-BANK-RT-NO       TO WK-X-HASH WK-T-HASH.
           MOVE BR-BANK-ACCTNO     TO AC-BANK-ACCOUNT-NUMBER.
           MOVE RC-DEPOSIT         TO AC-DEPOSIT-AMOUNT.
           ADD RC-DEPOSIT          TO WK-TOT-DEPOSIT.
           MOVE BR-NO              TO AC-NAME.
           INSPECT AC-NAME REPLACING FIRST "               "
                           BY BR-CITY AFTER INITIAL " ".
           INSPECT AC-NAME REPLACING FIRST "  " BY ", ".
           INSPECT AC-NAME REPLACING FIRST "  "
                           BY BR-STATE AFTER INITIAL " ".

           MOVE AC-NAME            TO AC-ID-NUMBER.
           MOVE 0                  TO AC-ADDENDA-REC-IND.
           MOVE GP-BANK-ROUTING-NO TO WK-ROUTING-NO.
           MOVE WK-ROUTE1          TO AC-TRACE-PREFIX.
           COMPUTE AC-TRACE-NUMBER = WK-BATCH-COUNT + 1.

           MOVE X"0D"     TO AC-CR.
           MOVE X"0A"     TO AC-LF.

           IF REPORT-FG NOT = "Y"
              PERFORM WRITE-AC-FILE.
           ADD 1 TO AC-KEY.
           ADD 1 TO REC-COUNT WK-BATCH-COUNT.

       EXIT-6-ENTRY-DETAIL.
           EXIT.


      ********************************************
      *6-OFFSETTING-DEPOSIT SECTION.
      ********************************************

      *    MOVE SPACE TO AC-REC.
      *    MOVE 6 TO AC-REC-TYPE.
      *    MOVE 22 TO AC-TRAN-CODE.
      *    MOVE GP-BANK-ROUTING-NO TO AC-BANK-RT-NUMBER.
      *    ADD AC-BANK-RT-NO TO WK-X-HASH WK-T-HASH.
      *    MOVE GP-BANK-ACCTNO TO AC-BANK-ACCOUNT-NUMBER.
      *    MOVE WK-TOT-DEPOSIT TO AC-DEPOSIT-AMOUNT.
      *    MOVE WK-TOT-DEPOSIT TO WK-NO-SIGN.
      *    ADD WK-NO-SIGN TO WK-TOT-CREDIT.
      *    MOVE GP-CORP-NAME TO AC-NAME.
      *    MOVE 0 TO AC-ADDENDA-REC-IND.

      *    MOVE GP-BANK-ROUTING-NO TO WK-ROUTING-NO.
      *    MOVE WK-ROUTE1 TO AC-TRACE-PREFIX.
      *    COMPUTE AC-TRACE-NUMBER =
      *       WK-BATCH-COUNT + 1.

      *    MOVE X"0D"     TO AC-CR.
      *    MOVE X"0A"     TO AC-LF.

      *    IF REPORT-FG NOT = "Y"
      *       PERFORM WRITE-AC-FILE.
      *    ADD 1 TO AC-KEY.
      *    ADD 1 TO REC-COUNT WK-BATCH-COUNT.

      *EXIT-6-OFFSETTING-DEPOSIT.
      *    EXIT.

      *************************************************
       UPDATE-RC-STATUS SECTION.
      *************************************************

           IF REPORT-FG = "Y"
              GO TO EXIT-UPDATE-RC.

           IF BR-BANK-ACCTNO = SPACES
              IF RC-DEPOSIT NOT = 0
                 GO TO EXIT-UPDATE-RC.

           PERFORM CLOSE-RC2-FILE.

           PERFORM OPEN-RC1-FILE.
           MOVE HOLD-RC-BRNO TO RC-BRNO.
           MOVE HOLD-RC-TRANS-DATE TO RC-TRANS-DATE.
           PERFORM READ-RC1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE "K1" TO RC-STATUS.

           PERFORM SET-POSTDATE.
           MOVE POST-TIME TO RC-FUNDS-TIME.
           MOVE POST-DATE TO RC-FUNDS-DATE.

           PERFORM REWRITE-RC1-FILE.
           PERFORM CLOSE-RC1-FILE.

           PERFORM OPEN-RC2-FILE.
           MOVE HOLD-RC2-KEY TO RC2-KEY.

           MOVE RC-STATUS  TO QRC2-WBEG-STATUS.
           MOVE "K1"       TO QRC2-WEND-STATUS.
           MOVE RC-BRNO    TO QRC2-WBEG-BRNO.
           MOVE END-BRANCH TO QRC2-WEND-BRNO.
           MOVE RC-TRANS-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WBEG-TRANS-DATE.
           MOVE END-DATE   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WEND-TRANS-DATE.

           PERFORM START-RC2-FILE.
       EXIT-UPDATE-RC.
           EXIT.

      *****************************************************************
       8-COMPANY-BATCH-CONTROL SECTION.
      *****************************************************************

      * CREATE REC TYPE 8: COMPANY/BATCH CONTROL

           MOVE SPACES TO AC-REC.

           MOVE 8              TO AC-REC-TYPE.
           MOVE 225            TO AC-SERVICE-CLASS
           MOVE WK-BATCH-COUNT TO AC-ENTRY-COUNT.
           ADD WK-BATCH-COUNT  TO WK-TOT-BATCH-COUNT.
           MOVE WK-HASH        TO AC-ENTRY-HASH.
           MOVE WK-TOT-DEBIT   TO AC-TOTAL-DEBIT.
           MOVE WK-TOT-CREDIT  TO AC-TOTAL-CREDIT.
           ADD WK-TOT-DEPOSIT  TO WK-GR-TOT-DEPOSIT.
           ADD WK-TOT-DEBIT    TO WK-GR-TOT-DEBIT.
           ADD WK-TOT-CREDIT   TO WK-GR-TOT-CREDIT.
           MOVE 1              TO AC-COMPANY-PREFIX2.
           MOVE GP-FED-TAX-ID  TO AC-FED-TAX-ID2.
           MOVE GP-BANK-ROUTING-NO TO WK-ROUTING-NO.
           MOVE WK-ROUTE1      TO AC-BANK-IDENTIFICATION.
           MOVE WK-BATCH-NO    TO AC-BATCH-NUMBER.

           MOVE X"0D"     TO AC-CR.
           MOVE X"0A"     TO AC-LF.

           IF REPORT-FG NOT = "Y"
              PERFORM WRITE-AC-FILE.
           ADD 1 TO AC-KEY.
           ADD 1 TO REC-COUNT.

       EXIT-8-COMPANY-BATCH-CONTROL.
           EXIT.

      *****************************************************************
       9-FILE-CONTROL SECTION.
      *****************************************************************

      * CREATE REC TYPE 9: TRAILER

           MOVE SPACE TO AC-REC.
           MOVE 9 TO AC-REC-TYPE.
           MOVE WK-BATCH-NO TO AC-BATCH-COUNT.
           ADD 1 TO REC-COUNT.
           DIVIDE REC-COUNT BY 10 GIVING AC-BLOCK-COUNT
                  REMAINDER WK-RECS-REM.
           IF WK-RECS-REM > 0
              ADD 1 TO AC-BLOCK-COUNT.
           MOVE WK-TOT-BATCH-COUNT TO AC-09-ENTRY-COUNT.
           MOVE WK-T-HASH TO WK-X-HASH.
           MOVE WK-HASH TO AC-09-ENTRY-HASH.
           MOVE WK-GR-TOT-CREDIT TO AC-09-TOTAL-CREDIT.
           MOVE WK-GR-TOT-DEBIT TO AC-09-TOTAL-DEBIT.

           MOVE X"0D"     TO AC-CR.
           MOVE X"0A"     TO AC-LF.

           IF REPORT-FG NOT = "Y"
              PERFORM WRITE-AC-FILE.
           ADD 1 TO AC-KEY.

       EXIT-9-FILE-CONTROL.
           EXIT.


      ********************************************
       PAD-FILE SECTION.
      ********************************************

           MOVE ALL "9" TO AC-REC.

           MOVE X"0D"     TO AC-CR.
           MOVE X"0A"     TO AC-LF.

           IF REPORT-FG NOT = "Y"
              PERFORM WRITE-AC-FILE.
           ADD 1 TO AC-KEY.
           ADD 1 TO REC-COUNT.

       EXIT-PAD-FILE.
           EXIT.


      *****************************************************************
       DETAIL-LINE-ROUTINE SECTION.
      *****************************************************************
           MOVE BR-NAME TO D-BRNAME.
           MOVE RC-BRNO TO D-BRNO.

           MOVE RC-TRANS-DATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-DATE.

           IF (RC-UPDATED OR RC-EOD-BATCH-DONE)
              MOVE "UPDATED" TO D-STATUS
           ELSE
              IF RC-BANK-DEPOSIT
                 MOVE "DEPOSITED" TO D-STATUS
              ELSE
                 MOVE RC-STATUS   TO D-STATUS.

           MOVE RC-CHECKS         TO D-WORLD-CHECKS.
           MOVE RC-DEPOSIT        TO D-WORLD-DEPOSIT
           IF BR-BANK-ACCTNO NOT = SPACES
              ADD RC-CHECKS       TO WK-GR-TOT-CHECKS.
           IF (BR-BANK-ACCTNO = SPACES AND RC-DEPOSIT = 0)
              ADD RC-CHECKS       TO WK-GR-TOT-CHECKS.
           IF RC-STATUS = "C1"
              MOVE "*** NOTE - DAILY BATCH NOT COMPLETED!***"
                                  TO D-WORLD-MSG.
           IF BR-BANK-ACCTNO = SPACES
              MOVE
            "*** NOTE - BRANCH BANK ACCTNO BLANK-NOT TRANSMITTED!***"
                                  TO D-WORLD-MSG.
           PERFORM WRITE-DETAIL-LINE.


      *****************************************************************
       PRINT-TOTAL-ROUTINE SECTION.
      *****************************************************************
           MOVE "TOTAL DEPOSIT" TO D-BRNAME.
           MOVE WK-GR-TOT-DEPOSIT TO D-DEPOSIT.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TOTAL DEBITS " TO D-BRNAME.
           MOVE WK-GR-TOT-DEBIT TO D-DEPOSIT.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TOTAL CREDIT " TO D-BRNAME.
           MOVE WK-GR-TOT-CREDIT TO D-DEPOSIT.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TOTAL CHECKS" TO D-BRNAME.
           MOVE WK-GR-TOT-CHECKS TO D-DEPOSIT.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************************************
       INITIALIZATION SECTION.
      *****************************************************************
           MOVE ACHCRE-QUEUE-POS TO Q-POS.
           MOVE ACHCRE-COPIES-POS TO C-POS.
           MOVE ACHCRE-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
      * SET THIS SO THE ENTER OF THE ACCEPT FLAG TAKES PLACE HERE AFTER
      * CKSTAT, NOT INSIDE EOINIT. A15 HAD A FLDVAL IN EOINIT 
           MOVE "Y" TO NOACCEPT-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF BEG-DATE = 0
              MOVE 19000101 TO BEG-DATE.

       STATUS-CHECK.
           MOVE "C" TO CKSTAT-ACTION.
           MOVE BEG-BRANCH TO CKSTAT-BEG-BRANCH.
           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP TO CKSTAT-GROUP.

           MOVE END-BRANCH TO CKSTAT-END-BRANCH.
           MOVE BEG-DATE   TO CKSTAT-DATE1.
           MOVE END-DATE   TO CKSTAT-DATE2.
           MOVE "C1" TO CKSTAT-HIGH-STATUS.
           MOVE "CL/CKSTAT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CKSTAT-BUF.
           CANCEL FORM-PROGX.
           IF CKSTAT-RETURN-STAT = "N"
              MOVE "RANGE INCLUDES BRANCHES NOT AT CORRECT STATUS!"
                 TO MESS
              PERFORM SEND-MESS
              PERFORM RC-INQUIRY
           ELSE
              MOVE "ALL BRANCHES AT CORRECT STATUS" TO MESS
              PERFORM SEND-MESS.

       ACCEPT-IT.
           IF CKSTAT-RETURN-STAT = "N" MOVE
             "* WARNING: BRANCHES WITH WRONG STATUS WILL BE SKIPPED! *"
              TO LEGEND
           ELSE
              MOVE SPACES TO LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE WARNING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "F1-CANCEL, F6-SCAN STATUSES, F7-EXIT, 'Y' TO ACCEPT"
              TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A000ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO EOINIT-SETPR.
           IF VDUSTATUS = "1U" GO TO EOINIT-ENTER-MENU.
           IF VDUSTATUS = "1<"
              PERFORM RC-INQUIRY
              GO TO ACCEPT-IT.
           IF VDUSTATUS NOT = "00" GO TO ACCEPT-IT.
           IF VDUBUF = "N" GO TO EOINIT-ENTER-MENU.
           IF VDUBUF NOT = "Y"
              GO TO ACCEPT-IT.

       BEGIN-EXTRACT.
           MOVE " DEPOSIT EXTRACTION IN PROGRESS..." TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.

           IF REPORT-FG NOT = "Y"
              PERFORM REMOVE-PRIOR-R1-FILES.

           IF REPORT-FG NOT = "Y"
              PERFORM OPEN-AC-FILE-OUTPUT
              PERFORM CLOSE-AC-FILE
              PERFORM OPEN-AC-FILE.


           PERFORM 1-FILE-HEADER.

           PERFORM 5-COMPANY-BATCH-HEADER.

           PERFORM OPEN-RC2-FILE.

           MOVE "C1"       TO RC-STATUS
                              QRC2-WBEG-STATUS.
           MOVE "K1"       TO QRC2-WEND-STATUS.
           MOVE BEG-BRANCH TO RC-BRNO
                              QRC2-WBEG-BRNO.
           MOVE END-BRANCH TO QRC2-WEND-BRNO.
           MOVE BEG-DATE   TO RC-TRANS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WBEG-TRANS-DATE.
           MOVE END-DATE   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WEND-TRANS-DATE.

           PERFORM START-RC2-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF GROUP-FLAG = "Y"
              PERFORM OPEN-GR1-FILE
              PERFORM SEARCH-GR-INIT.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      *****************************************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      *****************************************************************
       ENTER-PARAMETERS SECTION.
      *****************************************************************
       ENTER-PARM.

           MOVE SPACES TO WR-BUF.
           MOVE WARNING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF VDUSTATUS = "1U"
              GO TO REPORT-ONLY.

      *----------------------------------------------------------------

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO BEG-DT.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO BEG-DT.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BEG-DT.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO EXIT-PARM.

           PERFORM GET-DEFAULT-DATE.

           MOVE LEGEND-DATE1 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEGDT."    TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEG-DT.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *----------------------------------------------------------------
       END-DT.

           MOVE LEGEND-DATE2 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060ENDDT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEG-DT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO END-DT.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-DT.

       REPORT-ONLY.
           MOVE LEGEND-REPORT-ONLY TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E NA010REPORTONLY."    TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO END-DT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO REPORT-ONLY.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              GO TO REPORT-ONLY.
           MOVE VDUBUF TO REPORT-FG.
           IF REPORT-FG NOT = "Y"
              MOVE END-DATE TO DATE-YYYYMMDD
              MOVE DATE-YYYYMMDD-YY  TO AC-PATH-YR
              MOVE DATE-YYYYMMDD-MM  TO AC-PATH-MO
              MOVE DATE-YYYYMMDD-DD  TO AC-PATH-DA
              MOVE "." TO AC-PATH-DOT
              MOVE EXT-FILPATH-MACHINE TO AC-PATH-MACHINE2
              PERFORM LOAD-AC-FILE
              MOVE AC-PATH TO ACCESS-BUF
              PERFORM ACCESS-CALL
              IF STAT = "00"
                 MOVE "ACFILE.&&&&&& ALREADY EXISTS!" TO MESS
                 INSPECT MESS REPLACING FIRST
                        "&&" BY AC-PATH-YR
                 INSPECT MESS REPLACING FIRST
                        "&&" BY AC-PATH-MO
                 INSPECT MESS REPLACING FIRST
                        "&&" BY AC-PATH-DA
                 PERFORM SEND-MESS
                 MOVE "INVALID ENDING DATE !" TO MESS
                 PERFORM SEND-MESS
                 GO TO REPORT-ONLY.

       EFFECTIVE-DT.
           IF REPORT-FG = "Y"
              GO TO EXIT-PARM.

           MOVE "ENTER EFFECTIVE TRANSACTION DATE" TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM SET-POSTDATE.
           MOVE POST-DATE TO NUM-DATE.
           PERFORM WEEKDAY.
           IF DAY-IS-FRI
              MOVE 3 TO NDTE-HOLD
           ELSE IF DAY-IS-SAT
              MOVE 2 TO NDTE-HOLD
           ELSE
              MOVE 1 TO NDTE-HOLD.
           MOVE NUM-DATE TO NDTE-DATE.
           MOVE "D" TO DATER-UNITPER-CD.
           PERFORM INCREMENT-UNITPER.
           MOVE "S  8000EFFECTIVE." TO VDU.
           MOVE NDTE-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "ERNM060EFFECTIVE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REPORT-ONLY.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO EFFECTIVE-DT.

           MOVE VDU-DATE-YYYYMMDD TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD TO EFFECTIVE-DATE-YYMMDD.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

           GO TO EXIT-PARM.

       EXIT-PARM.
           EXIT.

      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

      *****************************************************************
       WRITE-DETAIL-LINE SECTION.
      *****************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD3-WORLD TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      *****************************************************************
       RC-INQUIRY SECTION.
      *****************************************************************
           MOVE BEG-BRANCH TO RCINQ-BEG-BRANCH.
           MOVE END-BRANCH TO RCINQ-END-BRANCH.
           MOVE BEG-DATE TO RCINQW-DATE1.
           MOVE END-DATE TO RCINQW-DATE2.
           MOVE "N" TO RCINQ-ENTER-RANGES.
           MOVE "Y" TO RCINQ-OPEN-FG.
           MOVE "N" TO RCINQ-UPDATED-FG.
           MOVE "N" TO RCINQ-EARNINGS-FG.
           MOVE "N" TO RCINQ-EOMUPD-FG.
           MOVE "N" TO RCINQ-DEPOSIT-FG.
           MOVE "N" TO RCINQ-CHECKS-FG.
           MOVE "WI/RCINQ" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH RCINQ-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      ***************************************************
       1-FILE-HEADER SECTION.
      ***************************************************

           MOVE SPACES TO AC-REC.
           MOVE 0 TO WK-BATCH-NO WK-GR-TOT-DEPOSIT
                     WK-GR-TOT-DEBIT WK-GR-TOT-CREDIT
                     WK-TRACE-NUMBER WK-TOT-BATCH-COUNT
                     WK-GR-TOT-CHECKS.

           MOVE 1                  TO AC-REC-TYPE AC-PRIORITY-CODE.
           MOVE GP-BANK-ROUTING-NO TO AC-DESTINATION.
           MOVE 1                  TO AC-ORIGIN-PREFIX.
           MOVE GP-FED-TAX-ID      TO AC-ORIGIN-TAX-ID.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYMMDD.
           MOVE DATE-YYMMDD        TO AC-CREATE-DATE-YYMMDD.
           MOVE "Y" TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE T-HH TO WK-HH.
           MOVE T-MM TO WK-MN.
           MOVE WK-TIME            TO AC-CREATE-TIME-HHMM.
           MOVE "A"                TO AC-FILE-MODIFIER.
           MOVE 94                 TO AC-REC-SIZE.
           MOVE 10                 TO AC-BLOCKING.
           MOVE 1                  TO AC-FORMAT-CODE.
           MOVE GP-BANK-NAME       TO AC-DESTINATION-NAME.
           MOVE GP-CORP-NAME       TO AC-ORIGIN-NAME.

           MOVE X"0D"     TO AC-CR.
           MOVE X"0A"     TO AC-LF.

           IF REPORT-FG NOT = "Y"
              PERFORM WRITE-AC-FILE.

           ADD 1 TO AC-KEY.
           ADD 1 TO REC-COUNT.

       EXIT-1-FILE-HEADER.
           EXIT.

      ***************************************************
       5-COMPANY-BATCH-HEADER SECTION.
      ***************************************************

           MOVE SPACES TO AC-REC.
           MOVE 0 TO WK-TOT-DEPOSIT WK-BATCH-COUNT WK-X-HASH
                     WK-TOT-DEBIT WK-TOT-CREDIT.

           MOVE 5             TO AC-REC-TYPE.
           MOVE 225           TO AC-SERVICE-CLASS.
           MOVE GP-CORP-NAME  TO AC-COMPANY-NAME.
           MOVE 1             TO AC-COMPANY-PREFIX.
           MOVE GP-FED-TAX-ID TO AC-FED-TAX-ID.
           MOVE "CCD"         TO AC-ENTRY-CLASS.
           MOVE "CONC DEBIT"  TO AC-COMPANY-ENTRY-DESC.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO AC-COMPANY-DESC-DATE.

           MOVE EFFECTIVE-DATE-YYMMDD TO AC-EFFECTIVE-ENTRY-DATE.
           MOVE 0                     TO AC-SETTLEMENT-DATE.
           MOVE 1                     TO AC-ORIG-STATUS.
           MOVE GP-BANK-ROUTING-NO    TO WK-ROUTING-NO.
           MOVE WK-ROUTE1             TO AC-BANK-IDENTIFICATION.
           ADD 1                      TO WK-BATCH-NO.
           MOVE WK-BATCH-NO           TO AC-BATCH-NUMBER.

           MOVE X"0D"     TO AC-CR.
           MOVE X"0A"     TO AC-LF.

           IF REPORT-FG NOT = "Y"
              PERFORM WRITE-AC-FILE.
           ADD 1 TO AC-KEY.
           ADD 1 TO REC-COUNT.

       EXIT-5-COMPANY-BATCH-HEADER.
           EXIT.

      *****************************************************************
       PRINT-RANGE-LINES SECTION.
      *****************************************************************


           MOVE 4 TO LN-FEED.
           MOVE "TOTAL NO. OF DETAIL RECORDS:" TO RANGE-LINE.
           MOVE WK-BATCH-COUNT TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           MOVE "TOTAL NO. OF RECORDS:" TO RANGE-LINE.
           MOVE REC-COUNT TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO LN-FEED.
           MOVE "BEGINNING BRANCH" TO RANGE-LINE.
           MOVE BEG-BRANCH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING BRANCH" TO RANGE-LINE.
           MOVE END-BRANCH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING DATE" TO RANGE-LINE.
           MOVE BEG-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING DATE" TO RANGE-LINE.
           MOVE END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT ONLY" TO RANGE-LINE.
           MOVE REPORT-FG TO R-FLAG.
           PERFORM WRITE-DETAIL-LINE.

       END-RANGE-LINES.
           EXIT.

      ******************************************************************
       REMOVE-PRIOR-R1-FILES SECTION.
      ******************************************************************

      * REMOVE PRIOR FTP FILES FOR WORLD1

           MOVE EXT-FILPATH-FIL TO ACHCRE-RM1-DATA.
           MOVE ACHCRE-REMOVE1-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

      ******************************************************************
       CP-TO-R1 SECTION.
      ******************************************************************

      * COPY EXTRACT FILE ON RISC1 TO FTP DIRECTORY

      * $1
           MOVE AC-PATH-FILENAME TO ACHCRE-CP-NAME.
      * $2
           MOVE EXT-FILPATH-FIL TO ACHCRE-CP-DATA.
           MOVE ACHCRE-CP-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".


      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO ACHCRE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ACHCRE-SYSDATE.
           DISPLAY ACHCRE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ACHCRE-FORM-FIELDS TO WR-BUF.
           MOVE ACHCRE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE ACHCRE-HELP--FILE TO HELP-LINK-FILE.
           MOVE ACHCRE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/ACHCRE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************************
       MISC-ROUTINE SECTION.
      ************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-DEFAULT-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NUM-DATE NDTE-DATE.
           PERFORM WEEKDAY.
           IF DAY-IS-MON
              MOVE -3 TO NDTE-HOLD
           ELSE
              MOVE -1 TO NDTE-HOLD.
           MOVE "D" TO DATER-UNITPER-CD.
           PERFORM INCREMENT-UNITPER.

           MOVE "D" TO CKSTAT-ACTION.
           MOVE BEG-BRANCH TO CKSTAT-BEG-BRANCH.
           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP TO CKSTAT-GROUP.
           MOVE END-BRANCH TO CKSTAT-END-BRANCH.
           MOVE EXT-JULIAN-BEG TO CKSTAT-DATE1.
           MOVE EXT-JULIAN-END TO CKSTAT-DATE2.
           MOVE "C1" TO CKSTAT-LOW-STATUS.
           MOVE "J1" TO CKSTAT-HIGH-STATUS.
           MOVE "CL/CKSTAT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CKSTAT-BUF.
           CANCEL FORM-PROGX.
           MOVE "S  A000BEGDT." TO VDU.
           IF CKSTAT-RETURN-DATE = 99999999
              MOVE NDTE-DATE TO DATE-YYYYMMDD
           ELSE
              MOVE CKSTAT-RETURN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A000ENDDT." TO VDU.
           MOVE NDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

      *******************************************
       IO-ROUTINE SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBRC1IN.CPY".
       COPY "LIBGB/GBRC2RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBSP/SPACI.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *******************************************
       END-PROGRAM SECTION.
      *******************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS.

      *----------------------------------------------------------------
       EXIT-PROG.
           MOVE "SP/BANKMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY ACHCRE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
