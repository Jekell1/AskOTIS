       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WFARGO.
       DATE-WRITTEN.    07-05-01.
      *****************************************************
      *
      *    DESC:  WELLS FARGO ASCII OUTPUT FILE
      *
      *    CREATES:  /USR/TMP/WFARGO.###     RSZ  432
      *
      **********************************************************************
      * WELLS FARGO FINANCIAL
      * BRANCH PROCEDURES DEPARTMENT
      * 206 EIGHTH STREET
      * DES MOINES, IA 50309
      *
      * WAYNE FLEENER   WAYNEFLEENER@FINANCIAL.WELLSFARGO.COM
      *         CAPITAL ^    ^       ^
      *
      *  RECORD LENGTH =   432
      *
      *  NOTE! INCLUDES PUNCTUATION
      *
      *  DATES INCLUDE SLASHES   99/99/99
      *
      *  DECIMALS POINTS IF ANY ARE INCLUDED   1000.95
      *
      *  SIGNED FIELDS ARE INDICATED WITH - TO THE RIGHT
      *
      *  LENGTH:        TYPE:     DESCRIPTION:           TOTAL BYTES:
      *  -------        -----     ------------           ------------
      *    16          ALPHA    BORROWER LAST NAME              16
      *    16          ALPHA    BORROWER FIRST NAME             32
      *    24          ALPHA    BORROWER ADDRESS LINE 1         56
      *    24          ALPHA    BORROWER CITY                   80
      *     2          ALPHA    BORROWER STATE                  82
      *    10          ALPHA    BORROWER ZIP                    92
      *    11  (999/999/999)    SSNO                           103
      * 
      *     1         ALPHA     INTEREST TYPE (I INT BEARING)  104
      *                                       (P PRECOMPUTE)
      *     4        (9999)     BRANCH #                       108
      *     6      (999999)     ACCOUNT NUMBER                 114
      *     9         (6.2)     REGULAR PAYMENT AMOUNT         123
      *     9         (6.2)     ODD PAYMENT AMOUNT             132
      *    11        (7.2-)     TOTAL REPAYABLE                143
      *    11        (7.2-)     AMOUNT FINANCED                154
      *    10        (6.2-)     ANTIC-INTEREST (PRECOMP)       164
      *     9         (6.2)     CURRENT BALANCE                173
      *     9         (6.2)     NET BALANCE                    182
      *     9         (6.2)     UNEARN-PRECOMP-INT             191
      *                         (REMAINING UNEARNED PRECOMP)
      *     7     (999.999)     EFFRATE (CONTRACT RATE)        198
      *     7     (999.999)     APPRATE (CONTRACT RATE)        205
      *     3         (999)     TOT-PMTS-MADE                  208
      *     8    (99/99/99)     LOANDATE                       216
      *     8    (99/99/99)     FIRST PAYMENT DATE(MM/DD/YY)   224
      *     3         (999)     ORIGINAL TERM (IN MONTHS)      227
      *     8    (99/99/99)     LASTPYDATE (DATE LAST PAYMENT) 235
      *     8    (99/99/99)     CONT-PAID-TO-DATE              243
      *                          (NEXT DUE DATE)
      *    10         ALPHA     LEGAL-IND                      253
      *                         (UP TO 3 DIFFERENT STATUS MAY
      *                         APPEAR, B BANKRUPT, J JUDGEMENT
      *                                 P P&L        )
      *     2            99     B-CHAPTER (BANKRUPTCY CHAPTER) 255
      *     1         ALPHA     REPO-IND                       256
      *     1         ALPHA     BULK-PURCHASE (Y = BULK-PURCH) 257
      *     2         ALPHA     COLLCD (COLLATERAL CODE)       259
      *
      *    WK-COLLATERAL  INFO OCCURS 2 TIMES                  327
      *     4         ALPHA     YEAR
      *    15         ALPHA     MAKE
      *    15         ALPHA     MODEL
      *    20         ALPHA     VIN
      *
      *
      *     3         (99-)     TOTNODEF ( # OF DEFERMENTS)    370
      *
      *     8      99/99/99     PROMO-IND-DATE                 378
      *                         (SAME AS CASH DATE NOT = 0)
      *
      *     1        ALPHA      NON-FILE Y/N FEECODE 91 FOUND  379
      *
      * ADDED THE FOLLOWING FOR SOAUTO PR# 3317
      *     2        99         PAY FREQ (LN-UNITPER-PER-YEAR) 381
      *                         (12,26,26 OR 52)
      *
      *     7       9(4).99     DEALER NO                      388
      *
      *    12       (8.2-)      AMT LAST PAY(LN-AMT-PAID-LAST) 400
      *
      *     8     99/99/99      MATURITY DATE                  408
      *
      *     1        ALPHA      P&L CODE                       409
      *
      *     2        ALPHA      PAYOFF CODE                    411
      *
      *     2        99         LOAN CLASS                     413
      *
      *     1        ALPHA      SALES FINANCE INDICATOR        414
      * ADDED THE PREVIOUS FOR SOAUTO PR# 3317
      *
      *     1  CARRIAGE RET     EXASCII2-CR                    415
      *     1  LINE FEED        EXASCII2-LF                    416
      **********************************************************************
      *
      *  REV:
      *  010706 *NEW*
      *  020904 BAH ADDED WK-VIN FOR HCREDIT, PR# 1904, ALSO EXPANDED
      *             WK-OWNBR FROM 9(2) TO 9(4) FOR A15.....
      *  020917 BAH ADDED WK-NON-FILE INDICATOR FOR FIRST COMMUNITY ONLY
      *             FEE CODE = 91, PR # 1911
      *  030506 CHANGED ADR2 TO BE CITY...PR# 2078, LIBERAL & SAVON
      *  031106 BAH WHEN CHANGED BRANCH TO 9(4), THAT ALSO CHANGED
      *             THE OUTPUT FILE TO HAVE .0001 ON THE END, DOS CANT
      *             HANDLE 4 DIGIT EXTENSION, CHANGED BACK TO 3.
      *  070807 BLF CHANGE GET-LT-INFO TO OPEN & CLOSE LTFILE; PAID-
      *             THRU-CALC-ADDON WAS PERFORMING GET-LT-P-REC
      *             WHICH CLOSES LTFILE, SO IF ACCOUNT HAD PAYMENT
      *             SCHEDULE FLAG TURNED ON, ALL THE RECORDS IN THE
      *             OUTPUT FILE AFTER IT WERE MISSING COLLATERAL &
      *             BANKRUPT INFO, UNION FINANCE
      *  080313 BAH DONT TEST LN-REPODATE FOR REPO INDICATOR, JUST MOVE
      *             THE LN-REPOCD, BARNETT  PL# 622
      *  090218 BAH ADDED FIELDS FOR SOAUTO PR# 3317
      *             WK-PAY-FREQ,WK-DLNO,WK-AMT-LAST-PAY,WK-MDTE,WK-PLCD,
      *             WK-POCD,WK-CLASS,WK-SALEFIN AND SKIP LOANS IN RESCISSION
      *  100720 BAH IF GP-I-AM-WORLD, DO NOT FILL IN BORROWER INFO, PR# 703
      *  100813 BAH ADD BW-STATE BACK IN FOR WORLD, JIMR
      *  151105 BAH CONT-PAID-TO-DATE WAS USING INCREMENT-MONTHS WHICH
      *             WON'T BE CORRECT FOR NON MONTHLY, CHANGED TO
      *             INCREMENT-UNITPER, AUTOMOTIVE CREDIT, PL#909
      *  151217 BAH ADDED WK-TITLE-1 AND 2, CFSC PR#4909
      *  BAH 160201 SPLIT OUT LTFILE, PD0003
      *  160816 BAH ADD "L" TO STATUSFLAGS CHECKED FOR WORLD ONLY PR#1177
      *  161201 BAH DO NOT FILL IN CHAPTER IF LOAN IS NOT BANKRUPT!
      *             RENEWALS BRING FORWARD BANKRUPT TRAILERS EVEN IF
      *             DISMISSED, THIS PROGRAM USES LT-B-CHAPTER WHICH WAS
      *             STILL FILLED IN, SO DO NOT READ THE LT BANKRUPT TRAILER
      *             UNLESS LOAN HAS A BANKRUPT DATE, FIRST METRO PL#956
      *  170203 BAH ADD "L" TO STATUSFLAGS CHECKED FOR FIRST METRO 
      *             JUST LIKE WORLD, PR#5220
      *  170905 BAH ADD BW-ZIP BACK IN, WELLS FARGO NEEDS IT TO RESEARCH
      *             HURRICAN HARVEY COLLATERAL EXPOSURE, WORLD PR#1274
      *  170914  CS ACTIVATE 8 DIGIT DATES
      *  180209 BAH WELLS FARGO ASKED FOR SSNO TO BE ELIMINATED FOR EVERYONE
      *             PL#988
      * BAH 181217 GLOBAL LNFILE
      * BAH 20191003 REMOVED ACCESS-CALL LNFILE
      * BAH 20200831 REMOVED RESET ON BR1 
      * RMZ 20250425 PERFORMANCE TUNING - ADDED STORED PROCEDURE. (S35Q-201)
      *            * THE 1ST DIFFERENCE BETWEEN THE DATA EXPORTED IN A15/S35 IS
      *              B-CHAPTER (BANKRUPTCY CHAPTER) SHOWED '00' IN S35 (FOR
      *              VERY FEW RECORDS), AND '  ' SPACES IN A15.  THIS IS
      *              BECAUSE AN LTBFILE RECORD WAS CREATED IN A15 BUT NO
      *              DATA WAS ENTERED ON THE SCREEN.  WHEN DATA WAS COPIED 
      *              FROM A15 TO S35, THE SPACES BECAME '00' IN THE LTBFILE
      *              ROW ON SQL (IT IS A SMALLINT COLUMN). 
      *            * A 2ND DIFFERENCE WAS FOR LTCFILE, WHERE S35 LOOKS AT
      *              TWO OCCURANCES OF MAKE/MODEL INFORMATION WHILE A15  
      *              ONLY LOOKS AT THE FIRST OCCURANCE. 
      *
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK-FILE ASSIGN MKTEMP-BUF
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY WK-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      *************************************
      *   LOAN ASCII FILE
      *************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
      *BORROWER INFORMATION
           03  WK-LNAME             PIC X(16).
           03  WK-FNAME             PIC X(16).
           03  WK-ADR1              PIC X(24).
           03  WK-CITY              PIC X(24).
           03  WK-STATE             PIC XX.
           03  WK-ZIP               PIC X(10).
           03  WK-SSNO              PIC 999/999/999.

      *LOAN INFORMATION
           03  WK-INTEREST-TYPE     PIC X.
           03  WK-OWNBR             PIC 9(4).
           03  WK-ACCTNO            PIC 9(6).
           03  WK-REGPYAMT          PIC 9(6).99.
           03  WK-ODDPYAMT          PIC 9(6).99.
           03  WK-TOTREPAY          PIC 9(7).99-.
           03  WK-AMTFIN            PIC 9(7).99-.
           03  WK-ANTIC-INTEREST    PIC 9(6).99-.
           03  WK-CURBAL            PIC 9(6).99.
           03  WK-NET-BAL           PIC 9(6).99.
           03  WK-UNEARN-PRECOMP-INT   PIC 9(6).99.
           03  WK-EFFRATE           PIC 999.999.
           03  WK-APRATE            PIC 999.999.
           03  WK-TOT-PMTS-MADE     PIC 9(3).
           03  WK-LOANDATE          PIC 99/99/99.
           03  WK-1STPAY            PIC 99/99/99.
           03  WK-ORGTERM           PIC 9(3).
           03  WK-LASTPYDATE        PIC 99/99/99.
           03  WK-CONT-PAID-TO-DATE PIC 99/99/99.
           03  WK-LEGAL-IND         PIC X(10).
           03  WK-B-CHAPTER         PIC 99.
           03  WK-REPO-IND          PIC X.
           03  WK-BULK-PURCHASE     PIC X.
           03  WK-COLLCD            PIC XX.

      *COLLATERAL INFO
           03  WK-COLLATERAL-INFO   OCCURS 2.
               05  WK-YEAR              PIC X(4).
               05  WK-MAKE              PIC X(15).
               05  WK-MODEL             PIC X(15).
               05  WK-VIN               PIC X(20).

      *MISC
           03  WK-TOTNODEF              PIC 99-.

           03  WK-PROMO-IND-DATE        PIC 99/99/99.

           03  WK-NON-FILE              PIC X.

      * ADDED THE FOLLOWING FOR SOAUTO PR# 3317
           03  WK-PAY-FREQ               PIC 99.
           03  WK-DLNO                   PIC 9(4).99.
           03  WK-AMT-LAST-PAY           PIC 9(8).99-.
           03  WK-MDTE                   PIC 99/99/99.
           03  WK-PLCD                   PIC X.
           03  WK-POCD                   PIC XX.
           03  WK-CLASS                  PIC 99.
           03  WK-SALEFIN                PIC X.
      * ADDED THE PREVIOUS FOR SOAUTO PR# 3317

           03  WK-TITLE-1                PIC X(8).
           03  WK-TITLE-2                PIC X(8).

           03  EXASCII2-CR               PIC X.
           03  EXASCII2-LF               PIC X.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/WFARGO S35 04/28/25-07:22:16 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.  
       01  QWS-BEG-DEALER       PIC 9(4)V99.
       01  QWS-END-DEALER       PIC 9(4)V99.
       01  QWS-BEG-LNAMT        PIC 9(6).
       01  QWS-END-LNAMT        PIC 9(6).
       01  QWS-COLCD            PIC X(2).      
       01  QWS-BEG-BAL          PIC 9(7).
       01  QWS-END-BAL          PIC 9(7).
       01  QWS-OWNED            PIC X.  
       01  QWS-PL               PIC X.
       01  QWS-PLACTIVE         PIC X.
       01  QWS-RC               PIC X.
       01  QWS-JD               PIC X.
       01  QWS-ZBAL             PIC X.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN3-CONN-OPEN-SW     PIC X        VALUE "N".

       01  WK-IFN               PIC XX       VALUE "WK".
       01  WK-KEY               PIC 9(6)     VALUE 0.
        
      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  DISPLAY-BUF.
           03  FILLER           PIC X(12) VALUE "PROCESSING..".
           03  DISPLAY-BRNO     PIC Z(4).
           03  FILLER           PIC X(2).
           03  DISPLAY-ACCT     PIC Z(6).
           03  FILLER           PIC X(4) VALUE SPACES.
           03  FILLER           PIC X(22) VALUE SPACES.

      * KEEP THIS 3 FOR A DOS EXTENSION !!
       01  HOLD-BEGBR           PIC 9(3).
       01  BAL-WK               PIC S9(7)V99 COMP-3 VALUE 0.

       01  SUB                  PIC 9(2) VALUE ZEROS.
       01  SUB2                 PIC 9(2) VALUE ZEROS.

       01  TEST-STATUS-TABLE.
           03  TEST-STATUS-TB   PIC X OCCURS 10 VALUE SPACES.
       01  TEST-STATUS-TABLE2.
           03  TEST-STATUS-TB2  PIC X OCCURS 10 VALUE SPACES.

       01  AGE-DATE        PIC 9(8).
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR          PIC 9(4).
           03  END-BR          PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  BEG-DEALER      PIC 9(4)V99.
           03  END-DEALER      PIC 9(4)V99.
           03  BEG-AGE         PIC 9(4).
           03  END-AGE         PIC 9(4).
           03  BEG-BAL         PIC 9(7).
           03  END-BAL         PIC 9(7).
           03  BEG-RAGE        PIC 9(4).
           03  END-RAGE        PIC 9(4).
           03  OWNED           PIC X.
           03  PL              PIC X.
           03  PLACTIVE        PIC X.
           03  JD              PIC X.
           03  RC              PIC X.
           03  ZBAL            PIC X.
           03  COLCD           PIC X(2).
           03  BEG-LNAMT       PIC 9(6).
           03  END-LNAMT       PIC 9(6).
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "C".

       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/WFARGO_DEF.CPY".
       COPY "LIBSP/WFARGO_WKS.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/WFARGO_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".


      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WFARGO" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.
           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           GO TO MOVE-BRANCH-PATH-INFO.
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BR
              GO TO END-ROUTINE.
       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LN-PATH-OVERRIDE
                              BW-PATH-OVERRIDE
                              LTB-PATH-OVERRIDE
                              LTP-PATH-OVERRIDE
                              LTC-PATH-OVERRIDE.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LTB1-FILE.
           PERFORM OPEN-LTC1-FILE.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE. *> SSP - LN,BW,LTC,LTB
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT. *> EMBEDDED - LN,BW,LTC,LTB
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BR
              GO TO END-OF-GROUP.

           IF LN-POCD-INRESCISSION
              GO TO NEXT-LN.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              GO TO NEXT-LN.

      *  POCD - **  IS A DELETED CONTRACT
           IF LN-POCD = "**"
              GO TO NEXT-LN.

           IF LN-DLNO < BEG-DEALER OR LN-DLNO > END-DEALER
              GO TO NEXT-LN.
           IF LN-LNAMT < BEG-LNAMT OR LN-LNAMT > END-LNAMT
              GO TO NEXT-LN.

           IF COLCD NOT = "**"
              IF LN-COLLCD NOT = COLCD
                 GO TO NEXT-LN.

           COMPUTE BAL-WK ROUNDED =
              LN-CURBAL + LN-INTBAL + LN-LCBAL + LN-OTHBAL.
           IF BAL-WK < BEG-BAL OR BAL-WK > END-BAL
              GO TO NEXT-LN.

           IF OWNED = "O"
              IF LN-SERVACCT = "Y"
                 GO TO NEXT-LN.
           IF OWNED = "S"
              IF LN-SERVACCT = "N"
                 GO TO NEXT-LN.

           IF PL = "O"
              IF LN-PLDATE = 0
                 GO TO NEXT-LN.
           IF PL = "S"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-LN.
           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "A"
                 IF LN-PLCD NOT = "P"
                    GO TO NEXT-LN.
           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "I"
                 IF LN-PLCD NOT = "I"
                    IF LN-PLCD NOT = "W"
                       GO TO NEXT-LN.

           IF LN-SALEFIN NOT = "Y"
              GO TO CONT-LN.

           IF RC = "F"
              IF LN-DLRECOURSE NOT = "F"
                 GO TO NEXT-LN.
           IF RC = "P"
              IF LN-DLRECOURSE NOT = "P"
                 GO TO NEXT-LN.
           IF RC = "N"
              IF LN-DLRECOURSE NOT = "N"
                 GO TO NEXT-LN.

       CONT-LN.
           IF JD = "O"
              IF LN-JDDATE = 0
                 GO TO NEXT-LN.
           IF JD = "S"
              IF LN-JDDATE NOT = 0
                 GO TO NEXT-LN.
           IF ZBAL = "O"
              IF LN-CURBAL NOT = 0 OR
                  LN-INTBAL NOT = 0 OR
                   LN-LCBAL NOT = 0 OR
                    LN-OTHBAL NOT = 0
                 GO TO NEXT-LN.
           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 IF LN-INTBAL = 0
                    IF LN-LCBAL = 0
                       IF LN-OTHBAL = 0
                          GO TO NEXT-LN.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *       PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF IO-FG NOT = 0
                 GO TO NEXT-LN.

           PERFORM COMPUTE-RECENCY.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-PAID-AHEAD.
           IF CONTRACTUAL < BEG-AGE OR CONTRACTUAL > END-AGE
              GO TO NEXT-LN.
           IF RECDEL < BEG-RAGE OR RECDEL > END-RAGE
              GO TO NEXT-LN.

           IF EXT-ROUTE-BUF = "00"
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE LN-OWNBR TO DISPLAY-BRNO
                 MOVE LN-ACCTNO TO DISPLAY-ACCT
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE MESS-LIST TO WR-LIST
                 PERFORM CALL-WRFORM.

           PERFORM CREATE-WK-RECORD.

           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-LTC1-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND.

           GO TO END-PROGRAM.


      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE WFARGO-QUEUE-POS TO Q-POS.
           MOVE WFARGO-COPIES-POS TO C-POS.
           MOVE WFARGO-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "LOAN EXTRACT IN PROGRESS    ........" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO AGE-DATE AGEING-DATE.
      *    PERFORM OPEN-SP1-FILE.
           PERFORM ARRAYSP-LOAD.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

      * CREATE OUTPUT ASCII FILE:
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           MOVE "WFARGO" TO MKTEMP-NAME.
           MOVE SPACES TO MESS.

           MOVE BEG-BR TO HOLD-BEGBR.
           STRING MKTEMP-BUF, ".", HOLD-BEGBR
                  DELIMITED BY "  "  INTO MESS.
           MOVE MESS TO MKTEMP-BUF.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              PERFORM OPEN-WK-FILE-OUTPUT
              GO TO INITIALIZATION-EXIT.

        INITIALIZATION-ASK.
           PERFORM ALARM.
           MOVE "FILE ALREADY EXISTS, CONTINUE (Y/N):" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO INITIALIZATION-ASK.
           IF VDUBUF = "N"
              MOVE "E" TO EXT-EOBUF-ERRCD
              GO TO INITIALIZATION-EXIT.
           IF VDUBUF NOT = "Y"
              GO TO INITIALIZATION-ASK.

           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-WK-FILE-OUTPUT.

       INITIALIZATION-EXIT.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************
           MOVE EXT-EOBUF-DATE2 TO AGE-DATE.

      ****************************************
       ENTER-PARAMETERS SECTION.
      ****************************************
       ENTER-PARM.
      * FOR RE-ENTRY ON F4 FROM ACCEPT.
           IF VDUSTATUS = "1U" GO TO COLCDENT.

       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
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
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO CL-01.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO CL-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.
           GO TO DL-01.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       DL-01.
           MOVE "ENNN042DL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-01.
           MOVE VDUNUM2 TO BEG-DEALER.
       DL-02.
           MOVE "ENNN042DL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-02.
           MOVE VDUNUM2 TO END-DEALER.
           IF BEG-DEALER > END-DEALER
              PERFORM INVALID-RANGE
              GO TO DL-01.
           GO TO AGE-01.
       ALL-DL.
           MOVE "SNNN042DL1." TO VDU.
           MOVE 0 TO BEG-DEALER VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNN042DL2." TO VDU.
           MOVE 9999.99 TO END-DEALER VDUNUM2.
           PERFORM SCREENIO.
       AGE-01.
           MOVE "ENNN040AGE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLAGE.
           IF VDUSTATUS = "1U" GO TO DL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGE-01.
           MOVE VDUNUM0 TO BEG-AGE.
       AGE-02.
           MOVE "ENNN040AGE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AGE-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGE-02.
           MOVE VDUNUM0 TO END-AGE.
           IF BEG-AGE > END-AGE
              PERFORM INVALID-RANGE
              GO TO AGE-01.
           GO TO RAGE01.
       ALLAGE.
           MOVE "SNNN040AGE1." TO VDU.
           MOVE 0 TO BEG-AGE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040AGE2." TO VDU.
           MOVE 9999 TO END-AGE VDUNUM0.
           PERFORM SCREENIO.
       RAGE01.
           MOVE "ENNN040RAGE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALRAGE.
           IF VDUSTATUS = "1U" GO TO AGE-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RAGE01.
           MOVE VDUNUM0 TO BEG-RAGE.
       RAGE02.
           MOVE "ENNN040RAGE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RAGE01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RAGE02.
           MOVE VDUNUM0 TO END-RAGE.
           IF BEG-RAGE > END-RAGE
              PERFORM INVALID-RANGE
              GO TO RAGE01.
           GO TO BAL01.
       ALRAGE.
           MOVE "SNNN040RAGE1." TO VDU.
           MOVE 0 TO BEG-RAGE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040RAGE2." TO VDU.
           MOVE 9999 TO END-RAGE VDUNUM0.
           PERFORM SCREENIO.
       BAL01.
           MOVE "ENNN070BB1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLBAL.
           IF VDUSTATUS = "1U" GO TO RAGE02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BAL01.
           MOVE VDUNUM0 TO BEG-BAL.
       BAL02.
           MOVE "ENNN070BB2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BAL01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BAL02.
           MOVE VDUNUM0 TO END-BAL.
           IF BEG-BAL > END-BAL
              PERFORM INVALID-RANGE
              GO TO BAL01.
           GO TO AMT01.
       ALLBAL.
           MOVE "SNNN070BB1." TO VDU.
           MOVE 0 TO BEG-BAL VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN070BB2." TO VDU.
           MOVE 9999999 TO END-BAL VDUNUM0.
           PERFORM SCREENIO.
       AMT01.
           MOVE "ENNN060LA1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLAMT.
           IF VDUSTATUS = "1U" GO TO BAL02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT01.
           MOVE VDUNUM0 TO BEG-LNAMT.
       AMT02.
           MOVE "ENNN060LA2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT02.
           MOVE VDUNUM0 TO END-LNAMT.
           IF BEG-LNAMT > END-LNAMT
              PERFORM INVALID-RANGE
              GO TO AMT01.
           GO TO TYPEYN.
       ALLAMT.
           MOVE "SNNN060LA1." TO VDU.
           MOVE 0 TO BEG-LNAMT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060LA2." TO VDU.
           MOVE 999999 TO END-LNAMT VDUNUM0.
           PERFORM SCREENIO.
       TYPEYN.
           MOVE "E  A010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TYPEYN.
           MOVE VDUBUF TO OWNED.
           IF NOT (OWNED = "O" OR "S" OR "B")
              GO TO TYPEYN.
       PLYN.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TYPEYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLYN.
           MOVE VDUBUF TO PL.
           IF PL = "I" OR "O" GO TO ACTVYN.
           IF PL NOT = "S" GO TO PLYN.
           MOVE "S  A010ACTIVE." TO VDU.
           MOVE "B" TO PLACTIVE VDUBUF.
           PERFORM SCREENIO.
           GO TO RECRSE.
       ACTVYN.
           MOVE "E  A010ACTIVE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PLYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ACTVYN.
           MOVE VDUBUF TO PLACTIVE.
           IF NOT (PLACTIVE = "I" OR "A" OR "B")
              GO TO ACTVYN.
       RECRSE.
           MOVE "E  A010RC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ACTVYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RECRSE.
           MOVE VDUBUF TO RC.
           IF NOT (RC = "F" OR "P" OR "N" OR "A")
              GO TO RECRSE.
       JDYN.
           MOVE "E  A010JD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RECRSE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO JDYN.
           MOVE VDUBUF TO JD.
           IF NOT (JD = "I" OR "S" OR "O") GO TO JDYN.
       ZBALYN.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO JDYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ZBALYN.
           MOVE VDUBUF TO ZBAL.
           IF NOT (ZBAL = "I" OR "S" OR "O") GO TO ZBALYN.
       COLCDENT.
           MOVE "E  A020COLCD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ZBALYN.
           IF VDUSTATUS = "16" GO TO ALL-COLCDENT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO COLCDENT.
           MOVE VDUBUF TO COLCD.
           MOVE COLCD TO CD-CODE.
           MOVE "CO" TO CD-TYPE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG = 9
              MOVE "INVALID COLLATERAL CODE" TO MESS
              PERFORM SEND-MESS
              GO TO COLCDENT.
           GO TO EXIT-PARM.
       ALL-COLCDENT.
           MOVE "S  A020COLCD." TO VDU.
           MOVE "**" TO COLCD VDUBUF.
           PERFORM SCREENIO.
           GO TO EXIT-PARM.
       EXIT-PARM.
           EXIT.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.
           MOVE "SN N020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N042DL1." TO VDU.
           MOVE BEG-DEALER TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SN N042DL2." TO VDU.
           MOVE END-DEALER TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SN N040AGE1." TO VDU.
           MOVE BEG-AGE TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N040AGE2." TO VDU.
           MOVE END-AGE TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N040RAGE1." TO VDU.
           MOVE BEG-RAGE TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N040RAGE2." TO VDU.
           MOVE END-RAGE TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N070BB1." TO VDU.
           MOVE BEG-BAL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N070BB2." TO VDU.
           MOVE END-BAL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N060LA1." TO VDU.
           MOVE BEG-LNAMT TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N060LA2." TO VDU.
           MOVE END-LNAMT TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A010TYPE." TO VDU.
           MOVE OWNED TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010PL." TO VDU.
           MOVE PL TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010ACTIVE." TO VDU.
           MOVE PLACTIVE TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010RC." TO VDU.
           MOVE RC TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010JD." TO VDU.
           MOVE JD TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020COLCD." TO VDU.
           MOVE COLCD TO VDUBUF.
           PERFORM SCREENIO.

      ****************************
       CREATE-WK-RECORD SECTION.
      ****************************

           MOVE SPACES TO WK-REC.

      *    MOVE LN-SSNO(1) TO BW-SSNO.
      *    PERFORM READ-BW1-FILE.
      *    IF IO-FG NOT = 0
           IF BW-SSNO = 0
              MOVE "* NOT ON FILE *" TO WK-LNAME
           ELSE
              MOVE BW-STATE TO WK-STATE
              MOVE BW-ZIP TO WK-ZIP.

           MOVE LN-OWNBR                TO WK-OWNBR.
           MOVE LN-ACCTNO               TO WK-ACCTNO.

           MOVE AGE-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE IBPC-FG                 TO WK-INTEREST-TYPE.

           MOVE LN-REGPYAMT             TO WK-REGPYAMT.
           MOVE LN-1STPYAMT             TO WK-ODDPYAMT
           IF LN-1STPYAMT NOT = LN-REGPYAMT
              MOVE LN-1STPYAMT          TO WK-ODDPYAMT
           ELSE
              MOVE LN-LASTPYAMT         TO WK-ODDPYAMT.

           PERFORM LOAN-CALCULATIONS.

           MOVE TOTAL-REPAY             TO WK-TOTREPAY.
           MOVE DISCLOSED-FINAMT        TO WK-AMTFIN.
           COMPUTE WK-ANTIC-INTEREST =
              LN-ANTICERN(1) + LN-ANTICADJ(1).
           MOVE LN-CURBAL               TO WK-CURBAL.
           COMPUTE WK-NET-BAL = LN-CURBAL -
                 (LN-ANTICERN(1) - LN-ACCERN(1)).
           COMPUTE WK-UNEARN-PRECOMP-INT =
                 (LN-ANTICERN(1) - LN-ACCERN(1)).
           MOVE LN-EFFRATE              TO WK-EFFRATE.
           MOVE LN-APRATE               TO WK-APRATE.

           MOVE LN-1STPYDATE            TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-PAYS               TO WK-TOT-PMTS-MADE.
           MOVE PDTH-DATE-FULL TO NDTE-DATE.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO WK-CONT-PAID-TO-DATE.

           MOVE LN-LOANDATE             TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO WK-LOANDATE.             


           MOVE LN-ORIG-1STPYDATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO WK-1STPAY.                
          
           MOVE LN-ORGTERM              TO WK-ORGTERM.
           
           MOVE LN-DATE-PAID-LAST       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO WK-LASTPYDATE.            

           MOVE LN-TOTNODEF             TO WK-TOTNODEF.

           MOVE LN-COLLCD               TO WK-COLLCD.

           PERFORM GET-LT-INFO.

           MOVE LN-MIDSTREAM            TO WK-BULK-PURCHASE.

           MOVE LN-REPOCD               TO WK-REPO-IND.

           PERFORM FIND-WORLD-LEGAL-STATUS.

           MOVE LN-CASHDATE             TO DATE-YYYYMMDD.     
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO WK-PROMO-IND-DATE.        

      * 12,26,26 OR 52
           MOVE LN-UNITPER-PER-YEAR TO WK-PAY-FREQ.
           MOVE LN-DLNO TO WK-DLNO.

           MOVE LN-AMT-PAID-LAST TO WK-AMT-LAST-PAY.

           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO WK-MDTE.                  

           MOVE LN-PLCD TO WK-PLCD.
           MOVE LN-POCD TO WK-POCD.
           MOVE LN-CLASS TO WK-CLASS.
           MOVE LN-SALEFIN TO WK-SALEFIN.

           MOVE X"0D"                TO EXASCII2-CR.
           MOVE X"0A"                TO EXASCII2-LF.

           ADD 1 TO WK-KEY.
           PERFORM WRITE-WK-FILE.
           MOVE "N" TO FIRST-TIME.


      ****************************
       GET-LT-INFO SECTION.
      ****************************

      *    MOVE LN-OWNBR  TO LTC-BRNO.
      *    MOVE LN-ACCTNO TO LTC-ACCTNO.
      *    MOVE 1         TO LTC-SEQNO.
      *    PERFORM READ-LTC1-FILE.
      *    IF IO-FG = 9
           IF LTC-ACCTNO = 0
              GO TO GET-BANKRUPT-INFO.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
              MOVE LTC-YEAR(SUB)   TO WK-YEAR(SUB)
              MOVE LTC-MAKE(SUB)   TO WK-MAKE(SUB)
              MOVE LTC-MODEL(SUB)  TO WK-MODEL(SUB)
              MOVE LTC-VIN(SUB)    TO WK-VIN(SUB)
           END-PERFORM.

           MOVE LTC-TITLE-NO(1)    TO WK-TITLE-1.
           MOVE LTC-TITLE-NO(2)    TO WK-TITLE-2.

       GET-BANKRUPT-INFO.
      * RENEWALS BRING FORWARD BANKRUPT TRAILERS EVEN IF DISMISSED, THIS
      * CHAPTER WILL STILL BE FILLED IN, DON'T READ UNLESS BANKRUPT!
           IF LN-BNKRPTDATE = 0
              GO TO EXIT-GET-LT-INFO.

      *     MOVE LN-OWNBR    TO LTB-BRNO.
      *     MOVE LN-ACCTNO   TO LTB-ACCTNO.
      *     MOVE 1           TO LTB-SEQNO.
      *     PERFORM READ-LTB1-FILE.
      *     IF IO-FG = 0
           IF LTB-ACCTNO NOT = 0
              MOVE LTB-CHAPTER TO WK-B-CHAPTER.

       EXIT-GET-LT-INFO.
           EXIT.

      ****************************
       FIND-WORLD-LEGAL-STATUS SECTION.
      ****************************
           MOVE LN-STATFLAGS TO TEST-STATUS-TABLE.
           MOVE SPACES       TO TEST-STATUS-TABLE2.
  
           MOVE 0  TO SUB2.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF TEST-STATUS-TB(SUB) = "B" OR 
                  TEST-STATUS-TB(SUB) = "J" OR
                   TEST-STATUS-TB(SUB) = "P" OR
                     TEST-STATUS-TB(SUB) = "L"
                  ADD 1 TO SUB2
                  MOVE TEST-STATUS-TB(SUB) TO TEST-STATUS-TB2(SUB2)
               END-IF 
           END-PERFORM.
           MOVE TEST-STATUS-TABLE2  TO WK-LEGAL-IND.
       EXIT-FIND-WORLD-LEGAL-STATUS.
           EXIT.


       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/ARRAYSP.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".


      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO WFARGO-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WFARGO-SYSDATE.
           DISPLAY WFARGO-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE WFARGO-FORM-FIELDS TO WR-BUF.
           MOVE WFARGO-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE WFARGO-HELP--FILE TO HELP-LINK-FILE.
           MOVE WFARGO-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/WFARGO_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************
       MISC-ROUTINES SECTION.
      ****************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/GETENV.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
      *COPY "LIBLP/LPLN3RN.CPY".
      * COPYMEMBER: LIBLP/LPLN3RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LN3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *-----------------------------------------------------------------
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

           IF LN3-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO LN3-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE. *> SSP - LN,BW,LTC,LTB
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           IF ( QLN3-WEND-ACCTNO = ZEROES    ) OR
              ( QLN3-WEND-ACCTNO NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN3-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN3-WEND-OWNBR
              MOVE LN-CLASS             TO QLN3-WBEG-CLASS
              MOVE ALL "9"              TO QLN3-WEND-CLASS
              MOVE LN-ACCTNO            TO QLN3-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN3-WEND-ACCTNO.

     
           MOVE BEG-DEALER              TO QWS-BEG-DEALER.
           MOVE END-DEALER              TO QWS-END-DEALER.
           MOVE BEG-LNAMT               TO QWS-BEG-LNAMT. 
           MOVE END-LNAMT               TO QWS-END-LNAMT. 
           MOVE COLCD                   TO QWS-COLCD.         
           MOVE BEG-BAL                 TO QWS-BEG-BAL.   
           MOVE END-BAL                 TO QWS-END-BAL.   
           MOVE OWNED                   TO QWS-OWNED.     
           MOVE PL                      TO QWS-PL.        
           MOVE PLACTIVE                TO QWS-PLACTIVE.  
           MOVE RC                      TO QWS-RC.        
           MOVE JD                      TO QWS-JD.        
           MOVE ZBAL                    TO QWS-ZBAL.      

      *  STORED PROCEDURE:
      *
      *   SELECT TEMP.LN_OWNBR,
      *          TEMP.LN_ACCTNO,
      *          TEMP.LN_CLASS,
      *          TEMP.LN_SUBCLASS,
      *          TEMP.LN_LAWCODE,
      *          TEMP.LN_SSNO1,
      *          TEMP.LN_MAKERCD1,
      *          TEMP.LN_SSNO2,
      *          TEMP.LN_MAKERCD2,
      *          TEMP.LN_SSNO3,
      *          TEMP.LN_MAKERCD3,
      *          TEMP.LN_SSNO4,
      *          TEMP.LN_MAKERCD4,
      *          TEMP.LN_ORGST,
      *          TEMP.LN_SPRCLASS,
      *          TEMP.LN_MIDSTREAM, 
      *          TEMP.LN_COLLCD, 
      *          TEMP.LN_POCD, 
      *          TEMP.LN_PLCD, 
      *          TEMP.LN_STATUSFG, 
      *          TEMP.LN_SALEFIN, 
      *          TEMP.LN_SERVACCT, 
      *          TEMP.LN_LOANTYPE, 
      *          TEMP.LN_PAY_SCHLD_FG, 
      *          TEMP.LN_STATFLAGS, 
      *          TEMP.LN_LOANDATE, 
      *          TEMP.LN_INTDATE, 
      *          TEMP.LN_CASHDATE, 
      *          TEMP.LN_ORIG_1STPYDATE,	 
      *          TEMP.LN_1STPYDATE, 
      *          TEMP.LN_JDDATE, 
      *          TEMP.LN_PLDATE, 
      *          TEMP.LN_ACCELDATE, 
      *          TEMP.LN_REDUDATE, 
      *          TEMP.LN_BNKRPTDATE, 
      *          TEMP.LN_APRATE,
      *          TEMP.LN_EFFRATE,
      *          TEMP.LN_JDRATE,
      *          TEMP.LN_LNAMT,
      *          TEMP.LN_INTCHG,
      *          TEMP.LN_SERCHG,
      *          TEMP.LN_EXTCHG,
      *          TEMP.LN_MAINTFEE,
      *          TEMP.LN_ORGTERM,
      *          TEMP.LN_1STPYAMT,
      *          TEMP.LN_NOREGPY,
      *          TEMP.LN_REGPYAMT,
      *          TEMP.LN_LASTPYAMT,
      *          TEMP.LN_FEECODE_1, 
      *          TEMP.LN_FEECODE_2, 
      *          TEMP.LN_FEECODE_3, 
      *          TEMP.LN_FEECODE_4, 
      *          TEMP.LN_FEECODE_5, 
      *          TEMP.LN_FEECODE_6, 
      *          TEMP.LN_FEECODE_7, 
      *          TEMP.LN_FEECODE_8, 
      *          TEMP.LN_FEECODE_9, 
      *          TEMP.LN_FEECODE_10, 
      *          TEMP.LN_FEECODE_11, 
      *          TEMP.LN_FEECODE_12, 
      *          TEMP.LN_FEECODE_13, 
      *          TEMP.LN_FEECODE_14, 
      *          TEMP.LN_FEECODE_15, 
      *          TEMP.LN_FEECODE_16, 
      *          TEMP.LN_FEECODE_17, 
      *          TEMP.LN_FEECODE_18, 
      *          TEMP.LN_FEECODE_19, 
      *          TEMP.LN_FEECODE_20, 
      *          TEMP.LN_FEEAMT_1,
      *          TEMP.LN_FEEAMT_2,
      *          TEMP.LN_FEEAMT_3,
      *          TEMP.LN_FEEAMT_4,
      *          TEMP.LN_FEEAMT_5,
      *          TEMP.LN_FEEAMT_6,
      *          TEMP.LN_FEEAMT_7,
      *          TEMP.LN_FEEAMT_8,
      *          TEMP.LN_FEEAMT_9,
      *          TEMP.LN_FEEAMT_10,
      *          TEMP.LN_FEEAMT_11,
      *          TEMP.LN_FEEAMT_12,
      *          TEMP.LN_FEEAMT_13,
      *          TEMP.LN_FEEAMT_14,
      *          TEMP.LN_FEEAMT_15,
      *          TEMP.LN_FEEAMT_16,
      *          TEMP.LN_FEEAMT_17,
      *          TEMP.LN_FEEAMT_18,
      *          TEMP.LN_FEEAMT_19,
      *          TEMP.LN_FEEAMT_20,
      *          TEMP.LN_FEEOURS_1, 
      *          TEMP.LN_FEEOURS_2, 
      *          TEMP.LN_FEEOURS_3, 
      *          TEMP.LN_FEEOURS_4, 
      *          TEMP.LN_FEEOURS_5, 
      *          TEMP.LN_FEEOURS_6, 
      *          TEMP.LN_FEEOURS_7, 
      *          TEMP.LN_FEEOURS_8, 
      *          TEMP.LN_FEEOURS_9, 
      *          TEMP.LN_FEEOURS_10, 
      *          TEMP.LN_FEEOURS_11, 
      *          TEMP.LN_FEEOURS_12, 
      *          TEMP.LN_FEEOURS_13, 
      *          TEMP.LN_FEEOURS_14, 
      *          TEMP.LN_FEEOURS_15, 
      *          TEMP.LN_FEEOURS_16, 
      *          TEMP.LN_FEEOURS_17, 
      *          TEMP.LN_FEEOURS_18, 
      *          TEMP.LN_FEEOURS_19, 
      *          TEMP.LN_FEEOURS_20, 
      *          TEMP.LN_FEEFINCHG_1, 
      *          TEMP.LN_FEEFINCHG_2, 
      *          TEMP.LN_FEEFINCHG_3, 
      *          TEMP.LN_FEEFINCHG_4, 
      *          TEMP.LN_FEEFINCHG_5, 
      *          TEMP.LN_FEEFINCHG_6, 
      *          TEMP.LN_FEEFINCHG_7, 
      *          TEMP.LN_FEEFINCHG_8, 
      *          TEMP.LN_FEEFINCHG_9, 
      *          TEMP.LN_FEEFINCHG_10, 
      *          TEMP.LN_FEEFINCHG_11, 
      *          TEMP.LN_FEEFINCHG_12, 
      *          TEMP.LN_FEEFINCHG_13, 
      *          TEMP.LN_FEEFINCHG_14, 
      *          TEMP.LN_FEEFINCHG_15, 
      *          TEMP.LN_FEEFINCHG_16, 
      *          TEMP.LN_FEEFINCHG_17, 
      *          TEMP.LN_FEEFINCHG_18, 
      *          TEMP.LN_FEEFINCHG_19, 
      *          TEMP.LN_FEEFINCHG_20, 
      *          TEMP.LN_INSOURS_1, 
      *          TEMP.LN_INSOURS_2, 
      *          TEMP.LN_INSOURS_3, 
      *          TEMP.LN_INSOURS_4, 
      *          TEMP.LN_INSOURS_5, 
      *          TEMP.LN_INSOURS_6, 
      *          TEMP.LN_INSOURS_7, 
      *          TEMP.LN_INSOURS_8, 
      *          TEMP.LN_INSOURS_9, 
      *          TEMP.LN_INSOURS_10, 
      *          TEMP.LN_INSPREM_1,
      *          TEMP.LN_INSPREM_2,
      *          TEMP.LN_INSPREM_3,
      *          TEMP.LN_INSPREM_4,
      *          TEMP.LN_INSPREM_5,
      *          TEMP.LN_INSPREM_6,
      *          TEMP.LN_INSPREM_7,
      *          TEMP.LN_INSPREM_8,
      *          TEMP.LN_INSPREM_9,
      *          TEMP.LN_INSPREM_10,
      *          TEMP.LN_INSFINCHG_1, 
      *          TEMP.LN_INSFINCHG_2, 
      *          TEMP.LN_INSFINCHG_3, 
      *          TEMP.LN_INSFINCHG_4, 
      *          TEMP.LN_INSFINCHG_5, 
      *          TEMP.LN_INSFINCHG_6, 
      *          TEMP.LN_INSFINCHG_7, 
      *          TEMP.LN_INSFINCHG_8, 
      *          TEMP.LN_INSFINCHG_9, 
      *          TEMP.LN_INSFINCHG_10, 
      *          TEMP.LN_PY_YYYYMM_RECDEL,
      *          TEMP.LN_MTD_PAYS_RECDEL,
      *          TEMP.LN_DLNO,
      *          TEMP.LN_DLRECOURSE,
      *          TEMP.LN_CURBAL,
      *          TEMP.LN_INTBAL,
      *          TEMP.LN_LCBAL,
      *          TEMP.LN_OTHBAL,
      *          TEMP.LN_ANTICERN_1,
      *          TEMP.LN_ANTICERN_2,
      *          TEMP.LN_ANTICERN_3,
      *          TEMP.LN_ANTICERN_4,
      *          TEMP.LN_ANTICERN_5,
      *          TEMP.LN_ANTICERN_6,
      *          TEMP.LN_ANTICERN_7,
      *          TEMP.LN_ANTICERN_8,
      *          TEMP.LN_ANTICERN_9,
      *          TEMP.LN_ANTICERN_10,
      *          TEMP.LN_ANTICADJ_1,
      *          TEMP.LN_ANTICADJ_2,
      *          TEMP.LN_ANTICADJ_3,
      *          TEMP.LN_ANTICADJ_4,
      *          TEMP.LN_ANTICADJ_5,
      *          TEMP.LN_ANTICADJ_6,
      *          TEMP.LN_ANTICADJ_7,
      *          TEMP.LN_ANTICADJ_8,
      *          TEMP.LN_ANTICADJ_9,
      *          TEMP.LN_ANTICADJ_10,
      *          TEMP.LN_ACCERN_1,
      *          TEMP.LN_ACCERN_2,
      *          TEMP.LN_ACCERN_3,
      *          TEMP.LN_ACCERN_4,
      *          TEMP.LN_ACCERN_5,
      *          TEMP.LN_ACCERN_6,
      *          TEMP.LN_ACCERN_7,
      *          TEMP.LN_ACCERN_8,
      *          TEMP.LN_ACCERN_9,
      *          TEMP.LN_ACCERN_10,
      *          TEMP.LN_TOTPAYMNTD,
      *          TEMP.LN_TOTNODEF,
      *          TEMP.LN_TOTNOALDEL,
      *          TEMP.LN_DATE_PAID_LAST, 
      *          TEMP.LN_AMT_PAID_LAST,
      *          TEMP.LN_CREDITLIM,
      *          TEMP.LN_SUM_ORGTERM,
      *          TEMP.LN_UNITPER_CD,
      *          TEMP.LN_UNITPER_FREQ,
      *          TEMP.LN_UNITPER_PER_YEAR,
      *          TEMP.LN_REPOCD,
      *          TEMP.BW_OWNBR, 
      *          TEMP.BW_SSNO,  
      *          TEMP.BW_LNAME, 
      *          TEMP.BW_FNAME, 
      *          TEMP.BW_STATE, 
      *          TEMP.BW_ZIP, 
      *          TEMP.LTC_BRNO,  
      *          TEMP.LTC_ACCTNO, 
      *          TEMP.LTC_SEQNO,  
      *          TEMP.LTC_DESC_1, 
      *          TEMP.LTC_MAKE_1, 
      *          TEMP.LTC_MODEL_1, 
      *          TEMP.LTC_YEAR_1, 
      *          TEMP.LTC_STYLE_1, 
      *          TEMP.LTC_CLASS_1, 
      *          TEMP.LTC_VIN_1,  
      *          TEMP.LTC_TITLE_NO_1, 
      *          TEMP.LTC_MILEAGE_1,   
      *          TEMP.LTC_VALUE_1,  
      *          TEMP.LTC_SELLPR_1,    
      *          TEMP.LTC_MISC_AMT_1,  
      *          TEMP.LTC_SENT_FLAG_1, 
      *          TEMP.LTC_VIN_CHG_DATE_1, 
      *          TEMP.LTC_DESC_2,      
      *          TEMP.LTC_MAKE_2,     
      *          TEMP.LTC_MODEL_2, 
      *          TEMP.LTC_YEAR_2,   
      *          TEMP.LTC_STYLE_2,    
      *          TEMP.LTC_CLASS_2,  
      *          TEMP.LTC_VIN_2, 
      *          TEMP.LTC_TITLE_NO_2,  
      *          TEMP.LTC_MILEAGE_2,   
      *          TEMP.LTC_VALUE_2,    
      *          TEMP.LTC_SELLPR_2,   
      *          TEMP.LTC_MISC_AMT_2,    
      *          TEMP.LTC_SENT_FLAG_2,  
      *          TEMP.LTC_VIN_CHG_DATE_2,    
      *          TEMP.LTB_BRNO,      
      *          TEMP.LTB_ACCTNO,  
      *          TEMP.LTB_SEQNO,      
      *          TEMP.LTB_CHAPTER     
      * 
      *     FROM
      * 
      * ( SELECT LNFILE.LN_OWNBR,
      *          LNFILE.LN_ACCTNO,
      *          LNFILE.LN_CLASS,
      *          LNFILE.LN_SUBCLASS,
      *          LNFILE.LN_LAWCODE,
      *          LNFILE.LN_SSNO1,
      *          LNFILE.LN_MAKERCD1,
      *          LNFILE.LN_SSNO2,
      *          LNFILE.LN_MAKERCD2,
      *          LNFILE.LN_SSNO3,
      *          LNFILE.LN_MAKERCD3,
      *          LNFILE.LN_SSNO4,
      *          LNFILE.LN_MAKERCD4,
      *          LNFILE.LN_ORGST,
      *          LNFILE.LN_SPRCLASS,
      *          LNFILE.LN_MIDSTREAM, 
      *          LNFILE.LN_COLLCD, 
      *          LNFILE.LN_POCD, 
      *          LNFILE.LN_PLCD, 
      *          LNFILE.LN_STATUSFG, 
      *          LNFILE.LN_SALEFIN, 
      *          LNFILE.LN_SERVACCT, 
      *          LNFILE.LN_LOANTYPE, 
      *          LNFILE.LN_PAY_SCHLD_FG, 
      *          LNFILE.LN_STATFLAGS, 
      *          CAST(LNFILE.LN_LOANDATE   AS VARCHAR(10)) AS LN_LOANDATE, 
      *          CAST(LNFILE.LN_INTDATE    AS VARCHAR(10)) AS LN_INTDATE , 
      *          CAST(LNFILE.LN_CASHDATE   AS VARCHAR(10)) AS LN_CASHDATE, 
      *          CAST(LNFILE.LN_ORIG_1STPYDATE			     
      *                                    AS VARCHAR(10)) AS LN_ORIG_1STPYDATE, 
      *          CAST(LNFILE.LN_1STPYDATE  AS VARCHAR(10)) AS LN_1STPYDATE, 
      *          CAST(LNFILE.LN_JDDATE     AS VARCHAR(10)) AS LN_JDDATE, 
      *          CAST(LNFILE.LN_PLDATE     AS VARCHAR(10)) AS LN_PLDATE, 
      *          CAST(LNFILE.LN_ACCELDATE  AS VARCHAR(10)) AS LN_ACCELDATE, 
      *          CAST(LNFILE.LN_REDUDATE   AS VARCHAR(10)) AS LN_REDUDATE, 
      *          CAST(LNFILE.LN_BNKRPTDATE AS VARCHAR(10)) AS LN_BNKRPTDATE, 
      *          LNFILE.LN_APRATE,
      *          LNFILE.LN_EFFRATE,
      *          LNFILE.LN_JDRATE,
      *          LNFILE.LN_LNAMT,
      *          LNFILE.LN_INTCHG,
      *          LNFILE.LN_SERCHG,
      *          LNFILE.LN_EXTCHG,
      *          LNFILE.LN_MAINTFEE,
      *          LNFILE.LN_ORGTERM,
      *          LNFILE.LN_1STPYAMT,
      *          LNFILE.LN_NOREGPY,
      *          LNFILE.LN_REGPYAMT,
      *          LNFILE.LN_LASTPYAMT,
      *          LNFILE.LN_FEECODE_1, 
      *          LNFILE.LN_FEECODE_2, 
      *          LNFILE.LN_FEECODE_3, 
      *          LNFILE.LN_FEECODE_4, 
      *          LNFILE.LN_FEECODE_5, 
      *          LNFILE.LN_FEECODE_6, 
      *          LNFILE.LN_FEECODE_7, 
      *          LNFILE.LN_FEECODE_8, 
      *          LNFILE.LN_FEECODE_9, 
      *          LNFILE.LN_FEECODE_10, 
      *          LNFILE.LN_FEECODE_11, 
      *          LNFILE.LN_FEECODE_12, 
      *          LNFILE.LN_FEECODE_13, 
      *          LNFILE.LN_FEECODE_14, 
      *          LNFILE.LN_FEECODE_15, 
      *          LNFILE.LN_FEECODE_16, 
      *          LNFILE.LN_FEECODE_17, 
      *          LNFILE.LN_FEECODE_18, 
      *          LNFILE.LN_FEECODE_19, 
      *          LNFILE.LN_FEECODE_20, 
      *          LNFILE.LN_FEEAMT_1,
      *          LNFILE.LN_FEEAMT_2,
      *          LNFILE.LN_FEEAMT_3,
      *          LNFILE.LN_FEEAMT_4,
      *          LNFILE.LN_FEEAMT_5,
      *          LNFILE.LN_FEEAMT_6,
      *          LNFILE.LN_FEEAMT_7,
      *          LNFILE.LN_FEEAMT_8,
      *          LNFILE.LN_FEEAMT_9,
      *          LNFILE.LN_FEEAMT_10,
      *          LNFILE.LN_FEEAMT_11,
      *          LNFILE.LN_FEEAMT_12,
      *          LNFILE.LN_FEEAMT_13,
      *          LNFILE.LN_FEEAMT_14,
      *          LNFILE.LN_FEEAMT_15,
      *          LNFILE.LN_FEEAMT_16,
      *          LNFILE.LN_FEEAMT_17,
      *          LNFILE.LN_FEEAMT_18,
      *          LNFILE.LN_FEEAMT_19,
      *          LNFILE.LN_FEEAMT_20,
      *          LNFILE.LN_FEEOURS_1, 
      *          LNFILE.LN_FEEOURS_2, 
      *          LNFILE.LN_FEEOURS_3, 
      *          LNFILE.LN_FEEOURS_4, 
      *          LNFILE.LN_FEEOURS_5, 
      *          LNFILE.LN_FEEOURS_6, 
      *          LNFILE.LN_FEEOURS_7, 
      *          LNFILE.LN_FEEOURS_8, 
      *          LNFILE.LN_FEEOURS_9, 
      *          LNFILE.LN_FEEOURS_10, 
      *          LNFILE.LN_FEEOURS_11, 
      *          LNFILE.LN_FEEOURS_12, 
      *          LNFILE.LN_FEEOURS_13, 
      *          LNFILE.LN_FEEOURS_14, 
      *          LNFILE.LN_FEEOURS_15, 
      *          LNFILE.LN_FEEOURS_16, 
      *          LNFILE.LN_FEEOURS_17, 
      *          LNFILE.LN_FEEOURS_18, 
      *          LNFILE.LN_FEEOURS_19, 
      *          LNFILE.LN_FEEOURS_20, 
      *          LNFILE.LN_FEEFINCHG_1, 
      *          LNFILE.LN_FEEFINCHG_2, 
      *          LNFILE.LN_FEEFINCHG_3, 
      *          LNFILE.LN_FEEFINCHG_4, 
      *          LNFILE.LN_FEEFINCHG_5, 
      *          LNFILE.LN_FEEFINCHG_6, 
      *          LNFILE.LN_FEEFINCHG_7, 
      *          LNFILE.LN_FEEFINCHG_8, 
      *          LNFILE.LN_FEEFINCHG_9, 
      *          LNFILE.LN_FEEFINCHG_10, 
      *          LNFILE.LN_FEEFINCHG_11, 
      *          LNFILE.LN_FEEFINCHG_12, 
      *          LNFILE.LN_FEEFINCHG_13, 
      *          LNFILE.LN_FEEFINCHG_14, 
      *          LNFILE.LN_FEEFINCHG_15, 
      *          LNFILE.LN_FEEFINCHG_16, 
      *          LNFILE.LN_FEEFINCHG_17, 
      *          LNFILE.LN_FEEFINCHG_18, 
      *          LNFILE.LN_FEEFINCHG_19, 
      *          LNFILE.LN_FEEFINCHG_20, 
      *          LNFILE.LN_INSOURS_1, 
      *          LNFILE.LN_INSOURS_2, 
      *          LNFILE.LN_INSOURS_3, 
      *          LNFILE.LN_INSOURS_4, 
      *          LNFILE.LN_INSOURS_5, 
      *          LNFILE.LN_INSOURS_6, 
      *          LNFILE.LN_INSOURS_7, 
      *          LNFILE.LN_INSOURS_8, 
      *          LNFILE.LN_INSOURS_9, 
      *          LNFILE.LN_INSOURS_10, 
      *          LNFILE.LN_INSPREM_1,
      *          LNFILE.LN_INSPREM_2,
      *          LNFILE.LN_INSPREM_3,
      *          LNFILE.LN_INSPREM_4,
      *          LNFILE.LN_INSPREM_5,
      *          LNFILE.LN_INSPREM_6,
      *          LNFILE.LN_INSPREM_7,
      *          LNFILE.LN_INSPREM_8,
      *          LNFILE.LN_INSPREM_9,
      *          LNFILE.LN_INSPREM_10,
      *          LNFILE.LN_INSFINCHG_1, 
      *          LNFILE.LN_INSFINCHG_2, 
      *          LNFILE.LN_INSFINCHG_3, 
      *          LNFILE.LN_INSFINCHG_4, 
      *          LNFILE.LN_INSFINCHG_5, 
      *          LNFILE.LN_INSFINCHG_6, 
      *          LNFILE.LN_INSFINCHG_7, 
      *          LNFILE.LN_INSFINCHG_8, 
      *          LNFILE.LN_INSFINCHG_9, 
      *          LNFILE.LN_INSFINCHG_10, 
      *          LNFILE.LN_PY_YYYYMM_RECDEL,
      *          LNFILE.LN_MTD_PAYS_RECDEL,
      *          LNFILE.LN_DLNO,
      *          LNFILE.LN_DLRECOURSE,
      *          LNFILE.LN_CURBAL,
      *          LNFILE.LN_INTBAL,
      *          LNFILE.LN_LCBAL,
      *          LNFILE.LN_OTHBAL,
      *          LNFILE.LN_ANTICERN_1,
      *          LNFILE.LN_ANTICERN_2,
      *          LNFILE.LN_ANTICERN_3,
      *          LNFILE.LN_ANTICERN_4,
      *          LNFILE.LN_ANTICERN_5,
      *          LNFILE.LN_ANTICERN_6,
      *          LNFILE.LN_ANTICERN_7,
      *          LNFILE.LN_ANTICERN_8,
      *          LNFILE.LN_ANTICERN_9,
      *          LNFILE.LN_ANTICERN_10,
      *          LNFILE.LN_ANTICADJ_1,
      *          LNFILE.LN_ANTICADJ_2,
      *          LNFILE.LN_ANTICADJ_3,
      *          LNFILE.LN_ANTICADJ_4,
      *          LNFILE.LN_ANTICADJ_5,
      *          LNFILE.LN_ANTICADJ_6,
      *          LNFILE.LN_ANTICADJ_7,
      *          LNFILE.LN_ANTICADJ_8,
      *          LNFILE.LN_ANTICADJ_9,
      *          LNFILE.LN_ANTICADJ_10,
      *          LNFILE.LN_ACCERN_1,
      *          LNFILE.LN_ACCERN_2,
      *          LNFILE.LN_ACCERN_3,
      *          LNFILE.LN_ACCERN_4,
      *          LNFILE.LN_ACCERN_5,
      *          LNFILE.LN_ACCERN_6,
      *          LNFILE.LN_ACCERN_7,
      *          LNFILE.LN_ACCERN_8,
      *          LNFILE.LN_ACCERN_9,
      *          LNFILE.LN_ACCERN_10,
      *          LNFILE.LN_TOTPAYMNTD,
      *          LNFILE.LN_TOTNODEF,
      *          LNFILE.LN_TOTNOALDEL,
      *          CAST(LNFILE.LN_DATE_PAID_LAST       AS VARCHAR(10))
      *                                              AS LN_DATE_PAID_LAST, 
      *          LNFILE.LN_AMT_PAID_LAST,
      *          LNFILE.LN_CREDITLIM,
      *          LNFILE.LN_SUM_ORGTERM,
      *          LNFILE.LN_UNITPER_CD,
      *          LNFILE.LN_UNITPER_FREQ,
      *          LNFILE.LN_UNITPER_PER_YEAR,
      *          LNFILE.LN_REPOCD,
      *          ISNULL(BWFILE.BW_OWNBR, 0)           AS BW_OWNBR, 
      *          ISNULL(BWFILE.BW_SSNO, 0)	          AS BW_SSNO, 
      *          ISNULL(BWFILE.BW_LNAME,'')           AS BW_LNAME,
      *          ISNULL(BWFILE.BW_FNAME,'')           AS BW_FNAME,
      *          ISNULL(BWFILE.BW_STATE,'')           AS BW_STATE,
      *          ISNULL(BWFILE.BW_ZIP,'')             AS BW_ZIP,
      *          ISNULL(LTCFILE.LTC_BRNO, 0)		  AS LTC_BRNO, 
      *          ISNULL(LTCFILE.LTC_ACCTNO, 0)		  AS LTC_ACCTNO,
      *          ISNULL(LTCFILE.LTC_SEQNO, 0)		  AS LTC_SEQNO,
      *          ISNULL(LTCFILE.LTC_DESC_1,'')        AS LTC_DESC_1,
      *          ISNULL(LTCFILE.LTC_MAKE_1,'')        AS LTC_MAKE_1,
      *          ISNULL(LTCFILE.LTC_MODEL_1,'')       AS LTC_MODEL_1,
      *          ISNULL(LTCFILE.LTC_YEAR_1,'') 	      AS LTC_YEAR_1,
      *          ISNULL(LTCFILE.LTC_STYLE_1,'') 	  AS LTC_STYLE_1,
      *          ISNULL(LTCFILE.LTC_CLASS_1,'') 	  AS LTC_CLASS_1,
      *          ISNULL(LTCFILE.LTC_VIN_1,'') 		  AS LTC_VIN_1,
      *          ISNULL(LTCFILE.LTC_TITLE_NO_1,'')    AS LTC_TITLE_NO_1,
      *          ISNULL(LTCFILE.LTC_MILEAGE_1, 0) 	  AS LTC_MILEAGE_1,
      *          ISNULL(LTCFILE.LTC_VALUE_1, 0) 	  AS LTC_VALUE_1,
      *          ISNULL(LTCFILE.LTC_SELLPR_1, 0) 	  AS LTC_SELLPR_1,
      *          ISNULL(LTCFILE.LTC_MISC_AMT_1, 0)    AS LTC_MISC_AMT_1,
      *          ISNULL(LTCFILE.LTC_SENT_FLAG_1,'')   AS LTC_SENT_FLAG_1,
      *          ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_1  
      *                      AS VARCHAR(10)),'0')     AS LTC_VIN_CHG_DATE_1, 
      *          ISNULL(LTCFILE.LTC_DESC_2,'') 	      AS LTC_DESC_2,
      *          ISNULL(LTCFILE.LTC_MAKE_2,'')  	  AS LTC_MAKE_2,
      *          ISNULL(LTCFILE.LTC_MODEL_2,'') 	  AS LTC_MODEL_2,
      *          ISNULL(LTCFILE.LTC_YEAR_2,'') 	      AS LTC_YEAR_2,
      *          ISNULL(LTCFILE.LTC_STYLE_2,'') 	  AS LTC_STYLE_2,
      *          ISNULL(LTCFILE.LTC_CLASS_2,'') 	  AS LTC_CLASS_2,
      *          ISNULL(LTCFILE.LTC_VIN_2,'') 		  AS LTC_VIN_2,
      *          ISNULL(LTCFILE.LTC_TITLE_NO_2,'')    AS LTC_TITLE_NO_2,
      *          ISNULL(LTCFILE.LTC_MILEAGE_2, 0) 	  AS LTC_MILEAGE_2,
      *          ISNULL(LTCFILE.LTC_VALUE_2, 0) 	  AS LTC_VALUE_2,
      *          ISNULL(LTCFILE.LTC_SELLPR_2, 0) 	  AS LTC_SELLPR_2,
      *          ISNULL(LTCFILE.LTC_MISC_AMT_2, 0)    AS LTC_MISC_AMT_2,
      *          ISNULL(LTCFILE.LTC_SENT_FLAG_2,'')   AS LTC_SENT_FLAG_2,
      *          ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_2  
      *                      AS VARCHAR(10)),'0')     AS LTC_VIN_CHG_DATE_2,	    
      *          ISNULL(LTBFILE.LTB_BRNO, 0)	      AS LTB_BRNO,  
      *          ISNULL(LTBFILE.LTB_ACCTNO, 0)	      AS LTB_ACCTNO,
      *          ISNULL(LTBFILE.LTB_SEQNO, 0)	      AS LTB_SEQNO, 
      *          ISNULL(LTBFILE.LTB_CHAPTER, 0)       AS LTB_CHAPTER, 
      * 
      *          CASE WHEN LNFILE.LN_POCD      IN ( '*I', '**')  
      *               THEN 'N'
      *               WHEN (LNFILE.LN_DLNO      < @QWS_BEG_DEALER
      *                 OR  LNFILE.LN_DLNO      > @QWS_END_DEALER)    
      *               THEN 'N'
      *               WHEN (LNFILE.LN_LNAMT     < @QWS_BEG_LNAMT
      *                 OR  LNFILE.LN_LNAMT     > @QWS_END_LNAMT)    
      *               THEN 'N'
      *               WHEN (@QWS_COLCD         != '**' 
      *                AND  LNFILE.LN_COLLCD   != @QWS_COLCD)    
      *               THEN 'N'
      *               WHEN (((LNFILE.LN_CURBAL  + LNFILE.LN_INTBAL +
      *                       LNFILE.LN_LCBAL   + LNFILE.LN_OTHBAL)
      *                                         < @QWS_BEG_BAL)
      *                OR   ((LNFILE.LN_CURBAL  + LNFILE.LN_INTBAL +
      *                       LNFILE.LN_LCBAL   + LNFILE.LN_OTHBAL)
      *                                         > @QWS_END_BAL))
      *               THEN 'N'
      *               WHEN (@QWS_OWNED          = 'O' 
      *                AND  LNFILE.LN_SERVACCT  = 'Y')    
      *               THEN 'N'
      *               WHEN (@QWS_OWNED          = 'S' 
      *                AND  LNFILE.LN_SERVACCT  = 'N')    
      *               THEN 'N'
      *               WHEN (@QWS_PL             = 'O' 
      *                AND  LNFILE.LN_PLDATE    = '1900-01-01')     
      *               THEN 'N'
      *               WHEN (@QWS_PL             = 'S' 
      *                AND  LNFILE.LN_PLDATE   != '1900-01-01')      
      *               THEN 'N'
      *               WHEN (LNFILE.LN_PLDATE   != '1900-01-01'
      *                AND  @QWS_PLACTIVE       = 'A' 
      *                AND  LNFILE.LN_PLCD     != 'P')   
      *               THEN 'N'
      *               WHEN (LNFILE.LN_PLDATE   != '1900-01-01'
      *                AND  @QWS_PLACTIVE       = 'I' 
      *                AND  LNFILE.LN_PLCD NOT IN ('I', 'W'))    
      *               THEN 'N'
      *               WHEN (LNFILE.LN_SALEFIN   = 'Y'
      *                AND  @QWS_RC             = 'F'
      *                AND  LNFILE.LN_DLRECOURSE
      *                                        != 'F')
      *               THEN 'N'
      *               WHEN (LNFILE.LN_SALEFIN   = 'Y'
      *                AND  @QWS_RC             = 'P'
      *                AND  LNFILE.LN_DLRECOURSE
      *                                        != 'P')
      *               THEN 'N'
      *               WHEN (LNFILE.LN_SALEFIN   = 'Y'
      *                AND  @QWS_RC             = 'N'
      *                AND  LNFILE.LN_DLRECOURSE
      *                                        != 'N')
      *               THEN 'N'
      *               WHEN (@QWS_JD             = 'O' 
      *                AND  LNFILE.LN_JDDATE    = '1900-01-01')    
      *               THEN 'N'
      *               WHEN (@QWS_JD             = 'S' 
      *                AND  LNFILE.LN_JDDATE   != '1900-01-01')    
      *               THEN 'N'
      *               WHEN @QWS_ZBAL            = 'O' 
      *                AND (LNFILE.LN_CURBAL   != 0
      *                 OR  LNFILE.LN_INTBAL   != 0
      *                 OR  LNFILE.LN_LCBAL    != 0 
      *                 OR  LNFILE.LN_OTHBAL   != 0) 
      *               THEN 'N'	
      *               WHEN @QWS_ZBAL           = 'S' 
      *                AND (LNFILE.LN_CURBAL   = 0
      *                AND  LNFILE.LN_INTBAL   = 0
      *                AND  LNFILE.LN_LCBAL    = 0 
      *                AND  LNFILE.LN_OTHBAL   = 0) 
      *               THEN 'N'	
      *               ELSE 'Y'
      *           END                          AS USE_SW
      * 
      *     FROM DBO.LNFILE  LNFILE
      * 
      *     LEFT OUTER JOIN
      *          DBO.BWFILE  BWFILE
      *       ON (  BWFILE.BW_OWNBR         = LNFILE.LN_OWNBR
      *      AND    BWFILE.BW_SSNO          = LNFILE.LN_SSNO1)
      *     
      *     LEFT OUTER JOIN     
      *          DBO.LTCFILE  LTCFILE     
      *       ON (  LTCFILE.LTC_BRNO        = LNFILE.LN_OWNBR
      *      AND    LTCFILE.LTC_ACCTNO      = LNFILE.LN_ACCTNO
      *      AND    LTCFILE.LTC_SEQNO       = 1 )
      * 
      *     LEFT OUTER JOIN
      *          DBO.LTBFILE  LTBFILE
      *       ON (  LTBFILE.LTB_BRNO        = LNFILE.LN_OWNBR
      *      AND    LTBFILE.LTB_ACCTNO      = LNFILE.LN_ACCTNO
      *      AND    LTBFILE.LTB_SEQNO       = 1 
      *      AND    LNFILE.LN_BNKRPTDATE   != '1900-01-01')
      * 
      *    WHERE (  LNFILE.LN_OWNBR         = @QLN3_WBEG_OWNBR
      *      AND    LNFILE.LN_CLASS   BETWEEN @QLN3_WBEG_CLASS
      *                                   AND @QLN3_WEND_CLASS  )) TEMP 
      * 
      *    WHERE TEMP.USE_SW = 'Y'
      * 
      *    ORDER BY TEMP.LN_OWNBR,
      *             TEMP.LN_CLASS,
      *             TEMP.LN_ACCTNO; 


           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = EXEC SSP_WFARGO_LNFILE_SELECT 
              ( :QLN3-WBEG-OWNBR, 
                :QLN3-WBEG-CLASS, 
                :QLN3-WEND-CLASS,
                :QWS-BEG-DEALER,  
                :QWS-END-DEALER, 
                :QWS-BEG-LNAMT,  
                :QWS-END-LNAMT,  
                :QWS-COLCD,      
                :QWS-BEG-BAL,    
                :QWS-END-BAL,    
                :QWS-OWNED,      
                :QWS-PL,         
                :QWS-PLACTIVE,   
                :QWS-RC,         
                :QWS-JD,         
                :QWS-ZBAL )
           END-EXEC.
 
           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

      *    MOVE ZEROES TO QLN3-WBEG-OWNBR
      *                   QLN3-WEND-OWNBR
      *                   QLN3-WBEG-CLASS
      *                   QLN3-WEND-CLASS
      *                   QLN3-WBEG-ACCTNO
      *                   QLN3-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT. *> EMBEDDED - LN,BW,LTC,LTB
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.
           INITIALIZE QBW-REC.
           INITIALIZE QLTC-REC.
           INITIALIZE QLTB-REC.
    
           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

              EXEC SQL 
                   FETCH NEXT LN3_CURSOR INTO
                     :QLN-OWNBR, 
                     :QLN-ACCTNO,
                     :QLN-CLASS,
                     :QLN-SUBCLASS,
                     :QLN-LAWCODE,
                     :QLN-SSNO1,
                     :QLN-MAKERCD1,
                     :QLN-SSNO2,
                     :QLN-MAKERCD2,
                     :QLN-SSNO3,
                     :QLN-MAKERCD3,
                     :QLN-SSNO4,
                     :QLN-MAKERCD4,
                     :QLN-ORGST,
                     :QLN-SPRCLASS,
                     :QLN-MIDSTREAM, 
                     :QLN-COLLCD, 
                     :QLN-POCD, 
                     :QLN-PLCD, 
                     :QLN-STATUSFG, 
                     :QLN-SALEFIN, 
                     :QLN-SERVACCT, 
                     :QLN-LOANTYPE, 
                     :QLN-PAY-SCHLD-FG, 
                     :QLN-STATFLAGS, 
                     :QLN-LOANDATE, 
                     :QLN-INTDATE, 
                     :QLN-CASHDATE, 
                     :QLN-ORIG-1STPYDATE,	 
                     :QLN-1STPYDATE, 
                     :QLN-JDDATE, 
                     :QLN-PLDATE, 
                     :QLN-ACCELDATE, 
                     :QLN-REDUDATE, 
                     :QLN-BNKRPTDATE, 
                     :QLN-APRATE,
                     :QLN-EFFRATE,
                     :QLN-JDRATE,
                     :QLN-LNAMT,
                     :QLN-INTCHG,
                     :QLN-SERCHG,
                     :QLN-EXTCHG,
                     :QLN-MAINTFEE,
                     :QLN-ORGTERM,
                     :QLN-1STPYAMT,
                     :QLN-NOREGPY,
                     :QLN-REGPYAMT,
                     :QLN-LASTPYAMT,
                     :QLN-FEECODE-1, 
                     :QLN-FEECODE-2, 
                     :QLN-FEECODE-3, 
                     :QLN-FEECODE-4, 
                     :QLN-FEECODE-5, 
                     :QLN-FEECODE-6, 
                     :QLN-FEECODE-7, 
                     :QLN-FEECODE-8, 
                     :QLN-FEECODE-9, 
                     :QLN-FEECODE-10, 
                     :QLN-FEECODE-11, 
                     :QLN-FEECODE-12, 
                     :QLN-FEECODE-13, 
                     :QLN-FEECODE-14, 
                     :QLN-FEECODE-15, 
                     :QLN-FEECODE-16, 
                     :QLN-FEECODE-17, 
                     :QLN-FEECODE-18, 
                     :QLN-FEECODE-19, 
                     :QLN-FEECODE-20, 
                     :QLN-FEEAMT-1,
                     :QLN-FEEAMT-2,
                     :QLN-FEEAMT-3,
                     :QLN-FEEAMT-4,
                     :QLN-FEEAMT-5,
                     :QLN-FEEAMT-6,
                     :QLN-FEEAMT-7,
                     :QLN-FEEAMT-8,
                     :QLN-FEEAMT-9,
                     :QLN-FEEAMT-10,
                     :QLN-FEEAMT-11,
                     :QLN-FEEAMT-12,
                     :QLN-FEEAMT-13,
                     :QLN-FEEAMT-14,
                     :QLN-FEEAMT-15,
                     :QLN-FEEAMT-16,
                     :QLN-FEEAMT-17,
                     :QLN-FEEAMT-18,
                     :QLN-FEEAMT-19,
                     :QLN-FEEAMT-20,
                     :QLN-FEEOURS-1, 
                     :QLN-FEEOURS-2, 
                     :QLN-FEEOURS-3, 
                     :QLN-FEEOURS-4, 
                     :QLN-FEEOURS-5, 
                     :QLN-FEEOURS-6, 
                     :QLN-FEEOURS-7, 
                     :QLN-FEEOURS-8, 
                     :QLN-FEEOURS-9, 
                     :QLN-FEEOURS-10, 
                     :QLN-FEEOURS-11, 
                     :QLN-FEEOURS-12, 
                     :QLN-FEEOURS-13, 
                     :QLN-FEEOURS-14, 
                     :QLN-FEEOURS-15, 
                     :QLN-FEEOURS-16, 
                     :QLN-FEEOURS-17, 
                     :QLN-FEEOURS-18, 
                     :QLN-FEEOURS-19, 
                     :QLN-FEEOURS-20, 
                     :QLN-FEEFINCHG-1, 
                     :QLN-FEEFINCHG-2, 
                     :QLN-FEEFINCHG-3, 
                     :QLN-FEEFINCHG-4, 
                     :QLN-FEEFINCHG-5, 
                     :QLN-FEEFINCHG-6, 
                     :QLN-FEEFINCHG-7, 
                     :QLN-FEEFINCHG-8, 
                     :QLN-FEEFINCHG-9, 
                     :QLN-FEEFINCHG-10, 
                     :QLN-FEEFINCHG-11, 
                     :QLN-FEEFINCHG-12, 
                     :QLN-FEEFINCHG-13, 
                     :QLN-FEEFINCHG-14, 
                     :QLN-FEEFINCHG-15, 
                     :QLN-FEEFINCHG-16, 
                     :QLN-FEEFINCHG-17, 
                     :QLN-FEEFINCHG-18, 
                     :QLN-FEEFINCHG-19, 
                     :QLN-FEEFINCHG-20, 
                     :QLN-INSOURS-1, 
                     :QLN-INSOURS-2, 
                     :QLN-INSOURS-3, 
                     :QLN-INSOURS-4, 
                     :QLN-INSOURS-5, 
                     :QLN-INSOURS-6, 
                     :QLN-INSOURS-7, 
                     :QLN-INSOURS-8, 
                     :QLN-INSOURS-9, 
                     :QLN-INSOURS-10, 
                     :QLN-INSPREM-1,
                     :QLN-INSPREM-2,
                     :QLN-INSPREM-3,
                     :QLN-INSPREM-4,
                     :QLN-INSPREM-5,
                     :QLN-INSPREM-6,
                     :QLN-INSPREM-7,
                     :QLN-INSPREM-8,
                     :QLN-INSPREM-9,
                     :QLN-INSPREM-10,
                     :QLN-INSFINCHG-1, 
                     :QLN-INSFINCHG-2, 
                     :QLN-INSFINCHG-3, 
                     :QLN-INSFINCHG-4, 
                     :QLN-INSFINCHG-5, 
                     :QLN-INSFINCHG-6, 
                     :QLN-INSFINCHG-7, 
                     :QLN-INSFINCHG-8, 
                     :QLN-INSFINCHG-9, 
                     :QLN-INSFINCHG-10, 
                     :QLN-PY-YYYYMM-RECDEL,
                     :QLN-MTD-PAYS-RECDEL,
                     :QLN-DLNO,
                     :QLN-DLRECOURSE,
                     :QLN-CURBAL,
                     :QLN-INTBAL,
                     :QLN-LCBAL,
                     :QLN-OTHBAL,
                     :QLN-ANTICERN-1,
                     :QLN-ANTICERN-2,
                     :QLN-ANTICERN-3,
                     :QLN-ANTICERN-4,
                     :QLN-ANTICERN-5,
                     :QLN-ANTICERN-6,
                     :QLN-ANTICERN-7,
                     :QLN-ANTICERN-8,
                     :QLN-ANTICERN-9,
                     :QLN-ANTICERN-10,
                     :QLN-ANTICADJ-1,
                     :QLN-ANTICADJ-2,
                     :QLN-ANTICADJ-3,
                     :QLN-ANTICADJ-4,
                     :QLN-ANTICADJ-5,
                     :QLN-ANTICADJ-6,
                     :QLN-ANTICADJ-7,
                     :QLN-ANTICADJ-8,
                     :QLN-ANTICADJ-9,
                     :QLN-ANTICADJ-10,
                     :QLN-ACCERN-1,
                     :QLN-ACCERN-2,
                     :QLN-ACCERN-3,
                     :QLN-ACCERN-4,
                     :QLN-ACCERN-5,
                     :QLN-ACCERN-6,
                     :QLN-ACCERN-7,
                     :QLN-ACCERN-8,
                     :QLN-ACCERN-9,
                     :QLN-ACCERN-10,
                     :QLN-TOTPAYMNTD,
                     :QLN-TOTNODEF,
                     :QLN-TOTNOALDEL,
                     :QLN-DATE-PAID-LAST, 
                     :QLN-AMT-PAID-LAST,
                     :QLN-CREDITLIM,
                     :QLN-SUM-ORGTERM,
                     :QLN-UNITPER-CD,
                     :QLN-UNITPER-FREQ,
                     :QLN-UNITPER-PER-YEAR,
                     :QLN-REPOCD,
                     :QBW-OWNBR, 
                     :QBW-SSNO,  
                     :QBW-LNAME, 
                     :QBW-FNAME,
                     :QBW-STATE, 
                     :QBW-ZIP, 
                     :QLTC-BRNO,  
                     :QLTC-ACCTNO, 
                     :QLTC-SEQNO,  
                     :QLTC-DESC-1, 
                     :QLTC-MAKE-1, 
                     :QLTC-MODEL-1, 
                     :QLTC-YEAR-1, 
                     :QLTC-STYLE-1, 
                     :QLTC-CLASS-1, 
                     :QLTC-VIN-1, 
                     :QLTC-TITLE-NO-1, 
                     :QLTC-MILEAGE-1,   
                     :QLTC-VALUE-1,   
                     :QLTC-SELLPR-1,    
                     :QLTC-MISC-AMT-1,  
                     :QLTC-SENT-FLAG-1, 
                     :QLTC-VIN-CHG-DATE-1, 
                     :QLTC-DESC-2,   
                     :QLTC-MAKE-2,    
                     :QLTC-MODEL-2,   
                     :QLTC-YEAR-2,   
                     :QLTC-STYLE-2,  
                     :QLTC-CLASS-2,   
                     :QLTC-VIN-2, 
                     :QLTC-TITLE-NO-2,  
                     :QLTC-MILEAGE-2,   
                     :QLTC-VALUE-2,    
                     :QLTC-SELLPR-2,  
                     :QLTC-MISC-AMT-2,    
                     :QLTC-SENT-FLAG-2,  
                     :QLTC-VIN-CHG-DATE-2,    
                     :QLTB-BRNO,       
                     :QLTB-ACCTNO,   
                     :QLTB-SEQNO,      
                     :QLTB-CHAPTER  
              END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
               GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
               PERFORM GET-LN-FIELDS
               PERFORM GET-BW-FIELDS
               PERFORM GET-LTC-FIELDS
               PERFORM GET-LTB-FIELDS.
 
          EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.
  
      *-----------------------------------------------------------------
       CLOSE-LN3-CONNECTION.

           IF LN3-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LN3_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LN3-CONN-OPEN-SW.

      *-----------------------------------------------------------------

       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTB1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       OPEN-WK-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-IFN TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE-OUTPUT.
       WRITE-WK-FILE.
           PERFORM WRITE-IT.
           MOVE WK-IFN TO E-FILE.
           WRITE WK-REC.
           IF IO-FG = 8
             GO TO WRITE-WK-FILE.
       CLOSE-WK-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *************************
       END-PROGRAM SECTION.
      *************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY WFARGO-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
