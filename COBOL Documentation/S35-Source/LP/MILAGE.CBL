       IDENTIFICATION DIVISION.
       PROGRAM-ID.      MILAGE.
       DATE-WRITTEN. 07-02-08
      ******************************************************************
      *                      (LP)
      *  DESCRIPTION:  MILEAGE REPORT - FROM END-OF-MONTH CASH & BANK
      *                REGISTER, ADDED GL BRANCH & ACCOUNT RANGE
      *                WORLD PR# 591
      *         CASH DISBURSE REGISTER - GL RANGE
      *
      *  *** NOTE *** IN THIS PROGRAM, TC-TYPE = "MS" DOES NOT SET A
      *               VALUE FOR WK2-TYPE IN PARAGRAPH 'SORT-TRC-NEXT'.
      *               WK2-TYPE IS USED TO DETERMINE THE SORT ORDER OF    
      *               THE DATA ON THE REPORT. CURRENTLY, THESE TYPES 
      *               OF RECORDS ARE USING THE PREVIOUS WK2-TYPE VALUE
      *               TO DETERMINE WHERE IT IS PRINTED ON THE REPORT,
      *               WHICH IS INCORRECT IN BOTH S35 & A15. CHANGING
      *               S35 TO DEFAULT THESE TYPES OF RECORDS TO A '0', 
      *               WHERE THEY WILL BE PRINTED 1ST ON THE REPORT.
      *               BECAUSE OF THIS, A15 AND S35 WILL NOT BE IN 
      *               THE SAME SORTED ORDER ON THE REPORT. *** NOTE ***
      *
      *
      *  PRINTS ONLY TRC TYPES "CA", "RR", "RC", "RP" AND "BK"
      *                        "RD" NEW TYPE
      *              TRP "RV" THAT ARE "NSF"
      *              TRD DEPOSITS
      *
      *  OPTIONALLY PRINTS MISC. CHECKS, MISC-CKS-FG
      *  OPTIONALLY PRINTS GL SUMMARY, GL-SUMMARY-FG
      *  RANGE OF GL BRANCHES & ACCOUNTS
      *
      * USES GROUP LOGIC
      * LOOPS THRU TRCFILE, TRDFILE AND TRPFILE AND WRITES WKFILE2
      * LOOPS THRU WKFILE2 AND READS EACH TR RECORD BY KEY AGAIN, PRINT
      *    DETAIL LINE, WRITE WKFILE BY BRANCH/GLNO
      * END OF PROGRAM, PRINT GRAND  TOTALS
      *
      *  REV:
      *  BLM 080718 FIX LOOP IF GROUP SELECTED FOR GL BRANCH RANGE, ADD
      *             GRAND TOTALS
      *  BAH 081121 CHANGE MISC CHECK FLAG TO I/S/O INSTEAD OF Y/N
      *               WORLD PR# 611
      *  BAH 170705 ACTIVATE 8 DIGIT DATES, PD0006
      *   CS 190516 CHANGED HOLD-GI1-KEY TO X(7) FROM X(6), QA0023
      * BAH 20190919 REMOVED ACCESS-CALL ON TRCFILE, TRD & TRP
      * BAH 20210104 A15 DID NOT INCLUDE DEPOSITS UNLESS YOU SAID F3 FOR ALL
      *              GL BRANCHES
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * RMZ 20230918 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *       *** SO TRCFILE, TRDFILE & TRPFILE'S ARE NOT READ MULTIPLE 
      *           TIMES IN THE PROGRAM, TRC-REC, TRD-REC & TRP-REC HAVE BEEN
      *           ADDED TO THE WK2 FILE, ALONG WITH HARDCODED SIZES.  IF  
      *           THE SIZE OF THESE TABLES INCREASE, THE 78 LEVELS ON 
      *           WK2-FILE WILL ALSO NEED TO BE INCREASED. ***
      *
      * JKC 20230920 MADE A CHANGE FROM 'BAH 20210104' TO NOW BE A15 DID
      *              NOT INCLUDE DEPOSITS UNLESS YOU SAID F3 FOR ALL
      *              GL ACCOUNTS (NOT GL BRANCHES)
      * RMZ 20240116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BR         PIC 9(4).
               05  WK-GLNO       PIC 9(11).
           03  WK-DRGLAMT        PIC S9(9)V99    COMP-3.
           03  WK-CRGLAMT        PIC S9(9)V99    COMP-3.

       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.

       78  WK2-TRC-SIZE                    VALUE 650. 
       78  WK2-TRD-SIZE                    VALUE 170.  
       78  WK2-TRP-SIZE                    VALUE 850.  

       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRNO                PIC 9(4).
               05  WK2-TYPE                PIC 9.
               05  WK2-DATE                PIC 9(8).
               05  WK2-CODE                PIC X.
               05  WK2-CLASS               PIC 9(3).
               05  WK2-SUBKEY.
                   07  WK2-CKNO            PIC 9(6).
                   07  WK2-SEQ             PIC 9(5).
                   07  WK2-ACCTNO          PIC 9(8).
                   07  WK2-LINE            PIC 9(3).
           03  WK2-TRC-REC                 PIC X(WK2-TRC-SIZE).
           03  WK2-TRD-REC                 PIC X(WK2-TRD-SIZE).
           03  WK2-TRP-REC                 PIC X(WK2-TRP-SIZE).
           03  WK2-BW-OWNBR                PIC 9(4).
           03  WK2-BW-SSNO                 PIC 9(9).
           03  WK2-BW-LNAME                PIC X(35).
           03  WK2-BW-FNAME                PIC X(35).
           03  WK2-LP-BRNO                 PIC 9(4).
           03  WK2-LP-ACCTNO               PIC 9(8).
           03  WK2-LP-SEQNO                PIC 9(5).
           03  WK2-LP-PAYDATE              PIC 9(8)      COMP-3. 



       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/MILAGE.CBL S35 12/22/23-11:37:04 >".
      ****************************
      *   F I L E   P A T H S    *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01TRD.CPY".
       COPY "LIBLP/LP01TRD_SQL.CPY".
       COPY "LIBLP/LPWSTRD.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK2-IFN              PIC X(3)     VALUE "WK2".
       01  WK-IFN               PIC X(2)     VALUE "WK".

       01  WK2-PATH             PIC X(35).

       01  TRP1-CONN-OPEN-SW    PIC X        VALUE "N".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6) COMP-3.
       01  SUBX                 PIC 9(6) COMP-3.
       01  FIRST-TIME           PIC X               VALUE "Y".
       01  RECORDS-FOUND        PIC X               VALUE " ".
           88  NO-RECORDS-FOUND                     VALUE "N".
       
       01  TEST-TR-CODE         PIC X               VALUE SPACES.
       01  GL-SUM-FG            PIC X               VALUE SPACES.
       01  MSEL-LIST            PIC X(5)            VALUE "MSEL.".
       01  MESS-LIST            PIC X(5)            VALUE "MESS.".
       01  WORK-GLNO            PIC 9(11).
       01  WORK-GL              REDEFINES WORK-GLNO.
           03  WORK-GLBR        PIC 9(4).
           03  WORK-GLNUM       PIC 9(7).
       01  HOLD-BRNO            PIC 9(4)            VALUE 0.
       01  HOLD-GI1-KEY         PIC X(7)            VALUE SPACES.
       01  HOLD-TYPE            PIC 9               VALUE 0.
       01  GLMAX                PIC 9       VALUE 0.
       01  GLAMT                PIC S9(9)V99 COMP-3 VALUE 0.
       01  GLNO                 PIC 9(11)    COMP-3 VALUE 0.
       01  GL-ACCT-IN-RANGE     PIC X               VALUE "N".

       01  WK-NSF-TEXT.
           03  FILLER           PIC X(13) VALUE "ORIG POSTED: ".
           03  WK-NSF-DATE      PIC 99/99/99.
       01  WK-NSF-TEXT2.
           03  FILLER           PIC X(09) VALUE "POSTED : ".
           03  WK-INTERBRANCH   PIC X(4).
           03  WK-NSF-BR        PIC 9(4).

       01  BANK-TOTAL           PIC S9(9)V99 COMP-3 OCCURS 2 VALUE 0.
       01  NSF-TOTAL            PIC S9(9)V99 COMP-3 OCCURS 2 VALUE 0.
       01  DEPOSIT-TOTAL        PIC S9(9)V99 COMP-3 OCCURS 2 VALUE 0.
       01  RECEIPT-TOTAL        PIC S9(9)V99 COMP-3 OCCURS 2 VALUE 0.
       01  DISBURSE-TOTAL       PIC S9(9)V99 COMP-3 OCCURS 2 VALUE 0.
       01  DEALER-TOTAL         PIC S9(9)V99 COMP-3 OCCURS 2 VALUE 0.
       01  CHECK-TOTAL          PIC S9(9)V99 COMP-3 OCCURS 2 VALUE 0.
       01  CK-TOTDR             PIC S9(9)V99 COMP-3 VALUE 0.
       01  CK-TOTCR             PIC S9(9)V99 COMP-3 VALUE 0.

      *-----------------------------------------------------------------
       01  WS-FIELDS.
           03  WS-ERROR-GI-FILE              PIC X(1)     VALUE "N".
           03  WS-ERROR-GI-MESSAGE.
               05  FILLER                    PIC X(10)    VALUE SPACES.
               05  FILLER                    PIC X(20)    VALUE ALL "*".
               05  FILLER                    PIC X(45)    VALUE
               "ERROR OCCURED TRYING TO ACCESS GI-RECORD --- ".
               05  FILLER                    PIC X(28)    VALUE
               "THIS BRANCH HAS BEEN SKIPPED".
               05  FILLER                    PIC X(20)    VALUE ALL "*".
               05  FILLER                    PIC X(9)     VALUE SPACES.

      *-----------------------------------------------------------------
       COPY "LIBLP/EMSFRG_CMD.CPY".
      *-----------------------------------------------------------------
       01  LEGEND               PIC X(70).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  END-DATE.
               05  END-MONTH   PIC 99.
               05  END-YEAR    PIC 99.
           03  EDATE REDEFINES END-DATE PIC 9999.
           03  DAY-OF-MONTH    PIC 99.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  BEG-GL-BR       PIC 9(4).
           03  END-GL-BR       PIC 9(4).
           03  GL-GROUP-FLAG   PIC X.
           03  ENT-GL-GROUP    PIC X(4).
           03  BEG-ACCT        PIC 9(7).
           03  END-ACCT        PIC 9(7).
           03  MISC-CKS-FG     PIC X.
           03  GL-SUMMARY-FG   PIC X.

       01  VERSION-CONTROL     PIC X VALUE "B".
       01  BEGINNING-DATE      PIC 9(8).
       01  ENDING-DATE         PIC 9(8).

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       COPY "LIBGB/PASSWDW.CPY".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(22)    VALUE " ".
           03  FILLER           PIC X(48)    VALUE
           "       - CASH/BANK DISBURSEMENT REGISTER -      ".

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC X(2)     VALUE " ".
           03  HD-BRNO     PIC 9(4).
           03  FILLER      PIC X        VALUE " ".
           03  HD-BRNAME   PIC X(20).
           03  FILLER      PIC X(70)    VALUE " ".

       01  HEAD4.
           03  FILLER      PIC X(17) VALUE "TYPE     --DATE--".
           03  FILLER      PIC X(13) VALUE "     AMOUNT  ".
           03  FILLER      PIC X(32) VALUE "PAYEE".
           03  FILLER      PIC X(37) VALUE "REF #  REASON".
           03  FILLER      PIC X(10) VALUE "TILL/CK# ".
           03  FILLER      PIC X(9)  VALUE "GL-NUMBER".
           03  FILLER      PIC X(8)  VALUE " ".
           03  FILLER      PIC X(6)  VALUE "AMOUNT".

       01  HEAD5.
           03  FILLER      PIC X(35) VALUE
           "------------GENERAL LEDGER SUMMARY:".

       01  HEAD6.
           03  FILLER      PIC X(53) VALUE
           " GL ACCOUNT   NAME".
           03  FILLER      PIC X(35) VALUE
           "DEBIT         CREDIT            NET".
      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

      *-----------------------------------------------------------------
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-TYPE      PIC X(8).
           03  FILLER      PIC X.
           03  D-CKDATE    PIC 99/99/99.
           03  D-CKAMT     PIC ----,---.99.
           03  FILLER      PIC XX.
           03  D-PAYEE     PIC X(30).
           03  D-RESERVE-STUFF REDEFINES D-PAYEE.
               05  D-DLPAYEE PIC X(19).
               05  FILLER    PIC X.
               05  D-DLCLASS PIC 9(2).
               05  FILLER    PIC X.
               05  D-DLNO    PIC Z(4).99 BLANK ZERO.
           03  FILLER      PIC X.
           03  D-REFNO     PIC ZZZZZ9     BLANK ZERO.
           03  FILLER      PIC XX.
           03  D-REASON    PIC X(30).
           03  FILLER      PIC X.
           03  D-CKNO      PIC ZZZZZ9.
           03  FILLER      PIC X(2).
           03  D-GLBR      PIC 9(4).
           03  D-DASH      PIC X.
           03  D-GLNUM     PIC 9(7).
           03  D-GLAMT     PIC -----,---.99.

      *-----------------------------------------------------------------
       01  T-LINE REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(16).
           03  T-CKTOTAL   PIC ZZZ,ZZZ,ZZZ.99-.

      *-----------------------------------------------------------------
       01  S-LINE REDEFINES DETAIL-LINE.
           03  S-GLBR      PIC 9(4).
           03  S-DASH      PIC X.
           03  S-GLNUM     PIC 9(7).
           03  FILLER      PIC X(3).
           03  S-GLNAME    PIC X(30).
           03  S-DRGLAMT   PIC ZZZ,ZZZ,ZZZ.99-.
           03  S-CRGLAMT   PIC ZZZ,ZZZ,ZZZ.99-.
           03  S-NET       PIC ZZZ,ZZZ,ZZZ.99-.

      *-----------------------------------------------------------------
       01  ERROR-GI-LINE REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(30).
           03  D-ERR-PROMPT1           PIC X(5).
           03  D-ERR-DAY               PIC 9(2).
           03  FILLER                  PIC X(5).
           03  D-ERR-PROMPT2           PIC X(8).
           03  D-ERR-BRNO              PIC 9(4).
           03  FILLER                  PIC X(5).
           03  D-ERR-PROMPT3           PIC X(6).
           03  D-ERR-CODE              PIC X(1).
           03  FILLER                  PIC X(5).
           03  D-ERR-PROMPT4           PIC X(7).
           03  D-ERR-CLASS             PIC 9(3).
           03  FILLER                  PIC X(4).
           03  D-ERR-PROMPT5           PIC X(8).
           03  D-ERR-SUBKEY            PIC X(22).
           03  FILLER                  PIC X(8).

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               05  R-NUM        PIC Z(5)9.
               05  FILLER       PIC X(6).
           03  RANGE-GL REDEFINES RANGE-SELECTION.
               05  R-BR         PIC 9(4).
               05  R-DASH       PIC X.
               05  R-ACCT       PIC 9(7).


       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/MILAGE_DEF.CPY".
       COPY "LIBLP/MILAGE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/MILAGE_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK2-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRP1-CONNECTION.
           PERFORM CLOSE-TRD1-FILE.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK2-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MILAGE" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
 
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "REPORT IN PROGRESS..............", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.


           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              TRC-PATH-OVERRIDE
                                              TRD-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.

           PERFORM OPEN-TRC1-FILE
           PERFORM SORT-TRC-FILE.

           PERFORM OPEN-TRD1-FILE.

      * MISC CHECKS ONLY THEN NO NEED TO SORT DEPOSITS AND PAYMENTS
           IF MISC-CKS-FG NOT = "O"
              PERFORM SORT-TRD-FILE.

           PERFORM OPEN-TRP1-FILE.

      * MISC CHECKS ONLY THEN NO NEED TO SORT DEPOSITS AND PAYMENTS
           IF MISC-CKS-FG NOT = "O"
              PERFORM SORT-TRP-FILE.

           PERFORM CLOSE-WK2-FILE.

           IF EXT-EOBUF-ERRCD = "X"
              GO TO NEXT-GROUP-LOC.

           PERFORM OPEN-WK2-FILE.
           MOVE "N" TO WS-ERROR-GI-FILE.

      *-----------------------------------------------------------------
       NEXT-RECORD.

           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-OF-GROUP.

           IF WK2-CODE = "C"
      *       PERFORM READ-TRC-RECORD
              MOVE WK2-TRC-REC TO TRC-REC
           ELSE
           IF WK2-CODE = "D"
      *       PERFORM READ-TRD-RECORD
              MOVE WK2-TRD-REC TO TRD-REC
           ELSE
           IF WK2-CODE = "P"
      *       PERFORM READ-TRP-RECORD.
              MOVE WK2-TRP-REC     TO TRP-REC
              MOVE WK2-BW-OWNBR    TO BW-OWNBR 
              MOVE WK2-BW-SSNO     TO BW-SSNO 
              MOVE WK2-BW-LNAME    TO BW-LNAME  
              MOVE WK2-BW-FNAME    TO BW-FNAME 
              MOVE WK2-LP-BRNO     TO LP-BRNO 
              MOVE WK2-LP-ACCTNO   TO LP-ACCTNO 
              MOVE WK2-LP-SEQNO    TO LP-SEQNO 
              MOVE WK2-LP-PAYDATE  TO LP-PAYDATE.               
      
           IF IO-FG NOT = 0
              MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           IF FIRST-TIME = "Y"
              MOVE WK2-BRNO TO HOLD-BRNO
              PERFORM SET-BRANCH-HEADINGS.

           IF WK2-BRNO NOT = HOLD-BRNO
              PERFORM BRANCH-TOTALS
              MOVE WK2-BRNO TO HOLD-BRNO
              PERFORM SET-BRANCH-HEADINGS
              PERFORM HEAD-PAGE.

           PERFORM DETAIL-LINE-PROCESS.

           IF ( WS-ERROR-GI-FILE = "Y" )
              PERFORM WRITE-ERROR-LINE
              PERFORM CLOSE-TRFILES
              PERFORM CLOSE-BW1-FILE
              PERFORM CLOSE-WK2-FILE
              PERFORM OPEN-WK2-FILE-OUTPUT
              GO TO NEXT-GROUP-LOC.

           GO TO NEXT-RECORD.

       END-OF-GROUP.
           PERFORM CLOSE-TRFILES
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM BRANCH-TOTALS.
           PERFORM GRAND-TOTALS.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE MILAGE-QUEUE-POS TO Q-POS.
           MOVE MILAGE-COPIES-POS TO C-POS.
           MOVE MILAGE-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.

      * CREATE SORT KEY FILE FOR TRFILE OUTPUT
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD
              GO TO ENDIT.
           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.

      * CREATE WORK FILE FOR GL SUMMARY:
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE GL WORKFILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS.............." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      * SET DATE RANGE FOR TESTING TR FILES
           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 1      TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 31      TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO ENDING-DATE.

           PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-GI1-FILE.
           PERFORM OPEN-GL1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************
       EO-EXEC-AUDIT SECTION.
      ******************************

           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO END-MONTH.
           MOVE S-YY TO END-YEAR.
           IF EXT-EOBUF-DATE1 NOT = EXT-EOBUF-DATE2
              MOVE 99 TO DAY-OF-MONTH
           ELSE
              MOVE S-DD TO DAY-OF-MONTH.

      ******************************
       ENTER-PARAMETERS SECTION.
      ******************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO GL-SUMMARY-YN.

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BR1." TO VDU.
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
              GO TO CHECK-BRANCH-SECURITY.
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

           GO TO CHECK-BRANCH-SECURITY.
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

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       DTE.
           IF EXT-ROUTE-BUF NOT  = "00"
              GO TO DA-01.

           MOVE LEGEND-DTE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           MOVE VDUNUM0 TO EDATE.
           IF END-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DTE.
           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           MOVE 01        TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
          
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           MOVE 50 TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              MOVE "INVALID DATE" TO MESS
              PERFORM SEND-MESS
              GO TO DTE.

       DA-01.
           MOVE LEGEND-DA-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020DA1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "16"
              MOVE "S  A020DA1." TO VDU
              MOVE "**" TO VDUBUF
              PERFORM SCREENIO
              MOVE 99 TO DAY-OF-MONTH
              GO TO GL-BR1.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO DTE
              ELSE
                 GO TO BR-02.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           IF VDUNUM0 = 0 GO TO DA-01.
           MOVE VDUNUM0 TO DAY-OF-MONTH.

           IF DAY-OF-MONTH > 31  OR
              (DAY-OF-MONTH > 30 AND
              (END-MONTH = 2 OR 4 OR 6 OR 9 OR 11)) OR
              (DAY-OF-MONTH > 29 AND END-MONTH = 2)
              MOVE "DAY IS PAST LAST DAY OF MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DA-01.

       GL-BR1.
           MOVE "E  A000GLBR1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM
           IF UP-KEY GO TO DA-01.
           IF F3-KEY GO TO ALL-GLBR.
           IF F7-KEY GO TO EXIT-PARM.

           IF F6-KEY
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO GL-BR1
              ELSE
                 MOVE "S  A000GLBR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO
                 MOVE GRSCAN-KEY TO VDUBUF
                 MOVE "00" TO VDUSTATUS.

           IF NOT ENTER-KEY GO TO GL-BR1.

           MOVE VDUBUF TO WGR-WORD ENT-GL-GROUP.
           IF WGR-WORD = SPACES
              GO TO GL-BR1.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO GL-BR1.
           IF WGR-TYPE = "A"
              IF GROUP-FLAG NOT = "Y"
                 MOVE "PLEASE ENTER BRANCH RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO GL-BR1.
           IF WGR-TYPE = "A"
              IF ENT-GL-GROUP NOT = ENT-GROUP
                 MOVE "ENTER BRANCH RANGE OR GROUP: &&&&"
                    TO MESS
                 INSPECT MESS REPLACING FIRST "&&&&" BY ENT-GROUP
                 PERFORM SEND-MESS
                 GO TO GL-BR1.
           IF WGR-TYPE = "A"
              MOVE "S  A000GLBR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GL-GROUP-FLAG
              MOVE 0 TO BEG-GL-BR
              MOVE 9999 TO END-GL-BR
              GO TO ACCT-01.
           MOVE "N" TO GL-GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-GL-BR.

           MOVE "SN N040GLBR1." TO VDU.
           MOVE BEG-GL-BR TO VDUNUM0.
           PERFORM SCREENIO.
       GL-BR2.
           MOVE "ENNN040GLBR2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO GL-BR1.
           IF F3-KEY GO TO ALL-GLBR.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO GL-BR2.
           MOVE VDUNUM0 TO END-GL-BR.
           IF BEG-GL-BR > END-GL-BR
              GO TO GL-BR1.
           GO TO ACCT-01.
       ALL-GLBR.
           MOVE "N" TO GL-GROUP-FLAG.
           MOVE "SN N040GLBR1." TO VDU.
           MOVE 0 TO BEG-GL-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N040GLBR2." TO VDU.
           MOVE 9999 TO END-GL-BR VDUNUM0.
           PERFORM SCREENIO.
       ACCT-01.
           MOVE "ENNN070ACCT1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO GL-BR2.
           IF F3-KEY GO TO ALL-ACCTS.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ACCT-01.
           MOVE VDUNUM0 TO BEG-ACCT.
       ACCT-02.
           MOVE "ENNN070ACCT2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO ACCT-01.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ACCT-02.
           MOVE VDUNUM0 TO END-ACCT.
           IF BEG-ACCT > END-ACCT
              GO TO ACCT-01.
           GO TO MISC-CKS.
       ALL-ACCTS.
           MOVE "SN N070ACCT1." TO VDU.
           MOVE 0 TO BEG-ACCT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N070ACCT2." TO VDU.
           MOVE 9999999 TO END-ACCT VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       MISC-CKS.
           MOVE "E  A010PRINTCKS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ACCT-02.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO MISC-CKS.

           MOVE VDUBUF TO MISC-CKS-FG.

           IF MISC-CKS-FG NOT = "I" AND NOT = "O" AND NOT = "S"
              MOVE "ENTER 'I', 'S' OR 'O'" TO MESS
              PERFORM SEND-MESS
              GO TO MISC-CKS.

      *-----------------------------------------------------------------
       GL-SUMMARY-YN.
           MOVE "E  A010GLSUMMARY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO MISC-CKS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO GL-SUMMARY-YN.

           MOVE VDUBUF TO GL-SUMMARY-FG.

           IF GL-SUMMARY-FG NOT = "Y" AND NOT = "N"
              MOVE "PLEASE ENTER Y OR N" TO MESS
              PERFORM SEND-MESS
              GO TO GL-SUMMARY-YN.

       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
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

           MOVE "SNNN020DA1." TO VDU.
           MOVE DAY-OF-MONTH TO VDUNUM0.
           PERFORM SCREENIO.

           IF GL-GROUP-FLAG = "Y"
              MOVE "S  A000GLBR1." TO VDU
              MOVE ENT-GL-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000GLBR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040GLBR1." TO VDU
              MOVE BEG-GL-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040GLBR2." TO VDU
              MOVE END-GL-BR TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN070ACCT1." TO VDU.
           MOVE BEG-ACCT TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN070ACCT2." TO VDU.
           MOVE END-ACCT TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000PRINTCKS." TO VDU.
           MOVE MISC-CKS-FG TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000GLSUMMARY." TO VDU.
           MOVE GL-SUMMARY-FG TO VDUBUF.
           PERFORM SCREENIO.


      ***************************************
       SORT-TRC-FILE SECTION.
      ***************************************

      * CHECKS "CA" "RR" "RC" "RP" "BK" "RD" 

           MOVE BR-NO          TO TC-BRNO
                                  QTRC1-WBEG-BRNO
                                  QTRC1-WEND-BRNO.
           MOVE BEGINNING-DATE TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE.
           MOVE ENDING-DATE    TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WEND-KEY-DATE.

           MOVE 0                   TO TC-CKNO TC-CLASS TC-SEQ
                                       QTRC1-WBEG-CKNO
                                       QTRC1-WBEG-CLASS
                                       QTRC1-WBEG-SEQ.
           MOVE ALL "9"             TO QTRC1-WEND-CKNO
                                       QTRC1-WEND-CLASS
                                       QTRC1-WEND-SEQ.

           PERFORM START-TRC1-FILE. *> EMBEDDED - TRCFILE
       SORT-TRC-NEXT.
           PERFORM READ-TRC1-FILE-NEXT. *> EMBEDDED - TRCFILE
           IF (IO-FG NOT = 0) OR (TRC-PATH-OWNBR NOT = TC-BRNO)
              GO TO EXIT-SORT-TRC-FILE.

           IF TC-KEY-DATE > ENDING-DATE
              GO TO EXIT-SORT-TRC-FILE.

           IF MISC-CKS-FG = "O"
              IF TC-TYPE NOT = "MS"
                 GO TO SORT-TRC-NEXT
              ELSE
                 GO TO SORT-TRC-CONTINUE
           ELSE
              IF MISC-CKS-FG = "I"
                 IF TC-TYPE = "MS"
                    GO TO SORT-TRC-CONTINUE.

      * "MS" SKIP WILL HIT THIS NEXT IF

           IF NOT (TC-TYPE = "CA" OR "RR" OR "RC"
                          OR "RP" OR "RD" OR "BK")
              GO TO SORT-TRC-NEXT.

       SORT-TRC-CONTINUE.
           MOVE "N" TO GL-ACCT-IN-RANGE.

           MOVE 0 TO SUBX.
       SORT-TRC-GL-LOOP.
           ADD 1 TO SUBX.

           IF SUBX > 5
              GO TO SORT-TRC-GL-LOOP-DONE.

           MOVE TC-GLNO(SUBX) TO WORK-GLNO.

           IF WORK-GLNUM < BEG-ACCT OR WORK-GLNUM > END-ACCT
              GO TO SORT-TRC-GL-LOOP.

           IF GL-GROUP-FLAG = "Y"
              MOVE WORK-GLBR TO WGR-SRESULT
              PERFORM SEARCH-GR-VERIFY
              IF WGR-ERR NOT = 0
                 GO TO SORT-TRC-GL-LOOP
              END-IF
           ELSE
              IF WORK-GLBR < BEG-GL-BR OR WORK-GLBR > END-GL-BR
                 GO TO SORT-TRC-GL-LOOP.

           MOVE "Y" TO GL-ACCT-IN-RANGE.

       SORT-TRC-GL-LOOP-DONE.
           IF GL-ACCT-IN-RANGE NOT = "Y"
              GO TO SORT-TRC-NEXT.

           MOVE TC-BRNO     TO WK2-BRNO.
           MOVE 0           TO WK2-TYPE.
           IF TC-TYPE = "BK"
              MOVE 1 TO WK2-TYPE
           ELSE
              IF TC-TYPE = "RR"
                 MOVE 4 TO WK2-TYPE
              ELSE
                 IF TC-TYPE = "RC"
                    MOVE 5 TO WK2-TYPE
                 ELSE
                    IF TC-TYPE = "RP"
                       MOVE 6 TO WK2-TYPE
                    ELSE
                       IF TC-TYPE = "RD"
                          MOVE 7 TO WK2-TYPE
                       ELSE
                          IF TC-TYPE = "CA"
                             IF TC-AMT < 0
                                MOVE 8 TO WK2-TYPE
                             ELSE
                                MOVE 9 TO WK2-TYPE.

           MOVE TC-KEY-DATE TO WK2-DATE.
           MOVE "C"         TO WK2-CODE.
           MOVE TC-CLASS    TO WK2-CLASS.
           MOVE TC-CKNO     TO WK2-CKNO.
           MOVE TC-SEQ      TO WK2-SEQ.
           MOVE 0           TO WK2-ACCTNO WK2-LINE.
           MOVE TRC-REC     TO WK2-TRC-REC.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TRC-NEXT.

       EXIT-SORT-TRC-FILE.
           EXIT.

      ***************************************
       SORT-TRD-FILE SECTION.
      ***************************************

      * DEPOSITS

      * A15 DID NOT INCLUDE DEPOSITS UNLESS YOU SAID F3 FOR ALL
      * GL ACCOUNTS
           IF NOT ( BEG-GL-BR = 0 AND END-GL-BR = 9999    ) AND
              NOT ( BEG-ACCT  = 0 AND END-ACCT  = 9999999 )
              GO TO EXIT-SORT-TRD-FILE.

           MOVE BR-NO          TO TD-BRNO
                                  QTRD1-WBEG-BRNO
                                  QTRD1-WEND-BRNO.

           MOVE BEGINNING-DATE TO TD-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRD1-WBEG-DATE.
           MOVE ENDING-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRD1-WEND-DATE.

           PERFORM START-TRD1-FILE. *> EMBEDDED - TRDFILE
       SORT-TRD-NEXT.
           PERFORM READ-TRD1-FILE-NEXT. *> EMBEDDED - TRDFILE
           IF (IO-FG NOT = 0) OR (TRD-PATH-OWNBR NOT = TD-BRNO)
              GO TO EXIT-SORT-TRD-FILE.

           IF TD-DATE > ENDING-DATE
              GO TO EXIT-SORT-TRD-FILE.

           MOVE TD-BRNO TO WK2-BRNO.
           MOVE 3       TO WK2-TYPE.
           MOVE TD-DATE TO WK2-DATE.
           MOVE "D"     TO WK2-CODE.
           MOVE 0       TO WK2-CLASS WK2-CKNO WK2-SEQ
                           WK2-ACCTNO WK2-LINE.
           MOVE TRD-REC TO WK2-TRD-REC.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TRD-NEXT.

       EXIT-SORT-TRD-FILE.
           EXIT.

      ***************************************
       SORT-TRP-FILE SECTION.
      ***************************************

      * PAYMENTS  "RV" THAT ARE "NSF"

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.

           MOVE 0                   TO TP-CLASS TP-ACCTNO TP-LINE
                                       QTRP1-WBEG-CLASS
                                       QTRP1-WBEG-ACCTNO
                                       QTRP1-WBEG-LINE.
       
           MOVE ALL "9"             TO QTRP1-WEND-CLASS
                                       QTRP1-WEND-ACCTNO
                                       QTRP1-WEND-LINE.

           MOVE TP-DATE              TO NDTE-DATE
           MOVE 1                    TO NDTE-HOLD
           PERFORM INCREMENT-DAYS
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE.

           IF ( TP-CLASS = 999 )
              MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
           ELSE
              COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1.

           IF ( TP-ACCTNO = 99999999 )
              MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
           ELSE
              COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1.

           IF ( TP-LINE = 999 )
              MOVE TP-LINE           TO QTRP1-WSBEG-LINE
           ELSE
              COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.

           PERFORM START-TRP1-FILE. *> SSP - TRPFILE
       SORT-TRP-NEXT.
           PERFORM READ-TRP1-FILE-NEXT. *> EMBEDDED - TRPFILE
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO EXIT-SORT-TRP-FILE.

           IF TP-DATE > ENDING-DATE
              GO TO EXIT-SORT-TRP-FILE.

           IF TP-REFNO NOT = "NSF  "
              GO TO SORT-TRP-NEXT.

           MOVE "N" TO GL-ACCT-IN-RANGE.

           MOVE 0 TO SUBX.
       SORT-TRP-GL-LOOP.
           ADD 1 TO SUBX.

           IF SUBX > 3
              GO TO SORT-TRP-GL-LOOP-DONE.

           MOVE TP-GLNO(SUBX) TO WORK-GLNO.

           IF WORK-GLNUM < BEG-ACCT OR WORK-GLNUM > END-ACCT
              GO TO SORT-TRP-GL-LOOP.

           IF GL-GROUP-FLAG = "Y"
              MOVE WORK-GLBR TO WGR-SRESULT
              PERFORM SEARCH-GR-VERIFY
              IF WGR-ERR NOT = 0
                 GO TO SORT-TRP-GL-LOOP
              END-IF
           ELSE
              IF WORK-GLBR < BEG-GL-BR OR WORK-GLBR > END-GL-BR
                 GO TO SORT-TRP-GL-LOOP.

           MOVE "Y" TO GL-ACCT-IN-RANGE.

       SORT-TRP-GL-LOOP-DONE.
           IF GL-ACCT-IN-RANGE NOT = "Y"
              GO TO SORT-TRP-NEXT.

           MOVE TP-BRNO    TO WK2-BRNO.
           MOVE 2          TO WK2-TYPE.
           MOVE TP-DATE    TO WK2-DATE.
           MOVE "P"        TO WK2-CODE.
           MOVE TP-CLASS   TO WK2-CLASS.
           MOVE 0          TO WK2-CKNO WK2-SEQ.
           MOVE TP-ACCTNO  TO WK2-ACCTNO.
           MOVE TP-LINE    TO WK2-LINE.
           MOVE TRP-REC    TO WK2-TRP-REC.

           MOVE BW-OWNBR   TO WK2-BW-OWNBR.
           MOVE BW-SSNO    TO WK2-BW-SSNO.
           MOVE BW-LNAME   TO WK2-BW-LNAME.
           MOVE BW-FNAME   TO WK2-BW-FNAME.
           MOVE LP-BRNO    TO WK2-LP-BRNO.
           MOVE LP-ACCTNO  TO WK2-LP-ACCTNO.
           MOVE LP-SEQNO   TO WK2-LP-SEQNO.
           MOVE LP-PAYDATE TO WK2-LP-PAYDATE.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TRP-NEXT.

       EXIT-SORT-TRP-FILE.
           EXIT.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

           MOVE WK2-BRNO TO GI-BRANCH.

           IF ( WK2-CODE = "C" )
              IF ( TC-CLASS = 0 )
                 MOVE BR-GLBANK   TO GI-BANK WK-GLNO
                 MOVE BR-GLONHAND TO GI-CASH
              ELSE
                 MOVE TC-CLASS TO GI-CLASS
                 PERFORM GET-GI
                 IF ( WS-ERROR-GI-FILE = "Y" )
                    GO TO END-DETAIL-LN-PROCESS.

           IF ( WK2-CODE = "P" )
              MOVE TP-CLASS TO GI-CLASS
              PERFORM GET-GI
              IF ( WS-ERROR-GI-FILE = "Y" )
                 GO TO END-DETAIL-LN-PROCESS
              ELSE
                 PERFORM UPDATE-NSF-REVERSAL
                 MOVE 3 TO GLMAX
                 MOVE "BANK NSF" TO D-TYPE
                 MOVE TP-ACCTNO TO D-REFNO
                 COMPUTE D-CKAMT = TP-TRAMT * -1
                 MOVE TP-PAYDATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO D-CKDATE
                 MOVE TP-SSNO TO BW-SSNO
      *          PERFORM READ-BW1-FILE
      *          IF IO-FG = 0
                 IF BW-LNAME NOT = SPACES
                    MOVE SPACES TO D-PAYEE
                    STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                        INTO D-PAYEE
                  END-IF
                  IF TP-OTHBR-FG NOT = "G"
                     ADD TP-TRAMT TO NSF-TOTAL(1) NSF-TOTAL(2)
                  END-IF
                PERFORM GET-NSF-REVERSED.

           IF ( WK2-CODE = "C" )
              PERFORM UPDATE-CHECK-TYPE
              MOVE 5 TO GLMAX
              MOVE TC-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-CKDATE
              MOVE TC-AMT TO D-CKAMT
              MOVE TC-PAYEE TO D-PAYEE
              MOVE TC-REASON TO D-REASON
              MOVE TC-REF TO D-REFNO
              IF TC-TYPE = "MS"
                 ADD TC-AMT TO CHECK-TOTAL(1) CHECK-TOTAL(2)
                 MOVE "MISC CK" TO D-TYPE
                 MOVE TC-CKNO TO D-CKNO
              ELSE
              IF TC-TYPE = "CA"
                 IF TC-AMT < 0
                    ADD TC-AMT TO RECEIPT-TOTAL(1) RECEIPT-TOTAL(2)
                    MOVE "RECEIPT" TO D-TYPE
                    MOVE TC-TILLNO TO D-CKNO
                 ELSE
                    ADD TC-AMT TO DISBURSE-TOTAL(1) DISBURSE-TOTAL(2)
                    MOVE "DISBURSE" TO D-TYPE
                    MOVE TC-TILLNO TO D-CKNO
              ELSE
                 IF TC-TYPE = "RR" OR "RC" OR "RP"
                    ADD TC-AMT TO DEALER-TOTAL(1) DEALER-TOTAL(2)
                    MOVE "RESERVE" TO D-TYPE
                    PERFORM TEST-RE-OR-SR
              ELSE
                 IF TC-TYPE = "RD"
                    ADD TC-AMT TO DEALER-TOTAL(1) DEALER-TOTAL(2)
                    MOVE "REC PROC" TO D-TYPE
                    PERFORM TEST-RE-OR-SR
                 ELSE
                    ADD TC-AMT TO BANK-TOTAL(1) BANK-TOTAL(2)
                    MOVE "BANK" TO D-TYPE
                    PERFORM TEST-BK-REASON.

           IF ( WK2-CODE = "D" )
              PERFORM UPDATE-DEPOSIT
              MOVE "DEPOSIT" TO D-TYPE
              MOVE TD-DEPOSIT TO D-CKAMT
              MOVE TD-DATE    TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-CKDATE
              ADD TD-DEPOSIT TO DEPOSIT-TOTAL(1) DEPOSIT-TOTAL(2)
              GO TO PRINT-NO-GL.

           MOVE 1 TO SUBX.

           IF WK2-CODE = "C"
              PERFORM SETUP-TC-GL
           ELSE
              PERFORM SETUP-TP-GL.

           IF GLNO NOT = 0
              MOVE WK2-CODE TO TEST-TR-CODE
              PERFORM UPDATE-GL-SUMMARY
              MOVE GLNO TO WORK-GLNO
              MOVE WORK-GLBR TO D-GLBR
              MOVE "-" TO D-DASH
              MOVE WORK-GLNUM TO D-GLNUM
              MOVE GLAMT TO D-GLAMT
              MOVE 2 TO SUBX.

      *-----------------------------------------------------------------
       PRINT-NO-GL.
           IF HOLD-TYPE NOT = WK2-TYPE
              IF FIRST-TIME = "Y"
                 MOVE WK2-TYPE TO HOLD-TYPE
              ELSE
                 MOVE WK2-TYPE TO HOLD-TYPE
                 MOVE 2 TO LN-FEED.

           PERFORM WRITE-DETAIL-LINE.

           IF ( WK2-CODE = "D" )
              GO TO END-DETAIL-LN-PROCESS.

      *-----------------------------------------------------------------
       PRINT-GLDIST.
           IF SUBX > GLMAX
              GO TO END-DETAIL-LN-PROCESS.

           IF ( WK2-CODE = "C" )
              PERFORM SETUP-TC-GL
           ELSE
              PERFORM SETUP-TP-GL.

           IF GLAMT = 0
              ADD 1 TO SUBX
              GO TO PRINT-GLDIST.

           MOVE WK2-CODE TO TEST-TR-CODE.
           PERFORM UPDATE-GL-SUMMARY.
           MOVE GLNO       TO WORK-GLNO.
           MOVE WORK-GLBR  TO D-GLBR.
           MOVE "-"        TO D-DASH.
           MOVE WORK-GLNUM TO D-GLNUM.
           MOVE GLAMT      TO D-GLAMT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO SUBX.
           GO TO PRINT-GLDIST.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ******************************************
       GET-NSF-REVERSED SECTION.
      ******************************************

      * PRINT ORIGINAL POSTING DATE OF PAYMENT THAT WAS "NSF"
      * REVERSED * RAC #0013, 7/9/96 BAH

      * DONT TRY TO GET PAYMENT REVERSED IF INTER BRANCH PAYMENT !!

           IF TP-BRNO NOT = TP-POSTING-BR
              IF TP-OTHBR-FG = "C"
                MOVE TP-POSTING-BR TO WK-NSF-BR
                MOVE "FOR"         TO WK-INTERBRANCH
                MOVE WK-NSF-TEXT2  TO D-REASON
              ELSE
                MOVE TP-POSTING-BR TO WK-NSF-BR
                MOVE "BY "         TO WK-INTERBRANCH
                MOVE WK-NSF-TEXT2  TO D-REASON
           ELSE
              MOVE TP-BRNO     TO LP-BRNO
              MOVE TP-ACCTNO   TO LP-ACCTNO
              MOVE TP-REVSEQNO TO LP-SEQNO
      *       PERFORM OPEN-LP1-FILE
      *       PERFORM READ-LP1-FILE
      *       PERFORM CLOSE-LP1-FILE
      *       IF IO-FG = 0

              MOVE LP-PAYDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY  TO WK-NSF-DATE
              MOVE WK-NSF-TEXT  TO D-REASON.

      ********************************************
       UPDATE-NSF-REVERSAL SECTION.
      ********************************************

      * NOTE: THESE UPDATE BY CHECK TYPE MUST MATCH LPEOD5.C

           MOVE "P" TO TEST-TR-CODE.
           IF TP-OTHBR-FG = " " OR "C"
              MOVE GI-NSF TO GLNO
              MOVE TP-TRAMT TO GLAMT
              PERFORM UPDATE-GL-SUMMARY
              IF TP-OTHBR-FG = "C"
                 MOVE GI-OTHBR-PAY TO GLNO
                 COMPUTE GLAMT = 0 - TP-TRAMT
                 PERFORM UPDATE-GL-SUMMARY
              END-IF
            END-IF.

           IF TP-OTHBR-FG = " " OR "G"
              MOVE GI-LATECHG TO GLNO
              COMPUTE GLAMT = 0 - TP-APLC
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-INTEREST TO GLNO
              COMPUTE GLAMT = 0 - TP-APINT
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-AR TO GLNO
              COMPUTE GLAMT = 0 - TP-APCUR
              PERFORM UPDATE-GL-SUMMARY
              IF TP-OTHBR-FG = "G"
                 MOVE GI-OTHBR-REC TO GLNO
                 MOVE TP-TRAMT TO GLAMT
                 PERFORM UPDATE-GL-SUMMARY
              END-IF
           END-IF.

      *****************************************
       UPDATE-DEPOSIT SECTION.
      *****************************************

           MOVE "D"        TO TEST-TR-CODE.

      * TD-DEPOSIT = ACTUAL ENDING - OPENING
           MOVE TD-BANK    TO GLNO.
           MOVE TD-DEPOSIT TO GLAMT.
           PERFORM UPDATE-GL-SUMMARY.

      * TD-OVERSHORT = EXPECTED ENDING - ACTUAL ENDING (SHORT=DEBIT)
           MOVE TD-GLOVERSHORT TO GLNO.
           MOVE TD-OVERSHORT TO GLAMT.
           PERFORM UPDATE-GL-SUMMARY.

      * CASH MUST GET NET TRANSACTIONS
           MOVE TD-CASH TO GLNO.
           COMPUTE GLAMT = 0 - TD-DEPOSIT - TD-OVERSHORT.
           PERFORM UPDATE-GL-SUMMARY.

      ********************************************
       UPDATE-CHECK-TYPE SECTION.
      ********************************************

      * ONLY TRC TYPES "CA" "RR" "RC" "RP" "RD" AND "BK"

      * THESE UPDATE BY CHECK TYPE MUST MATCH LPEOD5.C

           MOVE "C" TO TEST-TR-CODE.

           IF TC-TYPE = "CA"
              MOVE GI-CASH TO GLNO
              COMPUTE GLAMT = 0 - TC-AMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "BK"
              MOVE GI-BANK TO GLNO
              COMPUTE GLAMT = 0 - TC-AMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "RR"
              MOVE GI-CASH TO GLNO
              COMPUTE GLAMT = 0 - TC-AMT
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-DLRVPRIN(TC-DLRVNO) TO GLNO
              MOVE TC-AMT TO GLAMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "RC"
              MOVE GI-CASH TO GLNO
              COMPUTE GLAMT = 0 - TC-AMT
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-DLRVCHGS(TC-DLRVNO) TO GLNO
              MOVE TC-AMT TO GLAMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "RP"
              MOVE GI-CASH TO GLNO
              COMPUTE GLAMT = 0 - TC-AMT
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-DLPARV TO GLNO
              MOVE TC-AMT TO GLAMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "RD"
              MOVE GI-BANK TO GLNO
              MOVE TC-AMT TO GLAMT
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-DLPAYABLE TO GLNO
              COMPUTE GLAMT = 0 - TC-AMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

           MOVE GI-BANK TO GLNO.
           COMPUTE GLAMT = 0 - TC-AMT.
           PERFORM UPDATE-GL-SUMMARY.

       UPDATE-CHECK-TYPE-END.
           EXIT.

      ***********************************
       UPDATE-GL-SUMMARY SECTION.
      ***********************************
       UPDATE-GL-SUMMARY-REC.

           IF TEST-TR-CODE = "C"
              MOVE TC-BRNO TO WK-BR
           ELSE
              IF TEST-TR-CODE = "D"
                 MOVE TD-BRNO TO WK-BR
              ELSE
                 MOVE TP-BRNO TO WK-BR.
           MOVE GLNO     TO WK-GLNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              GO TO UPDATE-GL-SUMMARY-CREATE.

           IF GLAMT > 0
              ADD GLAMT TO WK-DRGLAMT
           ELSE
              ADD GLAMT TO WK-CRGLAMT.

           PERFORM REWRITE-WK-FILE.

           GO TO UPDATE-GL-SUMMARY-EXIT.

       UPDATE-GL-SUMMARY-CREATE.
           IF TEST-TR-CODE = "C"
              MOVE TC-BRNO TO WK-BR
           ELSE
              IF TEST-TR-CODE = "D"
                 MOVE TD-BRNO TO WK-BR
              ELSE
                 MOVE TP-BRNO TO WK-BR.
           MOVE GLNO     TO WK-GLNO.
           MOVE 0        TO WK-DRGLAMT WK-CRGLAMT.
           PERFORM WRITE-WK-FILE.

           GO TO UPDATE-GL-SUMMARY-REC.

       UPDATE-GL-SUMMARY-EXIT.
           EXIT.

      ****************************
       BRANCH-TOTALS SECTION.
      ****************************

           MOVE 2 TO LN-FEED.

           MOVE 1 TO SUB.
           PERFORM PRINT-TOTALS-ROUTINE.

           IF GL-SUMMARY-FG = "Y"
              MOVE 99 TO LN-COUNT
              MOVE "Y" TO GL-SUM-FG
              PERFORM PRINT-BR-GL-SUMMARY.

           MOVE 0 TO RECEIPT-TOTAL(1)
                     DISBURSE-TOTAL(1)
                     DEALER-TOTAL(1)
                     BANK-TOTAL(1)
                     DEPOSIT-TOTAL(1)
                     NSF-TOTAL(1)
                     CHECK-TOTAL(1).

           MOVE 99 TO LN-COUNT.
           MOVE SPACES TO GL-SUM-FG.
       END-BRANCH-TOTALS.
           EXIT.

      ****************************
       GRAND-TOTALS SECTION.
      ****************************

           MOVE 2 TO LN-FEED.
           MOVE "GRAND TOTALS:" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO SUB.
           PERFORM PRINT-TOTALS-ROUTINE.

      ***********************************
       PRINT-TOTALS-ROUTINE SECTION.
      ***********************************
           IF BANK-TOTAL(SUB) NOT = 0
              MOVE "TOTAL BANK     :" TO DETAIL-LINE
              MULTIPLY -1 BY BANK-TOTAL(SUB) GIVING T-CKTOTAL
              PERFORM WRITE-DETAIL-LINE.

           IF NSF-TOTAL(SUB) NOT = 0
              MOVE "NSF CHECKS     :" TO DETAIL-LINE
              MULTIPLY -1 BY NSF-TOTAL(SUB) GIVING T-CKTOTAL
              PERFORM WRITE-DETAIL-LINE.

           IF DEPOSIT-TOTAL(SUB) NOT = 0
              MOVE "TOTAL DEPOSITS :" TO DETAIL-LINE
              MOVE DEPOSIT-TOTAL(SUB) TO T-CKTOTAL
              PERFORM WRITE-DETAIL-LINE.

           IF DEALER-TOTAL(SUB) NOT = 0
              MOVE "TOTAL DEALER   :" TO DETAIL-LINE
              MULTIPLY -1 BY DEALER-TOTAL(SUB) GIVING T-CKTOTAL
              PERFORM WRITE-DETAIL-LINE.

           IF RECEIPT-TOTAL(SUB) NOT = 0
              MOVE "TOTAL RECEIPTS :" TO DETAIL-LINE
              MULTIPLY -1 BY RECEIPT-TOTAL(SUB) GIVING T-CKTOTAL
              PERFORM WRITE-DETAIL-LINE.

           IF DISBURSE-TOTAL(SUB) NOT = 0
              MOVE "TOTAL DISBURSE :" TO DETAIL-LINE
              MOVE DISBURSE-TOTAL(SUB) TO T-CKTOTAL
              PERFORM WRITE-DETAIL-LINE.

           IF CHECK-TOTAL(SUB) NOT = 0
              MOVE "TOTAL MISC CKS :" TO DETAIL-LINE
              MULTIPLY -1 BY CHECK-TOTAL(SUB) GIVING T-CKTOTAL
              PERFORM WRITE-DETAIL-LINE.

      ***********************************
       PRINT-BR-GL-SUMMARY SECTION.
      ***********************************

           MOVE "Y" TO GL-SUM-FG.

           MOVE 0         TO CK-TOTDR CK-TOTCR.
           MOVE HOLD-BRNO TO WK-BR.
           MOVE 0         TO WK-GLNO.

           PERFORM  START-WK-FILE.

       PRINT-BR-GL-SUM-NEXT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO PRINT-BR-GL-SUM-LINE.

           IF WK-BR NOT = HOLD-BRNO
              GO TO PRINT-BR-GL-SUM-LINE.

           MOVE WK-GLNO    TO WORK-GLNO.
           MOVE WORK-GLBR  TO S-GLBR.
           MOVE "-"        TO S-DASH.
           MOVE WORK-GLNUM TO S-GLNUM.

           MOVE WK-GLNO    TO GL-BR-ACCOUNT.
           PERFORM READ-GL1-FILE. *> EMBEDDED - GLFILE
           IF IO-FG NOT = 0
              MOVE "****NOT ON FILE***" TO S-GLNAME
           ELSE
              MOVE GL-DESCRIPTION TO S-GLNAME.

           IF WK-DRGLAMT NOT = 0
              MOVE WK-DRGLAMT TO S-DRGLAMT
              ADD WK-DRGLAMT TO CK-TOTDR.

           IF WK-CRGLAMT NOT = 0
              MOVE WK-CRGLAMT TO S-CRGLAMT
              ADD WK-CRGLAMT TO CK-TOTCR.

           COMPUTE S-NET = WK-DRGLAMT + WK-CRGLAMT.

           PERFORM WRITE-DETAIL-LINE.
           GO TO PRINT-BR-GL-SUM-NEXT.
       PRINT-BR-GL-SUM-LINE.
           MOVE SPACES TO DETAIL-LINE.
           MOVE "TOTAL:" TO S-GLNAME.
           MOVE CK-TOTDR TO S-DRGLAMT.
           MOVE CK-TOTCR TO S-CRGLAMT.
           COMPUTE S-NET = CK-TOTDR + CK-TOTCR.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       PRINT-BR-GL-SUM-END.
           MOVE 99 TO LN-COUNT.
           MOVE SPACES TO GL-SUM-FG.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       SET-BRANCH-HEADINGS SECTION.
      ******************************************************************
           MOVE HOLD-BRNO TO HD-BRNO.
           MOVE BR-NAME   TO HD-BRNAME.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO H-TIME.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE BR-GLBANK TO WK-GLNO.
      *--->MOVE HOLD-BRNO TO HD-BRNO.
      *--->MOVE BR-NAME TO HD-BRNAME.
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GL-SUM-FG NOT = "Y"
              MOVE HEAD4 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GL-SUM-FG = "Y"
              MOVE 4 TO LN-FEED
              MOVE HEAD5 TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE HEAD6 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE 9 TO LN-COUNT
           ELSE
              MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       WRITE-ERROR-LINE SECTION.
      ******************************************************************
           MOVE WS-ERROR-GI-MESSAGE TO DETAIL-LINE.
           MOVE 3                   TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DAY: "             TO D-ERR-PROMPT1.
           MOVE WK2-DATE            TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-DD    TO D-ERR-DAY.
           MOVE "BRANCH: "          TO D-ERR-PROMPT2.
           MOVE WK2-BRNO            TO D-ERR-BRNO.
           MOVE "CODE: "            TO D-ERR-PROMPT3.
           MOVE WK2-CODE            TO D-ERR-CODE.
           MOVE "CLASS: "           TO D-ERR-PROMPT4.
           MOVE WK2-CLASS           TO D-ERR-CLASS.
           MOVE "SUBKEY: "          TO D-ERR-PROMPT5.
           MOVE WK2-SUBKEY          TO D-ERR-SUBKEY.
           MOVE 2                   TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-WRITE-ERROR-LINE.
           EXIT.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 1 TO LN-COUNT.
      * DONT SKIP TO A WHOLE NEW PAGE
           MOVE 4 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE.

           MOVE "MONTH-END DATE" TO RANGE-LINE.
           MOVE EDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "DAY OF MONTH" TO RANGE-LINE.
           MOVE DAY-OF-MONTH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE MISC CKS" TO RANGE-LINE.
           MOVE MISC-CKS-FG TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT GL SUMMARY" TO RANGE-LINE.
           MOVE GL-SUMMARY-FG TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG GL ACCOUNT " TO RANGE-LINE.
           MOVE BEG-GL-BR TO R-BR.
           MOVE "-" TO R-DASH.
           MOVE BEG-ACCT TO R-ACCT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END GL ACCOUNT " TO RANGE-LINE.
           MOVE END-GL-BR TO R-BR.
           MOVE "-" TO R-DASH.
           MOVE END-ACCT TO R-ACCT.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO MILAGE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO MILAGE-SYSDATE.
           DISPLAY MILAGE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE MILAGE-FORM-FIELDS TO WR-BUF.
           MOVE MILAGE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE MILAGE-HELP--FILE TO HELP-LINK-FILE.
           MOVE MILAGE-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/MILAGE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       CLOSE-TRFILES.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-TRD1-FILE.
           PERFORM CLOSE-TRP1-FILE.
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       READ-TRC-RECORD.
           MOVE WK2-BRNO   TO TC-BRNO.
           MOVE WK2-DATE   TO TC-KEY-DATE.
           MOVE WK2-CKNO   TO TC-CKNO.
           MOVE WK2-CLASS  TO TC-CLASS.
           MOVE WK2-SEQ    TO TC-SEQ.
      *    PERFORM READ-TRC1-FILE.
      *-----------------------------------------------------------------
       READ-TRD-RECORD.
           MOVE WK2-BRNO   TO TD-BRNO.
           MOVE WK2-DATE   TO TD-DATE.
      *    PERFORM READ-TRD1-FILE.
      *-----------------------------------------------------------------
       READ-TRP-RECORD.
           MOVE WK2-BRNO   TO TP-BRNO.
           MOVE WK2-DATE   TO TP-DATE.
      *    MOVE WK2-CODE   TO TP-CODE.
      *    MOVE 0          TO TP-POOLID.
           MOVE WK2-CLASS  TO TP-CLASS.
           MOVE WK2-ACCTNO TO TP-ACCTNO.
           MOVE WK2-LINE   TO TP-LINE.
      *    PERFORM READ-TRP1-FILE.

      *-----------------------------------------------------------------
       TEST-BK-REASON.
           IF TC-REASON = SPACES
              IF TC-AMT < 0
                 MOVE "DEPOSIT" TO D-REASON
              ELSE
                 MOVE "WITHDRAWAL" TO D-REASON.
           IF TC-AMT < 0
              MOVE "INT.DEPO" TO D-TYPE
              MOVE "INTERIM" TO D-REASON
           ELSE
              MOVE "BNK WTHD" TO D-TYPE
              MOVE "WITHDRAWAL" TO D-REASON.
           IF TC-REASON NOT = SPACES
              MOVE TC-REASON TO D-REASON.
      *-----------------------------------------------------------------
       TEST-RE-OR-SR.
           MOVE SPACES TO D-PAYEE.
           MOVE TC-PAYEE TO D-DLPAYEE.
           MOVE TC-CLASS TO D-DLCLASS.
           MOVE TC-DLNO TO D-DLNO.
           IF TC-TYPE = "RR"
              STRING "RESERVE FROM PRINCIPAL ",TC-DLRVNO DELIMITED
                BY "  " INTO D-REASON
              STRING "RSVPRN ",TC-DLRVNO DELIMITED BY "  " INTO D-TYPE
           ELSE
           IF TC-TYPE = "RC"
              STRING "RESERVE FROM CHARGES ",TC-DLRVNO DELIMITED
                BY "  " INTO D-REASON
              STRING "RSVCHG ",TC-DLRVNO DELIMITED BY "  " INTO D-TYPE
           ELSE
           IF TC-TYPE = "RP"
              MOVE "RSVPAR "TO D-TYPE
              MOVE "PARTICIPATING RESERVE" TO D-REASON
           ELSE
           IF TC-TYPE = "RD"
              MOVE "RECPRO "TO D-TYPE
              MOVE "RECEIPT PROCEEDS" TO D-REASON.
           IF TC-REASON NOT = SPACES
              MOVE TC-REASON TO D-REASON.
      *-----------------------------------------------------------------
       SETUP-TC-GL.
           MOVE TC-GLNO(SUBX) TO GLNO.
           MOVE TC-GLAMT(SUBX) TO GLAMT.
      *-----------------------------------------------------------------
       SETUP-TP-GL.
      * THE ONLY "P" RECORDS ARE "NSF" IN THE REFNO
           MOVE TP-GLNO(SUBX) TO GLNO.
           MOVE TP-GLAMT(SUBX) TO GLAMT.
           MULTIPLY -1 BY GLAMT.
      *-----------------------------------------------------------------
       GET-GI.
           IF ( HOLD-GI1-KEY NOT = GI1-KEY )
              PERFORM READ-GI1-FILE *> EMBEDDED - GIFILE
              IF ( IO-FG NOT = 0 )
                 MOVE "Y" TO WS-ERROR-GI-FILE
              ELSE
                 MOVE GI1-KEY TO HOLD-GI1-KEY.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRDGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      * COPY "LIBLP/LPTRC1RN.CPY".
      * COPYMEMBER: LIBLP/LPTR1RN
       LOAD-TRC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRC-ALL-DATA
              MOVE TRC-ALL-PATH             TO TRC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRC-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRC-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRC-ALL-DATA
              MOVE TRC-ALL-PATH             TO TRC-PATH.

      *-----------------------------------------------------------------
       OPEN-TRC1-FILE.
           PERFORM LOAD-TRC1-FILE.
           PERFORM OPEN-IT.
           MOVE TRC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRC1-FILE. *> EMBEDDED - TRCFILE
           PERFORM START-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.

           IF ( TRC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRC1-FILE.

           IF ( QTRC1-WEND-SEQ = ZEROES    ) OR
              ( QTRC1-WEND-SEQ NOT NUMERIC )
              MOVE TRC-PATH-OWNBR       TO QTRC1-WBEG-BRNO
              MOVE TRC-PATH-OWNBR       TO QTRC1-WEND-BRNO
              MOVE TC-KEY-DATE          TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRC1-WBEG-KEY-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRC1-WEND-KEY-DATE
              MOVE TC-CKNO              TO QTRC1-WBEG-CKNO
              MOVE TC-CLASS             TO QTRC1-WBEG-CLASS
              MOVE TC-SEQ               TO QTRC1-WBEG-SEQ
              MOVE ALL "9"              TO QTRC1-WEND-CKNO
                                           QTRC1-WEND-CLASS
                                           QTRC1-WEND-SEQ.

           EXEC SQL
            DECLARE TRC1_CURSOR CURSOR FOR
            SELECT TRCFILE.TC_BRNO,
                   CAST(TRCFILE.TC_KEY_DATE AS VARCHAR(10)),         
                   TRCFILE.TC_CKNO,
                   TRCFILE.TC_CLASS,
                   TRCFILE.TC_SEQ,
                   TRCFILE.TC_TYPE,  
                   CAST(TRCFILE.TC_DATE AS VARCHAR(10)),
                   TRCFILE.TC_AMT,
                   TRCFILE.TC_PAYEE,
                   TRCFILE.TC_REF,
                   TRCFILE.TC_REASON,
                   TRCFILE.TC_GLNO_1,
                   TRCFILE.TC_GLNO_2,
                   TRCFILE.TC_GLNO_3,
                   TRCFILE.TC_GLNO_4,
                   TRCFILE.TC_GLNO_5,
                   TRCFILE.TC_GLAMT_1,
                   TRCFILE.TC_GLAMT_2,
                   TRCFILE.TC_GLAMT_3,
                   TRCFILE.TC_GLAMT_4,
                   TRCFILE.TC_GLAMT_5,
                   TRCFILE.TC_DLRVNO,
                   TRCFILE.TC_DLNO,
                   TRCFILE.TC_TILLNO
              FROM DBO.TRCFILE
             WHERE (TRCFILE.TC_BRNO           = :QTRC1-WBEG-BRNO
               AND (TRCFILE.TC_KEY_DATE BETWEEN :QTRC1-WBEG-KEY-DATE
                                            AND :QTRC1-WEND-KEY-DATE)) 
               AND (TRCFILE.TC_TYPE          IN ('CA', 'RR', 'RC', 'MS',
                                                 'RP', 'RD', 'BK'))
             ORDER BY TRCFILE.TC_BRNO,
                      TRCFILE.TC_KEY_DATE,
                      TRCFILE.TC_CKNO,
                      TRCFILE.TC_CLASS,
                      TRCFILE.TC_SEQ
           END-EXEC.

           EXEC SQL
               OPEN TRC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRC1-WBEG-BRNO
                          QTRC1-WEND-BRNO
                          QTRC1-WBEG-CKNO
                          QTRC1-WEND-CKNO
                          QTRC1-WBEG-CLASS
                          QTRC1-WEND-CLASS
                          QTRC1-WBEG-SEQ
                          QTRC1-WEND-SEQ.
           MOVE SPACES TO QTRC1-WBEG-KEY-DATE
                          QTRC1-WEND-KEY-DATE.

           MOVE STAT---GOOD TO TRC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRC1-FILE-NEXT. *> EMBEDDED - TRCFILE
           PERFORM READ-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.
           INITIALIZE QTRC-REC.

           IF ( TRC1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH TRC1_CURSOR 
                 INTO 
                   :QTC-BRNO,
                   :QTC-KEY-DATE,
                   :QTC-CKNO,
                   :QTC-CLASS,
                   :QTC-SEQ,
                   :QTC-TYPE,  
                   :QTC-DATE,
                   :QTC-AMT,
                   :QTC-PAYEE,
                   :QTC-REF,
                   :QTC-REASON,
                   :QTC-GLNO-1,
                   :QTC-GLNO-2,
                   :QTC-GLNO-3,
                   :QTC-GLNO-4,
                   :QTC-GLNO-5,
                   :QTC-GLAMT-1,
                   :QTC-GLAMT-2,
                   :QTC-GLAMT-3,
                   :QTC-GLAMT-4,
                   :QTC-GLAMT-5,
                   :QTC-DLRVNO,
                   :QTC-DLNO,
                   :QTC-TILLNO
              END-EXEC

           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRC1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRC1-CURSOR-STAT.


      * COPY "LIBLP/LPTRD1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRD1RN
       LOAD-TRD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRD-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRD-ALL-DATA
              MOVE TRD-ALL-PATH             TO TRD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRD-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRD-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRD-ALL-DATA
              MOVE TRD-ALL-PATH             TO TRD-PATH.

      *-----------------------------------------------------------------
       OPEN-TRD1-FILE.
           PERFORM LOAD-TRD1-FILE.
           PERFORM OPEN-IT.
           MOVE TRD-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRD1-FILE. *> EMBEDDED - TRDFILE
           PERFORM START-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.

           IF ( TRD1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRD1-FILE.

           IF ( QTRD1-WEND-BRNO = ZEROES    ) OR
              ( QTRD1-WEND-BRNO NOT NUMERIC )
              MOVE TRD-PATH-OWNBR       TO QTRD1-WBEG-BRNO
              MOVE TRD-PATH-OWNBR       TO QTRD1-WEND-BRNO
              MOVE TD-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WEND-DATE.


           EXEC SQL
            DECLARE TRD1_CURSOR CURSOR FOR
             SELECT TRDFILE.TD_BRNO,
                    CAST(TRDFILE.TD_DATE AS VARCHAR(10)),
                    TRDFILE.TD_DEPOSIT,
                    TRDFILE.TD_BANK,
                    TRDFILE.TD_CASH,
                    TRDFILE.TD_GLOVERSHORT,
                    TRDFILE.TD_OVERSHORT
               FROM DBO.TRDFILE
              WHERE TRDFILE.TD_BRNO       = :QTRD1-WBEG-BRNO
                AND TRDFILE.TD_DATE BETWEEN :QTRD1-WBEG-DATE 
                                        AND :QTRD1-WEND-DATE
              ORDER BY TRDFILE.TD_BRNO,
                       TRDFILE.TD_DATE
           END-EXEC.

           EXEC SQL
               OPEN TRD1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRD1-WBEG-BRNO 
                          QTRD1-WEND-BRNO.
 
           MOVE SPACES TO QTRD1-WBEG-DATE
                          QTRD1-WEND-DATE.

           MOVE STAT---GOOD TO TRD1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRD1-FILE-NEXT. *> EMBEDDED - TRDFILE
           PERFORM READ-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.
           INITIALIZE QTRD-REC.

           IF ( TRD1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH TRD1_CURSOR 
                 INTO 
                   :QTD-BRNO,
                   :QTD-DATE,
                   :QTD-DEPOSIT,
                   :QTD-BANK,
                   :QTD-CASH,
                   :QTD-GLOVERSHORT,
                   :QTD-OVERSHORT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRD1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRD-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRD1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRD1-CURSOR-STAT.


      * COPY "LIBLP/LPTRP1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRP1RN
      * 20220923 BAH ADDED TP-LCDUE *1570
       LOAD-TRP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH             TO TRP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRP-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRP-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH             TO TRP-PATH.

      *-----------------------------------------------------------------
       OPEN-TRP1-FILE.
           PERFORM LOAD-TRP1-FILE.
           PERFORM OPEN-IT.
           MOVE TRP-PATH-SQL TO E-FILE.

           IF TRP1-CONN-OPEN-SW = "N"
              MOVE "Y" TO TRP1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRP1_CONNECT
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TRP1-FILE. *> SSP - TRP,BW,LP

           PERFORM START-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.

           IF ( TRP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRP1-FILE.

           IF ( QTRP1-WEND-LINE = ZEROES    ) OR
              ( QTRP1-WEND-LINE NOT NUMERIC )
              MOVE TRP-PATH-OWNBR        TO QTRP1-WBEG-BRNO
              MOVE TRP-PATH-OWNBR        TO QTRP1-WEND-BRNO
              MOVE TP-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WBEG-DATE
              MOVE 99991231         TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WEND-DATE
              MOVE TP-CLASS             TO QTRP1-WBEG-CLASS
              MOVE TP-ACCTNO            TO QTRP1-WBEG-ACCTNO
              MOVE TP-LINE              TO QTRP1-WBEG-LINE
              MOVE ALL "9"              TO QTRP1-WEND-CLASS
                                           QTRP1-WEND-ACCTNO
                                           QTRP1-WEND-LINE
      *- - - -{ ADD ONE TO WHERE SCAN CONTINUE FIELDS
              MOVE TP-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE

              IF ( TP-CLASS = 999 )
                 MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
              ELSE
                 COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1
              END-IF

              IF ( TP-ACCTNO = 99999999 )
                 MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1
              END-IF

              IF ( TP-LINE = 999 )
                 MOVE TP-LINE           TO QTRP1-WSBEG-LINE
              ELSE
                 COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.

      * STORED PROCEDURE: 
      *
      *  SELECT TRPFILE.TP_BRNO,
      *         CAST(TRPFILE.TP_DATE AS VARCHAR(10)),
      *         TRPFILE.TP_CLASS,
      *         TRPFILE.TP_ACCTNO,
      *         TRPFILE.TP_LINE,
      *         TRPFILE.TP_SSNO,
      *         TRPFILE.TP_REFNO,
      *         CAST(TRPFILE.TP_PAYDATE AS VARCHAR(10)),
      *         TRPFILE.TP_TRAMT,
      *         TRPFILE.TP_APINT,
      *         TRPFILE.TP_APLC,
      *         TRPFILE.TP_APCUR,
      *         TRPFILE.TP_GLNO_1,
      *         TRPFILE.TP_GLNO_2,
      *         TRPFILE.TP_GLNO_3,
      *         TRPFILE.TP_GLAMT_1,
      *         TRPFILE.TP_GLAMT_2,
      *         TRPFILE.TP_GLAMT_3,
      *         TRPFILE.TP_POSTING_BR,
      *         TRPFILE.TP_OTHBR_FG,
      *         TRPFILE.TP_REVSEQNO,
      *         ISNULL(BWFILE.BW_OWNBR, 0),
      *         ISNULL(BWFILE.BW_SSNO, 0),
      *         ISNULL(BWFILE.BW_LNAME,' '),
      *         ISNULL(BWFILE.BW_FNAME,' '),
      *         ISNULL(LPFILE.LP_BRNO, 0),
      *         ISNULL(LPFILE.LP_ACCTNO, 0),
      *         ISNULL(LPFILE.LP_SEQNO, 0),
      *         ISNULL(CAST(LPFILE.LP_PAYDATE AS VARCHAR(10)), '1900-01-01')
      *
      *    FROM DBO.TRPFILE TRPFILE
      *
      *    LEFT OUTER JOIN
      *         DBO.BWFILE  BWFILE
      *      ON (TRPFILE.TP_BRNO       = BWFILE.BW_OWNBR
      *     AND  TRPFILE.TP_SSNO       = BWFILE.BW_SSNO)
      *
      *    LEFT OUTER JOIN
      *         DBO.LPFILE  LPFILE
      *      ON (TRPFILE.TP_BRNO       = LPFILE.LP_BRNO
      *     AND  TRPFILE.TP_ACCTNO     = LPFILE.LP_ACCTNO
      *     AND  TRPFILE.TP_REVSEQNO   = LPFILE.LP_SEQNO)
      *
      *   WHERE (TRPFILE.TP_BRNO       = @QTRP1_WBEG_BRNO
      *     AND  TRPFILE.TP_DATE BETWEEN @QTRP1_WBEG_DATE
      *                              AND @QTRP1_WEND_DATE)
      *     AND (TRPFILE.TP_REFNO       = 'NSF  ')
      *   ORDER BY TRPFILE.TP_BRNO,
      *            TRPFILE.TP_DATE,
      *            TRPFILE.TP_CLASS,
      *            TRPFILE.TP_ACCTNO,
      *            TRPFILE.TP_LINE

           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRP1_CURSOR CURSOR FOR
                :TRP1-RETURN-CODE = EXEC SSP_MILAGE_TRPFILE_SELECT
              ( :QTRP1-WBEG-BRNO,   
                :QTRP1-WBEG-DATE,   
                :QTRP1-WEND-DATE )
           END-EXEC.

           EXEC SQL
               OPEN TRP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRP1-WBEG-BRNO
                          QTRP1-WEND-BRNO
                          QTRP1-WBEG-CLASS
                          QTRP1-WEND-CLASS
                          QTRP1-WBEG-ACCTNO
                          QTRP1-WEND-ACCTNO
                          QTRP1-WBEG-LINE
                          QTRP1-WEND-LINE
                          QTRP1-WSBEG-CLASS
                          QTRP1-WSBEG-ACCTNO
                          QTRP1-WSBEG-LINE.
           MOVE SPACES TO QTRP1-WBEG-DATE
                          QTRP1-WEND-DATE
                          QTRP1-WSBEG-DATE.

           MOVE STAT---GOOD TO TRP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRP1-FILE-NEXT. *> EMBEDDED - TRP,BW,LP
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           INITIALIZE QTRP-REC.
           INITIALIZE QBW-REC.
           INITIALIZE QLP-REC.

           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH TRP1_CURSOR INTO  
                  :QTP-BRNO, 
                  :QTP-DATE,
                  :QTP-CLASS,
                  :QTP-ACCTNO,
                  :QTP-LINE,
                  :QTP-SSNO,
                  :QTP-REFNO,
                  :QTP-PAYDATE,
                  :QTP-TRAMT,
                  :QTP-APINT,
                  :QTP-APLC,
                  :QTP-APCUR,
                  :QTP-GLNO-1,
                  :QTP-GLNO-2,
                  :QTP-GLNO-3,
                  :QTP-GLAMT-1,
                  :QTP-GLAMT-2,
                  :QTP-GLAMT-3,
                  :QTP-POSTING-BR,
                  :QTP-OTHBR-FG,
                  :QTP-REVSEQNO,
                  :QBW-OWNBR, 
                  :QBW-SSNO,  
                  :QBW-LNAME, 
                  :QBW-FNAME, 
                  :QLP-BRNO, 
                  :QLP-ACCTNO, 
                  :QLP-SEQNO,  
                  :QLP-PAYDATE
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRP-FIELDS
              PERFORM GET-BW-FIELDS
              PERFORM GET-LP-FIELDS. 

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRP1-FILE.

           PERFORM CLOSE-IT.
           IF (( TRP1-CURSOR-STAT-GOOD ) AND
               ( TRP1-CONN-OPEN-SW = "Y" ))
              EXEC SQL SET CONNECTION TRP1_CONNECT  END-EXEC
              EXEC SQL
                  CLOSE TRP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRP1-CURSOR-STAT  
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRP1-CONNECTION.

           IF TRP1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT TRP1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO TRP1-CONN-OPEN-SW.


       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBGL/GLGI1RN.CPY".
      * COPYMEMBER: LIBGL/GLGI1RN
       LOAD-GI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GI-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GI-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GI-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GI-ALL-MACHINE
              MOVE GI-ALL-PATH             TO GI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GI-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GI-ALL-BASE
              MOVE FILPATH-MACHINE         TO GI-ALL-MACHINE
              MOVE GI-ALL-PATH             TO GI-PATH.

      *-----------------------------------------------------------------
       OPEN-GI1-FILE.
           PERFORM LOAD-GI1-FILE.
           PERFORM OPEN-IT.
           MOVE GI-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-GI1-FILE. *> EMBEDDED - GIFILE
           PERFORM READ-IT.
           MOVE GI-PATH-SQL TO E-FILE.
           MOVE GI1-KEY TO E-KEYX.
           INITIALIZE QGI-REC.

           MOVE GI-BRANCH    TO QGI-BRANCH. 
           MOVE GI-CLASS     TO QGI-CLASS.

           EXEC SQL
            SELECT GIFILE.GI_BRANCH,
                   GIFILE.GI_CLASS,
                   GIFILE.GI_CASH,
                   GIFILE.GI_BANK,
                   GIFILE.GI_NSF,
                   GIFILE.GI_AR,
                   GIFILE.GI_OTHBR_REC,
                   GIFILE.GI_OTHBR_PAY,
                   GIFILE.GI_DLPAYABLE,
                   GIFILE.GI_DLRVPRIN_1,
                   GIFILE.GI_DLRVPRIN_2,
                   GIFILE.GI_DLRVPRIN_3,
                   GIFILE.GI_DLRVCHGS_1,
                   GIFILE.GI_DLRVCHGS_2,
                   GIFILE.GI_DLRVCHGS_3,
                   GIFILE.GI_DLPARV,
                   GIFILE.GI_LATECHG,
                   GIFILE.GI_INTEREST
              INTO
                  :QGI-BRANCH,
                  :QGI-CLASS,
                  :QGI-CASH,
                  :QGI-BANK,
                  :QGI-NSF,
                  :QGI-AR,
                  :QGI-OTHBR-REC,
                  :QGI-OTHBR-PAY,
                  :QGI-DLPAYABLE,
                  :QGI-DLRVPRIN-1,
                  :QGI-DLRVPRIN-2,
                  :QGI-DLRVPRIN-3,
                  :QGI-DLRVCHGS-1,
                  :QGI-DLRVCHGS-2,
                  :QGI-DLRVCHGS-3,
                  :QGI-DLPARV,
                  :QGI-LATECHG,
                  :QGI-INTEREST
              FROM DBO.GIFILE
             WHERE GIFILE.GI_BRANCH = :QGI-BRANCH
               AND GIFILE.GI_CLASS  = :QGI-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GI1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GI-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GI1-FILE.
           PERFORM CLOSE-IT.


      * COPY "LIBGL/GLGL1RN.CPY".
      * COPYMEMBER: LIBGL/GLGL1RN
       LOAD-GL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GL-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GL-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GL-ALL-MACHINE
              MOVE GL-ALL-PATH             TO GL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GL-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GL-ALL-BASE
              MOVE FILPATH-MACHINE         TO GL-ALL-MACHINE
              MOVE GL-ALL-PATH             TO GL-PATH.

      *-----------------------------------------------------------------
       OPEN-GL1-FILE.
           PERFORM LOAD-GL1-FILE.
           PERFORM OPEN-IT.
           MOVE GL-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-GL1-FILE. *> EMBEDDED - GLFILE
           PERFORM READ-IT.
           MOVE GL-PATH-SQL TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.
           INITIALIZE QGL-REC.

           MOVE GL-BR-ACCOUNT TO QGL-BR-ACCOUNT.

           EXEC SQL
            SELECT GLFILE.GL_BR_ACCOUNT,
                   GLFILE.GL_DESCRIPTION
              INTO 
                  :QGL-BR-ACCOUNT,
                  :QGL-DESCRIPTION
              FROM DBO.GLFILE
             WHERE GLFILE.GL_BR_ACCOUNT = :QGL-BR-ACCOUNT
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GL-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GL1-FILE.
           PERFORM CLOSE-IT.
 

       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ***************************************************
       END-PROGRAM SECTION.
      ***************************************************
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY MILAGE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
