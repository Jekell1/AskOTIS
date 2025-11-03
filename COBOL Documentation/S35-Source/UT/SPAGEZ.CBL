      *  A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPAGEZ.
      *****************************************************
      *
      *    CREATES PIPE DELIMITED /USR/EXTRACT/             
      *  REV:   AGEING USING BANKRULE OF SPACES          
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

           SELECT WK-AGEINZ-FILE ASSIGN OUTPUT WK-AGEINZ-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.


       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      **************************************************
      *   LOAN ASCII FILE    RSZ =  (INCLUDES 0D0A)
      **************************************************
       FD  WK-AGEINZ-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-AGEINZ-REC.
           03  WK-AGEINZ-BYTE        PIC X OCCURS 2000 TIMES.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/SPAGEZ S35 03/31/23-09:23:59 >".

      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".


       01  WK-AGEINZ-PATH.
           03  WK-AGEINZ-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-AGEINZ-FILENAME               PIC X(08) 
                 VALUE "/AGEINZ.".                    

           03  WK-AGEINZ-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-AGEINZ-PATH-YR                PIC 99.
           03  WK-AGEINZ-PATH-MO                PIC 99.
           03  WK-AGEINZ-PATH-DA                PIC 99.

       01  WS-AGEINZ-REC.
           03  WS-AGEINZ-LN-BRACCT                PIC Z(14).
           03  WS-AGEINZ-LN-ACCTNO                PIC Z(10).
           03  WS-AGEINZ-LN-OWNBR                 PIC Z(6).
           03  WS-AGEINZ-AGEING-DATE              PIC 99/99/9999.
           03  WS-AGEINZ-SP1-KEY                   PIC X(9).
           03  WS-AGEINZ-E-1STPYAMT              PIC ZZZZZZZZ9.99-.
           03  WS-AGEINZ-E-REGPYAMT              PIC ZZZZZZZZ9.99-.
           03  WS-AGEINZ-E-LASTPYAMT             PIC ZZZZZZZZ9.99-.
           03  WS-AGEINZ-LN-TOTPAYMNTD            PIC ZZZZZZZZ9.99-.
           03  WS-AGEINZ-LN-DATE-PAID-LAST        PIC 99/99/9999.
           03  WS-AGEINZ-PDTH-PAYS                PIC ZZZZZZ9.
           03  WS-AGEINZ-PDTH-REMAIN              PIC .999999999999.
           03  WS-AGEINZ-WORK                PIC ZZZZZZZZ9.999999999-.
           03  WS-AGEINZ-PDTH-DATE-FULL           PIC 99/99/9999.
           03  WS-AGEINZ-LN-PY-MMYY-RECDEL        PIC 99/99.
           03  WS-AGEINZ-LN-MTD-PAYS-RECDEL       PIC ZZZZZZZZ9.99-.
           03  WS-AGEINZ-LN-UNITPER-CD            PIC X.
      *CONTRACTUAL
           03  WS-AGEINZ-SP-BANKRULE              PIC X.
           03  WS-AGEINZ-SP-CONTRFRMLA            PIC X.
           03  WS-AGEINZ-CONTRACTUAL-DESC         PIC X(20).
           03  WS-AGEINZ-SP-CONTPDTH              PIC ZZZZZ9.
           03  WS-AGEINZ-SP-BANKUNITPER-CD               PIC X.
           03  WS-AGEINZ-SP-CONTR-CREDIT-CD       PIC X.
           03  WS-AGEINZ-SP-CONTR-CREDIT          PIC ZZZ9.99.
           03  WS-AGEINZ-SP-CPOT30-CD             PIC X.
           03  WS-AGEINZ-CPOT30                   PIC X.
           03  WS-AGEINZ-SP-CPOTCHGOFF-CD         PIC X.
           03  WS-AGEINZ-CPOTCHGOFF               PIC X.
           03  WS-AGEINZ-CSUB-ALDEL-FG            PIC X.
           03  WS-AGEING-WORK              PIC ZZZZZZZZ9.999999999-.
           03  WS-AGEINZ-CSUB                     PIC ZZZZZZZ9-.
      *CONTRACTUAL STATUS
           03  WS-AGEINZ-CONTRACTUAL              PIC ZZZZZZ9-.
      * RECENCY
           03  WS-AGEINZ-SP-RECFRMLA              PIC X.
           03  WS-AGEINZ-RECENCY-DESC             PIC X(20).
           03  WS-AGEINZ-SP-RECPDTH               PIC ZZZZ9.
           03  WS-AGEINZ-SP-RECPAYCD              PIC X.
           03  WS-AGEINZ-RDSUB                    PIC ZZZZZZZ9-.
           03  WS-AGEINZ-RSUB                     PIC ZZZZZZZ9-.
           03  WS-AGEINZ-SP-RDPOT30-CD            PIC X.
           03  WS-AGEINZ-RDPOT30                  PIC X.
           03  WS-AGEINZ-SP-RDPOTCHGOFF-CD        PIC X.
           03  WS-AGEINZ-RDPOTCHGOFF              PIC X.
      *TRUE RECENCY
           03  WS-AGEINZ-RECENCY                  PIC ZZZZZZ9-.
      *RECENCY STATUS
           03  WS-AGEINZ-RECDEL                   PIC ZZZZZZ9-.
      *PAID AHEAD
           03  WS-AGEINZ-PSUB                    PIC ZZZZZZZ9-.


       01  WK-AGEINZ-IFN        PIC X(6)     VALUE "AGEINZ".



       01  HOLD-SP1-KEY                PIC X(9)     VALUE SPACES.
       01  FILLER            REDEFINES HOLD-SP1-KEY.
           03  HOLD-SP-ORGST           PIC X(2).
           03  HOLD-SP-SPRCLASS        PIC 9(3).
           03  HOLD-SP-SUBCLASS        PIC 9(2).
           03  HOLD-SP-LAWCODE         PIC 9(2).

       01  HOLD-FIL-PATH.
           03  FILLER           PIC X(9).
           03  HOLD-FIL-MACHINE PIC X(2).
           03  FILLER           PIC X(7). 

       01  HOLD-PDTH-E-OPTION   PIC X.
       01  HOLD-NOCAP-FG        PIC X.
       01  HOLD-DATE-CAP        PIC X.
       01  HOLD-INCDEF-FG       PIC X.
       01  HOLD-INCALDEL-FG     PIC X.
       01  BW-SOLICIT           PIC X.
       01  HOLD-BR-LOAN-NUM-FLAG PIC X.

       01  SUB                  PIC 99.
       01  JUSTIFY-TYPE     PIC X VALUE "L".

       01  TODAYS-DATE      PIC 9(8).
 
       01  WS-BRACCT.
           03  WS-OWNBR     PIC Z(6).
           03  WS-NUMBER    PIC 9(6).
       01  WS-LN-ACCTNO  PIC 9(8).
       01  WS-LN-OWNBR   PIC 9(4).

       01  CR-1          PIC X VALUE X"0D".
      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99   COMP VALUE 0.
           03  LN-COUNT         PIC 999  COMP VALUE 99.
           03  LN-WK            PIC 999  COMP VALUE 0.
       01  PAGE-NUMBER          PIC 9(4) COMP VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
 
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  DISPLAY-BUF.
           03  FILLER           PIC X(12) VALUE "PROCESSING..".
           03  DISPLAY-BRNO     PIC Z(4).
           03  FILLER           PIC X(7).
           03  FILLER           PIC X(17) VALUE SPACES.

       01  TOTAL-RECORDS        PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-BYPASSED       PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-INCLUDED       PIC S9(13)  COMP-3 VALUE 0.
    
       01  CALCULATE-ORG-ANTIC-TBL-X.
           03  CALCULATED-ORG-ANTIC-1       PIC S9(7)V99.
           03  CALCULATED-ORG-ANTIC-2       PIC S9(7)V99.
           03  CALCULATED-ORG-ANTIC-3       PIC S9(7)V99.
           03  CALCULATED-ORG-ANTIC-4       PIC S9(7)V99.
           03  CALCULATED-ORG-ANTIC-5       PIC S9(7)V99.
           03  CALCULATED-ORG-ANTIC-6       PIC S9(7)V99.


       01  CALCULATED-ORG-ANTIC-TBL REDEFINES 
                              CALCULATE-ORG-ANTIC-TBL-X  OCCURS 6.
           03  CALCULATED-ORG-ANTIC   PIC S9(7)V99.         
    
       01  ORIGINAL-FIELDS.
           03  ORIGINAL-SP-EXPR-FRMLA       PIC X.
           03  ORIGINAL-SP-EXPD-FRMLA       PIC X.
           03  ORIGINAL-SP-EXPD-MONTHS      PIC 99.
           03  ORIGINAL-SP-EXPD-DAYS        PIC 99.
           03  ORIGINAL-LN-TOTNODEF         PIC S99.
           03  ORIGINAL-LN-TOTNOALDEL       PIC S99.
           03  ORIGINAL-SP-CONTR-CREDIT-CD  PIC X.
           03  ORIGINAL-SP-CONTR-CREDIT     PIC 99V99. 
           03  ORIGINAL-SP-LCMINCHG         PIC 99V99. 
           03  ORIGINAL-SP-LCMAXAMT         PIC 9999V99.
           03  ORIGINAL-GP-INCL-OTH-IN-EARNED PIC X.
           03  ORIGINAL-SP-LNCOST-AMT       PIC S9(7)V99.
           03  ORIGINAL-SP-LNCOST-FRMLA     PIC X.
           03  ORIGINAL-GP-INC-OTH-IN-EARNED  PIC X.
           03  ORIGINAL-GP-PDTH-GT-SCHED-PAY PIC X.
           03  ORIGINAL-SP-BANKRULE          PIC X.
           03  ORIGINAL-SP-BANKUNITPER-CD    PIC X.
           03  ORIGINAL-SP-CONTRFRMLA        PIC X.
           03  ORIGINAL-SP-CONTPDTH          PIC 9(4).
           03  ORIGINAL-SP-RECFRMLA          PIC X.
           03  ORIGINAL-SP-RECPAYCD          PIC X.
           03  ORIGINAL-SP-RECPDTH           PIC 9(3).
           03  ORIGINAL-SP-CPOT30-CD         PIC X.
           03  ORIGINAL-SP-CPOTCHGOFF-CD     PIC X.
           03  ORIGINAL-SP-RDPOT30-CD        PIC X.
           03  ORIGINAL-SP-RDPOTCHGOFF-CD    PIC X.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR          PIC 9999.
           03  END-BR          PIC 9999.
           03  BEG-CLASS       PIC 999.
           03  END-CLASS       PIC 999.
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  BEG-ACCTNO      PIC 9(6).
           03  END-ACCTNO      PIC 9(6).

           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "B".

       01  SV-BEG-BR           PIC 9(4)   VALUE 9999.
       01  SV-END-BR           PIC 9(4)   VALUE 9999.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(13)    VALUE " ".

       01  HEAD3.
           03  FILLER           PIC X(9)    VALUE "BRANCH NO".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(13)   VALUE "TOTAL RECORDS".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(12)   VALUE "TOTAL LN AMT".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(11)   VALUE "TOTAL FILES".


       01  DETAIL-LINE          PIC X(80) VALUE SPACES.
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC X(2).
           03  D-NUMBER         PIC Z(6).
           03  FILLER           PIC X(2).
           03  D-NAME           PIC X(15).
           03  FILLER           PIC X(2).
           03  D-REPODATE       PIC 99/99/99.
           03  FILLER           PIC X(5).
           03  D-CURBAL         PIC Z(6).99.

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  D-TOTAL-BRNO     PIC Z(4).
           03  FILLER           PIC X(10).
           03  D-TOTAL-REC      PIC Z(7).
           03  FILLER           PIC X(20).
           03  D-TOTAL-AMT      PIC Z(9).99.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(5).
           03  RANGE1           PIC X(10).
           03  RANGE1N REDEFINES RANGE1 PIC 9(10).
           03  RANGE1D REDEFINES RANGE1 PIC ZZ99/99/99.
           03  FILLER           PIC X(2).
           03  RANGE2           PIC X(10).
           03  RANGE2N REDEFINES RANGE2 PIC 9(10).
           03  RANGE2D REDEFINES RANGE2 PIC ZZ99/99/99.

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".

       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/GETENVW.CPY".
      *COPY "LIBCL/GLOBRDW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
      *COPY "LIBLP/LPUPERW.CPY".
      *COPY "LIBLP/LPUPWKW.CPY".
      *COPY "LIBLP/LPXDTEW.CPY".
      *COPY "LIBLP/LPITRMW.CPY".
      *COPY "LIBLP/LPAMTSW.CPY".
      *COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPCEPPW.CPY". 
      *COPY "LIBLP/LNCOSTW.CPY".
      *COPY "LIBLP/LPEARNW.CPY".
      *COPY "LIBLP/LPIBPCW.CPY".
      *COPY "LIBLP/LPCDELW.CPY".
      *COPY "LIBLP/LPLCASW.CPY".
      *COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
      *COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/TESTR2_DEF.CPY".
       COPY "LIBSP/TESTR2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/TESTR2_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-AGEINZ-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               PR-FILE WK-AGEINZ-FILE.

       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      *************************************
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TESTR2" TO VDU-FORM-NAME.

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
           PERFORM NPR-TRAP-INTERRUPT.

           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BR
              GO TO END-ROUTINE.
       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.
           MOVE "B"        TO LN-PATH-OVERRIDE
                              GB-PATH-OVERRIDE
                             LTP-PATH-OVERRIDE.

           PERFORM LOAD-LN3-FILE.
           PERFORM LOAD-LTP1-FILE.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-LTP1-FILE.


           PERFORM GLOBAL-UPDATE.

           PERFORM CLOSE-LN3-FILE.

       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      **************************
       GLOBAL-UPDATE SECTION.
      **************************
           IF EXT-ROUTE-BUF = "00"
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE BR-NO TO DISPLAY-BRNO
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE MESS-LIST   TO WR-LIST
                 PERFORM CALL-WRFORM.

           MOVE BR-NO TO LN-OWNBR.
           MOVE 0     TO LN-CLASS LN-ACCTNO.
           PERFORM START-LN3-FILE.
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
*L2G       IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO EXIT-GLOBAL-UPDATE.
           IF LN-ACCTNO =  4749  
      D    STOP MESS
             MOVE SPACES TO MESS.

           ADD 1 TO TOTAL-RECORDS.

           IF LN-OWNBR > END-BR
              GO TO EXIT-GLOBAL-UPDATE.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF LN-LOANDATE < BEG-DATE OR LN-LOANDATE > END-DATE
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF LN-ACCTNO < BEG-ACCTNO OR LN-ACCTNO > END-ACCTNO
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.
           
           IF LN-POCD-NONLOAN
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

      * EXCLUDE ZERO BALANCES
           IF LN-CURBAL = 0 AND LN-INTBAL = 0 AND LN-LCBAL = 0
               AND LN-OTHBAL = 0
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

      *CHECK FOR NO PAYOFF DATE ON PL.
           IF LN-PLDATE NOT = ZEROS
              IF LN-POFFDATE NOT = 0
                 ADD 1 TO TOTAL-BYPASSED
                 GO TO NEXT-LN.

      * EXCLUDE PL < 01/01/2010
           IF LN-PLDATE NOT = ZEROS
              IF LN-PLDATE NOT > 20100101
                 ADD 1 TO  TOTAL-BYPASSED
                 GO TO NEXT-LN.

       GET-SPR.
           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
           PERFORM READ-SP1-FILE
           IF IO-FG NOT = 0
               ADD 1 TO TOTAL-BYPASSED
               GO TO NEXT-LN.
           MOVE SP1-KEY TO HOLD-SP1-KEY.           

           PERFORM SET-ORIGINAL-HOLD-FIELDS.
           PERFORM SAVE-ORIGINAL-FIELDS.
          
           PERFORM CREATE-AGEINZ-WORK.  

           GO TO NEXT-LN.

       EXIT-GLOBAL-UPDATE.
           EXIT.





      /**********************************
      *   I N I T I A L I Z A T I O N   *
      ***********************************
       INITIALIZATION SECTION.
           MOVE TESTR2-QUEUE-POS TO Q-POS.
           MOVE TESTR2-COPIES-POS TO C-POS.
           MOVE TESTR2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

           PERFORM OPEN-PR-FILE.
           PERFORM ENABLE-INTERRUPT.

           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                             END-BR.

           MOVE 0 TO TOTAL-RECORDS.

           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD  TO TODAYS-DATE.

           MOVE "FIL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO  HOLD-FIL-PATH.
           IF STAT NOT = "00"
              GO TO IO-ERROR. 


           MOVE HOLD-FIL-MACHINE TO WK-AGEINZ-MACHINE.

           MOVE TODAYS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY-DD TO WK-AGEINZ-PATH-DA.
           MOVE DATE-MMDDYY-MM TO WK-AGEINZ-PATH-MO.
           MOVE DATE-MMDDYY-YY TO WK-AGEINZ-PATH-YR.

           CALL "C$TOLOWER" USING WK-AGEINZ-PATH1,VALUE 12.
           PERFORM OPEN-WK-AGEINZ-FILE-OUTPUT.
      *-----------------------------------------------
           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.



      ****************************************
      *    SETUP DATES FOR BATCH PROCESS
      ****************************************
       EO-EXEC-AUDIT SECTION.
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ****************************************
      *       ENTER REPORT PARAMETERS
      ****************************************
       ENTER-PARAMETERS SECTION.
       ENTER-PARM.
      * FOR RE-ENTRY ON F4 FROM ACCEPT.

       BR-01.
           MOVE "E RA000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE GRSCAN-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              MOVE SCAN-BUF TO GRSCAN-BUF
              IF SCAN-STATUS NOT = "00"
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
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
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
           GO TO BEGDATE-01.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       BEGDATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
      *-----------------------------------------------------------
       ENDDATE-02.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGDATE-01.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO ENDDATE-02.

           MOVE VDU-DATE-YYYYMMDD  TO END-DATE.

      * MAKE SURE THE DATE RANGE IS VALID.

           IF BEG-DATE NOT = 0
              MOVE BEG-DATE  TO NUM-DATE
              MOVE END-DATE TO SYS-DATE
              PERFORM CMPDAT
              IF SYS-IS-LT
                 MOVE "INVALID DATE RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO BEGDATE-01.

           GO TO ENTER-BEG-ACCTNO.            

      *----------------------------------------------------------
       ALL-DATE.

           MOVE "S  8080DATE1." TO VDU.
           MOVE JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DATE2."   TO VDU.
           MOVE JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------
       ENTER-BEG-ACCTNO.         
           MOVE "ENNN060ACCT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "16" GO TO ENTER-ALL-ACCTNO 
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO ENDDATE-02 
              ELSE
                 GO TO CL-02.
           MOVE VDUNUM0 TO BEG-ACCTNO.

      *----------------------------------------------------------
       ENTER-END-ACCTNO.
           MOVE "ENNN060ACCT2." TO VDU.
           PERFORM SCREENIO.
           
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
               GO TO ENTER-BEG-ACCTNO.
           MOVE VDUNUM0 TO END-ACCTNO.
           
           GO TO EXIT-PARM.
      *-----------------------------------------------------------
       ENTER-ALL-ACCTNO.
           MOVE "SNNN060ACCT1." TO VDU.
           MOVE 0 TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060ACCT2." TO VDU.
           MOVE 999999 TO END-ACCTNO VDUNUM0.
           PERFORM SCREENIO.          


      *============================================
       EXIT-PARM.
           EXIT.


      ****************************************
      *       DISPLAY REPORT PARAMETERS
      ****************************************
       DISPLAY-PARAMETERS SECTION.

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR    TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SN N020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
    
      ****************************
       CREATE-AGEINZ-WORK SECTION.
      ****************************
      *USES LPCEPP,LPPDUE  
      *1)BANKRULE SPACE = 30 AFTER 2 MONTHS FROM DUE DATE  100%
      * CONTRACTUAL FRMLA TOTAL MONIES
      *-------------------------------------------------
           PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WS-AGEINZ-REC WK-AGEINZ-REC.
           MOVE ZEROS TO AGEING-E-1STPYAMT
                         AGEING-E-REGPYAMT
                         AGEING-E-LASTPYAMT 
                         AGEING-WORK
                         PSUB.                         
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-AGEINZ-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-AGEINZ-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-AGEINZ-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-OWNBR, JUSTIFY-TYPE.

           MOVE TODAYS-DATE TO AGEING-DATE, DATE-YYYYMMDD.        
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-AGEING-DATE.           

           MOVE SP1-KEY TO WS-AGEINZ-SP1-KEY.


           MOVE SP-CONTR-CREDIT-CD TO WS-AGEINZ-SP-CONTR-CREDIT-CD.
           MOVE SP-CONTR-CREDIT TO WS-AGEINZ-SP-CONTR-CREDIT.          
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTR-CREDIT,
                                                 JUSTIFY-TYPE.

           MOVE LN-TOTPAYMNTD  TO WS-AGEINZ-LN-TOTPAYMNTD.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-TOTPAYMNTD,
                                             JUSTIFY-TYPE.
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-LN-DATE-PAID-LAST.

           MOVE LN-PY-YYYYMM-RECDEL  TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY TO WS-AGEINZ-LN-PY-MMYY-RECDEL.

           MOVE LN-MTD-PAYS-RECDEL  TO WS-AGEINZ-LN-MTD-PAYS-RECDEL.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-MTD-PAYS-RECDEL 
                                                 JUSTIFY-TYPE.

           MOVE LN-UNITPER-CD TO WS-AGEINZ-LN-UNITPER-CD.     
      *-----------------------------------------------------
           MOVE SPACES        TO SP-BANKRULE
                                 WS-AGEINZ-SP-BANKRULE.
           MOVE "2 MO FROM DUE DATE" TO WS-AGEINZ-CONTRACTUAL-DESC.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTPDTH,
                                            JUSTIFY-TYPE.

           MOVE "A"           TO SP-CONTRFRMLA
                                 WS-AGEINZ-SP-CONTRFRMLA.
  
           MOVE 100                  TO SP-CONTPDTH
                                        WS-AGEINZ-SP-CONTPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTPDTH,
                                              JUSTIFY-TYPE.

           MOVE SPACES TO SP-BANKUNITPER-CD
                          WS-AGEINZ-SP-BANKUNITPER-CD.

           MOVE "C"           TO SP-RECFRMLA
                                 WS-AGEINZ-SP-RECFRMLA.
           MOVE "CALENDAR"    TO WS-AGEINZ-RECENCY-DESC.

%%    *    MOVE 100           TO SP-RECPDTH
%%    *                          WS-AGEINZ-SP-RECPDTH.
%%    *    CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
%%    *                                  JUSTIFY-TYPE.
             
           MOVE SP-RECPDTH TO WS-AGEINZ-SP-RECPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
                                         JUSTIFY-TYPE.


           MOVE SPACES        TO SP-RECPAYCD
                                 WS-AGEINZ-SP-RECPAYCD.

%%         MOVE SPACES        TO SP-CPOT30-CD
                                 WS-AGEINZ-SP-CPOT30-CD.

%%         MOVE SPACES        TO SP-CPOTCHGOFF-CD
                                 WS-AGEINZ-SP-CPOTCHGOFF-CD.

%%         MOVE SPACES        TO SP-RDPOT30-CD
                                 WS-AGEINZ-SP-RDPOT30-CD.

%%    *ACCOUNTS WILL NOT BE FLAGGED AS POTENTIAL CHARGEOFF
           MOVE SPACES        TO SP-RDPOTCHGOFF-CD
                                 WS-AGEINZ-SP-RDPOTCHGOFF-CD.

      *-----------------------------------------------------
           PERFORM UPDATE-AGEINZ-WORK-REC.            
      *-----------------------------------------------------

      *USES LPCEPP,LPPDUE  BANKRULE " ", CONTPDTH  50%
      *2)CONTRACTUAL SPACE = 30 AFTER 2 MONTHS FROM DUE DATE
      *  CONTRACTUAL FORMULA TOTAL MONIES
      *-------------------------------------------------
           PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WS-AGEINZ-REC WK-AGEINZ-REC.
           MOVE ZEROS TO AGEING-E-1STPYAMT
                         AGEING-E-REGPYAMT
                         AGEING-E-LASTPYAMT 
                         AGEING-WORK
                         PSUB.                         
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-AGEINZ-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-AGEINZ-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-AGEINZ-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-OWNBR, JUSTIFY-TYPE.

           MOVE TODAYS-DATE TO AGEING-DATE, DATE-YYYYMMDD.        
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-AGEING-DATE.           

           MOVE SP1-KEY TO WS-AGEINZ-SP1-KEY.

           MOVE SP-CONTR-CREDIT-CD TO WS-AGEINZ-SP-CONTR-CREDIT-CD.
           MOVE SP-CONTR-CREDIT TO WS-AGEINZ-SP-CONTR-CREDIT.          
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTR-CREDIT,
                                                 JUSTIFY-TYPE.

           MOVE LN-TOTPAYMNTD  TO WS-AGEINZ-LN-TOTPAYMNTD.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-TOTPAYMNTD,
                                             JUSTIFY-TYPE.
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-LN-DATE-PAID-LAST.

           MOVE LN-PY-YYYYMM-RECDEL  TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY TO WS-AGEINZ-LN-PY-MMYY-RECDEL.

           MOVE LN-MTD-PAYS-RECDEL  TO WS-AGEINZ-LN-MTD-PAYS-RECDEL.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-MTD-PAYS-RECDEL 
                                                 JUSTIFY-TYPE.

           MOVE LN-UNITPER-CD TO WS-AGEINZ-LN-UNITPER-CD.
      *-----------------------------------------------------
           MOVE SPACES        TO SP-BANKRULE
                                 WS-AGEINZ-SP-BANKRULE.
           MOVE "2 MO FROM DUE DATE" TO WS-AGEINZ-CONTRACTUAL-DESC.
           MOVE "A"                  TO SP-CONTRFRMLA
                                        WS-AGEINZ-SP-CONTRFRMLA.

           MOVE  50                  TO SP-CONTPDTH
                                        WS-AGEINZ-SP-CONTPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTPDTH,
                                            JUSTIFY-TYPE.

           MOVE SPACES TO SP-BANKUNITPER-CD
                          WS-AGEINZ-SP-BANKUNITPER-CD.

           MOVE "C"           TO SP-RECFRMLA
                                 WS-AGEINZ-SP-RECFRMLA.
           MOVE "CALENDAR"    TO WS-AGEINZ-RECENCY-DESC.

%%    *    MOVE 100           TO SP-RECPDTH
%%    *                          WS-AGEINZ-SP-RECPDTH.
%%    *    CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
%%    *                                  JUSTIFY-TYPE.
           MOVE SP-RECPDTH TO WS-AGEINZ-SP-RECPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
                                         JUSTIFY-TYPE.

           MOVE SPACES        TO SP-RECPAYCD
                                 WS-AGEINZ-SP-RECPAYCD.

%%         MOVE "A"           TO SP-CPOT30-CD
                                 WS-AGEINZ-SP-CPOT30-CD.
      *ALL 150, 180 + MISSED C/O
%%         MOVE "A"           TO SP-CPOTCHGOFF-CD
                                 WS-AGEINZ-SP-CPOTCHGOFF-CD.

%%         MOVE SPACES        TO SP-RDPOT30-CD
                                 WS-AGEINZ-SP-RDPOT30-CD.
      *ALL 150 DAYS RECENCY, MISSED ALL 180+ RECENCY
%%         MOVE "A"           TO SP-RDPOTCHGOFF-CD
                                 WS-AGEINZ-SP-RDPOTCHGOFF-CD.

      *-----------------------------------------------------
           PERFORM UPDATE-AGEINZ-WORK-REC.            
      *-----------------------------------------------------


      *USES LPCEPP,LPPDUE  BANKRULE " ", CONTPDTH SPACES  100%
      *3)CONTRACTUAL SPACE = 30 AFTER 2 MONTHS FROM DUE DATE
      *  CONTRACTUAL FORMULA = SPACES
      *-------------------------------------------------
           PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WS-AGEINZ-REC WK-AGEINZ-REC.
           MOVE ZEROS TO AGEING-E-1STPYAMT
                         AGEING-E-REGPYAMT
                         AGEING-E-LASTPYAMT 
                         AGEING-WORK
                         PSUB.                         
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-AGEINZ-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-AGEINZ-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-AGEINZ-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-OWNBR, JUSTIFY-TYPE.

           MOVE TODAYS-DATE TO AGEING-DATE, DATE-YYYYMMDD.        
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-AGEING-DATE.           

           MOVE SP1-KEY TO WS-AGEINZ-SP1-KEY.

           MOVE SP-CONTR-CREDIT-CD TO WS-AGEINZ-SP-CONTR-CREDIT-CD.
           MOVE SP-CONTR-CREDIT TO WS-AGEINZ-SP-CONTR-CREDIT.          
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTR-CREDIT,
                                                 JUSTIFY-TYPE.

           MOVE LN-TOTPAYMNTD  TO WS-AGEINZ-LN-TOTPAYMNTD.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-TOTPAYMNTD,
                                             JUSTIFY-TYPE.
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-LN-DATE-PAID-LAST.

           MOVE LN-PY-YYYYMM-RECDEL  TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY TO WS-AGEINZ-LN-PY-MMYY-RECDEL.

           MOVE LN-MTD-PAYS-RECDEL  TO WS-AGEINZ-LN-MTD-PAYS-RECDEL.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-MTD-PAYS-RECDEL 
                                                 JUSTIFY-TYPE.

           MOVE LN-UNITPER-CD TO WS-AGEINZ-LN-UNITPER-CD.
      *-----------------------------------------------------
           MOVE SPACES        TO SP-BANKRULE
                                 WS-AGEINZ-SP-BANKRULE.
           MOVE "2 MO FROM DUE DATE" TO WS-AGEINZ-CONTRACTUAL-DESC.
           MOVE SPACES TO SP-CONTRFRMLA, WS-AGEINZ-SP-CONTRFRMLA.

           MOVE  100                 TO SP-CONTPDTH
                                        WS-AGEINZ-SP-CONTPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTPDTH,
                                            JUSTIFY-TYPE.

           MOVE SPACES TO SP-BANKUNITPER-CD
                          WS-AGEINZ-SP-BANKUNITPER-CD.

           MOVE "C"           TO SP-RECFRMLA
                                 WS-AGEINZ-SP-RECFRMLA.
           MOVE "CALENDAR"    TO WS-AGEINZ-RECENCY-DESC.
%%    *    MOVE  75           TO SP-RECPDTH
%%    *                          WS-AGEINZ-SP-RECPDTH.
%%    *    CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
%%    *                                  JUSTIFY-TYPE.

           MOVE SP-RECPDTH TO WS-AGEINZ-SP-RECPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
                                         JUSTIFY-TYPE.
           MOVE SPACES        TO SP-RECPAYCD
                                 WS-AGEINZ-SP-RECPAYCD.

%%         MOVE "A"           TO SP-CPOT30-CD
                                 WS-AGEINZ-SP-CPOT30-CD.
      *ALL 180, MISSED 210+ C/O
%%         MOVE "B"           TO SP-CPOTCHGOFF-CD
                                 WS-AGEINZ-SP-CPOTCHGOFF-CD.

%%         MOVE SPACES        TO SP-RDPOT30-CD
                                 WS-AGEINZ-SP-RDPOT30-CD.
      *ALL 150 DAYS RECENCY, MISSED ALL 180+ RECENCY
%%         MOVE "A"           TO SP-RDPOTCHGOFF-CD
                                 WS-AGEINZ-SP-RDPOTCHGOFF-CD.

      *-----------------------------------------------------
           PERFORM UPDATE-AGEINZ-WORK-REC.            
      *-----------------------------------------------------

      *USES LPCEPP,LPPDUE  BANKRULE " ", CONTPDTH SPACES 50% 
      *4)CONTRACTUAL SPACE = 30 AFTER 2 MONTHS FROM DUE DATE
      *  CONTRACTUAL FORMULA = SPACES
      *-------------------------------------------------
           PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WS-AGEINZ-REC WK-AGEINZ-REC.
           MOVE ZEROS TO AGEING-E-1STPYAMT
                         AGEING-E-REGPYAMT
                         AGEING-E-LASTPYAMT 
                         AGEING-WORK
                         PSUB.                         
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-AGEINZ-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-AGEINZ-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-AGEINZ-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-OWNBR, JUSTIFY-TYPE.

           MOVE TODAYS-DATE TO AGEING-DATE, DATE-YYYYMMDD.        
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-AGEING-DATE.           

           MOVE SP1-KEY TO WS-AGEINZ-SP1-KEY.

           MOVE SP-CONTR-CREDIT-CD TO WS-AGEINZ-SP-CONTR-CREDIT-CD.
           MOVE SP-CONTR-CREDIT TO WS-AGEINZ-SP-CONTR-CREDIT.          
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTR-CREDIT,
                                                 JUSTIFY-TYPE.

           MOVE LN-TOTPAYMNTD  TO WS-AGEINZ-LN-TOTPAYMNTD.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-TOTPAYMNTD,
                                             JUSTIFY-TYPE.
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-LN-DATE-PAID-LAST.

           MOVE LN-PY-YYYYMM-RECDEL  TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY TO WS-AGEINZ-LN-PY-MMYY-RECDEL.

           MOVE LN-MTD-PAYS-RECDEL  TO WS-AGEINZ-LN-MTD-PAYS-RECDEL.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-MTD-PAYS-RECDEL 
                                                 JUSTIFY-TYPE.

           MOVE LN-UNITPER-CD TO WS-AGEINZ-LN-UNITPER-CD.
      *-----------------------------------------------------
           MOVE SPACES        TO SP-BANKRULE
                                 WS-AGEINZ-SP-BANKRULE.
           MOVE "2 MO FROM DUE DATE" TO WS-AGEINZ-CONTRACTUAL-DESC.
           MOVE SPACES TO SP-CONTRFRMLA, WS-AGEINZ-SP-CONTRFRMLA.

           MOVE SPACES        TO SP-CONTRFRMLA,
                                WS-AGEINZ-SP-CONTRFRMLA.

           MOVE  50                  TO SP-CONTPDTH
                                        WS-AGEINZ-SP-CONTPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTPDTH,
                                            JUSTIFY-TYPE.

           MOVE SPACES TO SP-BANKUNITPER-CD
                          WS-AGEINZ-SP-BANKUNITPER-CD.

           MOVE "C"           TO SP-RECFRMLA
                                 WS-AGEINZ-SP-RECFRMLA.
           MOVE "CALENDAR"    TO WS-AGEINZ-RECENCY-DESC.
%%    *    MOVE  50           TO SP-RECPDTH
%%    *                          WS-AGEINZ-SP-RECPDTH.
%%    *    CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
%%    *                                  JUSTIFY-TYPE.

           MOVE SP-RECPDTH TO WS-AGEINZ-SP-RECPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
                                         JUSTIFY-TYPE.
           MOVE SPACES        TO SP-RECPAYCD
                                 WS-AGEINZ-SP-RECPAYCD.

%%         MOVE "A"           TO SP-CPOT30-CD
                                 WS-AGEINZ-SP-CPOT30-CD.
      *ALL 120 DAYS                     
%%         MOVE "C"           TO SP-CPOTCHGOFF-CD
                                 WS-AGEINZ-SP-CPOTCHGOFF-CD.

           MOVE SPACES        TO SP-RDPOT30-CD
                                 WS-AGEINZ-SP-RDPOT30-CD.
      *ALL 150 DAYS RECENCY, MISSED ALL 180+ RECENCY
           MOVE " "           TO SP-RDPOTCHGOFF-CD
                                 WS-AGEINZ-SP-RDPOTCHGOFF-CD.

      *-----------------------------------------------------
           PERFORM UPDATE-AGEINZ-WORK-REC.            
      *-----------------------------------------------------

      *USES LPCEPP,LPPDUE  BANKRULE " ", CONTPDTH SPACES  75% 
      *5)CONTRACTUAL ' '   = 30 AFTER 2 MONTHS FROM DUE DATE
      *  CONTRACTUAL FORMULA = SPACES
      *  RECENCY FRMLA 'D'
      *-------------------------------------------------
           PERFORM RESTORE-ORIGINAL-FIELDS.


           MOVE SPACES TO WS-AGEINZ-REC WK-AGEINZ-REC.
           MOVE ZEROS TO AGEING-E-1STPYAMT
                         AGEING-E-REGPYAMT
                         AGEING-E-LASTPYAMT 
                         AGEING-WORK
                         PSUB.                         
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-AGEINZ-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-AGEINZ-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-AGEINZ-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-OWNBR, JUSTIFY-TYPE.

           MOVE TODAYS-DATE TO AGEING-DATE, DATE-YYYYMMDD.        
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-AGEING-DATE.           

           MOVE SP1-KEY TO WS-AGEINZ-SP1-KEY.

           MOVE SP-CONTR-CREDIT-CD TO WS-AGEINZ-SP-CONTR-CREDIT-CD.
           MOVE SP-CONTR-CREDIT TO WS-AGEINZ-SP-CONTR-CREDIT.          
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTR-CREDIT,
                                                 JUSTIFY-TYPE.

           MOVE LN-TOTPAYMNTD  TO WS-AGEINZ-LN-TOTPAYMNTD.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-TOTPAYMNTD,
                                             JUSTIFY-TYPE.
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-LN-DATE-PAID-LAST.

           MOVE LN-PY-YYYYMM-RECDEL  TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY TO WS-AGEINZ-LN-PY-MMYY-RECDEL.

           MOVE LN-MTD-PAYS-RECDEL  TO WS-AGEINZ-LN-MTD-PAYS-RECDEL.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-MTD-PAYS-RECDEL 
                                                 JUSTIFY-TYPE.
           MOVE LN-UNITPER-CD TO WS-AGEINZ-LN-UNITPER-CD.
      *-----------------------------------------------------
           MOVE SPACES        TO SP-BANKRULE
                                 WS-AGEINZ-SP-BANKRULE.
           MOVE "2 MO FROM DUE DATE" TO WS-AGEINZ-CONTRACTUAL-DESC.
           MOVE "A"    TO SP-CONTRFRMLA, WS-AGEINZ-SP-CONTRFRMLA.

           MOVE  100                 TO SP-CONTPDTH
                                        WS-AGEINZ-SP-CONTPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTPDTH,
                                            JUSTIFY-TYPE.

           MOVE SPACES TO SP-BANKUNITPER-CD
                          WS-AGEINZ-SP-BANKUNITPER-CD.

           MOVE "D"           TO SP-RECFRMLA
                                 WS-AGEINZ-SP-RECFRMLA.
           MOVE "DUE DAY "    TO WS-AGEINZ-RECENCY-DESC.
  
      *DOESNT APPEAR TO BE USED
      *    MOVE 100           TO SP-RECPDTH
      *                          WS-AGEINZ-SP-RECPDTH.
      *    CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
      *                                  JUSTIFY-TYPE.

           MOVE SP-RECPDTH TO WS-AGEINZ-SP-RECPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
                                         JUSTIFY-TYPE.
           MOVE SPACES        TO SP-RECPAYCD
                                 WS-AGEINZ-SP-RECPAYCD.
%%         MOVE "A"           TO SP-CPOT30-CD
                                 WS-AGEINZ-SP-CPOT30-CD.
      *ALL 120 DAYS                     
%%         MOVE "C"           TO SP-CPOTCHGOFF-CD
                                 WS-AGEINZ-SP-CPOTCHGOFF-CD.

%%         MOVE SPACES        TO SP-RDPOT30-CD
                                 WS-AGEINZ-SP-RDPOT30-CD.
      *ALL 150 DAYS RECENCY, MISSED ALL 180+ RECENCY
%%         MOVE "A"           TO SP-RDPOTCHGOFF-CD
                                 WS-AGEINZ-SP-RDPOTCHGOFF-CD.


      *-----------------------------------------------------
           PERFORM UPDATE-AGEINZ-WORK-REC.            
      *-----------------------------------------------------

      *USES LPCEPP,LPPDUE  BANKRULE " ", CONTPDTH SPACES  50% 
      *6)CONTRACTUAL 'A'   = 30 AFTER 2 MONTHS FROM DUE DATE
      *  CONTRACTUAL FORMULA = SPACES
      *  RECENCY FRMLA 'P'   50%
      *-------------------------------------------------
      *%%
           PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WS-AGEINZ-REC WK-AGEINZ-REC.
           MOVE ZEROS TO AGEING-E-1STPYAMT
                         AGEING-E-REGPYAMT
                         AGEING-E-LASTPYAMT 
                         AGEING-WORK
                         PSUB.                         
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-AGEINZ-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-AGEINZ-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-AGEINZ-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-OWNBR, JUSTIFY-TYPE.

           MOVE TODAYS-DATE TO AGEING-DATE, DATE-YYYYMMDD.        
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-AGEING-DATE.           

           MOVE SP1-KEY TO WS-AGEINZ-SP1-KEY.

           MOVE SP-CONTR-CREDIT-CD TO WS-AGEINZ-SP-CONTR-CREDIT-CD.
           MOVE SP-CONTR-CREDIT TO WS-AGEINZ-SP-CONTR-CREDIT.          
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTR-CREDIT,
                                                 JUSTIFY-TYPE.

           MOVE LN-TOTPAYMNTD  TO WS-AGEINZ-LN-TOTPAYMNTD.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-TOTPAYMNTD,
                                             JUSTIFY-TYPE.
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-LN-DATE-PAID-LAST.

           MOVE LN-PY-YYYYMM-RECDEL  TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY TO WS-AGEINZ-LN-PY-MMYY-RECDEL.

           MOVE LN-MTD-PAYS-RECDEL  TO WS-AGEINZ-LN-MTD-PAYS-RECDEL.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-MTD-PAYS-RECDEL 
                                                 JUSTIFY-TYPE.
           MOVE LN-UNITPER-CD TO WS-AGEINZ-LN-UNITPER-CD.
      *-----------------------------------------------------
           MOVE SPACES        TO SP-BANKRULE
                                 WS-AGEINZ-SP-BANKRULE.
           MOVE "2 MO FROM DUE DATE" TO WS-AGEINZ-CONTRACTUAL-DESC.
           MOVE "A"    TO SP-CONTRFRMLA, WS-AGEINZ-SP-CONTRFRMLA.

           MOVE   50                 TO SP-CONTPDTH
                                        WS-AGEINZ-SP-CONTPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTPDTH,
                                            JUSTIFY-TYPE.

           MOVE SPACES TO SP-BANKUNITPER-CD
                          WS-AGEINZ-SP-BANKUNITPER-CD.

           MOVE "P"           TO SP-RECFRMLA
                                 WS-AGEINZ-SP-RECFRMLA.
           MOVE "PERCENT "    TO WS-AGEINZ-RECENCY-DESC.
           MOVE  50           TO SP-RECPDTH
                                 WS-AGEINZ-SP-RECPDTH.

           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
                                         JUSTIFY-TYPE.

           MOVE SPACES        TO SP-RECPAYCD
                                 WS-AGEINZ-SP-RECPAYCD.

%%         MOVE "A"           TO SP-CPOT30-CD
                                 WS-AGEINZ-SP-CPOT30-CD.
      *ALL 120 DAYS                     
%%         MOVE "B"           TO SP-CPOTCHGOFF-CD
                                 WS-AGEINZ-SP-CPOTCHGOFF-CD.

%%         MOVE SPACES        TO SP-RDPOT30-CD
                                 WS-AGEINZ-SP-RDPOT30-CD.
       
%%         MOVE "A"           TO SP-RDPOTCHGOFF-CD
                                 WS-AGEINZ-SP-RDPOTCHGOFF-CD.

      *-----------------------------------------------------
           PERFORM UPDATE-AGEINZ-WORK-REC.            
      *-----------------------------------------------------
    
      *7) TEST OUT THE DIFFERENT SP-BANKUNITPER-CD 'A' NON MONTHLY
      *CONTRFRMLA = 'A', ALL ACCTS CREDIT FOR PARTIAL PAYMENTS OF 50%
      *OR MORE, BANK UNITPERCD 'A'
           PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WS-AGEINZ-REC WK-AGEINZ-REC.
           MOVE ZEROS TO AGEING-E-1STPYAMT
                         AGEING-E-REGPYAMT
                         AGEING-E-LASTPYAMT 
                         AGEING-WORK
                         PSUB.                         
 
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-AGEINZ-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-AGEINZ-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-AGEINZ-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-OWNBR, JUSTIFY-TYPE.

           MOVE TODAYS-DATE TO AGEING-DATE, DATE-YYYYMMDD.        
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-AGEING-DATE.           

           MOVE SP1-KEY TO WS-AGEINZ-SP1-KEY.
           MOVE SP-CONTR-CREDIT-CD TO WS-AGEINZ-SP-CONTR-CREDIT-CD.
           MOVE SP-CONTR-CREDIT TO WS-AGEINZ-SP-CONTR-CREDIT.          
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTR-CREDIT,
                                      JUSTIFY-TYPE.

           MOVE LN-TOTPAYMNTD  TO WS-AGEINZ-LN-TOTPAYMNTD.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-TOTPAYMNTD,
                                             JUSTIFY-TYPE.
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-LN-DATE-PAID-LAST.

           MOVE LN-PY-YYYYMM-RECDEL  TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY TO WS-AGEINZ-LN-PY-MMYY-RECDEL.

           MOVE LN-MTD-PAYS-RECDEL  TO WS-AGEINZ-LN-MTD-PAYS-RECDEL.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-MTD-PAYS-RECDEL 
                                                 JUSTIFY-TYPE.

           MOVE LN-UNITPER-CD TO WS-AGEINZ-LN-UNITPER-CD.
      *-----------------------------------------------------------
           MOVE SPACES TO SP-BANKRULE  WS-AGEINZ-SP-BANKRULE.
           MOVE "2 MO FROM DUE DATE" TO WS-AGEINZ-CONTRACTUAL-DESC.                   
           MOVE "A"        TO  SP-CONTRFRMLA
                               WS-AGEINZ-SP-CONTRFRMLA.

           MOVE  100          TO  SP-CONTPDTH 
                                 WS-AGEINZ-SP-CONTPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTPDTH,
                                              JUSTIFY-TYPE.
         
           MOVE "A" TO SP-BANKUNITPER-CD WS-AGEINZ-SP-BANKUNITPER-CD.

           MOVE "C" TO SP-RECFRMLA WS-AGEINZ-SP-RECFRMLA.
           MOVE "CALENDAR"        TO WS-AGEINZ-RECENCY-DESC.
    
%%    *    MOVE 100 TO SP-RECPDTH
%%    *                WS-AGEINZ-SP-RECPDTH.
%%    *    CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
%%    *                              JUSTIFY-TYPE.
       

           MOVE SP-RECPDTH TO WS-AGEINZ-SP-RECPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
                                         JUSTIFY-TYPE.
%%         MOVE "A"           TO SP-CPOT30-CD
                                 WS-AGEINZ-SP-CPOT30-CD.
      *ALL 120 DAYS                     
%%         MOVE "B"           TO SP-CPOTCHGOFF-CD
                                 WS-AGEINZ-SP-CPOTCHGOFF-CD.

%%         MOVE SPACES        TO SP-RDPOT30-CD
                                 WS-AGEINZ-SP-RDPOT30-CD.
       
%%         MOVE "A"           TO SP-RDPOTCHGOFF-CD
                                 WS-AGEINZ-SP-RDPOTCHGOFF-CD.

      *---------------------------------------------------------
           PERFORM UPDATE-AGEINZ-WORK-REC.            
      *---------------------------------------------------------


      *8) TEST OUT THE DIFFERENT SP-BANKUNITPER-CD 'A' NON MONTHLY
      *CONTRFRMLA = 'A', ALL ACCTS CREDIT FOR PARTIAL PAYMENTS OF 50%
      *OR MORE, BANK UNITPERCD 'B'
           PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WS-AGEINZ-REC WK-AGEINZ-REC.
           MOVE ZEROS TO AGEING-E-1STPYAMT
                         AGEING-E-REGPYAMT
                         AGEING-E-LASTPYAMT 
                         AGEING-WORK
                         PSUB.                         
 
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-AGEINZ-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-AGEINZ-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-AGEINZ-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-OWNBR, JUSTIFY-TYPE.

           MOVE TODAYS-DATE TO AGEING-DATE, DATE-YYYYMMDD.        
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-AGEING-DATE.           

           MOVE SP1-KEY TO WS-AGEINZ-SP1-KEY.
           MOVE SP-CONTR-CREDIT-CD TO WS-AGEINZ-SP-CONTR-CREDIT-CD.
           MOVE SP-CONTR-CREDIT TO WS-AGEINZ-SP-CONTR-CREDIT.          
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTR-CREDIT,
                                      JUSTIFY-TYPE.

           MOVE LN-TOTPAYMNTD  TO WS-AGEINZ-LN-TOTPAYMNTD.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-TOTPAYMNTD,
                                             JUSTIFY-TYPE.
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-LN-DATE-PAID-LAST.

           MOVE LN-PY-YYYYMM-RECDEL  TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY TO WS-AGEINZ-LN-PY-MMYY-RECDEL.

           MOVE LN-MTD-PAYS-RECDEL  TO WS-AGEINZ-LN-MTD-PAYS-RECDEL.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-MTD-PAYS-RECDEL 
                                                 JUSTIFY-TYPE.

           MOVE LN-UNITPER-CD TO WS-AGEINZ-LN-UNITPER-CD.
      *-----------------------------------------------------------
           MOVE SPACES TO SP-BANKRULE  WS-AGEINZ-SP-BANKRULE.
           MOVE "2 MO FROM DUE DATE" TO WS-AGEINZ-CONTRACTUAL-DESC.                   
           MOVE "A"        TO  SP-CONTRFRMLA
                               WS-AGEINZ-SP-CONTRFRMLA.

           MOVE  50           TO  SP-CONTPDTH 
                                 WS-AGEINZ-SP-CONTPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTPDTH,
                                              JUSTIFY-TYPE.
         
           MOVE "B" TO SP-BANKUNITPER-CD WS-AGEINZ-SP-BANKUNITPER-CD.

           MOVE "C" TO SP-RECFRMLA WS-AGEINZ-SP-RECFRMLA.
           MOVE "CALENDAR"        TO WS-AGEINZ-RECENCY-DESC.
    
%%    *    MOVE  75 TO SP-RECPDTH
%%    *                WS-AGEINZ-SP-RECPDTH.
%%    *    CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
%%    *                              JUSTIFY-TYPE.

           MOVE SP-RECPDTH TO WS-AGEINZ-SP-RECPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
                                         JUSTIFY-TYPE.
%%         MOVE "A"           TO SP-CPOT30-CD
                                 WS-AGEINZ-SP-CPOT30-CD.
      *ALL 120 DAYS                     
%%         MOVE "B"           TO SP-CPOTCHGOFF-CD
                                 WS-AGEINZ-SP-CPOTCHGOFF-CD.

%%         MOVE SPACES        TO SP-RDPOT30-CD
                                 WS-AGEINZ-SP-RDPOT30-CD.
       
%%         MOVE "A"           TO SP-RDPOTCHGOFF-CD
                                 WS-AGEINZ-SP-RDPOTCHGOFF-CD.

      *---------------------------------------------------------
           PERFORM UPDATE-AGEINZ-WORK-REC.            
      *---------------------------------------------------------


      *9) TEST OUT THE DIFFERENT SP-BANKUNITPER-CD 'A' NON MONTHLY
      *CONTRFRMLA = 'A', ALL ACCTS CREDIT FOR PARTIAL PAYMENTS OF 50%
      *OR MORE, BANK UNITPERCD 'C'
           PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WS-AGEINZ-REC WK-AGEINZ-REC.
           MOVE ZEROS TO AGEING-E-1STPYAMT
                         AGEING-E-REGPYAMT
                         AGEING-E-LASTPYAMT 
                         AGEING-WORK
                         PSUB.                         
 
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-AGEINZ-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-AGEINZ-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-AGEINZ-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-OWNBR, JUSTIFY-TYPE.

           MOVE TODAYS-DATE TO AGEING-DATE, DATE-YYYYMMDD.        
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-AGEING-DATE.           

           MOVE SP1-KEY TO WS-AGEINZ-SP1-KEY.
           MOVE SP-CONTR-CREDIT-CD TO WS-AGEINZ-SP-CONTR-CREDIT-CD.
           MOVE SP-CONTR-CREDIT TO WS-AGEINZ-SP-CONTR-CREDIT.          
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTR-CREDIT,
                                      JUSTIFY-TYPE.

           MOVE LN-TOTPAYMNTD  TO WS-AGEINZ-LN-TOTPAYMNTD.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-TOTPAYMNTD,
                                             JUSTIFY-TYPE.
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-LN-DATE-PAID-LAST.

           MOVE LN-PY-YYYYMM-RECDEL  TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY TO WS-AGEINZ-LN-PY-MMYY-RECDEL.

           MOVE LN-MTD-PAYS-RECDEL  TO WS-AGEINZ-LN-MTD-PAYS-RECDEL.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-MTD-PAYS-RECDEL 
                                                 JUSTIFY-TYPE.

           MOVE LN-UNITPER-CD TO WS-AGEINZ-LN-UNITPER-CD.
      *-----------------------------------------------------------
           MOVE SPACES TO SP-BANKRULE  WS-AGEINZ-SP-BANKRULE.
           MOVE "2 MO FROM DUE DATE" TO WS-AGEINZ-CONTRACTUAL-DESC.                   
           MOVE "A"        TO  SP-CONTRFRMLA
                               WS-AGEINZ-SP-CONTRFRMLA.

           MOVE 100           TO  SP-CONTPDTH 
                                 WS-AGEINZ-SP-CONTPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTPDTH,
                                              JUSTIFY-TYPE.
         
           MOVE "C" TO SP-BANKUNITPER-CD WS-AGEINZ-SP-BANKUNITPER-CD.

           MOVE "C" TO SP-RECFRMLA WS-AGEINZ-SP-RECFRMLA.
           MOVE "CALENDAR"        TO WS-AGEINZ-RECENCY-DESC.
    
%%    *    MOVE 100 TO SP-RECPDTH
%%    *                WS-AGEINZ-SP-RECPDTH.
%%    *    CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
%%    *                              JUSTIFY-TYPE.

           MOVE SP-RECPDTH TO WS-AGEINZ-SP-RECPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
                                         JUSTIFY-TYPE.
%%         MOVE "A"           TO SP-CPOT30-CD
                                 WS-AGEINZ-SP-CPOT30-CD.
      *ALL 120 DAYS                     
%%         MOVE "B"           TO SP-CPOTCHGOFF-CD
                                 WS-AGEINZ-SP-CPOTCHGOFF-CD.

%%         MOVE SPACES        TO SP-RDPOT30-CD
                                 WS-AGEINZ-SP-RDPOT30-CD.
       
%%         MOVE "A"           TO SP-RDPOTCHGOFF-CD
                                 WS-AGEINZ-SP-RDPOTCHGOFF-CD.

      *---------------------------------------------------------
           PERFORM UPDATE-AGEINZ-WORK-REC.            
      *---------------------------------------------------------


      *10) TEST OUT THE DIFFERENT SP-BANKUNITPER-CD 'A' NON MONTHLY
      *CONTRFRMLA = 'A', ALL ACCTS CREDIT FOR PARTIAL PAYMENTS OF 50%
      *OR MORE, BANK UNITPERCD 'D'
           PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WS-AGEINZ-REC WK-AGEINZ-REC.
           MOVE ZEROS TO AGEING-E-1STPYAMT
                         AGEING-E-REGPYAMT
                         AGEING-E-LASTPYAMT 
                         AGEING-WORK
                         PSUB.                         
 
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-AGEINZ-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-AGEINZ-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-AGEINZ-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-OWNBR, JUSTIFY-TYPE.

           MOVE TODAYS-DATE TO AGEING-DATE, DATE-YYYYMMDD.        
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-AGEING-DATE.           

           MOVE SP1-KEY TO WS-AGEINZ-SP1-KEY.
           MOVE SP-CONTR-CREDIT-CD TO WS-AGEINZ-SP-CONTR-CREDIT-CD.
           MOVE SP-CONTR-CREDIT TO WS-AGEINZ-SP-CONTR-CREDIT.          
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTR-CREDIT,
                                      JUSTIFY-TYPE.

           MOVE LN-TOTPAYMNTD  TO WS-AGEINZ-LN-TOTPAYMNTD.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-TOTPAYMNTD,
                                             JUSTIFY-TYPE.
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-LN-DATE-PAID-LAST.

           MOVE LN-PY-YYYYMM-RECDEL  TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY TO WS-AGEINZ-LN-PY-MMYY-RECDEL.

           MOVE LN-MTD-PAYS-RECDEL  TO WS-AGEINZ-LN-MTD-PAYS-RECDEL.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-MTD-PAYS-RECDEL 
                                                 JUSTIFY-TYPE.

           MOVE LN-UNITPER-CD TO WS-AGEINZ-LN-UNITPER-CD.
      *-----------------------------------------------------------
           MOVE SPACES TO SP-BANKRULE  WS-AGEINZ-SP-BANKRULE.
           MOVE "2 MO FROM DUE DATE" TO WS-AGEINZ-CONTRACTUAL-DESC.                   
           MOVE "A"        TO  SP-CONTRFRMLA
                               WS-AGEINZ-SP-CONTRFRMLA.

           MOVE  75           TO  SP-CONTPDTH 
                                 WS-AGEINZ-SP-CONTPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTPDTH,
                                              JUSTIFY-TYPE.
         
           MOVE "D" TO SP-BANKUNITPER-CD WS-AGEINZ-SP-BANKUNITPER-CD.

           MOVE "P" TO SP-RECFRMLA WS-AGEINZ-SP-RECFRMLA.
           MOVE "CALENDAR"        TO WS-AGEINZ-RECENCY-DESC.
    
           MOVE  50 TO SP-RECPDTH
                       WS-AGEINZ-SP-RECPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
                                     JUSTIFY-TYPE.
%%         MOVE "A"           TO SP-CPOT30-CD
                                 WS-AGEINZ-SP-CPOT30-CD.
      *ALL 120 DAYS                     
%%         MOVE "B"           TO SP-CPOTCHGOFF-CD
                                 WS-AGEINZ-SP-CPOTCHGOFF-CD.

%%         MOVE SPACES        TO SP-RDPOT30-CD
                                 WS-AGEINZ-SP-RDPOT30-CD.
       
%%         MOVE "A"           TO SP-RDPOTCHGOFF-CD
                                 WS-AGEINZ-SP-RDPOTCHGOFF-CD.

      *---------------------------------------------------------
           PERFORM UPDATE-AGEINZ-WORK-REC.            
      *---------------------------------------------------------

      *11) TEST OUT THE DIFFERENT SP-BANKUNITPER-CD 'A' NON MONTHLY
      *CONTRFRMLA = 'A', ALL ACCTS CREDIT FOR PARTIAL PAYMENTS OF 50%
      *OR MORE, BANK UNITPERCD 'E'
           PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WS-AGEINZ-REC WK-AGEINZ-REC.
           MOVE ZEROS TO AGEING-E-1STPYAMT
                         AGEING-E-REGPYAMT
                         AGEING-E-LASTPYAMT 
                         AGEING-WORK
                         PSUB.                         
 
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-AGEINZ-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-AGEINZ-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-AGEINZ-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-OWNBR, JUSTIFY-TYPE.

           MOVE TODAYS-DATE TO AGEING-DATE, DATE-YYYYMMDD.        
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-AGEING-DATE.           

           MOVE SP1-KEY TO WS-AGEINZ-SP1-KEY.
           MOVE SP-CONTR-CREDIT-CD TO WS-AGEINZ-SP-CONTR-CREDIT-CD.
           MOVE SP-CONTR-CREDIT TO WS-AGEINZ-SP-CONTR-CREDIT.          
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTR-CREDIT,
                                      JUSTIFY-TYPE.

           MOVE LN-TOTPAYMNTD  TO WS-AGEINZ-LN-TOTPAYMNTD.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-TOTPAYMNTD,
                                             JUSTIFY-TYPE.
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-LN-DATE-PAID-LAST.

           MOVE LN-PY-YYYYMM-RECDEL  TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY TO WS-AGEINZ-LN-PY-MMYY-RECDEL.

           MOVE LN-MTD-PAYS-RECDEL  TO WS-AGEINZ-LN-MTD-PAYS-RECDEL.
           CALL "C$JUSTIFY" USING WS-AGEINZ-LN-MTD-PAYS-RECDEL 
                                                 JUSTIFY-TYPE.

           MOVE LN-UNITPER-CD TO WS-AGEINZ-LN-UNITPER-CD.
%%         MOVE "A"           TO SP-CPOT30-CD
                                 WS-AGEINZ-SP-CPOT30-CD.
      *ALL 120 DAYS                     
%%         MOVE "B"           TO SP-CPOTCHGOFF-CD
                                 WS-AGEINZ-SP-CPOTCHGOFF-CD.

%%         MOVE SPACES        TO SP-RDPOT30-CD
                                 WS-AGEINZ-SP-RDPOT30-CD.
       
%%         MOVE "A"           TO SP-RDPOTCHGOFF-CD
                                 WS-AGEINZ-SP-RDPOTCHGOFF-CD.
      *-----------------------------------------------------------
           MOVE SPACES TO SP-BANKRULE  WS-AGEINZ-SP-BANKRULE.
           MOVE "2 MO FROM DUE DATE" TO WS-AGEINZ-CONTRACTUAL-DESC.                   
           MOVE "A"        TO  SP-CONTRFRMLA
                               WS-AGEINZ-SP-CONTRFRMLA.

           MOVE  75           TO  SP-CONTPDTH 
                                 WS-AGEINZ-SP-CONTPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-CONTPDTH,
                                              JUSTIFY-TYPE.
         
           MOVE "E" TO SP-BANKUNITPER-CD WS-AGEINZ-SP-BANKUNITPER-CD.

           MOVE "C" TO SP-RECFRMLA WS-AGEINZ-SP-RECFRMLA.
           MOVE "CALENDAR"        TO WS-AGEINZ-RECENCY-DESC.
    
%%    *    MOVE  75 TO SP-RECPDTH
%%    *                WS-AGEINZ-SP-RECPDTH.
%%    *    CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
%%    *                              JUSTIFY-TYPE.

           MOVE SP-RECPDTH TO WS-AGEINZ-SP-RECPDTH.
           CALL "C$JUSTIFY" USING WS-AGEINZ-SP-RECPDTH,
                                         JUSTIFY-TYPE.
      *---------------------------------------------------------
           PERFORM UPDATE-AGEINZ-WORK-REC.            
      *---------------------------------------------------------


       UPDATE-AGEINZ-WORK-REC SECTION.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.
           PERFORM COMPUTE-PAID-AHEAD.     

           MOVE PDTH-PAYS TO WS-AGEINZ-PDTH-PAYS.
           CALL "C$JUSTIFY" USING WS-AGEINZ-PDTH-PAYS,
                                      JUSTIFY-TYPE.

           MOVE PDTH-REMAIN TO WS-AGEINZ-PDTH-REMAIN.

           MOVE AGEING-WORK       TO WS-AGEINZ-WORK.    
           CALL "C$JUSTIFY" USING WS-AGEINZ-WORK,JUSTIFY-TYPE.    

           MOVE PDTH-DATE-FULL TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEINZ-PDTH-DATE-FULL.

      *CONTRACTUAL
           MOVE CSUB     TO WS-AGEINZ-CSUB.
           CALL "C$JUSTIFY" USING WS-AGEINZ-CSUB,
                                              JUSTIFY-TYPE.

           MOVE CPOT30      TO WS-AGEINZ-CPOT30.
           MOVE CPOTCHGOFF TO WS-AGEINZ-CPOTCHGOFF.
            
           MOVE CSUB-ALDEL-FG  TO WS-AGEINZ-CSUB-ALDEL-FG.

           MOVE CONTRACTUAL TO WS-AGEINZ-CONTRACTUAL.
           CALL "C$JUSTIFY" USING WS-AGEINZ-CONTRACTUAL,JUSTIFY-TYPE.

           MOVE PSUB TO WS-AGEINZ-PSUB.
           CALL "C$JUSTIFY" USING WS-AGEINZ-PSUB,JUSTIFY-TYPE.

      *RECENCY
           MOVE RDSUB   TO WS-AGEINZ-RDSUB.
           CALL "C$JUSTIFY" USING WS-AGEINZ-RDSUB,JUSTIFY-TYPE.
 
           MOVE RSUB TO WS-AGEINZ-RSUB.
           CALL "C$JUSTIFY" USING WS-AGEINZ-RSUB,JUSTIFY-TYPE.
       

           MOVE RDPOT30  TO   WS-AGEINZ-RDPOT30.

           MOVE RDPOTCHGOFF TO   WS-AGEINZ-RDPOTCHGOFF.
      *TRUE RECENCY
           MOVE RECENCY TO WS-AGEINZ-RECENCY.
           CALL "C$JUSTIFY" USING WS-AGEINZ-RECENCY,JUSTIFY-TYPE.

      *THIS IS RECENCY 30 60 ...
           MOVE RECDEL TO WS-AGEINZ-RECDEL.
           CALL "C$JUSTIFY" USING WS-AGEINZ-RECDEL,
                                              JUSTIFY-TYPE.
           MOVE CSUB     TO WS-AGEINZ-CSUB.
           CALL "C$JUSTIFY" USING WS-AGEINZ-CSUB,
                                              JUSTIFY-TYPE.

           MOVE AGEING-E-1STPYAMT  TO WS-AGEINZ-E-1STPYAMT.
           CALL "C$JUSTIFY" USING WS-AGEINZ-E-1STPYAMT,JUSTIFY-TYPE.

           MOVE AGEING-E-REGPYAMT  TO WS-AGEINZ-E-REGPYAMT.
           CALL "C$JUSTIFY" USING WS-AGEINZ-E-REGPYAMT,JUSTIFY-TYPE.
          
           MOVE AGEING-E-LASTPYAMT TO WS-AGEINZ-E-LASTPYAMT.
           CALL "C$JUSTIFY" USING WS-AGEINZ-E-LASTPYAMT,JUSTIFY-TYPE.

  
       SETUP-WS-AGEINZ-REC.
               STRING
               WS-AGEINZ-LN-BRACCT             ,"|"
               WS-AGEINZ-LN-ACCTNO             ,"|"
               WS-AGEINZ-LN-OWNBR              ,"|"
               WS-AGEINZ-AGEING-DATE           ,"|"
               WS-AGEINZ-SP1-KEY               ,"|"
               WS-AGEINZ-E-1STPYAMT           ,"|"
               WS-AGEINZ-E-REGPYAMT           ,"|"
               WS-AGEINZ-E-LASTPYAMT          ,"|"
               WS-AGEINZ-LN-TOTPAYMNTD         ,"|"
               WS-AGEINZ-LN-DATE-PAID-LAST     ,"|" 
               WS-AGEINZ-PDTH-PAYS             ,"|"
               WS-AGEINZ-PDTH-REMAIN           ,"|"
               WS-AGEINZ-PDTH-DATE-FULL        ,"|"
               WS-AGEINZ-LN-PY-MMYY-RECDEL     ,"|"
               WS-AGEINZ-LN-MTD-PAYS-RECDEL    ,"|" 
               WS-AGEINZ-LN-UNITPER-CD         ,"|"
               WS-AGEINZ-SP-BANKRULE           ,"|"
               WS-AGEINZ-SP-CONTRFRMLA         ,"|"
               WS-AGEINZ-CONTRACTUAL-DESC      ,"|"
               WS-AGEINZ-SP-CONTPDTH           ,"|"           
               WS-AGEINZ-SP-BANKUNITPER-CD     ,"|"
               WS-AGEINZ-SP-CONTR-CREDIT-CD    ,"|"
               WS-AGEINZ-SP-CONTR-CREDIT       ,"|"
               WS-AGEINZ-SP-CPOT30-CD          ,"|"
               WS-AGEINZ-CPOT30                ,"|"
               WS-AGEINZ-SP-CPOTCHGOFF-CD      ,"|"
               WS-AGEINZ-CPOTCHGOFF            ,"|"
               WS-AGEINZ-CSUB-ALDEL-FG         ,"|"
               WS-AGEINZ-WORK                  ,"|"
               WS-AGEINZ-CSUB                  ,"|"
               WS-AGEINZ-CONTRACTUAL           ,"|"
               WS-AGEINZ-SP-RECFRMLA           ,"|"
               WS-AGEINZ-RECENCY-DESC          ,"|"
               WS-AGEINZ-SP-RECPDTH           ,"|"
               WS-AGEINZ-SP-RECPAYCD           ,"|"
               WS-AGEINZ-RDSUB                 ,"|"
               WS-AGEINZ-RSUB                  ,"|"
               WS-AGEINZ-RDPOT30               ,"|"
               WS-AGEINZ-RDPOTCHGOFF           ,"|"
               WS-AGEINZ-RECENCY               ,"|"                 
               WS-AGEINZ-RECDEL                ,"|"
               WS-AGEINZ-PSUB,CR-1                        
                  DELIMITED BY "  " INTO WK-AGEINZ-REC.
           PERFORM WRITE-WK-AGEINZ-FILE.
           IF IO-FG NOT = 0
               GO TO IO-ERROR.

      *=============================================================
       SET-COMMON SECTION.
           MOVE 0 TO WS-OWNBR WS-NUMBER WS-LN-ACCTNO WS-LN-OWNBR.
           MOVE LN-OWNBR   TO WS-OWNBR.
           MOVE LN-ACCTNO  TO WS-NUMBER.
           
           MOVE LN-ACCTNO  TO WS-LN-ACCTNO.
           MOVE LN-OWNBR   TO WS-LN-OWNBR.

      *=============================================================
       SET-ORIGINAL-HOLD-FIELDS SECTION.
           MOVE SPACES TO ORIGINAL-FIELDS.
           MOVE ZEROS TO ORIGINAL-LN-TOTNOALDEL 
                         ORIGINAL-LN-TOTNODEF
                         ORIGINAL-SP-CONTPDTH
                         ORIGINAL-SP-RECPDTH
                         ORIGINAL-SP-CONTR-CREDIT
                         ORIGINAL-SP-CONTPDTH 
                         ORIGINAL-SP-RECPDTH.

       SAVE-ORIGINAL-FIELDS SECTION.
           MOVE GP-PDTH-GT-SCHED-PAY TO
                              ORIGINAL-GP-PDTH-GT-SCHED-PAY.
           MOVE LN-TOTNOALDEL   TO ORIGINAL-LN-TOTNOALDEL.
           MOVE LN-TOTNODEF     TO ORIGINAL-LN-TOTNODEF.
           MOVE SP-BANKRULE     TO ORIGINAL-SP-BANKRULE.
           MOVE SP-CONTR-CREDIT-CD TO ORIGINAL-SP-CONTR-CREDIT-CD.
           MOVE SP-CONTR-CREDIT TO ORIGINAL-SP-CONTR-CREDIT.     
           MOVE SP-BANKUNITPER-CD TO ORIGINAL-SP-BANKUNITPER-CD.
           MOVE SP-CONTRFRMLA   TO   ORIGINAL-SP-CONTRFRMLA.
           MOVE SP-CONTPDTH     TO ORIGINAL-SP-CONTPDTH.
%%         MOVE SP-RECFRMLA     TO ORIGINAL-SP-RECFRMLA.
%%         MOVE SP-RECPDTH      TO ORIGINAL-SP-RECPDTH.
%%         MOVE SP-RECPAYCD     TO ORIGINAL-SP-RECPAYCD.
%%         MOVE SP-CPOT30-CD    TO ORIGINAL-SP-CPOT30-CD
%%         MOVE SP-CPOTCHGOFF-CD TO ORIGINAL-SP-CPOTCHGOFF-CD.
%%         MOVE SP-RDPOT30-CD   TO ORIGINAL-SP-RDPOT30-CD.
%%         MOVE SP-RDPOTCHGOFF-CD TO ORIGINAL-SP-RDPOTCHGOFF-CD.

       RESTORE-ORIGINAL-FIELDS SECTION.
           MOVE ORIGINAL-GP-PDTH-GT-SCHED-PAY TO
                              GP-PDTH-GT-SCHED-PAY.
           MOVE ORIGINAL-LN-TOTNODEF       TO LN-TOTNODEF.
           MOVE ORIGINAL-LN-TOTNOALDEL     TO LN-TOTNOALDEL.
           MOVE ORIGINAL-SP-BANKRULE       TO SP-BANKRULE.
           MOVE ORIGINAL-SP-CONTR-CREDIT-CD TO SP-CONTR-CREDIT-CD.
           MOVE ORIGINAL-SP-CONTR-CREDIT TO SP-CONTR-CREDIT.

           MOVE ORIGINAL-SP-BANKUNITPER-CD TO SP-BANKUNITPER-CD.
           MOVE ORIGINAL-SP-CONTRFRMLA     TO SP-CONTRFRMLA.
           MOVE ORIGINAL-SP-CONTPDTH       TO SP-CONTPDTH.
%%         MOVE ORIGINAL-SP-RECFRMLA       TO SP-RECFRMLA.
%%         MOVE ORIGINAL-SP-RECPDTH        TO SP-RECPDTH.
%%         MOVE ORIGINAL-SP-RECPAYCD       TO SP-RECPAYCD.
%%         MOVE ORIGINAL-SP-RECFRMLA       TO   SP-RECFRMLA.
%%         MOVE ORIGINAL-SP-RECPDTH        TO SP-RECPDTH.
%%         MOVE ORIGINAL-SP-RECPAYCD       TO SP-RECPAYCD.
%%         MOVE ORIGINAL-SP-CPOT30-CD      TO SP-CPOT30-CD.
%%         MOVE ORIGINAL-SP-CPOTCHGOFF-CD  TO SP-CPOTCHGOFF-CD.
%%         MOVE ORIGINAL-SP-RDPOT30-CD     TO SP-RDPOT30-CD.
%%         MOVE ORIGINAL-SP-RDPOTCHGOFF-CD TO SP-RDPOTCHGOFF-CD.
              


      ***************************
       PRINT-RANGE-LINES SECTION.
      ***************************

           MOVE 2 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-SELECTION
              MOVE ENT-GROUP TO RANGE1
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BR - END BR" TO RANGE-SELECTION
              MOVE BEG-BR TO RANGE1N
              MOVE END-BR TO RANGE2N
              PERFORM WRITE-DETAIL-LINE.


           MOVE "BEG CLASS - END CLASS" TO RANGE-SELECTION.
           MOVE BEG-CLASS TO RANGE1N.
           MOVE END-CLASS TO RANGE2N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG DATE - END DATE" TO RANGE-SELECTION.
           MOVE BEG-DATE TO RANGE1D.
           MOVE END-DATE TO RANGE2D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS READ" TO RANGE-SELECTION.
           MOVE TOTAL-RECORDS TO RANGE1N.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS OUT OF RANGE" TO RANGE-SELECTION.
           MOVE TOTAL-BYPASSED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS SUBMITTED" TO RANGE-SELECTION.
           MOVE TOTAL-INCLUDED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ****************************
      *    WRITE DETAIL LINE     *
      ****************************
       WRITE-DETAIL-LINE SECTION.
       WRITE-DET-LINE.
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
       EXIT-WRITE-DETAIL-LINE.
           EXIT.

      *******************************
      *    HEAD PAGE ROUTINE        *
      *******************************
       HEAD-PAGE SECTION.
       HEAD-ROUTINE.
           MOVE "N" TO FIRST-TIME.
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

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.



       COPY "LIBGB/GPENV.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPMDTE.CPY".
      *COPY "LIBLP/LPUPWK.CPY".
      *COPY "LIBLP/LPUPER.CPY".
      *COPY "LIBLP/LPXDTE.CPY".
      *COPY "LIBLP/LPITRM.CPY".
      *COPY "LIBLP/LPAMTS.CPY".
      *COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPCEPP.CPY".
      *COPY "LIBLP/LPIBPC.CPY".
      *COPY "LIBLP/LPTBNO.CPY".
      *COPY "LIBLP/LNCOST.CPY".
      *COPY "LIBLP/LPEARN.CPY".
      *COPY "LIBLP/LPCDEL.CPY".
      *COPY "LIBLP/LPLCAS.CPY".
      *COPY "LIBLP/LPLCAP.CPY".
      *COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPPDUE.CPY".



      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO TESTR2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TESTR2-SYSDATE.
           DISPLAY TESTR2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TESTR2-FORM-FIELDS TO WR-BUF. |MJD001
           MOVE TESTR2-FORM-LIST TO WR-LIST. |MJD001
           PERFORM CALL-WRFORM. |MJD001

       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE TESTR2-HELP--FILE TO HELP-LINK-FILE.
           MOVE TESTR2-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/TESTR2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ***********************
       MISC-ROUTINES SECTION.
      ***********************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETPR.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       COPY "LIBLP/LPLTPCL.CPY".

      **********************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
      **********************
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".


       OPEN-WK-AGEINZ-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-AGEINZ-IFN TO E-FILE.
           OPEN OUTPUT WK-AGEINZ-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-AGEINZ-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-AGEINZ-FILE
              GO TO OPEN-WK-AGEINZ-FILE-OUTPUT.
       WRITE-WK-AGEINZ-FILE.
           PERFORM WRITE-IT.
           MOVE WK-AGEINZ-IFN TO E-FILE.
           WRITE WK-AGEINZ-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-AGEINZ-FILE.
       CLOSE-WK-AGEINZ-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-AGEINZ-FILE.


       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BR
              MOVE SV-END-BR TO END-BR.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY TESTR2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
