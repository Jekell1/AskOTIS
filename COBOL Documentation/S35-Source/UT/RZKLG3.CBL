       IDENTIFICATION DIVISION.
       PROGRAM-ID.   RZKLG2.
       DATE-WRITTEN. 070409.
      ******************************************************************
      * WALKS ACROSS BRANCHES
      *       DESCRIPTION:  REPOSSESSION/LEGAL REPORT
      *
      *   IF BOTH BK AND LEGAL, BK STATUS TAKES PRIORITY
      *   IF INCLUDE BK AND SKIP LEGAL, AND ACCOUNT IS BOTH BK AND LEGAL, IT
      *      WILL BE INCLUDED
      *   IF SKIP BK AND INCLUDE LEGAL, AND ACCOUNT IS BOTH BK AND LEGAL, IT
      *      WILL NOT BE INCLUDED BECAUSE BK SKIP TAKES PRIORITY
      *
      *   SAME AS BNKLGL EXCEPT SORTED BY BK/LEGAL DATE, SO NO CLASS TOTALS
      *
      *
      *  REV:
      *  CS  070409 NEW FOR WORLD, PR# 537
      *  BLF 070827 FIX CLASS RANGES; REPORT TOOK LOGIC FROM BNKLGL BUT
      *             CHANGED TO READ LN1 INSTEAD OF LN3, SO NEEDED TO
      *             CHECK BEG & ENDING CLASS DIFFERENTLY, WORLD PR# 553
      *             ALSO FIXED SORT TO BE BY YYYYMMDD, NOT MMDDYYYY
      *  BLF 070830 FIX ANOTHER RANGE PROBLEM, CHANGE TITLE TO BE
      *             BANKRUPT/LEGAL BY STATUS DATE
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *   CS 160825 CHANGED SETUP OF LN-OWNBR TO USE BR-NO AND NOT EXT-
      *             FILPATH-BRNO
      *  BAH 170508 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2018.0423 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *         LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *         WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *         LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      *
      * BAH 181203 GLOBAL LNFILE, L2G
      * BAH 20190922 REMOVED ACCESS-CALL LNFILE
      * BAH 20200827 REMOVED RESET ON FIRST FIELD
      ******************************************************************
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
      ******************************************************************
      *  SUMMARY TOTALS WORK RECORD
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BR      PIC 9(4).

           03  WK-NOLOANS         PIC S9(7)     COMP-3.
           03  WK-BALANCE         PIC S9(9)V99  COMP-3.
           03  WK-7-NOLOANS       PIC S9(7)     COMP-3.
           03  WK-7-BALANCE       PIC S9(9)V99  COMP-3.
           03  WK-11-NOLOANS      PIC S9(7)     COMP-3.
           03  WK-11-BALANCE      PIC S9(9)V99  COMP-3.
           03  WK-12-NOLOANS      PIC S9(7)     COMP-3.
           03  WK-12-BALANCE      PIC S9(9)V99  COMP-3.
           03  WK-13-NOLOANS      PIC S9(7)     COMP-3.
           03  WK-13-BALANCE      PIC S9(9)V99  COMP-3.

       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRNO          PIC 9(4).
               05  WK2-BK-LGL-DATE   PIC 9(8).
               05  WK2-ACCTNO        PIC 9(8).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/BNKLG2.CBL S35 11/28/22-10:17:10 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
      * COPY "LIBLP/LP01LN_SQL.CPY".
      * COPY "LIBLP/LP01LN_SQL.CPY".
      * COPY "LIBLP/LP01LN_SQL.CPY".
      * COPYMEMBER: LIBLP/LP01LN_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN MASTER FILE
      *
      *=================================================================
      * REV:
      * JKC 2021-0430 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  LN1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LN1-CURSOR-STAT-GOOD              VALUE "00".
           88  LN1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LN3-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LN3-CURSOR-STAT-GOOD              VALUE "00".
           88  LN3-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LN4-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LN4-CURSOR-STAT-GOOD              VALUE "00".
           88  LN4-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LN5-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LN5-CURSOR-STAT-GOOD              VALUE "00".
           88  LN5-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".


      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LN1-CURSOR-GR-STAT        PIC X(2)    VALUE SPACES.
           88  LN1-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  LN1-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LN3-CURSOR-GR-STAT        PIC X(2)    VALUE SPACES.
           88  LN3-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  LN3-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{NOT GREATER CURSOR FOR START-NOT-GREATER & READ-PREVIOUS}
       01  LN4-CURSOR-NG-STAT        PIC X(2)    VALUE SPACES.
           88  LN4-CURSOR-NG-STAT-GOOD           VALUE "00".
           88  LN4-CURSOR-NG-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LN5-CURSOR-NG-STAT        PIC X(2)    VALUE SPACES.
           88  LN5-CURSOR-NG-STAT-GOOD           VALUE "00".
           88  LN5-CURSOR-NG-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".


       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LN-COMMAND                PIC X(8192) VALUE SPACES.

       01  RET-CODE                                  PIC S9(4) COMP-5.  

       01  QLN1-WHERE-BEG-KEY.
           03  QLN1-WBEG-OWNBR                       PIC 9(4).
           03  QLN1-WBEG-ACCTNO                      PIC 9(8).

       01  QLN1-WHERE-END-KEY.
           03  QLN1-WEND-OWNBR                       PIC 9(4).
           03  QLN1-WEND-ACCTNO                      PIC 9(8).

       01  QLN3-WHERE-BEG-KEY.
           03  QLN3-WBEG-OWNBR                       PIC 9(4).
           03  QLN3-WBEG-CLASS                       PIC 9(3).
           03  QLN3-WBEG-ACCTNO                      PIC 9(8).

       01  QLN3-WHERE-END-KEY.
           03  QLN3-WEND-OWNBR                       PIC 9(4).
           03  QLN3-WEND-CLASS                       PIC 9(3).
           03  QLN3-WEND-ACCTNO                      PIC 9(8).

       01  QLN4-WHERE-BEG-KEY.
           03  QLN4-WBEG-OWNBR                       PIC 9(4).
           03  QLN4-WBEG-SSNO1                       PIC 9(9).

       01  QLN4-WHERE-END-KEY.
           03  QLN4-WEND-OWNBR                       PIC 9(4).
           03  QLN4-WEND-SSNO1                       PIC 9(9).

       01  QLN5-WHERE-BEG-KEY.
           03  QLN5-WBEG-OWNBR                       PIC 9(4).
           03  QLN5-WBEG-SSNO2                       PIC 9(9).

       01  QLN5-WHERE-END-KEY.
           03  QLN5-WEND-OWNBR                       PIC 9(4).
           03  QLN5-WEND-SSNO2                       PIC 9(9).


       01  QLN-REC.
           03  QLN-OWNBR                             PIC 9(4).
           03  QLN-ACCTNO                            PIC 9(8).
           03  QLN-CLASS                             PIC 9(3).
           03  QLN-SUBCLASS                          PIC 99.
           03  QLN-LAWCODE                           PIC 99.
           03  QLN-SSNO1                             PIC 9(9).
           03  QLN-MAKERCD1                          PIC X.
           03  QLN-SSNO2                             PIC 9(9).
           03  QLN-MAKERCD2                          PIC X.
           03  QLN-SSNO3                             PIC 9(9).
           03  QLN-MAKERCD3                          PIC X.
           03  QLN-SSNO4                             PIC 9(9).
           03  QLN-MAKERCD4                          PIC X.
           03  QLN-ORGST                             PIC XX.
           03  QLN-ORGBR                             PIC 9(4).
           03  QLN-OWNST                             PIC XX.
           03  QLN-SPRCLASS                          PIC 9(3).
           03  QLN-CONVERCD                          PIC X.
           03  QLN-MIDSTREAM                         PIC X.
           03  QLN-PURCD                             PIC XXX.
           03  QLN-SRCD                              PIC XX.
           03  QLN-COLLCD                            PIC X(2).
           03  QLN-COLLID                            PIC X(20).
           03  QLN-POCD                              PIC XX.
           03  QLN-PLCD                              PIC X.
           03  QLN-PLTYPE                            PIC X.
           03  QLN-CONFIDENTCD                       PIC X.
           03  QLN-ALLOTCD                           PIC X.
           03  QLN-RECASTCD                          PIC X.
           03  QLN-SKIP-TRW                          PIC X.
           03  QLN-STATUSFG                          PIC X.
           03  QLN-SMALLOAN                          PIC X.
           03  QLN-SALEFIN                           PIC X.
           03  QLN-SERVACCT                          PIC X.
           03  QLN-LOANTYPE                          PIC X.
           03  QLN-ADDON-FG                          PIC X.
           03  QLN-PAY-SCHLD-FG                      PIC X.
           03  QLN-LC-CODE                           PIC 99.
           03  QLN-CAN-LOAN-NO                       PIC XX.
           03  QLN-STATFLAGS                         PIC X(20).
           03  QLN-ENTDATE                           PIC X(10).
           03  QLN-LOANDATE                          PIC X(10).
           03  QLN-PURDATE                           PIC X(10).
           03  QLN-INTDATE                           PIC X(10).
           03  QLN-CASHDATE                          PIC X(10).
           03  QLN-FILEDATE                          PIC X(10).
           03  QLN-RESCISSION-DATE                   PIC X(10).
           03  QLN-PART-RECOURSE-DATE                PIC X(10).
           03  QLN-ORIG-1STPYDATE                    PIC X(10).
           03  QLN-1STPYDATE                         PIC X(10).
           03  QLN-JDDATE                            PIC X(10).
           03  QLN-PLDATE                            PIC X(10).
           03  QLN-PL-TRDATE                         PIC X(10).
           03  QLN-ACCELDATE                         PIC X(10).
           03  QLN-REDUDATE                          PIC X(10).
           03  QLN-POFFDATE                          PIC X(10).
           03  QLN-PO-TRDATE                         PIC X(10).
           03  QLN-LEGALDATE                         PIC X(10).
           03  QLN-BNKRPTDATE                        PIC X(10).
           03  QLN-BNKRPT-DISMISS-DATE               PIC X(10).
           03  QLN-BNKRPT-DISCHARGE-DATE             PIC X(10).
           03  QLN-BNKRPT-REAFFIRM-DATE              PIC X(10).
           03  QLN-DEBTAIDATE                        PIC X(10).
           03  QLN-REPODATE                          PIC X(10).
           03  QLN-REDEEMDATE                        PIC X(10).
           03  QLN-SOLDDATE                          PIC X(10).
           03  QLN-INS-CONFIRM-DATE                  PIC X(10).
           03  QLN-PRIOR-RN-FOR10-DATE               PIC X(10).
           03  QLN-LAST-RN-FOR10-DATE                PIC X(10).
           03  QLN-TRW-DOFD-DATE                     PIC X(10).
           03  QLN-LAST-PYPA-DATE                    PIC X(10).
           03  QLN-BT-TRDATE                         PIC X(10).
           03  QLN-METRO2-LAST-PMT-DATE              PIC X(10).
           03  QLN-FOLLOWUP-DATE                     PIC X(10).
           03  QLN-LAST-ACCRUAL-DATE                 PIC X(10).
           03  QLN-APRATE                            PIC 999V999.
           03  QLN-EFFRATE                           PIC 999V999.
           03  QLN-SMPRATE                           PIC 999V999.
           03  QLN-YLDRATE                           PIC 999V999.
           03  QLN-OVRDRATE                          PIC 999V999.
           03  QLN-ENDORATE                          PIC 999V999.
           03  QLN-JDRATE                            PIC 999V999.
           03  QLN-ACCRATE                           PIC 999V999.
           03  QLN-REDURATE                          PIC 999V999.
           03  QLN-CONRATE                           PIC 999V999.
           03  QLN-SERVPER                           PIC 9(3)V99.
           03  QLN-SELLPR                            PIC 9(7)V99.
           03  QLN-DOWNPY                            PIC 9(7)V99.
           03  QLN-TRADEIN                           PIC S9(7)V99.
           03  QLN-LNAMT                             PIC 9(7)V99.
           03  QLN-FINCHG                            PIC 9(7)V99.
           03  QLN-INTCHG                            PIC 9(7)V99.
           03  QLN-SERCHG                            PIC 9(5)V99.
           03  QLN-EXTCHG                            PIC 9(5)V99.
           03  QLN-MAINTFEE                          PIC 9(5)V99.
           03  QLN-POINTS                            PIC 99V99.
           03  QLN-PROCEEDS                          PIC S9(7)V99.
           03  QLN-ORGTERM                           PIC 9(3).
           03  QLN-1STPYAMT                          PIC 9(7)V99.
           03  QLN-NOREGPY                           PIC 9(3).
           03  QLN-REGPYAMT                          PIC 9(7)V99 .
           03  QLN-LASTPYAMT                         PIC S9(7)V99.
           03  QLN-PROCASH                           PIC S9(7)V99.
           03  QLN-TILLNO                            PIC 9(2).
           03  QLN-STATE-FLAG                        PIC X.
           03  QLN-STATE-WORKER                      PIC S9(7)V99.
           03  QLN-STATE-WORKER2                     PIC S9(5)V99.
           03  QLN-FEECODE-1                         PIC XX.
           03  QLN-FEEAMT-1                          PIC 9(5)V99.
           03  QLN-FEEOURS-1                         PIC X.
           03  QLN-FEEFINCHG-1                       PIC X.
           03  QLN-FEECODE-2                         PIC XX.
           03  QLN-FEEAMT-2                          PIC 9(5)V99.
           03  QLN-FEEOURS-2                         PIC X.
           03  QLN-FEEFINCHG-2                       PIC X.
           03  QLN-FEECODE-3                         PIC XX.
           03  QLN-FEEAMT-3                          PIC 9(5)V99.
           03  QLN-FEEOURS-3                         PIC X.
           03  QLN-FEEFINCHG-3                       PIC X.
           03  QLN-FEECODE-4                         PIC XX.
           03  QLN-FEEAMT-4                          PIC 9(5)V99.
           03  QLN-FEEOURS-4                         PIC X.
           03  QLN-FEEFINCHG-4                       PIC X.
           03  QLN-FEECODE-5                         PIC XX.
           03  QLN-FEEAMT-5                          PIC 9(5)V99.
           03  QLN-FEEOURS-5                         PIC X.
           03  QLN-FEEFINCHG-5                       PIC X.
           03  QLN-FEECODE-6                         PIC XX.
           03  QLN-FEEAMT-6                          PIC 9(5)V99.
           03  QLN-FEEOURS-6                         PIC X.
           03  QLN-FEEFINCHG-6                       PIC X.
           03  QLN-FEECODE-7                         PIC XX.
           03  QLN-FEEAMT-7                          PIC 9(5)V99.
           03  QLN-FEEOURS-7                         PIC X.
           03  QLN-FEEFINCHG-7                       PIC X.
           03  QLN-FEECODE-8                         PIC XX.
           03  QLN-FEEAMT-8                          PIC 9(5)V99.
           03  QLN-FEEOURS-8                         PIC X.
           03  QLN-FEEFINCHG-8                       PIC X.
           03  QLN-FEECODE-9                         PIC XX.
           03  QLN-FEEAMT-9                          PIC 9(5)V99.
           03  QLN-FEEOURS-9                         PIC X.
           03  QLN-FEEFINCHG-9                       PIC X.
           03  QLN-FEECODE-10                        PIC XX.
           03  QLN-FEEAMT-10                         PIC 9(5)V99.
           03  QLN-FEEOURS-10                        PIC X.
           03  QLN-FEEFINCHG-10                      PIC X.
           03  QLN-FEECODE-11                        PIC XX.
           03  QLN-FEEAMT-11                         PIC 9(5)V99.
           03  QLN-FEEOURS-11                        PIC X.
           03  QLN-FEEFINCHG-11                      PIC X.
           03  QLN-FEECODE-12                        PIC XX.
           03  QLN-FEEAMT-12                         PIC 9(5)V99.
           03  QLN-FEEOURS-12                        PIC X.
           03  QLN-FEEFINCHG-12                      PIC X.
           03  QLN-FEECODE-13                        PIC XX.
           03  QLN-FEEAMT-13                         PIC 9(5)V99.
           03  QLN-FEEOURS-13                        PIC X.
           03  QLN-FEEFINCHG-13                      PIC X.
           03  QLN-FEECODE-14                        PIC XX.
           03  QLN-FEEAMT-14                         PIC 9(5)V99.
           03  QLN-FEEOURS-14                        PIC X.
           03  QLN-FEEFINCHG-14                      PIC X.
           03  QLN-FEECODE-15                        PIC XX.
           03  QLN-FEEAMT-15                         PIC 9(5)V99.
           03  QLN-FEEOURS-15                        PIC X.
           03  QLN-FEEFINCHG-15                      PIC X.
           03  QLN-FEECODE-16                        PIC XX.
           03  QLN-FEEAMT-16                         PIC 9(5)V99.
           03  QLN-FEEOURS-16                        PIC X.
           03  QLN-FEEFINCHG-16                      PIC X.
           03  QLN-FEECODE-17                        PIC XX.
           03  QLN-FEEAMT-17                         PIC 9(5)V99.
           03  QLN-FEEOURS-17                        PIC X.
           03  QLN-FEEFINCHG-17                      PIC X.
           03  QLN-FEECODE-18                        PIC XX.
           03  QLN-FEEAMT-18                         PIC 9(5)V99.
           03  QLN-FEEOURS-18                        PIC X.
           03  QLN-FEEFINCHG-18                      PIC X.
           03  QLN-FEECODE-19                        PIC XX.
           03  QLN-FEEAMT-19                         PIC 9(5)V99.
           03  QLN-FEEOURS-19                        PIC X.
           03  QLN-FEEFINCHG-19                      PIC X.
           03  QLN-FEECODE-20                        PIC XX.
           03  QLN-FEEAMT-20                         PIC 9(5)V99.
           03  QLN-FEEOURS-20                        PIC X.
           03  QLN-FEEFINCHG-20                      PIC X.
           03  QLN-INSURED                           PIC X.
           03  QLN-INSTYPES                          PIC X.
           03  QLN-INSELIGIBLE                       PIC X.
           03  QLN-INSCOMP-1                         PIC 99.
           03  QLN-INSOURS-1                         PIC X.
           03  QLN-INSEFF-DATE-1                     PIC X(10).
           03  QLN-INSEXP-DATE-1                     PIC X(10).
           03  QLN-INSPREM-1                         PIC 9(5)V99.
           03  QLN-INSCOVR-1                         PIC 9(7)V99.
           03  QLN-INSCOMM-1                         PIC 9(5)V99.
           03  QLN-INSANTICERN-1                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-1                     PIC S9(7)V99.
           03  QLN-INSUFERN-1                        PIC S9(7)V99.
           03  QLN-INSACCERN-1                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-1                   PIC 99.
           03  QLN-INSFINCHG-1                       PIC X.
           03  QLN-INSCOMP-2                         PIC 99.
           03  QLN-INSOURS-2                         PIC X.
           03  QLN-INSEFF-DATE-2                     PIC X(10).
           03  QLN-INSEXP-DATE-2                     PIC X(10).
           03  QLN-INSPREM-2                         PIC 9(5)V99.
           03  QLN-INSCOVR-2                         PIC 9(7)V99.
           03  QLN-INSCOMM-2                         PIC 9(5)V99.
           03  QLN-INSANTICERN-2                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-2                     PIC S9(7)V99.
           03  QLN-INSUFERN-2                        PIC S9(7)V99.
           03  QLN-INSACCERN-2                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-2                   PIC 99.
           03  QLN-INSFINCHG-2                       PIC X.
           03  QLN-INSCOMP-3                         PIC 99.
           03  QLN-INSOURS-3                         PIC X.
           03  QLN-INSEFF-DATE-3                     PIC X(10).
           03  QLN-INSEXP-DATE-3                     PIC X(10).
           03  QLN-INSPREM-3                         PIC 9(5)V99.
           03  QLN-INSCOVR-3                         PIC 9(7)V99.
           03  QLN-INSCOMM-3                         PIC 9(5)V99.
           03  QLN-INSANTICERN-3                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-3                     PIC S9(7)V99.
           03  QLN-INSUFERN-3                        PIC S9(7)V99.
           03  QLN-INSACCERN-3                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-3                   PIC 99.
           03  QLN-INSFINCHG-3                       PIC X.
           03  QLN-INSCOMP-4                         PIC 99.
           03  QLN-INSOURS-4                         PIC X.
           03  QLN-INSEFF-DATE-4                     PIC X(10).
           03  QLN-INSEXP-DATE-4                     PIC X(10).
           03  QLN-INSPREM-4                         PIC 9(5)V99.
           03  QLN-INSCOVR-4                         PIC 9(7)V99.
           03  QLN-INSCOMM-4                         PIC 9(5)V99.
           03  QLN-INSANTICERN-4                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-4                     PIC S9(7)V99.
           03  QLN-INSUFERN-4                        PIC S9(7)V99.
           03  QLN-INSACCERN-4                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-4                   PIC 99.
           03  QLN-INSFINCHG-4                       PIC X.
           03  QLN-INSCOMP-5                         PIC 99.
           03  QLN-INSOURS-5                         PIC X.
           03  QLN-INSEFF-DATE-5                     PIC X(10).
           03  QLN-INSEXP-DATE-5                     PIC X(10).
           03  QLN-INSPREM-5                         PIC 9(5)V99.
           03  QLN-INSCOVR-5                         PIC 9(7)V99.
           03  QLN-INSCOMM-5                         PIC 9(5)V99.
           03  QLN-INSANTICERN-5                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-5                     PIC S9(7)V99.
           03  QLN-INSUFERN-5                        PIC S9(7)V99.
           03  QLN-INSACCERN-5                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-5                   PIC 99.
           03  QLN-INSFINCHG-5                       PIC X.
           03  QLN-INSCOMP-6                         PIC 99.
           03  QLN-INSOURS-6                         PIC X.
           03  QLN-INSEFF-DATE-6                     PIC X(10).
           03  QLN-INSEXP-DATE-6                     PIC X(10).
           03  QLN-INSPREM-6                         PIC 9(5)V99.
           03  QLN-INSCOVR-6                         PIC 9(7)V99.
           03  QLN-INSCOMM-6                         PIC 9(5)V99.
           03  QLN-INSANTICERN-6                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-6                     PIC S9(7)V99.
           03  QLN-INSUFERN-6                        PIC S9(7)V99.
           03  QLN-INSACCERN-6                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-6                   PIC 99.
           03  QLN-INSFINCHG-6                       PIC X.
           03  QLN-INSCOMP-7                         PIC 99.
           03  QLN-INSOURS-7                         PIC X.
           03  QLN-INSEFF-DATE-7                     PIC X(10).
           03  QLN-INSEXP-DATE-7                     PIC X(10).
           03  QLN-INSPREM-7                         PIC 9(5)V99.
           03  QLN-INSCOVR-7                         PIC 9(7)V99.
           03  QLN-INSCOMM-7                         PIC 9(5)V99.
           03  QLN-INSANTICERN-7                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-7                     PIC S9(7)V99.
           03  QLN-INSUFERN-7                        PIC S9(7)V99.
           03  QLN-INSACCERN-7                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-7                   PIC 99.
           03  QLN-INSFINCHG-7                       PIC X.
           03  QLN-INSCOMP-8                         PIC 99.
           03  QLN-INSOURS-8                         PIC X.
           03  QLN-INSEFF-DATE-8                     PIC X(10).
           03  QLN-INSEXP-DATE-8                     PIC X(10).
           03  QLN-INSPREM-8                         PIC 9(5)V99.
           03  QLN-INSCOVR-8                         PIC 9(7)V99.
           03  QLN-INSCOMM-8                         PIC 9(5)V99.
           03  QLN-INSANTICERN-8                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-8                     PIC S9(7)V99.
           03  QLN-INSUFERN-8                        PIC S9(7)V99.
           03  QLN-INSACCERN-8                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-8                   PIC 99.
           03  QLN-INSFINCHG-8                       PIC X.
           03  QLN-INSCOMP-9                         PIC 99.
           03  QLN-INSOURS-9                         PIC X.
           03  QLN-INSEFF-DATE-9                     PIC X(10).
           03  QLN-INSEXP-DATE-9                     PIC X(10).
           03  QLN-INSPREM-9                         PIC 9(5)V99.
           03  QLN-INSCOVR-9                         PIC 9(7)V99.
           03  QLN-INSCOMM-9                         PIC 9(5)V99.
           03  QLN-INSANTICERN-9                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-9                     PIC S9(7)V99.
           03  QLN-INSUFERN-9                        PIC S9(7)V99.
           03  QLN-INSACCERN-9                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-9                   PIC 99.
           03  QLN-INSFINCHG-9                       PIC X.
           03  QLN-INSCOMP-10                        PIC 99.
           03  QLN-INSOURS-10                        PIC X.
           03  QLN-INSEFF-DATE-10                    PIC X(10).
           03  QLN-INSEXP-DATE-10                    PIC X(10).
           03  QLN-INSPREM-10                        PIC 9(5)V99.
           03  QLN-INSCOVR-10                        PIC 9(7)V99.
           03  QLN-INSCOMM-10                        PIC 9(5)V99.
           03  QLN-INSANTICERN-10                    PIC S9(7)V99.
           03  QLN-INSANTICADJ-10                    PIC S9(7)V99.
           03  QLN-INSUFERN-10                       PIC S9(7)V99.
           03  QLN-INSACCERN-10                      PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-10                  PIC 99.
           03  QLN-INSFINCHG-10                      PIC X.
           03  QLN-PY-YYYYMM-RECDEL                  PIC 9(6).
           03  QLN-MTD-PAYS-RECDEL                   PIC S9(7)V99.
           03  QLN-GARNISH-EXTR                      PIC X.
           03  QLN-SHIP-SAME-FG                      PIC X.
           03  QLN-AD-D-INS-COVR                     PIC 9(7).
           03  QLN-TOT-PARTIAL-CHGOFF                PIC S9(7)V99.
           03  QLN-LPO                               PIC X(4).
           03  QLN-REP-CD                            PIC X(8).
           03  QLN-EMP-ID                            PIC 9(8).
           03  QLN-PROCESS-CD                        PIC X.
           03  QLN-WCBC-CREDITLIM                    PIC S9(5).
           03  QLN-FOLLOWUP-CD                       PIC X(4).
           03  QLN-MIP-FG                            PIC X.
           03  QLN-MIP-MTD-CLAHPAID                  PIC S9(5)V99.
           03  QLN-MIP-CLPDTH-DATE                   PIC X(10).
           03  QLN-MIP-TOT-CLPAID                    PIC S9(7)V99.
           03  QLN-MIP-AHPDTH-DATE                   PIC X(10).
           03  QLN-MIP-TOT-AHPAID                    PIC S9(7)V99.
           03  QLN-IL-GROSS-INCOME                   PIC S9(7)V99.
           03  QLN-IL-GROSS-INCOME2                  PIC S9(7)V99.
           03  QLN-RESCISSION                        PIC X(1).
           03  QLN-INTERIM-TRW                       PIC X(1).
           03  QLN-DLNO                              PIC 9(4)V99.
           03  QLN-DLPOOL                            PIC 9(5).
           03  QLN-DLRECOURSE                        PIC X.
           03  QLN-DLPARTRCAMT                       PIC 9(7)V99.
           03  QLN-ORIGPURAMT                        PIC 9(7)V99.
           03  QLN-PURBAL                            PIC S9(7)V99.
           03  QLN-DLRVPRIN-1                        PIC 9(7)V99.
           03  QLN-DLRVPRIN-2                        PIC 9(7)V99.
           03  QLN-DLRVPRIN-3                        PIC 9(7)V99.
           03  QLN-DLRVCHGS-1                        PIC 9(7)V99.
           03  QLN-DLRVCHGS-2                        PIC 9(7)V99.
           03  QLN-DLRVCHGS-3                        PIC 9(7)V99.
           03  QLN-DLPARVHELD                        PIC 9(7)V99.
           03  QLN-DLPARVPAID                        PIC 9(7)V99.
           03  QLN-DLDISC                            PIC 9(7)V99.
           03  QLN-LCPDTH-DATE                       PIC X(10).
           03  QLN-INTPDTH-DATE                      PIC X(10).
           03  QLN-CURBAL                            PIC S9(7)V99.
           03  QLN-INTBAL                            PIC S9(7)V99.
           03  QLN-LCBAL                             PIC S9(7)V99.
           03  QLN-OTHBAL                            PIC S9(7)V99.
           03  QLN-OT2BAL                            PIC S9(7)V99.
           03  QLN-LCPAID                            PIC S9(7)V99.
           03  QLN-LCPARTIALS                        PIC S9(7)V99.
           03  QLN-STMT-COUNTER                      PIC 9(5).
           03  QLN-PRIOR-STMT-COUNTER                PIC 9(5).
           03  QLN-FROZEN-REASON                     PIC X(2).
           03  QLN-COUPON-STATEMENT                  PIC X.
           03  QLN-MONTHEND-BAL                      PIC 9(7)V99.
           03  QLN-ANTICTERM                         PIC 9(3).
           03  QLN-ANTICERN-1                        PIC S9(7)V99.
           03  QLN-ANTICADJ-1                        PIC S9(7)V99.
           03  QLN-UFERN-1                           PIC S9(7)V99.
           03  QLN-ACCERN-1                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-1                      PIC 99.
           03  QLN-ANTICERN-2                        PIC S9(7)V99.
           03  QLN-ANTICADJ-2                        PIC S9(7)V99.
           03  QLN-UFERN-2                           PIC S9(7)V99.
           03  QLN-ACCERN-2                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-2                      PIC 99.
           03  QLN-ANTICERN-3                        PIC S9(7)V99.
           03  QLN-ANTICADJ-3                        PIC S9(7)V99.
           03  QLN-UFERN-3                           PIC S9(7)V99.
           03  QLN-ACCERN-3                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-3                      PIC 99.
           03  QLN-ANTICERN-4                        PIC S9(7)V99.
           03  QLN-ANTICADJ-4                        PIC S9(7)V99.
           03  QLN-UFERN-4                           PIC S9(7)V99.
           03  QLN-ACCERN-4                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-4                      PIC 99.
           03  QLN-ANTICERN-5                        PIC S9(7)V99.
           03  QLN-ANTICADJ-5                        PIC S9(7)V99.
           03  QLN-UFERN-5                           PIC S9(7)V99.
           03  QLN-ACCERN-5                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-5                      PIC 99.
           03  QLN-ANTICERN-6                        PIC S9(7)V99.
           03  QLN-ANTICADJ-6                        PIC S9(7)V99.
           03  QLN-UFERN-6                           PIC S9(7)V99.
           03  QLN-ACCERN-6                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-6                      PIC 99.
           03  QLN-ANTICERN-7                        PIC S9(7)V99.
           03  QLN-ANTICADJ-7                        PIC S9(7)V99.
           03  QLN-UFERN-7                           PIC S9(7)V99.
           03  QLN-ACCERN-7                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-7                      PIC 99.
           03  QLN-ANTICERN-8                        PIC S9(7)V99.
           03  QLN-ANTICADJ-8                        PIC S9(7)V99.
           03  QLN-UFERN-8                           PIC S9(7)V99.
           03  QLN-ACCERN-8                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-8                      PIC 99.
           03  QLN-ANTICERN-9                        PIC S9(7)V99.
           03  QLN-ANTICADJ-9                        PIC S9(7)V99.
           03  QLN-UFERN-9                           PIC S9(7)V99.
           03  QLN-ACCERN-9                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-9                      PIC 99.
           03  QLN-ANTICERN-10                       PIC S9(7)V99.
           03  QLN-ANTICADJ-10                       PIC S9(7)V99.
           03  QLN-UFERN-10                          PIC S9(7)V99.
           03  QLN-ACCERN-10                         PIC S9(7)V99.
           03  QLN-EARN-ORDER-10                     PIC 99.
           03  QLN-LMO-PRIOR-INTEREST                PIC S9(7)V99.
           03  QLN-PRIOR-UNEARNED                    PIC S9(7)V99.
           03  QLN-ERNDELQ-STOP                      PIC X.
           03  QLN-NONACRERN                         PIC S9(7)V99.
           03  QLN-PRIOR-NONACRERN                   PIC S9(7)V99.
           03  QLN-ERN-IB-PDTH-DATE                  PIC X(10).
           03  QLN-ERN-IB-PRINBAL                    PIC S9(7)V99.
           03  QLN-ERN-IB-INTOWE                     PIC S9(7)V99.
           03  QLN-TOTERN                            PIC S9(7)V99.
           03  QLN-TOTOTH                            PIC S9(7)V99.
           03  QLN-TOTINT                            PIC S9(7)V99.
           03  QLN-TOTLCHG                           PIC S9(7)V99.
           03  QLN-TOTDEF                            PIC S9(7)V99.
           03  QLN-TOTSERACCT                        PIC S9(7)V99.
           03  QLN-LCWAIVERS                         PIC S9(5)V99.
           03  QLN-INTWAIVERS                        PIC S9(5)V99.
           03  QLN-TOTPAYMNTD                        PIC S9(7)V99.
           03  QLN-TOTEXCPAYMNTD                     PIC S9(7)V99.
           03  QLN-TOTNODEF                          PIC S99.
           03  QLN-YTDNODEF                          PIC S99.
           03  QLN-TOTNODEFACT                       PIC S99.
           03  QLN-YTDNODEFACT                       PIC S99.
           03  QLN-TOTNOALDEL                        PIC S99.
           03  QLN-YTDNOALDEL                        PIC S99.
           03  QLN-YTDINTPD                          PIC S9(7)V99.
           03  QLN-LYRINTPD                          PIC S9(7)V99.
           03  QLN-TOTINTPD                          PIC S9(7)V99.
           03  QLN-RECASTINT                         PIC 9(7)V99.
           03  QLN-RECASTPRIN                        PIC 9(7)V99.
           03  QLN-CL-REBATE                         PIC X.
           03  QLN-AH-REBATE                         PIC X.
           03  QLN-PP-REBATE                         PIC X.
           03  QLN-INT-REBATE                        PIC X.
           03  QLN-SC-REBATE                         PIC X.
           03  QLN-MF-REBATE                         PIC X.
           03  QLN-DF-REBATE                         PIC X.
           03  QLN-EX-REBATE                         PIC X.
           03  QLN-O1-REBATE                         PIC X.
           03  QLN-O2-REBATE                         PIC X.
           03  QLN-O3-REBATE                         PIC X.
           03  QLN-O4-REBATE                         PIC X.
           03  QLN-O5-REBATE                         PIC X.
           03  QLN-O6-REBATE                         PIC X.
           03  QLN-O7-REBATE                         PIC X.
           03  QLN-UN-O7-REB                         PIC X.
           03  QLN-UN-O8-REB                         PIC X.
           03  QLN-UN-O9-REB                         PIC X.
           03  QLN-UN-O10-REB                        PIC X.
           03  QLN-PLREASON                          PIC XX.
           03  QLN-COLLECTOR                         PIC X(3).
           03  QLN-ACTIONCD                          PIC X(4).
           03  QLN-ACTIONCD2                         PIC X(4).
           03  QLN-ATCODE                            PIC X(3).
           03  QLN-RECODE                            PIC X(3).
           03  QLN-ESCROW-FG                         PIC X.
           03  QLN-PROMDAYS-PASTDUE                  PIC S9(3).
           03  QLN-PROMID                            PIC X(3).
           03  QLN-PROMDATE                          PIC X(10).
           03  QLN-PROMAMT                           PIC 9(7)V99.
           03  QLN-PROMDATE2                         PIC X(10).
           03  QLN-DATE-PAID-LAST                    PIC X(10).
           03  QLN-AMT-PAID-LAST                     PIC 9(7)V99.
           03  QLN-LASTCFORMNO                       PIC 9(5).
           03  QLN-LASTCFORMDATE                     PIC X(10).
           03  QLN-DELCNT                            PIC 99.
           03  QLN-DELCNT2                           PIC 99.
           03  QLN-CREDITLIM                         PIC 9(7).
           03  QLN-CRSCORE                           PIC S9(3).
           03  QLN-LASTSFORMNO                       PIC 9(5).
           03  QLN-LASTSFORMDATE                     PIC X(10).
           03  QLN-SOLCNT                            PIC 99.
           03  QLN-MESSAGE                           PIC X(30).
           03  QLN-OLDACCTNO                         PIC 9(8).
           03  QLN-PRLNNO                            PIC 9(8).
           03  QLN-PRBAL                             PIC S9(7)V99.
           03  QLN-PRINTDUE                          PIC S9(7)V99.
           03  QLN-PRFINCHGRB                        PIC S9(7)V99.
           03  QLN-PRINSRB                           PIC S9(7)V99.
           03  QLN-PRPOCD                            PIC XX.
           03  QLN-PRPODATE                          PIC X(10).
           03  QLN-PRDELINQAMT                       PIC S9(7)V99.
           03  QLN-PRLNAMT                           PIC 9(7)V99.
           03  QLN-PRPDUE-DAYS                       PIC S9(5).
           03  QLN-PRDLDISC-UNEARN                   PIC S9(7)V99.
           03  QLN-PRCONPROFILE                      PIC 9(12).
           03  QLN-SUM-ORIGPURAMT                    PIC 9(7)V99.
           03  QLN-SUM-LNAMT                         PIC 9(7)V99.
           03  QLN-SUM-FINCHG                        PIC 9(7)V99.
           03  QLN-SUM-INTCHG                        PIC 9(7)V99.
           03  QLN-SUM-ORGTERM                       PIC 9(3).
           03  QLN-SUM-REGPYAMT                      PIC 9(7)V99.
           03  QLN-SUM-EXTCHG                        PIC 9(5)V99.
           03  QLN-SUM-SERCHG                        PIC 9(5)V99.
           03  QLN-SUM-MAINTFEE                      PIC 9(7)V99.
           03  QLN-UNITPER-CD                        PIC X.
           03  QLN-UNITPER-FREQ                      PIC 999.
           03  QLN-DAYS-IN-UNITPER                   PIC 9(3).
           03  QLN-UNITPER-PER-YEAR                  PIC 9(3)V9(12).
           03  QLN-REPOCD                            PIC X.
           03  QLN-BR-TFR-FG                         PIC X.
           03  QLN-O3INS-DLPROC                      PIC 9(7)V99.
           03  QLN-INSPREM-DLADJ                     PIC S9(5)V99.
           03  QLN-PLAMT                             PIC 9(7)V99.
           03  QLN-MTD-PRIN                          PIC S9(7)V99.
           03  QLN-MTD-LC                            PIC S9(5)V99.
           03  QLN-MTD-INT                           PIC S9(5)V99.
           03  QLN-MTD-OT                            PIC S9(5)V99.
           03  QLN-MTD-EX                            PIC S9(7)V99.
           03  QLN-BNKRPT-CHAPTER                    PIC 9(2).
           03  QLN-BMO-CON-STATUS                    PIC X.
           03  QLN-BMO-REC-STATUS                    PIC X.
           03  QLN-APP-NUMBER                        PIC 9(09).
           03  QLN-GIFTNET                           PIC X.
           03  QLN-GIFTNET-OUR-PO                    PIC 9(7).
           03  QLN-PROC-DISBURSED                    PIC X.
           03  QLN-NOODDPY                           PIC X.
           03  QLN-CONPROFILE-1                      PIC 9(12).
           03  QLN-CONPROFILE-2                      PIC 9(12).
           03  QLN-CONPROFILE-3                      PIC 9(12).
           03  QLN-CONPROFILE-4                      PIC 9(12).
           03  QLN-CONPROFILE-5                      PIC 9(12).
           03  QLN-RECPROFILE-1                      PIC 9(12).
           03  QLN-RECPROFILE-2                      PIC 9(12).
           03  QLN-RECPROFILE-3                      PIC 9(12).
           03  QLN-RECPROFILE-4                      PIC 9(12).
           03  QLN-RECPROFILE-5                      PIC 9(12).
           03  QLN-TRW-STATUS                        PIC XX.
           03  QLN-ORIGIN                            PIC X(4).
           03  QLN-PO-TRCD                           PIC XX.
           03  QLN-NEXIS-CRSCORE                     PIC 9(3).
           03  QLN-TRW-B-CONSUMER-IND                PIC X(2).
           03  QLN-PARVHELD-PAID-FG                  PIC X.
           03  QLN-ACCT-FROZEN                       PIC X.
           03  QLN-TRW-SPECIAL-CODE                  PIC X(2).
           03  QLN-CREATION-STATUS                   PIC X(1).
           03  QLN-TOTNOLCHG                         PIC S9(3).
           03  QLN-LBOX                              PIC X(1).
           03  QLN-EXTRA-PRIN-LFP                    PIC S9(7)V99.
           03  QLN-REMAIN-INT-SINCE-LFP              PIC S9(7)V99.
           03  QLN-REMAIN-PRIN-SINCE-LFP             PIC S9(7)V99.
           03  QLN-INTERIM-COUPON                    PIC X.
           03  QLN-DLCOMM-O3-CMSI                    PIC 9(5)V99.
           03  QLN-DLCOMM-PP-CMSI                    PIC 9(5)V99.
           03  QLN-TRW-J-CONSUMER-IND                PIC X(2).
           03  QLN-ACH                               PIC X.
           03  QLN-1099-C-FG                         PIC X.
           03  QLN-TRW-COMPLIANCE-CODE               PIC XX.
           03  QLN-UNWIND-REASON-CODE                PIC X(2).
           03  QLN-PBI-PBD-FG                        PIC X.
           03  QLN-OLD-ACCT-FORMAT-FG                PIC X.
           03  QLN-APPROVED-BY-USER                  PIC X(10).
           03  QLN-APPROVED-BY-CODE                  PIC X(3).
           03  QLN-APPROVED-BY-DATE                  PIC X(10).
           03  QLN-APPROVED-BY-TIME                  PIC 9(6).
           03  QLN-LTOUCH-DATE                       PIC X(10).
           03  QLN-LTOUCH-TIME                       PIC 9(6).

       EXEC SQL END DECLARE SECTION END-EXEC.	
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

       01  WK-IFN               PIC X(4)  VALUE "WK".
       01  WK2-IFN              PIC X(3)  VALUE "WK2".

       01  WK2-PATH             PIC X(35) VALUE SPACES.

      *-----------------------------------------------------------------
      * LINE WORKERS
      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

       01  HOLD-BALANCE         PIC S9(9)V99 COMP-3 VALUE 0.
      * (1) ALL LOANS
      * (2) CHAPTER 7
      * (3) CHAPTER 11
      * (4) CHAPTER 12
      * (5) CHAPTER 13
       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL                 OCCURS 10 TIMES.
               05  WS-GTT-TABLE                         OCCURS 5 TIMES.
                   07  WS-GTT-NOLOANS         PIC S9(7)     COMP.
                   07  WS-GTT-BALANCE         PIC S9(9)V99  COMP.

       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.
      ******************************************************************
      *  TOTALS LEV1 = CLASS TOTALS
      *            2 = BRANCH TOTALS
      *            3 = GRAND TOTALS
      ******************************************************************
       01  TOTALS-TAB.
           03  TOTAL-TBL       OCCURS 3.
               05  TOTAL-TABLE OCCURS 5 TIMES.
                   07  T-NOLOANS         PIC S9(7)     COMP-3.
                   07  T-BALANCE         PIC S9(9)V99  COMP-3.

      *-----------------------------------------------------------------
      * MISC WORKERS
      *-----------------------------------------------------------------
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".
       01  SUB                  PIC 9(2)  COMP.
       01  SUB1               PIC 9(6)  COMP.
       01  BANKRUPT-TEST        PIC X VALUE SPACES.
       01  LEGAL-TEST           PIC X VALUE SPACES.
       01  NUM-BW               PIC 9(9).
       01  NUM-BW-WK            REDEFINES NUM-BW.
           03  NUM-BW1          PIC 999.
           03  NUM-BW2          PIC 99.
           03  NUM-BW3          PIC 9999.
       01  ALP-BW.
           03  ALP-BW1          PIC 999.
           03  ALP-HYPHEN1      PIC X.
           03  ALP-BW2          PIC 99.
           03  ALP-HYPHEN2      PIC X.
           03  ALP-BW3          PIC 9999.
       01  NAME-WORKER          PIC X(34).

       01  HOLD-TODAY-DATE      PIC 9(8)  VALUE 0.
       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-BRNAME          PIC X(20) VALUE SPACES.


       01  SAVE-WK-KEY.
           03  SAVE-WK-BR       PIC 9(4).
           03  SAVE-WK-CL       PIC 9(2).

       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
           88  SKIP-RECORDS-FOUND         VALUE " ".

       01  C-S-Z           PIC X(66).
       01  STATE           PIC XX.
       01  ZIP             PIC X(10).

       01  FIRST-NAME           PIC X(16).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  LN1-OPENED-SW        PIC X        VALUE "N".

       01  LEGEND               PIC X(70).
       COPY "LIBLP/EMSFRG_CMD.CPY".
       01  LEGEND-DTE-01.
           03  FILLER           PIC X(70)  VALUE
               " F7-EXIT, ^, V, ENTER BEGINNING PAYOFF DATE:".
       01  LEGEND-DTE-02.
           03  FILLER           PIC X(70)  VALUE
               " F7-EXIT, ^, V, ENTER ENDING PAYOFF DATE:".
       01  LEGEND-AC-01.
           03  FILLER           PIC X(40)  VALUE
               " F7-EXIT, ^, V, ENTER BEGINNING ACCOUNT ".
           03  FILLER           PIC X(17)  VALUE
               "NUMBER. (F3-ALL):".
       01  LEGEND-AC-02.
           03  FILLER           PIC X(37)  VALUE
               " F7-EXIT, ^, V, ENTER ENDING ACCOUNT ".
           03  FILLER           PIC X(8)  VALUE
               "NUMBER.:".

      *-----------------------------------------------------------------
      *     S C R E E N   F I E L D S   *
      *-----------------------------------------------------------------
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH       PIC 9(4).
           03  END-BRANCH       PIC 9(4).
           03  BEG-CLASS        PIC 9(3).
           03  END-CLASS        PIC 9(3).
           03  BEG-DATE         PIC 9(8).
           03  END-DATE         PIC 9(8).
           03  INCLUDE-BANKRUPT PIC X.
           03  INCLUDE-LEGAL    PIC X.
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).
           03  GTT-ISO          PIC X.
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE PIC X OCCURS 10.

       01  VERSION-CONTROL     PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      *-----------------------------------------------------------------
      *     P R I N T   B U F F E R S   *
      *-----------------------------------------------------------------

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(6)    VALUE " ".
           03  FILLER           PIC X(29) VALUE
               "BANKRUPT/LEGAL BY STATUS DATE".

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC ZZZ9.
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).

       01  HEAD5.
           03  FILLER             PIC X(65) VALUE  SPACES.
           03  FILLER             PIC X(12) VALUE  "LEGAL".

       01  HEAD6.
           03  FILLER             PIC X(17) VALUE  "ACCOUNT #".
           03  FILLER             PIC X(22) VALUE  "NAME".
           03  FILLER             PIC X(12) VALUE  "LOANDATE".
           03  FILLER             PIC X(11) VALUE  "BALANCE".
           03  FILLER             PIC X(10) VALUE  "BANKRUPT".
           03  FILLER             PIC X(4)  VALUE  "CH".
           03  FILLER             PIC X(4)  VALUE  "CON".


       01  DETAIL-LINE        PIC X(80)  VALUE SPACES.
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-SLOAN-NUM    PIC X(16).
           03  FILLER         PIC X.
           03  D-SNAME        PIC X(20).
           03  FILLER         PIC X(2).
           03  D-SLNDATE      PIC 99/99/99     BLANK ZERO.
           03  FILLER         PIC X(2).
           03  D-SBALANCE     PIC Z(6).99-    BLANK ZERO.
           03  FILLER         PIC XX.
           03  D-SDATE        PIC 99/99/99     BLANK ZERO.
           03  D-STYPE        PIC X.
           03  FILLER         PIC X(2).
           03  D-SCHAPTER     PIC 9(2).
           03  FILLER         PIC X.
           03  D-SCONX.
               05  D-SCON     PIC ZZZ9         BLANK ZERO.

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  G-GROUP              PIC X(14).
           03  G-GROUP-1 REDEFINES G-GROUP.
               05  G-TOTNAME        PIC X(14).
               05  G-CODENAME REDEFINES G-TOTNAME.
                   07  FILLER       PIC X(13).
                   07  G-COLON      PIC X.
           03  G-GROUP-2 REDEFINES G-GROUP.
               05  G-PROMPT1        PIC X(7).
               05  FILLER           PIC X.
               05  G-GRNO           PIC X(4).
               05  G-BRNO           REDEFINES G-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X.
               05  G-GR-LEVEL       PIC 9(1).
           03  FILLER           PIC X(01).
           03  G-HEADING1       PIC X(12).
           03  G-AMOUNT-1       PIC ZZZZZZZZ9.99-  BLANK WHEN ZERO.
           03  FILLER REDEFINES G-AMOUNT-1.
               05  FILLER       PIC X(03).
               05  G-LOANS      PIC ZZZZZ9-    BLANK WHEN ZERO.
               05  FILLER       PIC X(03).
           03  FILLER           PIC X(9).
           03  G-TOTBAL         PIC Z,ZZZ,ZZ9.99-  BLANK WHEN ZERO.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(9)9.
               04  R-NUM-REDEF2 REDEFINES R-NUM.
                   05  R-DATE   PIC 9999/99/99.

       COPY "LIBGB/PASSWDW.CPY".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".

       COPY "LIBLP/ARRAYSPW.CPY". 
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPWDELW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BNKLG2_DEF.CPY".
       COPY "LIBLP/BNKLG2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/BNKLG2_SCN.CPY".

      ******************************************************************
      * P R O G R A M   C O N T R O L
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *-----------------------------------------------------------------
      * MAIN PROGRAM SECTION
      *-----------------------------------------------------------------
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BNKLG2" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
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
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.

           PERFORM OPEN-BW1-FILE.

           IF LN1-OPENED-SW = "N"
               PERFORM OPEN-LN1-FILE
               MOVE "Y" TO LN1-OPENED-SW.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE BR-NO         TO LN-PATH-OWNBR
                                 LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.

           PERFORM START-LN1-FILE.

       NEXT-LN.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO CONTINUE-WK2-PROCESSING.

           PERFORM TRAP-INTERRUPT.

           IF LN-POCD-NONLOAN
              GO TO NEXT-LN.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              GO TO NEXT-LN.

           IF LN-BNKRPTDATE = 0 AND LN-LEGALDATE = 0
              GO TO NEXT-LN.

           IF INCLUDE-BANKRUPT = "N"
              IF LN-BNKRPTDATE NOT = 0
                 GO TO NEXT-LN.
           IF INCLUDE-BANKRUPT = "Y"
              IF LN-BNKRPTDATE = 0
                 GO TO NEXT-LN.

      * AT THIS POINT, YOU HAVE A VALID ACCOUNT BASED UPON
      * BANKRUPT STATUS.
      * IF BOTH DATES SET, AND INCLUDE BK AND SKIP LEGAL, THE
      * ACCOUNT WOULD BE INCLUDED.
      * IF BOTH DATES SET, AND SKIPPING BANKRUPT AND INCLUDE LEGAL,
      * IT WILL BE SKIPPED.
      * EFFECTIVE DATED REBATES
           IF INCLUDE-LEGAL = "N"
              IF LN-LEGALDATE NOT = 0
                 IF (LN-BNKRPTDATE NOT = 0) AND
                        (INCLUDE-BANKRUPT = "Y")
                    NEXT SENTENCE
                 ELSE
                    GO TO NEXT-LN.

           IF INCLUDE-LEGAL = "N"
              IF INCLUDE-BANKRUPT = "Y"
                 IF LN-BNKRPTDATE < BEG-DATE OR LN-BNKRPTDATE > END-DATE
                    GO TO NEXT-LN.

           IF INCLUDE-BANKRUPT = "N"
              IF INCLUDE-LEGAL = "Y"
                 IF LN-LEGALDATE < BEG-DATE OR LN-LEGALDATE > END-DATE
                    GO TO NEXT-LN.

           MOVE "Y" TO LEGAL-TEST BANKRUPT-TEST.

      * ONLY OTHER OPTION IS BOTH FLAGS SET TO "Y"
      * ONLY ONE HAS TO BE WITHIN RANGE
           IF LN-BNKRPTDATE = 0
              MOVE "N" TO BANKRUPT-TEST
           ELSE
              IF LN-BNKRPTDATE < BEG-DATE OR LN-BNKRPTDATE > END-DATE
                 MOVE "N" TO BANKRUPT-TEST.

           IF LN-LEGALDATE = 0
              MOVE "N" TO LEGAL-TEST
           ELSE
              IF LN-LEGALDATE < BEG-DATE OR LN-LEGALDATE > END-DATE
                 MOVE "N" TO LEGAL-TEST.

      * NEITHER PASSED DATE RANGE TEST
           IF LEGAL-TEST = "N" AND BANKRUPT-TEST = "N"
              GO TO NEXT-LN.

           PERFORM CREATE-SORTED-FILE.

           GO TO NEXT-LN.

       CONTINUE-WK2-PROCESSING.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.

           MOVE ZEROS TO WK2-BRNO.
           MOVE ZEROS TO WK2-BK-LGL-DATE.
           MOVE ZEROS TO WK2-ACCTNO.
           PERFORM START-WK2-FILE.

       NEXT-WK2-REC.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-BAD
              GO TO END-OF-GROUP.

           IF FIRST-TIME = "Y"
              MOVE BR-NO TO HOLD-BRNO
              MOVE BR-NAME TO HOLD-BRNAME
              PERFORM SET-BRANCH-HEADINGS.

      * TEST BREAK IN BRANCH
           IF BR-NO NOT = HOLD-BRNO
              PERFORM BRANCH-TOTALS
              MOVE BR-NO TO HOLD-BRNO
              MOVE BR-NAME TO HOLD-BRNAME
              PERFORM SET-BRANCH-HEADINGS
              PERFORM HEAD-PAGE.

           MOVE WK2-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.

           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
			  PERFORM ARRAYSP-READ
      *        PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           PERFORM LN-DETAIL1.

           GO TO  NEXT-WK2-REC.

       END-OF-GROUP.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-WK2-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              GO TO END-PROGRAM.

           PERFORM BRANCH-TOTALS.
           PERFORM GRAND-TOTALS.
           IF ( GTT-ISO NOT = "S" )
              MOVE 99 TO LN-COUNT
              PERFORM GROUP-TOTAL-TABLE
              PERFORM PRINT-RANGE-LINES
           ELSE
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ********************************************
       CREATE-SORTED-FILE SECTION.
      ********************************************

      * CREATE SORTED FILE BY BANKRUPT/LEGAL DATE

           MOVE SPACES      TO WK2-REC.
           MOVE BR-NO       TO WK2-BRNO.
           MOVE LN-ACCTNO   TO WK2-ACCTNO.

           IF INCLUDE-LEGAL = "Y"
              MOVE LN-LEGALDATE TO WK2-BK-LGL-DATE.

           IF INCLUDE-BANKRUPT = "Y"
              MOVE LN-BNKRPTDATE TO WK2-BK-LGL-DATE.

           PERFORM WRITE-WK2-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

        CREATE-SORTED-FILE-EXIT.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE BNKLG2-QUEUE-POS TO Q-POS.
           MOVE BNKLG2-COPIES-POS TO C-POS.
           MOVE BNKLG2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE 0 TO PAGE-NUMBER.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-TODAY-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP  TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF (BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              MOVE 0 TO T-NOLOANS(1,SUB1) T-NOLOANS(2,SUB1)
                                         T-NOLOANS(3,SUB1)
              MOVE 0 TO T-BALANCE(1,SUB1) T-BALANCE(2,SUB1)
                                         T-BALANCE(3,SUB1)
           END-PERFORM.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM CLOSE-WK2-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.
      *     PERFORM OPEN-SP1-FILE.

           PERFORM ARRAYSP-LOAD.   

           PERFORM ENABLE-INTERRUPT.
           MOVE "N" TO RECORDS-FOUND.

           IF ( GROUP-FLAG = "Y" )
              MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y"    TO TOTAL-LEVEL-TABLE(9).

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LEGAL.

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A000BR1." TO VDU.
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
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO CHECK-BRANCH-SECURITY.

       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       CL-01.
           MOVE LEGEND-CL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
           MOVE LEGEND-CL-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.

           GO TO BEGIN-DATE.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
      
       BEGIN-DATE.
           IF EXT-ROUTE-BUF NOT = "00" GO TO ENTER-BANKRUPT.

           MOVE LEGEND-DTE-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-DATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
      
       ENDING-DATE.
           IF EXT-ROUTE-BUF NOT = "00" GO TO ENTER-BANKRUPT.
           MOVE LEGEND-DTE-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDING-DATE.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-DATE.

           GO TO ENTER-BANKRUPT.

       ALL-DATE.
           MOVE "S  8080BEGDATE." TO VDU.
           MOVE JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080ENDDATE."   TO VDU.
           MOVE JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       ENTER-BANKRUPT.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010BANKRUPT." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO ENDING-DATE.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO ENTER-BANKRUPT.

           IF NOT ( VDUBUF = "Y" OR "N")
              GO TO ENTER-BANKRUPT.

           MOVE VDUBUF TO INCLUDE-BANKRUPT.

       ENTER-LEGAL.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010LEGAL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO ENTER-BANKRUPT.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO ENTER-LEGAL.

           IF NOT ( VDUBUF = "Y" OR "N")
              GO TO ENTER-LEGAL.

           MOVE VDUBUF TO INCLUDE-LEGAL.
           IF INCLUDE-LEGAL = "N" AND INCLUDE-BANKRUPT = "N"
              MOVE "MUST INCLUDE SOMETHING" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LEGAL.

       GTT-ISO-PARAGRAPH.

           IF ( GROUP-FLAG NOT = "Y" )
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "S  A010GTT-ISO." TO VDU
              MOVE "S" TO VDUBUF GTT-ISO
              PERFORM SCREENIO
              GO TO END-PARM.

           MOVE LEGEND-GTT-01 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010GTT-ISO." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO ENTER-LEGAL.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO GTT-ISO-PARAGRAPH.

           IF NOT ( VDUBUF = "I" OR "S" OR "O" )
              GO TO GTT-ISO-PARAGRAPH.

           MOVE VDUBUF TO GTT-ISO.

           IF ( GTT-ISO = "I" OR "O" )
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE.

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
              MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP
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
      
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "S  A010BANKRUPT." TO VDU.
           MOVE INCLUDE-BANKRUPT TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010LEGAL." TO VDU.
           MOVE INCLUDE-LEGAL TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       LN-DETAIL1 SECTION.
      ******************************************************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           MOVE 1 TO SUB.
           PERFORM PROCESS-BORROWER.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-SLOAN-NUM.

           MOVE HOLD-TODAY-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           IF CPOT30 = "Y"
              MOVE "<P>" TO D-SCONX
           ELSE
           IF CPOT30 = "M"
              MOVE "<M>" TO D-SCONX
           ELSE
           IF CPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-SCONX
           ELSE
           IF CPOTCHGOFF = "M"
              MOVE "<PL>" TO D-SCONX
           ELSE
              MOVE CONTRACTUAL TO D-SCON.

           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-SLNDATE.
           IF INCLUDE-LEGAL = "Y"
              MOVE LN-LEGALDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-SDATE
              MOVE "L" TO D-STYPE.
           IF INCLUDE-BANKRUPT = "Y"
              MOVE LN-BNKRPTDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-SDATE
              MOVE "B" TO D-STYPE
              IF LN-BNKRPTDATE = 0
                 MOVE LN-LEGALDATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO D-SDATE
                 MOVE "L" TO D-STYPE.

           IF GP-CPCORP-CURBAL = "Y"
              IF GP-INCLUDE-O2-IN-BAL = "Y"
                 MOVE LN-CURBAL TO HOLD-BALANCE
              ELSE
                 COMPUTE HOLD-BALANCE = LN-CURBAL - LN-OT2BAL
              END-IF
           ELSE
              IF GP-INCLUDE-O2-IN-BAL = "Y"
                 COMPUTE HOLD-BALANCE = LN-CURBAL + LN-OTHBAL
                                   + LN-INTBAL + LN-LCBAL
              ELSE
                 COMPUTE HOLD-BALANCE = LN-CURBAL + LN-OTHBAL
                                   + LN-INTBAL + LN-LCBAL
                                   - LN-OT2BAL.

           MOVE HOLD-BALANCE TO D-SBALANCE.
           MOVE LN-BNKRPT-CHAPTER TO  D-SCHAPTER.

           PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO T-NOLOANS(2,1).
           ADD HOLD-BALANCE TO T-BALANCE(2,1).
           IF LN-BNKRPT-CHAPTER = 7
              ADD 1 TO T-NOLOANS(2,2)
              ADD HOLD-BALANCE TO T-BALANCE(2,2)
           ELSE
           IF LN-BNKRPT-CHAPTER = 11
              ADD 1 TO T-NOLOANS(2,3)
              ADD HOLD-BALANCE TO T-BALANCE(2,3)
           ELSE
           IF LN-BNKRPT-CHAPTER = 12
              ADD 1 TO T-NOLOANS(2,4)
              ADD HOLD-BALANCE TO T-BALANCE(2,4)
           ELSE
           IF LN-BNKRPT-CHAPTER = 13
              ADD 1 TO T-NOLOANS(2,5)
              ADD HOLD-BALANCE TO T-BALANCE(2,5).

           PERFORM CREATE-WK.

       EXIT-LN-DETAIL.
           EXIT.

      *****************************************
       CREATE-WK SECTION.
      *****************************************

           MOVE BR-NO TO SAVE-WK-BR.
           MOVE LN-CLASS TO SAVE-WK-CL.

       SAVE-WORK-KEY.
           MOVE SAVE-WK-KEY TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              GO TO CREATE-WK-UPDATE.

           MOVE " " TO WK-REC.
           MOVE SAVE-WK-KEY TO WK-KEY.

           MOVE 0 TO WK-NOLOANS WK-7-NOLOANS WK-11-NOLOANS
                                WK-12-NOLOANS WK-13-NOLOANS.
           MOVE 0 TO WK-BALANCE WK-7-BALANCE WK-11-BALANCE
                                WK-12-BALANCE WK-13-BALANCE.

           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SAVE-WORK-KEY.

       CREATE-WK-UPDATE.
           ADD 1 TO WK-NOLOANS.
           ADD HOLD-BALANCE TO WK-BALANCE.
           IF LN-BNKRPT-CHAPTER = 7
              ADD 1 TO WK-7-NOLOANS
              ADD HOLD-BALANCE TO WK-7-BALANCE
           ELSE
           IF LN-BNKRPT-CHAPTER = 11
              ADD 1 TO WK-11-NOLOANS
              ADD HOLD-BALANCE TO WK-11-BALANCE
           ELSE
           IF LN-BNKRPT-CHAPTER = 12
              ADD 1 TO WK-12-NOLOANS
              ADD HOLD-BALANCE TO WK-12-BALANCE
           ELSE
           IF LN-BNKRPT-CHAPTER = 13
              ADD 1 TO WK-13-NOLOANS
              ADD HOLD-BALANCE TO WK-13-BALANCE.

           PERFORM REWRITE-WK-FILE.

       EXIT-CREATE-WK.
           EXIT.

      ****************************************************
       BRANCH-TOTALS SECTION.
      ****************************************************

           IF ( GTT-ISO = "O" )
              GO TO END-BRANCH-TOTALS.

           MOVE "BRANCH TOTALS:" TO G-TOTNAME.

           MOVE "CHAPTER  7 :" TO G-HEADING1.
           MOVE T-NOLOANS(2,2) TO G-LOANS.
           MOVE T-BALANCE(2,2) TO G-TOTBAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 11 :" TO G-HEADING1.
           MOVE T-NOLOANS(2,3) TO G-LOANS.
           MOVE T-BALANCE(2,3) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 12 :" TO G-HEADING1.
           MOVE T-NOLOANS(2,4) TO G-LOANS.
           MOVE T-BALANCE(2,4) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 13 :" TO G-HEADING1.
           MOVE T-NOLOANS(2,5) TO G-LOANS.
           MOVE T-BALANCE(2,5) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALL LOANS  :" TO G-HEADING1.
           MOVE T-NOLOANS(2,1) TO G-LOANS.
           MOVE T-BALANCE(2,1) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

       END-BRANCH-TOTALS.
           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              ADD T-NOLOANS(2,SUB1) TO T-NOLOANS(3,SUB1)
              ADD T-BALANCE(2,SUB1) TO T-BALANCE(3,SUB1)
           END-PERFORM.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              MOVE 0 TO T-NOLOANS(2,SUB1) T-BALANCE(2,SUB1)
           END-PERFORM.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *****************************
       GRAND-TOTALS SECTION.
      *****************************

           IF ( GTT-ISO = "O" )
              GO TO END-GRAND-TOTALS.

           MOVE "GRAND  TOTALS:" TO G-TOTNAME.

           MOVE "CHAPTER  7 :" TO G-HEADING1.
           MOVE T-NOLOANS(3,2) TO G-LOANS.
           MOVE T-BALANCE(3,2) TO G-TOTBAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 11 :" TO G-HEADING1.
           MOVE T-NOLOANS(3,3) TO G-LOANS.
           MOVE T-BALANCE(3,3) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 12 :" TO G-HEADING1.
           MOVE T-NOLOANS(3,4) TO G-LOANS.
           MOVE T-BALANCE(3,4) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 13 :" TO G-HEADING1.
           MOVE T-NOLOANS(3,5) TO G-LOANS.
           MOVE T-BALANCE(3,5) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALL LOANS  :" TO G-HEADING1.
           MOVE T-NOLOANS(3,1) TO G-LOANS.
           MOVE T-BALANCE(3,1) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

       END-GRAND-TOTALS.
           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              MOVE 0 TO T-NOLOANS(3,SUB1) T-BALANCE(3,SUB1)
           END-PERFORM.
           MOVE 3 TO SUB.

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************
           ADD WK-NOLOANS TO WS-GTT-NOLOANS (GTT-TOTAL-SUB,1).
           ADD WK-BALANCE TO WS-GTT-BALANCE (GTT-TOTAL-SUB,1).
           ADD WK-7-NOLOANS TO WS-GTT-NOLOANS(GTT-TOTAL-SUB,2).
           ADD WK-7-BALANCE TO WS-GTT-BALANCE(GTT-TOTAL-SUB,2).
           ADD WK-11-NOLOANS TO WS-GTT-NOLOANS(GTT-TOTAL-SUB,3).
           ADD WK-11-BALANCE TO WS-GTT-BALANCE(GTT-TOTAL-SUB,3).
           ADD WK-12-NOLOANS TO WS-GTT-NOLOANS(GTT-TOTAL-SUB,4).
           ADD WK-12-BALANCE TO WS-GTT-BALANCE(GTT-TOTAL-SUB,4).
           ADD WK-13-NOLOANS TO WS-GTT-NOLOANS(GTT-TOTAL-SUB,5).
           ADD WK-13-BALANCE TO WS-GTT-BALANCE(GTT-TOTAL-SUB,5).

      ***********************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      ***********************************
           MOVE SPACES      TO WK-KEY.
           MOVE WGR-SRESULT TO WK-BR.
           PERFORM START-WK-FILE.

       GTT-NEXT-WK1.
           PERFORM READ-WK-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR
              ( WK-BR NOT = WGR-SRESULT )
                 GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           MOVE "N" TO GTT-FIRST-TIME.
           PERFORM TRAP-INTERRUPT.

           PERFORM GTT-CHECK-ADD-TOTALS.

           GO TO GTT-NEXT-WK1.

       EXIT-GTT-COMPILE-GROUP-TOTALS.
           EXIT.

      ************************************
       GTT-PRINT-GROUP-BR-TOTALS SECTION.
      ************************************

           MOVE WGR-SID(GTT-PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB              TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "BRANCH #&&&& NOT ON FILE" TO BR-NAME
              INSPECT BR-NAME REPLACING FIRST "&&&&" BY BR-NO.

           MOVE SPACES    TO DETAIL-LINE.
           MOVE 9         TO GTT-TOTAL-SUB.
           MOVE "BRANCH:" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE BR-NO     TO G-BRNO.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      *********************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      *********************************

           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP :" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE GR-ID     TO G-GRNO.
           MOVE GR-LEVEL  TO G-GR-LEVEL.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-TOTALS.
           EXIT.

      *****************************
       GTT-PRINT-TOTALS SECTION.
      *****************************

           MOVE "CHAPTER  7 :" TO G-HEADING1.
           MOVE WS-GTT-NOLOANS(GTT-TOTAL-SUB,2) TO G-LOANS.
           MOVE WS-GTT-BALANCE(GTT-TOTAL-SUB,2) TO G-TOTBAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 11 :" TO G-HEADING1.
           MOVE WS-GTT-NOLOANS(GTT-TOTAL-SUB,3) TO G-LOANS.
           MOVE WS-GTT-BALANCE(GTT-TOTAL-SUB,3) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 12 :" TO G-HEADING1.
           MOVE WS-GTT-NOLOANS(GTT-TOTAL-SUB,4) TO G-LOANS.
           MOVE WS-GTT-BALANCE(GTT-TOTAL-SUB,4) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 13 :" TO G-HEADING1.
           MOVE WS-GTT-NOLOANS(GTT-TOTAL-SUB,5) TO G-LOANS.
           MOVE WS-GTT-BALANCE(GTT-TOTAL-SUB,5) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALL LOANS  :" TO G-HEADING1.
           MOVE WS-GTT-NOLOANS(GTT-TOTAL-SUB,1) TO G-LOANS.
           MOVE WS-GTT-BALANCE(GTT-TOTAL-SUB,1) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           IF ( WS-HOLD-PROMPT = "GROUP :" )
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

           MOVE LOW-VALUES TO WS-GTT-TABLE-GROUP-LEVEL(GTT-TOTAL-SUB).

       EXIT-GTT-PRINT-TOTALS.
           EXIT.

      ****************************
       PROCESS-BORROWER SECTION.
      ****************************
           MOVE SPACES TO D-SNAME.

           MOVE LN-SSNO(SUB) TO BW-SSNO.

           IF BW-SSNO = 0
              GO TO END-PROCESS-BORROWER.

           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.

           PERFORM READ-BW1-FILE.
           IF IO-BAD
               MOVE "**NOT ON FILE**" TO D-SNAME
               GO TO END-PROCESS-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".

           INSPECT NAME-WORKER REPLACING FIRST "?               "
               BY FIRST-NAME.

           MOVE NAME-WORKER TO D-SNAME.

       END-PROCESS-BORROWER.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
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
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE HEAD5 TO PR-REC
           MOVE 2 TO LN-FEED
           PERFORM WRITE-PR-REC.
           MOVE HEAD6 TO PR-REC
           MOVE 1 TO LN-FEED
           PERFORM WRITE-PR-REC.
           MOVE 4 TO LN-COUNT
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
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
      
           MOVE "BEG CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "END CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEG DATE" TO RANGE-LINE.
           MOVE BEG-DATE TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "END DATE" TO RANGE-LINE.
           MOVE END-DATE TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE BANKRUPTS" TO RANGE-LINE.
           MOVE INCLUDE-BANKRUPT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "INCLUDE LEGALS" TO RANGE-LINE.
           MOVE INCLUDE-LEGAL TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".

       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBLP/ARRAYSP.CPY".

      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO BNKLG2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BNKLG2-SYSDATE.
           DISPLAY BNKLG2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BNKLG2-FORM-FIELDS TO WR-BUF.
           MOVE BNKLG2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE BNKLG2-HELP--FILE TO HELP-LINK-FILE.
           MOVE BNKLG2-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BNKLG2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SET-BRANCH-HEADINGS.
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE HOLD-BRNAME TO H-BRDESC.
       NUMBW-TO-ALPBW.
           MOVE NUM-BW1 TO ALP-BW1.
           MOVE NUM-BW2 TO ALP-BW2.
           MOVE NUM-BW3 TO ALP-BW3.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.

      ******************************************************
       IO-ROUTINES SECTION.
      ******************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBPRINT.CPY".

      * COPY "LIBLP/LPBW1RN.CPY".
      * COPYMEMBER: LIBLP/LPBW1RN
       LOAD-BW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BWFILE NOT = "S" )
              MOVE BW-PATH-BEG             TO BW-SELECT-PATH-BEG
              MOVE BW-PATH-FILE            TO BW-SELECT-PATH-FILE
           ELSE
              MOVE BW-PATH-SQL             TO BW-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BW1-FILE.
           PERFORM LOAD-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE BW-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BW1-FILE.
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           IF ( BW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW1-FILE.

           IF ( QBW1-WEND-SSNO = ZEROES    ) OR
              ( QBW1-WEND-SSNO NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW1-WBEG-OWNBR
                                           QBW1-WEND-OWNBR
              MOVE BW-SSNO              TO QBW1-WBEG-SSNO
              MOVE ALL "9"              TO QBW1-WEND-SSNO.

           EXEC SQL
            DECLARE BW1_CURSOR CURSOR FOR
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   ISNULL(BWFILE.BW_LNAME,''),
                   ISNULL(BWFILE.BW_FNAME,''),
                   ISNULL(BWFILE.BW_MNAME,''),
                   ISNULL(BWFILE.BW_NICKNAME,''),
                   ISNULL(BWFILE.BW_GENERATION,''),
                   ISNULL(BWFILE.BW_ADR1,''),
                   ISNULL(BWFILE.BW_ADR2,''),
                   ISNULL(BWFILE.BW_CITY,''),
                   ISNULL(BWFILE.BW_STATE,''),
                   ISNULL(BWFILE.BW_ZIP,''),
                   ISNULL(CAST(BWFILE.BW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_BDATE AS VARCHAR(10)),'0'),
                   BWFILE.BW_PHONE,
                   BWFILE.BW_CELL_PHONE,
                   ISNULL(BWFILE.BW_NATIONALITY,''),
                   ISNULL(BWFILE.BW_GENDER,''),
                   BWFILE.BW_DEPEND,
                   ISNULL(BWFILE.BW_RENTCD,''),
                   ISNULL(BWFILE.BW_TIMEADDR,''),
                   ISNULL(BWFILE.BW_CREDITCD,''),
                   ISNULL(BWFILE.BW_STRTGCD,''),
                   ISNULL(BWFILE.BW_DWELLCD,''),
                   ISNULL(BWFILE.BW_COUNTY,''),
                   ISNULL(BWFILE.BW_ORIGIN,''),
                   ISNULL(BWFILE.BW_CENSUSCD,''),
                   BWFILE.BW_ORGSLSMN,
                   ISNULL(BWFILE.BW_REPCODE,''),
                   BWFILE.BW_SPSSNO,
                   ISNULL(BWFILE.BW_SPLNAME,''),
                   ISNULL(BWFILE.BW_SPFNAME,''),
                   ISNULL(CAST(BWFILE.BW_SP_BDATE AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMPLOYER,''),
                   ISNULL(BWFILE.BW_EMPLPOS,''),
                   BWFILE.BW_EMPPHONE,
                   BWFILE.BW_EMPEXT,
                   BWFILE.BW_EMPYEARS,
                   BWFILE.BW_EMPPAY,
                   BWFILE.BW_EMPPAYOT,
                   ISNULL(BWFILE.BW_EMPPAYSR,''),
                   ISNULL(BWFILE.BW_EMPADDR1,''),
                   ISNULL(BWFILE.BW_EMPADDR2,''),
                   ISNULL(BWFILE.BW_EMPCITY,''),
                   ISNULL(BWFILE.BW_EMPSTATE,''),
                   ISNULL(BWFILE.BW_EMPZIP,''),
                   ISNULL(BWFILE.BW_WORKHRS,''),
                   BWFILE.BW_LOANS,
                   BWFILE.BW_INTYTD,
                   BWFILE.BW_INTLYR,
                   BWFILE.BW_LIASMLN,
                   BWFILE.BW_LIAOTH,
                   ISNULL(CAST(BWFILE.BW_LASTPODATE
                                                  AS VARCHAR(10)),'0'),
                   BWFILE.BW_PRLOANS,
                   BWFILE.BW_RE_INTYTD,
                   BWFILE.BW_RE_INTLYR,
                   BWFILE.BW_CREDIT_LIMIT,
                   BWFILE.BW_CREDIT_SCORE,
                   BWFILE.BW_CRED_SCORE_1,
                   BWFILE.BW_CRED_SCORE_2,
                   BWFILE.BW_CRED_SCORE_3,
                   BWFILE.BW_CRED_SCORE_4,
                   BWFILE.BW_BNKRPT_SCORE_1,
                   BWFILE.BW_BNKRPT_SCORE_2,
                   BWFILE.BW_BNKRPT_SCORE_3,
                   BWFILE.BW_BNKRPT_SCORE_4,
                   BWFILE.BW_LAST_FORM_NO,
                   ISNULL(CAST(BWFILE.BW_LAST_FORM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_ADR_PHONE_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_BAD_ADR_FG,''),
                   ISNULL(BWFILE.BW_BAD_PHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_EMPPHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE1_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE2_FG,''),
                   ISNULL(BWFILE.BW_BAD_CELL_FG,''),
                   BWFILE.BW_MISC_PHONE_1,
                   BWFILE.BW_MISC_EXT_1,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_1,''),
                   BWFILE.BW_MISC_PHONE_2,
                   BWFILE.BW_MISC_EXT_2,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_2,''),
                   BWFILE.BW_MISC_PHONE_3,
                   BWFILE.BW_MISC_EXT_3,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_3,''),
                   BWFILE.BW_MISC_PHONE_4,
                   BWFILE.BW_MISC_EXT_4,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_4,''),
                   BWFILE.BW_MISC_PHONE_5,
                   BWFILE.BW_MISC_EXT_5,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_5,''),
                   BWFILE.BW_MISC_PHONE_6,
                   BWFILE.BW_MISC_EXT_6,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_1,''),
                   BWFILE.BW_CONTACT_PHONE_1,
                   BWFILE.BW_CONTACT_EXT_1,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_1,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_2,''),
                   BWFILE.BW_CONTACT_PHONE_2,
                   BWFILE.BW_CONTACT_EXT_2,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_2,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_3,''),
                   BWFILE.BW_CONTACT_PHONE_3,
                   BWFILE.BW_CONTACT_EXT_3,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_3,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_4,''),
                   BWFILE.BW_CONTACT_PHONE_4,
                   BWFILE.BW_CONTACT_EXT_4,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_4,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_5,''),
                   BWFILE.BW_CONTACT_PHONE_5,
                   BWFILE.BW_CONTACT_EXT_5,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_5,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_6,''),
                   BWFILE.BW_CONTACT_PHONE_6,
                   BWFILE.BW_CONTACT_EXT_6,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_7,''),
                   BWFILE.BW_CONTACT_PHONE_7,
                   BWFILE.BW_CONTACT_EXT_7,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_7,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_8,''),
                   BWFILE.BW_CONTACT_PHONE_8,
                   BWFILE.BW_CONTACT_EXT_8,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_8,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_9,''),
                   BWFILE.BW_CONTACT_PHONE_9,
                   BWFILE.BW_CONTACT_EXT_9,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_9,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_10,''),
                   BWFILE.BW_CONTACT_PHONE_10,
                   BWFILE.BW_CONTACT_EXT_10,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_10,''),
                   ISNULL(BWFILE.BW_SRCD,''),
                   ISNULL(BWFILE.BW_CALL_ON_SUNDAY,''),
                   ISNULL(BWFILE.BW_SOLICIT,''),
                   ISNULL(BWFILE.BW_CONFIDENTIAL,''),
                   ISNULL(BWFILE.BW_PRIVACY_OPT_OUT,''),
                   ISNULL(BWFILE.BW_OPT_IN_EMAIL,''),
                   ISNULL(BWFILE.BW_OPT_IN_TEXT,''),
                   ISNULL(BWFILE.BW_CITY_TAX_CD,''),
                   ISNULL(CAST(BWFILE.BW_DEALTRACK_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_DECEASED,''),
                   ISNULL(CAST(BWFILE.BW_DECEASED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMAIL,''),
                   ISNULL(BWFILE.BW_IDENT_NO,''),
                   ISNULL(BWFILE.BW_IDENT_TYPE,''),
                   ISNULL(BWFILE.BW_IDENT_STATE,''),
                   ISNULL(BWFILE.BW_MILITARY_FLAG,''),
                   BWFILE.BW_ORGBR
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR >= :QBW1-WBEG-OWNBR
              AND BWFILE.BW_OWNBR <= :QBW1-WEND-OWNBR
              AND BWFILE.BW_SSNO >= :QBW1-WBEG-SSNO
              AND BWFILE.BW_SSNO <= :QBW1-WEND-SSNO
              ORDER BY BWFILE.BW_OWNBR,
                       BWFILE.BW_SSNO
           END-EXEC.

           EXEC SQL
               OPEN BW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW1-WBEG-OWNBR 
                          QBW1-WEND-OWNBR
                          QBW1-WBEG-SSNO
                          QBW1-WEND-SSNO.

           MOVE STAT---GOOD TO BW1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BW1-FILE.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR. 
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           MOVE BW-OWNBR   TO QBW-OWNBR.
           MOVE BW-SSNO    TO QBW-SSNO.

           EXEC SQL
            SELECT  BWFILE.BW_OWNBR,
                    BWFILE.BW_SSNO,
                    BWFILE.BW_LNAME,
                    BWFILE.BW_FNAME,
                    BWFILE.BW_SOLICIT
              INTO 
                   :QBW-OWNBR,
                   :QBW-SSNO,
                   :QBW-LNAME,
                   :QBW-FNAME,
                   :QBW-SOLICIT
              FROM DBO.BWFILE
             WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
               AND BWFILE.BW_SSNO = :QBW-SSNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       READ-BW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BW1_CURSOR INTO :QBW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           PERFORM CLOSE-IT.
           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BW1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BW1-CURSOR-STAT.


       COPY "LIBLP/LPSP1RN.CPY".

      * COPY "LIBLP/LPLN1RN.CPY".
      * COPYMEMBER: LIBLP/LPLN1RN
       LOAD-LN1-FILE.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LNFILE NOT = "S" )
              MOVE LN-PATH-BEG             TO LN-SELECT-PATH-BEG
              MOVE LN-PATH-FILE            TO LN-SELECT-PATH-FILE
           ELSE
              MOVE LN-PATH-SQL             TO LN-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LN1-FILE.
           PERFORM LOAD-LN1-FILE.
           PERFORM OPEN-IT.
           MOVE LN-SELECT-PATH TO E-FILE.

           EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.


      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN1-FILE.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           IF ( LN1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN1-FILE.

           IF ( QLN1-WEND-OWNBR = ZEROES    ) OR
              ( QLN1-WEND-OWNBR NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN1-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN1-WEND-OWNBR
              MOVE LN-ACCTNO            TO QLN1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN1-WEND-ACCTNO.

           EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN1_CURSOR CURSOR FOR
                :RET-CODE = 
                 EXEC SP_RZKLG3_LNFILE_Select
              ( :QLN1-WBEG-OWNBR,  :QLN1-WEND-OWNBR, 
                :QLN1-WBEG-ACCTNO, :QLN1-WEND-ACCTNO )
           END-EXEC.

      *     EXEC SQL
      *      DECLARE LN1_CURSOR CURSOR FOR
      *      SELECT LNFILE.LN_OWNBR,
      *             LNFILE.LN_ACCTNO,
      *             LNFILE.LN_CLASS,
      *             LNFILE.LN_SUBCLASS,
      *             LNFILE.LN_LAWCODE,
      *             LNFILE.LN_SSNO1,
      *             LNFILE.LN_MAKERCD1,
      *             LNFILE.LN_SSNO2,
      *             LNFILE.LN_MAKERCD2,
      *             LNFILE.LN_SSNO3,
      *             LNFILE.LN_MAKERCD3,
      *             LNFILE.LN_SSNO4,
      *             LNFILE.LN_MAKERCD4,
      *             LNFILE.LN_ORGST,
      *             LNFILE.LN_SPRCLASS,
      *             LNFILE.LN_POCD,
      *             LNFILE.LN_PLCD,
      *             LNFILE.LN_PAY_SCHLD_FG,
      *             LNFILE.LN_STATFLAGS,
      *             CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)), 
      *             CAST(LNFILE.LN_ORIG_1STPYDATE
      *                                     AS VARCHAR(10)), 
      *             CAST(LNFILE.LN_1STPYDATE
      *                                     AS VARCHAR(10)), 
      *             CAST(LNFILE.LN_LEGALDATE
      *                                     AS VARCHAR(10)),
      *             CAST(LNFILE.LN_BNKRPTDATE
      *                                     AS VARCHAR(10)),
      *             LNFILE.LN_ORGTERM,
      *             LNFILE.LN_1STPYAMT,
      *             LNFILE.LN_NOREGPY,
      *             LNFILE.LN_REGPYAMT,
      *             LNFILE.LN_LASTPYAMT,
      *             LNFILE.LN_PY_YYYYMM_RECDEL,
      *             LNFILE.LN_MTD_PAYS_RECDEL,
      *             LNFILE.LN_CURBAL,
      *             LNFILE.LN_INTBAL,
      *             LNFILE.LN_LCBAL,
      *             LNFILE.LN_OTHBAL,
      *             LNFILE.LN_OT2BAL,
      *             LNFILE.LN_TOTPAYMNTD,
      *             LNFILE.LN_TOTNODEF,
      *             LNFILE.LN_TOTNOALDEL,
      *             CAST(LNFILE.LN_DATE_PAID_LAST
      *                                     AS VARCHAR(10)), 
      *             LNFILE.LN_SUM_ORGTERM,
      *             LNFILE.LN_UNITPER_CD,
      *             LNFILE.LN_UNITPER_FREQ,
      *             LNFILE.LN_BNKRPT_CHAPTER
      *       FROM DBO.LNFILE
      *      WHERE LNFILE.LN_OWNBR >= :QLN1-WBEG-OWNBR
      *        AND LNFILE.LN_OWNBR <= :QLN1-WEND-OWNBR
      *        AND LNFILE.LN_ACCTNO >= :QLN1-WBEG-ACCTNO
      *        AND LNFILE.LN_ACCTNO <= :QLN1-WEND-ACCTNO 
      *      ORDER BY LNFILE.LN_OWNBR,
      *               LNFILE.LN_ACCTNO
      *     END-EXEC.

           EXEC SQL
               OPEN LN1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN1-WBEG-OWNBR
                          QLN1-WEND-OWNBR
                          QLN1-WBEG-ACCTNO
                          QLN1-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN1-FILE-GREATER.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           IF ( LN1-CURSOR-GR-STAT-GOOD )
              PERFORM CLOSE-LN1-FILE.

           IF ( QLN1-WEND-OWNBR = ZEROES    ) OR
              ( QLN1-WEND-OWNBR NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN1-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN1-WEND-OWNBR
              MOVE LN-ACCTNO            TO QLN1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN1-WEND-ACCTNO.

           EXEC SQL
            DECLARE LN1_CURSOR_GR CURSOR FOR
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   ISNULL(LNFILE.LN_MAKERCD1,''),
                   LNFILE.LN_SSNO2,
                   ISNULL(LNFILE.LN_MAKERCD2,''),
                   LNFILE.LN_SSNO3,
                   ISNULL(LNFILE.LN_MAKERCD3,''),
                   LNFILE.LN_SSNO4,
                   ISNULL(LNFILE.LN_MAKERCD4,''),
                   ISNULL(LNFILE.LN_ORGST,''),
                   LNFILE.LN_ORGBR,
                   ISNULL(LNFILE.LN_OWNST,''),
                   LNFILE.LN_SPRCLASS,
                   ISNULL(LNFILE.LN_CONVERCD,''),
                   ISNULL(LNFILE.LN_MIDSTREAM,''),
                   ISNULL(LNFILE.LN_PURCD,''),
                   ISNULL(LNFILE.LN_SRCD,''),
                   ISNULL(LNFILE.LN_COLLCD,''),
                   ISNULL(LNFILE.LN_COLLID,''),
                   ISNULL(LNFILE.LN_POCD,''),
                   ISNULL(LNFILE.LN_PLCD,''),
                   ISNULL(LNFILE.LN_PLTYPE,''),
                   ISNULL(LNFILE.LN_CONFIDENTCD,''),
                   ISNULL(LNFILE.LN_ALLOTCD,''),
                   ISNULL(LNFILE.LN_RECASTCD,''),
                   ISNULL(LNFILE.LN_SKIP_TRW,''),
                   ISNULL(LNFILE.LN_STATUSFG,''),
                   ISNULL(LNFILE.LN_SMALLOAN,''),
                   ISNULL(LNFILE.LN_SALEFIN,''),
                   ISNULL(LNFILE.LN_SERVACCT,''),
                   ISNULL(LNFILE.LN_LOANTYPE,''),
                   ISNULL(LNFILE.LN_ADDON_FG,''),
                   ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
                   LNFILE.LN_LC_CODE,
                   ISNULL(LNFILE.LN_CAN_LOAN_NO,''),
                   ISNULL(LNFILE.LN_STATFLAGS,''),
                   ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REPODATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_SOLDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APRATE,
                   LNFILE.LN_EFFRATE,
                   LNFILE.LN_SMPRATE,
                   LNFILE.LN_YLDRATE,
                   LNFILE.LN_OVRDRATE,
                   LNFILE.LN_ENDORATE,
                   LNFILE.LN_JDRATE,
                   LNFILE.LN_ACCRATE,
                   LNFILE.LN_REDURATE,
                   LNFILE.LN_CONRATE,
                   LNFILE.LN_SERVPER,
                   LNFILE.LN_SELLPR,
                   LNFILE.LN_DOWNPY,
                   LNFILE.LN_TRADEIN,
                   LNFILE.LN_LNAMT,
                   LNFILE.LN_FINCHG,
                   LNFILE.LN_INTCHG,
                   LNFILE.LN_SERCHG,
                   LNFILE.LN_EXTCHG,
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_POINTS,
                   LNFILE.LN_PROCEEDS,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PROCASH,
                   LNFILE.LN_TILLNO,
                   ISNULL(LNFILE.LN_STATE_FLAG,''),
                   LNFILE.LN_STATE_WORKER,
                   LNFILE.LN_STATE_WORKER2,
                   ISNULL(LNFILE.LN_FEECODE_1,''),
                   LNFILE.LN_FEEAMT_1,
                   ISNULL(LNFILE.LN_FEEOURS_1,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_1,''),
                   ISNULL(LNFILE.LN_FEECODE_2,''),
                   LNFILE.LN_FEEAMT_2,
                   ISNULL(LNFILE.LN_FEEOURS_2,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_2,''),
                   ISNULL(LNFILE.LN_FEECODE_3,''),
                   LNFILE.LN_FEEAMT_3,
                   ISNULL(LNFILE.LN_FEEOURS_3,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_3,''),
                   ISNULL(LNFILE.LN_FEECODE_4,''),
                   LNFILE.LN_FEEAMT_4,
                   ISNULL(LNFILE.LN_FEEOURS_4,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_4,''),
                   ISNULL(LNFILE.LN_FEECODE_5,''),
                   LNFILE.LN_FEEAMT_5,
                   ISNULL(LNFILE.LN_FEEOURS_5,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_5,''),
                   ISNULL(LNFILE.LN_FEECODE_6,''),
                   LNFILE.LN_FEEAMT_6,
                   ISNULL(LNFILE.LN_FEEOURS_6,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_6,''),
                   ISNULL(LNFILE.LN_FEECODE_7,''),
                   LNFILE.LN_FEEAMT_7,
                   ISNULL(LNFILE.LN_FEEOURS_7,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_7,''),
                   ISNULL(LNFILE.LN_FEECODE_8,''),
                   LNFILE.LN_FEEAMT_8,
                   ISNULL(LNFILE.LN_FEEOURS_8,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_8,''),
                   ISNULL(LNFILE.LN_FEECODE_9,''),
                   LNFILE.LN_FEEAMT_9,
                   ISNULL(LNFILE.LN_FEEOURS_9,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_9,''),
                   ISNULL(LNFILE.LN_FEECODE_10,''),
                   LNFILE.LN_FEEAMT_10,
                   ISNULL(LNFILE.LN_FEEOURS_10,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_10,''),
                   ISNULL(LNFILE.LN_FEECODE_11,''),
                   LNFILE.LN_FEEAMT_11,
                   ISNULL(LNFILE.LN_FEEOURS_11,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_11,''),
                   ISNULL(LNFILE.LN_FEECODE_12,''),
                   LNFILE.LN_FEEAMT_12,
                   ISNULL(LNFILE.LN_FEEOURS_12,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_12,''),
                   ISNULL(LNFILE.LN_FEECODE_13,''),
                   LNFILE.LN_FEEAMT_13,
                   ISNULL(LNFILE.LN_FEEOURS_13,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_13,''),
                   ISNULL(LNFILE.LN_FEECODE_14,''),
                   LNFILE.LN_FEEAMT_14,
                   ISNULL(LNFILE.LN_FEEOURS_14,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_14,''),
                   ISNULL(LNFILE.LN_FEECODE_15,''),
                   LNFILE.LN_FEEAMT_15,
                   ISNULL(LNFILE.LN_FEEOURS_15,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_15,''),
                   ISNULL(LNFILE.LN_FEECODE_16,''),
                   LNFILE.LN_FEEAMT_16,
                   ISNULL(LNFILE.LN_FEEOURS_16,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_16,''),
                   ISNULL(LNFILE.LN_FEECODE_17,''),
                   LNFILE.LN_FEEAMT_17,
                   ISNULL(LNFILE.LN_FEEOURS_17,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_17,''),
                   ISNULL(LNFILE.LN_FEECODE_18,''),
                   LNFILE.LN_FEEAMT_18,
                   ISNULL(LNFILE.LN_FEEOURS_18,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_18,''),
                   ISNULL(LNFILE.LN_FEECODE_19,''),
                   LNFILE.LN_FEEAMT_19,
                   ISNULL(LNFILE.LN_FEEOURS_19,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_19,''),
                   ISNULL(LNFILE.LN_FEECODE_20,''),
                   LNFILE.LN_FEEAMT_20,
                   ISNULL(LNFILE.LN_FEEOURS_20,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_20,''),
                   ISNULL(LNFILE.LN_INSURED,''),
                   ISNULL(LNFILE.LN_INSTYPES,''),
                   ISNULL(LNFILE.LN_INSELIGIBLE,''),
                   LNFILE.LN_INSCOMP_1,
                   ISNULL(LNFILE.LN_INSOURS_1,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_1,
                   LNFILE.LN_INSCOVR_1,
                   LNFILE.LN_INSCOMM_1,
                   LNFILE.LN_INSANTICERN_1,
                   LNFILE.LN_INSANTICADJ_1,
                   LNFILE.LN_INSUFERN_1,
                   LNFILE.LN_INSACCERN_1,
                   LNFILE.LN_INSEARN_ORDER_1,
                   ISNULL(LNFILE.LN_INSFINCHG_1,''),
                   LNFILE.LN_INSCOMP_2,
                   ISNULL(LNFILE.LN_INSOURS_2,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_2,
                   LNFILE.LN_INSCOVR_2,
                   LNFILE.LN_INSCOMM_2,
                   LNFILE.LN_INSANTICERN_2,
                   LNFILE.LN_INSANTICADJ_2,
                   LNFILE.LN_INSUFERN_2,
                   LNFILE.LN_INSACCERN_2,
                   LNFILE.LN_INSEARN_ORDER_2,
                   ISNULL(LNFILE.LN_INSFINCHG_2,''),
                   LNFILE.LN_INSCOMP_3,
                   ISNULL(LNFILE.LN_INSOURS_3,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_3,
                   LNFILE.LN_INSCOVR_3,
                   LNFILE.LN_INSCOMM_3,
                   LNFILE.LN_INSANTICERN_3,
                   LNFILE.LN_INSANTICADJ_3,
                   LNFILE.LN_INSUFERN_3,
                   LNFILE.LN_INSACCERN_3,
                   LNFILE.LN_INSEARN_ORDER_3,
                   ISNULL(LNFILE.LN_INSFINCHG_3,''),
                   LNFILE.LN_INSCOMP_4,
                   ISNULL(LNFILE.LN_INSOURS_4,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_4,
                   LNFILE.LN_INSCOVR_4,
                   LNFILE.LN_INSCOMM_4,
                   LNFILE.LN_INSANTICERN_4,
                   LNFILE.LN_INSANTICADJ_4,
                   LNFILE.LN_INSUFERN_4,
                   LNFILE.LN_INSACCERN_4,
                   LNFILE.LN_INSEARN_ORDER_4,
                   ISNULL(LNFILE.LN_INSFINCHG_4,''),
                   LNFILE.LN_INSCOMP_5,
                   ISNULL(LNFILE.LN_INSOURS_5,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_5,
                   LNFILE.LN_INSCOVR_5,
                   LNFILE.LN_INSCOMM_5,
                   LNFILE.LN_INSANTICERN_5,
                   LNFILE.LN_INSANTICADJ_5,
                   LNFILE.LN_INSUFERN_5,
                   LNFILE.LN_INSACCERN_5,
                   LNFILE.LN_INSEARN_ORDER_5,
                   ISNULL(LNFILE.LN_INSFINCHG_5,''),
                   LNFILE.LN_INSCOMP_6,
                   ISNULL(LNFILE.LN_INSOURS_6,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_6,
                   LNFILE.LN_INSCOVR_6,
                   LNFILE.LN_INSCOMM_6,
                   LNFILE.LN_INSANTICERN_6,
                   LNFILE.LN_INSANTICADJ_6,
                   LNFILE.LN_INSUFERN_6,
                   LNFILE.LN_INSACCERN_6,
                   LNFILE.LN_INSEARN_ORDER_6,
                   ISNULL(LNFILE.LN_INSFINCHG_6,''),
                   LNFILE.LN_INSCOMP_7,
                   ISNULL(LNFILE.LN_INSOURS_7,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_7,
                   LNFILE.LN_INSCOVR_7,
                   LNFILE.LN_INSCOMM_7,
                   LNFILE.LN_INSANTICERN_7,
                   LNFILE.LN_INSANTICADJ_7,
                   LNFILE.LN_INSUFERN_7,
                   LNFILE.LN_INSACCERN_7,
                   LNFILE.LN_INSEARN_ORDER_7,
                   ISNULL(LNFILE.LN_INSFINCHG_7,''),
                   LNFILE.LN_INSCOMP_8,
                   ISNULL(LNFILE.LN_INSOURS_8,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_8,
                   LNFILE.LN_INSCOVR_8,
                   LNFILE.LN_INSCOMM_8,
                   LNFILE.LN_INSANTICERN_8,
                   LNFILE.LN_INSANTICADJ_8,
                   LNFILE.LN_INSUFERN_8,
                   LNFILE.LN_INSACCERN_8,
                   LNFILE.LN_INSEARN_ORDER_8,
                   ISNULL(LNFILE.LN_INSFINCHG_8,''),
                   LNFILE.LN_INSCOMP_9,
                   ISNULL(LNFILE.LN_INSOURS_9,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_9,
                   LNFILE.LN_INSCOVR_9,
                   LNFILE.LN_INSCOMM_9,
                   LNFILE.LN_INSANTICERN_9,
                   LNFILE.LN_INSANTICADJ_9,
                   LNFILE.LN_INSUFERN_9,
                   LNFILE.LN_INSACCERN_9,
                   LNFILE.LN_INSEARN_ORDER_9,
                   ISNULL(LNFILE.LN_INSFINCHG_9,''),
                   LNFILE.LN_INSCOMP_10,
                   ISNULL(LNFILE.LN_INSOURS_10,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_10,
                   LNFILE.LN_INSCOVR_10,
                   LNFILE.LN_INSCOMM_10,
                   LNFILE.LN_INSANTICERN_10,
                   LNFILE.LN_INSANTICADJ_10,
                   LNFILE.LN_INSUFERN_10,
                   LNFILE.LN_INSACCERN_10,
                   LNFILE.LN_INSEARN_ORDER_10,
                   ISNULL(LNFILE.LN_INSFINCHG_10,''),
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   ISNULL(LNFILE.LN_GARNISH_EXTR,''),
                   ISNULL(LNFILE.LN_SHIP_SAME_FG,''),
                   LNFILE.LN_AD_D_INS_COVR,
                   LNFILE.LN_TOT_PARTIAL_CHGOFF,
                   ISNULL(LNFILE.LN_LPO,''),
                   ISNULL(LNFILE.LN_REP_CD,''),
                   LNFILE.LN_EMP_ID,
                   ISNULL(LNFILE.LN_PROCESS_CD,''),
                   LNFILE.LN_WCBC_CREDITLIM,
                   ISNULL(LNFILE.LN_FOLLOWUP_CD,''),
                   ISNULL(LNFILE.LN_MIP_FG,''),
                   LNFILE.LN_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_CLPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_AHPAID,
                   LNFILE.LN_IL_GROSS_INCOME,
                   LNFILE.LN_IL_GROSS_INCOME2,
                   ISNULL(LNFILE.LN_RESCISSION,''),
                   ISNULL(LNFILE.LN_INTERIM_TRW,''),
                   LNFILE.LN_DLNO,
                   LNFILE.LN_DLPOOL,
                   ISNULL(LNFILE.LN_DLRECOURSE,''),
                   LNFILE.LN_DLPARTRCAMT,
                   LNFILE.LN_ORIGPURAMT,
                   LNFILE.LN_PURBAL,
                   LNFILE.LN_DLRVPRIN_1,
                   LNFILE.LN_DLRVPRIN_2,
                   LNFILE.LN_DLRVPRIN_3,
                   LNFILE.LN_DLRVCHGS_1,
                   LNFILE.LN_DLRVCHGS_2,
                   LNFILE.LN_DLRVCHGS_3,
                   LNFILE.LN_DLPARVHELD,
                   LNFILE.LN_DLPARVPAID,
                   LNFILE.LN_DLDISC,
                   ISNULL(CAST(LNFILE.LN_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_LCPAID,
                   LNFILE.LN_LCPARTIALS,
                   LNFILE.LN_STMT_COUNTER,
                   LNFILE.LN_PRIOR_STMT_COUNTER,
                   ISNULL(LNFILE.LN_FROZEN_REASON,''),
                   ISNULL(LNFILE.LN_COUPON_STATEMENT,''),
                   LNFILE.LN_MONTHEND_BAL,
                   LNFILE.LN_ANTICTERM,
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_UFERN_1,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_EARN_ORDER_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_UFERN_2,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_EARN_ORDER_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_UFERN_3,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_EARN_ORDER_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_UFERN_4,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_EARN_ORDER_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_UFERN_5,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_EARN_ORDER_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_UFERN_6,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_EARN_ORDER_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_UFERN_7,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_EARN_ORDER_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_UFERN_8,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_EARN_ORDER_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_UFERN_9,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_EARN_ORDER_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_UFERN_10,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_EARN_ORDER_10,
                   LNFILE.LN_LMO_PRIOR_INTEREST,
                   LNFILE.LN_PRIOR_UNEARNED,
                   ISNULL(LNFILE.LN_ERNDELQ_STOP,''),
                   LNFILE.LN_NONACRERN,
                   LNFILE.LN_PRIOR_NONACRERN,
                   ISNULL(CAST(LNFILE.LN_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_ERN_IB_PRINBAL,
                   LNFILE.LN_ERN_IB_INTOWE,
                   LNFILE.LN_TOTERN,
                   LNFILE.LN_TOTOTH,
                   LNFILE.LN_TOTINT,
                   LNFILE.LN_TOTLCHG,
                   LNFILE.LN_TOTDEF,
                   LNFILE.LN_TOTSERACCT,
                   LNFILE.LN_LCWAIVERS,
                   LNFILE.LN_INTWAIVERS,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTEXCPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_YTDNODEF,
                   LNFILE.LN_TOTNODEFACT,
                   LNFILE.LN_YTDNODEFACT,
                   LNFILE.LN_TOTNOALDEL,
                   LNFILE.LN_YTDNOALDEL,
                   LNFILE.LN_YTDINTPD,
                   LNFILE.LN_LYRINTPD,
                   LNFILE.LN_TOTINTPD,
                   LNFILE.LN_RECASTINT,
                   LNFILE.LN_RECASTPRIN,
                   ISNULL(LNFILE.LN_CL_REBATE,''),
                   ISNULL(LNFILE.LN_AH_REBATE,''),
                   ISNULL(LNFILE.LN_PP_REBATE,''),
                   ISNULL(LNFILE.LN_INT_REBATE,''),
                   ISNULL(LNFILE.LN_SC_REBATE,''),
                   ISNULL(LNFILE.LN_MF_REBATE,''),
                   ISNULL(LNFILE.LN_DF_REBATE,''),
                   ISNULL(LNFILE.LN_EX_REBATE,''),
                   ISNULL(LNFILE.LN_O1_REBATE,''),
                   ISNULL(LNFILE.LN_O2_REBATE,''),
                   ISNULL(LNFILE.LN_O3_REBATE,''),
                   ISNULL(LNFILE.LN_O4_REBATE,''),
                   ISNULL(LNFILE.LN_O5_REBATE,''),
                   ISNULL(LNFILE.LN_O6_REBATE,''),
                   ISNULL(LNFILE.LN_O7_REBATE,''),
                   ISNULL(LNFILE.LN_UN_O7_REB,''),
                   ISNULL(LNFILE.LN_UN_O8_REB,''),
                   ISNULL(LNFILE.LN_UN_O9_REB,''),
                   ISNULL(LNFILE.LN_UN_O10_REB,''),
                   ISNULL(LNFILE.LN_PLREASON,''),
                   ISNULL(LNFILE.LN_COLLECTOR,''),
                   ISNULL(LNFILE.LN_ACTIONCD,''),
                   ISNULL(LNFILE.LN_ACTIONCD2,''),
                   ISNULL(LNFILE.LN_ATCODE,''),
                   ISNULL(LNFILE.LN_RECODE,''),
                   ISNULL(LNFILE.LN_ESCROW_FG,''),
                   LNFILE.LN_PROMDAYS_PASTDUE,
                   ISNULL(LNFILE.LN_PROMID,''),
                   ISNULL(CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PROMAMT,
                   ISNULL(CAST(LNFILE.LN_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_AMT_PAID_LAST,
                   LNFILE.LN_LASTCFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_DELCNT,
                   LNFILE.LN_DELCNT2,
                   LNFILE.LN_CREDITLIM,
                   LNFILE.LN_CRSCORE,
                   LNFILE.LN_LASTSFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_SOLCNT,
                   ISNULL(LNFILE.LN_MESSAGE,''),
                   LNFILE.LN_OLDACCTNO,
                   LNFILE.LN_PRLNNO,
                   LNFILE.LN_PRBAL,
                   LNFILE.LN_PRINTDUE,
                   LNFILE.LN_PRFINCHGRB,
                   LNFILE.LN_PRINSRB,
                   ISNULL(LNFILE.LN_PRPOCD,''),
                   ISNULL(CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PRDELINQAMT,
                   LNFILE.LN_PRLNAMT,
                   LNFILE.LN_PRPDUE_DAYS,
                   LNFILE.LN_PRDLDISC_UNEARN,
                   LNFILE.LN_PRCONPROFILE,
                   LNFILE.LN_SUM_ORIGPURAMT,
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_FINCHG,
                   LNFILE.LN_SUM_INTCHG,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_SUM_REGPYAMT,
                   LNFILE.LN_SUM_EXTCHG,
                   LNFILE.LN_SUM_SERCHG,
                   LNFILE.LN_SUM_MAINTFEE,
                   ISNULL(LNFILE.LN_UNITPER_CD,''),
                   LNFILE.LN_UNITPER_FREQ,
                   LNFILE.LN_DAYS_IN_UNITPER,
                   LNFILE.LN_UNITPER_PER_YEAR,
                   ISNULL(LNFILE.LN_REPOCD,''),
                   ISNULL(LNFILE.LN_BR_TFR_FG,''),
                   LNFILE.LN_O3INS_DLPROC,
                   LNFILE.LN_INSPREM_DLADJ,
                   LNFILE.LN_PLAMT,
                   LNFILE.LN_MTD_PRIN,
                   LNFILE.LN_MTD_LC,
                   LNFILE.LN_MTD_INT,
                   LNFILE.LN_MTD_OT,
                   LNFILE.LN_MTD_EX,
                   LNFILE.LN_BNKRPT_CHAPTER,
                   ISNULL(LNFILE.LN_BMO_CON_STATUS,''),
                   ISNULL(LNFILE.LN_BMO_REC_STATUS,''),
                   LNFILE.LN_APP_NUMBER,
                   ISNULL(LNFILE.LN_GIFTNET,''),
                   LNFILE.LN_GIFTNET_OUR_PO,
                   ISNULL(LNFILE.LN_PROC_DISBURSED,''),
                   ISNULL(LNFILE.LN_NOODDPY,''),
                   LNFILE.LN_CONPROFILE_1,
                   LNFILE.LN_CONPROFILE_2,
                   LNFILE.LN_CONPROFILE_3,
                   LNFILE.LN_CONPROFILE_4,
                   LNFILE.LN_CONPROFILE_5,
                   LNFILE.LN_RECPROFILE_1,
                   LNFILE.LN_RECPROFILE_2,
                   LNFILE.LN_RECPROFILE_3,
                   LNFILE.LN_RECPROFILE_4,
                   LNFILE.LN_RECPROFILE_5,
                   ISNULL(LNFILE.LN_TRW_STATUS,''),
                   ISNULL(LNFILE.LN_ORIGIN,''),
                   ISNULL(LNFILE.LN_PO_TRCD,''),
                   LNFILE.LN_NEXIS_CRSCORE,
                   ISNULL(LNFILE.LN_TRW_B_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_PARVHELD_PAID_FG,''),
                   ISNULL(LNFILE.LN_ACCT_FROZEN,''),
                   ISNULL(LNFILE.LN_TRW_SPECIAL_CODE,''),
                   ISNULL(LNFILE.LN_CREATION_STATUS,''),
                   LNFILE.LN_TOTNOLCHG,
                   ISNULL(LNFILE.LN_LBOX,''),
                   LNFILE.LN_EXTRA_PRIN_LFP,
                   LNFILE.LN_REMAIN_INT_SINCE_LFP,
                   LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(LNFILE.LN_INTERIM_COUPON,''),
                   LNFILE.LN_DLCOMM_O3_CMSI,
                   LNFILE.LN_DLCOMM_PP_CMSI,
                   ISNULL(LNFILE.LN_TRW_J_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_ACH,''),
                   ISNULL(LNFILE.LN_1099_C_FG,''),
                   ISNULL(LNFILE.LN_TRW_COMPLIANCE_CODE,''),
                   ISNULL(LNFILE.LN_UNWIND_REASON_CODE,''),
                   ISNULL(LNFILE.LN_PBI_PBD_FG,''),
                   ISNULL(LNFILE.LN_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_USER,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_CODE,''),
                   ISNULL(CAST(LNFILE.LN_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APPROVED_BY_TIME,
                   ISNULL(CAST(LNFILE.LN_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_LTOUCH_TIME
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR = :QLN1-WBEG-OWNBR
              AND LNFILE.LN_ACCTNO > :QLN1-WBEG-ACCTNO
              AND LNFILE.LN_ACCTNO <= :QLN1-WEND-ACCTNO 
            ORDER BY LNFILE.LN_OWNBR,
                     LNFILE.LN_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN LN1_CURSOR_GR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN1-WBEG-OWNBR
                          QLN1-WEND-OWNBR
                          QLN1-WBEG-ACCTNO
                          QLN1-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN1-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       READ-LN1-FILE.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           MOVE LN-OWNBR  TO QLN-OWNBR.
           MOVE LN-ACCTNO TO QLN-ACCTNO.

           EXEC SQL
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   LNFILE.LN_MAKERCD1,
                   LNFILE.LN_SSNO2,
                   LNFILE.LN_MAKERCD2,
                   LNFILE.LN_SSNO3,
                   LNFILE.LN_MAKERCD3,
                   LNFILE.LN_SSNO4,
                   LNFILE.LN_MAKERCD4,
                   LNFILE.LN_ORGST,
                   LNFILE.LN_SPRCLASS,
                   LNFILE.LN_POCD,
                   LNFILE.LN_PLCD,
                   LNFILE.LN_PAY_SCHLD_FG,
                   LNFILE.LN_STATFLAGS,
                   CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)), 
                   CAST(LNFILE.LN_ORIG_1STPYDATE
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_1STPYDATE
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_LEGALDATE
                                           AS VARCHAR(10)),
                   CAST(LNFILE.LN_BNKRPTDATE
                                           AS VARCHAR(10)),
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_TOTNOALDEL,
                   CAST(LNFILE.LN_DATE_PAID_LAST
                                           AS VARCHAR(10)), 
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_UNITPER_CD,
                   LNFILE.LN_UNITPER_FREQ,
                   LNFILE.LN_BNKRPT_CHAPTER
              INTO  
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
                   :QLN-POCD,
                   :QLN-PLCD,
                   :QLN-PAY-SCHLD-FG,
                   :QLN-STATFLAGS,
                   :QLN-LOANDATE, 
                   :QLN-ORIG-1STPYDATE, 
                   :QLN-1STPYDATE, 
                   :QLN-LEGALDATE,
                   :QLN-BNKRPTDATE,
                   :QLN-ORGTERM,
                   :QLN-1STPYAMT,
                   :QLN-NOREGPY,
                   :QLN-REGPYAMT,
                   :QLN-LASTPYAMT,
                   :QLN-PY-YYYYMM-RECDEL,
                   :QLN-MTD-PAYS-RECDEL,
                   :QLN-CURBAL,
                   :QLN-INTBAL,
                   :QLN-LCBAL,
                   :QLN-OTHBAL,
                   :QLN-OT2BAL,
                   :QLN-TOTPAYMNTD,
                   :QLN-TOTNODEF,
                   :QLN-TOTNOALDEL,
                   :QLN-DATE-PAID-LAST, 
                   :QLN-SUM-ORGTERM,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ,
                   :QLN-BNKRPT-CHAPTER
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
              AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       READ-LN1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           INITIALIZE QLN-REC.

           EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LN1_CURSOR INTO 
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
                   :QLN-POCD,
                   :QLN-PLCD,
                   :QLN-PAY-SCHLD-FG,
                   :QLN-STATFLAGS,
                   :QLN-LOANDATE, 
                   :QLN-ORIG-1STPYDATE, 
                   :QLN-1STPYDATE, 
                   :QLN-LEGALDATE,
                   :QLN-BNKRPTDATE,
                   :QLN-ORGTERM,
                   :QLN-1STPYAMT,
                   :QLN-NOREGPY,
                   :QLN-REGPYAMT,
                   :QLN-LASTPYAMT,
                   :QLN-PY-YYYYMM-RECDEL,
                   :QLN-MTD-PAYS-RECDEL,
                   :QLN-CURBAL,
                   :QLN-INTBAL,
                   :QLN-LCBAL,
                   :QLN-OTHBAL,
                   :QLN-OT2BAL,
                   :QLN-TOTPAYMNTD,
                   :QLN-TOTNODEF,
                   :QLN-TOTNOALDEL,
                   :QLN-DATE-PAID-LAST, 
                   :QLN-SUM-ORGTERM,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ,
                   :QLN-BNKRPT-CHAPTER
              END-EXEC
           ELSE
           IF ( LN1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   FETCH LN1_CURSOR_GR INTO :QLN-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN1-FILE.

           EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.
		   
           PERFORM CLOSE-IT.
           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LN1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN1-CURSOR-STAT.

           IF ( LN1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   CLOSE LN1_CURSOR_GR
              END-EXEC
              MOVE STAT---BAD TO LN1-CURSOR-GR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      * COPY "LIBGB/GBBRRN.CPY".
      * COPYMEMBER: LIBGB/GBBRRN
      * 20220923 BAH ADDED BR_VERITEC_LICENSE #1570
      * 20230419 BAH ADDED BR-TEST-PP-AUTO-INSCOVR #1594A
       LOAD-BR-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BR-ALL-BASE
              MOVE FILPATH-MACHINE         TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BRFILE NOT = "S" )
              MOVE BR-PATH-BEG             TO BR-SELECT-PATH-BEG
              MOVE BR-PATH-FILE            TO BR-SELECT-PATH-FILE
           ELSE
              MOVE BR-PATH-SQL             TO BR-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BR-FILE.
           PERFORM LOAD-BR-FILE.
           PERFORM OPEN-IT.
           MOVE BR-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BR-FILE.
           PERFORM START-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           IF ( BR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BR-FILE.

           IF ( QBR-WEND-NO = ZEROES    ) OR
              ( QBR-WEND-NO NOT NUMERIC )
              MOVE BR-NO                TO QBR-WBEG-NO
              MOVE ALL "9"              TO QBR-WEND-NO.

           EXEC SQL
            DECLARE BR_CURSOR CURSOR FOR
            SELECT BRFILE.BR_NO,
                   BRFILE.BR_NAME,
                   BRFILE.BR_LOAN_NUM_FLAG,
                   BRFILE.BR_DATA_PATH,
                   BRFILE.BR_MACHINE
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO >= :QBR-WBEG-NO
              AND BRFILE.BR_NO <= :QBR-WEND-NO
            ORDER BY BR_NO
           END-EXEC.

           EXEC SQL
               OPEN BR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBR-WBEG-NO
                          QBR-WEND-NO.

           MOVE STAT---GOOD TO BR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BR-FILE.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           MOVE BR-NO TO QBR-NO.

           EXEC SQL
            SELECT BRFILE.BR_NO,
                   BRFILE.BR_NAME,
                   BRFILE.BR_LOAN_NUM_FLAG,
                   BRFILE.BR_DATA_PATH,
                   BRFILE.BR_MACHINE
              INTO 
                  :QBR-NO,
                  :QBR-NAME,
                  :QBR-LOAN-NUM-FLAG,
                  :QBR-DATA-PATH,
                  :QBR-MACHINE
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       READ-BR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           INITIALIZE QBR-REC.

           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BR_CURSOR INTO  
                    :QBR-NO,
                    :QBR-NAME,
                    :QBR-LOAN-NUM-FLAG,
                    :QBR-DATA-PATH,
                    :QBR-MACHINE
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BR-FILE.
           PERFORM CLOSE-IT.
           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BR-CURSOR-STAT.
 

       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF ( SKIP-RECORDS-FOUND )
              GO TO EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

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
              MOVE "LP/LPR2MU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BNKLG2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
