       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RZRNRP.
       DATE-WRITTEN. 03-15-89
      ******************************************************************
      *
      *                        (LP)
      * DESCRIPTION:    UNEARNED REPORT
      *
      *=================================================================
      * REV:
      * JFF 100695 ADDED CLASS SCAN
      * JFF 070996 ADDED GROUP LOGIC
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER
      *            READING BWFILE.
      * BLV 060497 CHANGED TO WORK FROM NEW BATCH DRIVER (BREXEC)
      * BAH 072197 EXPANDED WK TOTALS AND TOTAL-TBL TOTALS
      *            FOR AUTOPRIME, PL# 136
      * JTG 110397 EXPANDED WK TOTALS AND TOTAL-TBL TOTALS
      *            FOR RELIANCE, PL# XXX
      * BAH 120197 EXPANDED TOTAL-TBL FROM S9(7) TO S9(9) FOR RAC
      * BAH 030298 CLASS RANGE TEST NOT CORRECT
      * MJD 031798 REMOVED SCREEN DISPLAY OF ACCOUNT # DURING RUN.
      *            DISPLAY BRANCH NUMBER ONLY.
      * BAH 060198 ADDED DEALER NO. OPTINO FOR AUTOPRIME A50 UPGRADE...
      * BAH 070798 END-BRANCH WAS GETTING SET TO 999 FOR WGR-TYPE "A"
      *            AND WORLD WAS LOSING BRANCH 9992 FOR "SC"
      * MJD 990802 ADDED GPENV(W). NEEDED FOR CHANGES TO LPEARN.
      * BAH 991102 ACCOUNTS WITH JUST UNEARN(6) WERE BEING SKIPPED
      *            BECAUSE THE GP-INCL-OTH-IN-EARNED = "N" WHICH
      *            CAUSES EARN-TOT-UNEARN TO BE 0 BUT THEY NEED TO SEE
      *            THESE ACCOUNTS, PL#  253, REGENCY
      *  CS 081218 ACCOUNTS WITH UNEARNED(4) WERE SKIPPED IF GP-INCL-
      *            OTH-IN-EARNED WAS "R"   PR#210
      * BLM 130131 ADD OPTION FOR SKIP, INCLUDE, ONLY FOR GROUP TOTALS,
      *            WORLD PR# 849
      * BAH 140429 REPLACED RESERVED WORD INT/DISC TO WS PREFIX, VERYANT
      *  CS 140805 'REPORT IN PROGRESS' DISPLAYING OVER DATE WHEN RUNNING
      *            IN BATCH, IF YOU TRY AND RE-RUN BATCH WITHOUT CLEARING
      *            SCREEN, IT GIVES 'INVALID DATE' AND MUST SPACE THRU DATE
      * KEC 2017.0605 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 181004 GLOBAL BWFILE, L2G
      * BAH 181211 GLOBAL LNFILE, L2G
      * BAH 20190920 REMOVED ACCESS-CALL ON LNFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK-FILE ASSIGN MKTEMP-BUF
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK1-KEY
                  ALTERNATE RECORD KEY WK2-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      /*****************************************************
      *   SUMMARY TOTALS WORK RECORD                       *
      ******************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK1-KEY.
               05  WK-BRANCH           PIC 9(4).
               05  WK2-KEY.
                   07  WK-CLASS        PIC 9(3).
           03  WK-NOACCTS              PIC 9(6)      COMP-3.
           03  WK-ACCERN               PIC S9(9)V99  COMP-3.
           03  WK-INT                  PIC S9(9)V99  COMP-3.
           03  WK-ADDON                PIC S9(9)V99  COMP-3.
           03  WK-SRV                  PIC S9(9)V99  COMP-3.
           03  WK-MFEE                 PIC S9(9)V99  COMP-3.
           03  WK-DISC                 PIC S9(9)V99  COMP-3.
           03  WK-OTHER                PIC S9(9)V99  COMP-3.
           03  WK-ENDING               PIC S9(9)V99  COMP-3.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/RZRNRP S35 05/01/23-08:55:01 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBLP/LP01LN.CPY".
      *COPY "LIBLP/LP01LN_SQL.CPY".
	  
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
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
      * COPY "LIBGB/GB01BR.CPY".
      * COPY "LIBGB/GB01BR_SQL.CPY".

      * COPY "LIBGB/GB01BR.CPY".
      * COPYMEMBER: LIBGB/GB01BR
      **************************************************************************
      *    BRANCH FILE RECORD      (GB01BR)   
      *         INDEXED VERSION
      *
      * BR-NO = BR-NO
      *
      *REV:
      * JTG 033095 ADDED BR-UNITPER-FG AND BR-UNITPER-Y-D-FG
      * JTG 051595 ADDED BR-RI-BEFORE-REPO
      *                  BR-RI-BEFORE-P4P5
      * JTG 061595 ADDED BR-NOT-OURS-REFUND
      * JTG 061995 ADDED BR-SFDLO3-WINDOW-FG
      * JTG 061995 ADDED BR-CALC-INS-ADJ
      * JTG 051595 ADDED BR-AC-BEFORE-REPO
      *                  BR-AC-BEFORE-P4P5
      * MJD 063095 ADDED BR-BANK-CLEARING-GL
      *                  BR-BANK-ROUTING-NO
      *                  BR-BANK-ACCTNO.
      * JTG 070595 ADDED BR-REB-METHOD-2       OCCURS 5
      *                  BR-REBDAYS-MONTH-2    OCCURS 5
      *                  BR-ACCRUAL-MONTHS-2   OCCURS 5
      * JTG 071395 ADDED BR-DEFAULT-SC-SUBCLASS
      * BLL 071795 EXPANDED UNUSED, BANK ACCT, ADDED BANKGL-DEPOSIT
      * BAH 083195 REMOVED BR-CASH-METHOD-ONLY AND BR-IB-ACCRUAL
      *            (MOVED TO GP RECORD)
      * MJD 011296 ADDED BR-NO-OUT-DLR-RSV.
      * KEC 020796 ADDED BR-COLLECTOR-QUEUE AND BR-COLL-QUEUE-CON-REC.
      * JTG 022096 ADDED BR-LNCHGS-WINDOW
      * JTG 062596 ADDED BR-EXPAND-APP-INPUT
      * BAH 082696 ADDED BR-A50-CPCORP TO USE A50'S COLLECTION REPORT (INSTANT)
      * MJD 100396 ADDED BR-INBR-BILNSC-FG. IF 'Y' THEN GLOBAL SCAN IS
      *            CALLED, ELSE A SINGLE BRANCH IS ENTERED.
      * KEC 112096 ADDED BR-LEDGER-JUST-INTCHG FOR QUICK CREDIT,
      *             FOR "MQ" AND "MR" TO ONLY PRINT INTCHG IN THE
      *             FINANCE CHARGE BOX.  FOR COPYING LP/LONPMQ FROM A48.
      * MJD 012897 ADDED BR-BYINQ-START-PAGE AND BR-DSINQ-START-PAGE.
      *            THESE ARE THE DEFAULT START PAGES FOR INQUIRIES.
      * JTG 022097 ADDED BR-APP-REQUIRED-FOR-INPUT  RE: STATISTICS UPDATE
      * BLV 022597 ADDED BR-CREDIT-SCORING-FG
      * BLV 041497 ADDED BR-COL-QUEUE-LOW-TO-HI
      * MJD 071797 ADDED BR-CAN-GROUP-ID.
      * KEC 072297 ADDED BR-CREDIT-BUREAU-SERVICES (5 BYTES).
      * MJD 072397 ADDED BR-AUTO-LNSTAT-WINDOW.
      * BLV 072497 ADDED BR-TIME-ZONE-FACTOR.
      * MJD 072897 ADDED BR-LONPF-OPT-DISPLAY.
      * JTG 081397 ADEED BR-DUEDATE-CHANGE-CD RELIANCE, #63
      * KEC 082097 ***CONVERSION-A90-TO-A10***
      *               ADDED BR-GIFTNET, BR-GIFTNET-LEDGER-LINE.
      * KEC 082697 REMOVED BR-DL-CHECK-NAME-CHG PER BETH BECAUSE OF PR#0543;
      *             CHANGING OF PAYEE IS ALREADY ALLOWING, SO THIS FLAG IS
      *             NOT NECESSARY.  MADE INTO FILLER.
      * MJD 082797 ADDED BR-NO-FT-EMP AND BR-NO-PT-EMP FOR BYMISC
      * MJD 082797 ADDED BR-NO-FT-EMP AND BR-NO-PT-EMP FOR MN/BYMISC.C
      * JTG 092597 ADDED BR-BALLOON-LOANS-FG, CASTLE FINANCE
      * BLV 103097 ADDED BR-GL-ENTRY-DEFAULT, BR-GL-TYPE-ALLOWED
      * BLV 110497 ADDED BR-MIDSTREAM-1STPAY-FG
      * JTG 111197 ADDED BR-DUEDATE-CHANGE-CD OPTION 'M' MERCURY
      * JTG 111197 ADDED BR-NODEF-1ST-X-MONTHS OPTION MERCURY
      * JTG 112197 EXPANDED RECORD BY 52 BYTES 798 --> 850
      * JTG 120197 ADDED WORLD BR-ADD-INS-FEECODE AD&D INSURANCE GA
      *                        BR-ROAD-G-SGL-FEECODE FOR VOYAGER
      *                        BR-ROAD-G-FAM-FEECODE
      * JTG 120397 ADDED BR-NODEF-IF-ZERO-AMT OPTION MERCURY
      * KEC 121797 ADDED BR-FXPRESS-OPTION FOR FORMSXPRESS FORMS.
      * MJD 012898 ADDED BR-NEXT-SOLICIT
      * BAH 022698 ADDED BR-ADDON-FEECD-O3 PIC XX OCCURS 2
      * BAH 022698 REMOVED BR-MEDIA-PURGE
      * MJD 031398 ADDED BR-SIZE - 78 LEVEL DOES NOT AFFECT REC SIZE.
      * BAH 032498 ADDED BR-IMMEDIATE-RECEIPT AND BR-IMMEDIATE-DOC-PRINT
      *            FROM A90 FOR WORLD...
      * BAH 042998 CHANGED BR-ADDON-FEECD TO BR-ADDON-FEECD-O2, BECAUSE WE
      *            DONT ALLOW FEES ON OTHER 1 ANYMORE..
      * BLV 052298 ADDED BR-CHECK-EOM-FG: CHECK EOM & CALENDAR EOY
      *            COMPLETE BEFORE ALLOWING LONPA0 TO OPEN DAY
      * BAH 060298 ADDED BR-AUTO-PL-REF-AC FOR MERCURY #E180 TO AUTOMATICALLY
      *            REFUND EVERYTHING AND ACCELERATE THE ACCOUNT
      * BLV 090298 TRASHED BR-EOM1-PROCEDURE, BR-EOM2-PROCEDURE, THESE ARE
      *            NOT USED & ARE CONFUSING PEOPLE
      * KEC 100698 ADDED BR-REVOLVING & BR-FAX-NUMBER. REGENCY REVOVLING
      * JTG 101298 CHANGED BR-REVOLVING TO BR-REVOLVING-SF-FG
      *                          AND ADDED BR-REVOLVING-LN-FG
      * CS  111798 ADDED BR-ALPHA-DRAWERS
      * KEC 011599 ADDED BR-REVOLVING-CONV-DATE SO TO PREVENT ANY
      *             UPDATES TO 'GL' FOR MANUAL CONVERSION OF REVOLVING
      *             ACCOUNTS (REGENCY).
      * MJD 990125 ----- UPDATED IN FLEXGEN -----
      * CS  1999.0312 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * GLF 990323 ADDED BR-LATECHG-ON-ALLOTMENTS, TFC #018
      * MJD 990323 ADDED BR-PRINTERFG-VALID. 88 LEVEL FOR VALID PRINTER
      *            CODES.
      * MJD 990412 CHANGED BR-BP-NEXT-BATCH TO BE PIC 9(2) COMP-3
      * KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * MJD 990617 ADDED BR-ALLOW-SYS-AS-EFFDATE (REG MGMT REQ # 867)
      * GLF 990715 ADDED BR-TAX-PREBOOK-CUST AND BR-TAX-PREBOOK-NONCUST
      *            WORLD PR#211
      * GLB 990715 ADD BR-PASSWORD-DRAWER FLAG, PR #901, REGMGM
      * BAH 990723 REMOVED BR-FXPRESS-OPTION
      * KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * MJD 990901 ADDED BR-INBR-BILNSC-FG-VALID - FLAG NOW CAN HAVE
      *            VALUE OF "M" (MUL PR# 925)
      * JTG 991129 ADDED BR-ONESTEP-RN-FG, TO ALLOW ONE STEP RENEWALS, WORLD
      * KEC 991130 ADDED BR-LI-LISCAN-FG; FOR CMSI/CSI PROJECT
      * MJD 000223 ADDED BR-HIDE-END-BAL (WORLD PR#219)
      * KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * MJD 000420 ADDED A SECOND SET OF BATCH ALLOTMENT FLAGS FOR LOCK
      *            BOX PROCESSING (MERC PR#227)
      * BAH 000531 ADDED NEW VALUE FOR BR-NO-CASH-PROCEEDS "B" TO ALLOW
      *            CASH PROCEEDS ON PB ACCTS ONLY, SUNBELT PR# 1213
      * MJD 000911 ADDED BR-LBOX AND BR-LATECHG-ON-LBOX (MFC PR#227)
      * JTG 001117 ADDED BR-ALLOW-OV-TRCD FOR 'OV' OVERPAYMENT TRANSACTION
      *                                                            PR#J015
      * MJD 001120 ADDED BR-GLOBAL-QUEUE (TFC PR#60)
      * JTG 001218 ADDED THE FOLLOWING FIELDS, RE: GP-ALLOW-SET-FOR-NONCASH
      *            MERCURY PR#239
      *            03  BR-BP-PAYOFF-NONCASH      PIC X.
      *            03  BR-BP-PAYOFF-OV           PIC X.
      *            03  BR-LBOX-PAYOFF-NONCASH    PIC X.
      *            03  BR-LBOX-PAYOFF-OV         PIC X.
      * BLV 010604 ADDED BR-ALLOW-INTERBR - IF "N" DON'T ALLOW OTHER
      *            BRANCHES TO POST INTERBRANCH PAYMENTS TO THIS BRANCH,
      *            REGACC PR# 1510
      * BAH 010621 NO PHYSICAL CHANGE, JUST CHANGED THE USE OF REB-METHOD-2
      *            SUBSCRIBT 4 FROM O6 TO CL(LEV), THE LOAN RECORD DOES NOT
      *            EVEN ALLOW O6, REGWAN PR# 1500
      * BLV 011207 ADDED BR-COLL-QUEUE-PAST-DUE, REGACC PR# 1677
      * JTG 020208 ADDED BR-FORCE-VALID-CITY-COUNTY                 WORLD PR#288
      * CS  020926 ADDED BR-LI-DLCOMM-CMSI-PP-MAX, BR-LI-DLCOMM-CMSI-O3-MAX..FOR
      *            MAXIUMUM AMOUNT OF DEALER COMMISSION FOR PP & O3 INSURANCE
      *            FED FROM CSMI(LOAN APP INTERFACE UPD) TO LOAN MANAGER
      * BAH 030507 ADDED BR-ASSESS-LATE-CHGS FOR MODERN PR# 2036, THEY WILL
      *            SET THIS TO "N", THEY DONT WANT LATE CHARGES IN LEGAL BRANCH
      * BAH 030917 REMOVED BR-FAIRCO-CPCORP-FG, BR-LNCHGS-WINDOW,
      *            BR-FAIRCO-TRW-OPT
      * BAH 030918 REMOVED BR-AS-TRAILER-NUMBER,MADE FILLER X(5),
      *            NOT USED ANYWHERE
      * KEC 030918 {PR#00039} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            ADDED   03  BR-LR-GROUP-COLLID       PIC X.
      * BLV 031112 ADDED BR-INS-PRODUCER, WORLD NEEDS THIS FIELD IN BRANCH,
      *            NOT GLOBAL FOR CLAIMS; REDEFINED BR-STMNT2 SINCE ALMOST
      *            OUT OF SPACE
      * BLV 040218 ADDED BR-JE-ALLOCATION-SKIP, FLAG TO CAUSE GL JOURNAL ENTRY
      *            ALLOCATIONS TO SKIP BRANCH, WORLD
      * JTG 040219 ALLOWED NON MONTHLY CAN LOANS (NMQUOT.C) WORLD J043
      *             ADDED BR-NON-MONTHLY-CAN-LOANS
      * BLV 040315 RENAME BR-JE-ALLOCATION-SKIP TO BR-WORLD-BRANCH-TYPE, WILL
      *            BE USED BY WORLD TO INDICATE STATE ADMIN BRANCHES, 1ST &
      *            2ND YEAR DENOVO'S
      * BLV 040323 OKAY, NEED BOTH BR-JE-ALLOCATION-SKIP & WORLD-BRANCH-TYPE,
      *            SO RENAMED THE ALLOCATION FLAG BACK & ADDED THE TYPE, WORLD
      * BLV 040615 ADDED BR-COL-QUEUE-BEHAVIOR-FG, WHETHER TO ORDER QUEUE
      *            BY BEHAVIOR GRADE, REGACC PR# 59
      * JTG 040826 ALLOW BATCH PAYMENTS TO ACCOUNTS WITH OTHER BALANCE BUT
      *            NOT APPLY APPLY MONIES TO OTHER BALANCE          REGACC #0070
      * BAH 040830 ADDED BR-REGION-NUMBER FOR REGACC PR# 76
      * BAH 150930 CHANGED FROM RELATIVE TO INDEXED, PD0005
      * BAH 151020 COMBINED WITH B2FILE, CHANGED COMP-6 TO COMP-3, ADDED
      *            FILLER, INCREASED BR-OPEN-BAL TO OCCURS 20, REMOVED
      *            BR-REVOLVING-CONV-DATE, MADE BR-INS-PRODUCER ITS OWN
      *            FIELD INSTEAD OF REDEFINE OF STMNT2, INCREASED
      *            BR-REB-METHOD AND METHOD-2 TO 99, PD0003
      *  CS 160120 REMOVED BR-REB-METHOD-2 FIELDS
      * BLM 160914 ADD BR-ALLOW-SCANNED-CKS, WORLD PR# 1188
      * BLM 160915 ADD BR-CHECK-DEPOSIT-GL, WORLD PR# 1188 
      * AMM 161031 ADD BR-OPTION-PST-LASER, WORLD PR# 1200
      * BAH 180214 ADDED BR-USE-DEBIT-CARDS, BR-GL-DEBIT-CARD-PROCEEDS
      *            FOR NETSPEND, WORLD PR#1306
      * AMM 181113 REMOVED CREDIT BUREAU SERVCES FIELDS FORM RECORD LAYOUT
      *            SDP_002_T02
      *  CS 190124 ADD BR-WKDAY-BANKID FOR BANK INTEGRATION WORKDAY PR#1353A
      *  CS 190408 ADD FUTURE FIELDS,SAVED GB01BR.190408
      *  BAH 20190625 REMOVED BR-REVOLVING-SF-FG BR-REVOLVING-LN-FG PT0153
      *  BAH 20190702 REMOVED BR-USE-PARTIC-FLAT-TABLE PT0051
      *  BAH 20190730 ADDED BR-PURGE-NOW-FLAG TO REPLACE THE "PURGEME" TAG
      *               IN LNARCH. LNARCH GETS PUT IN A BATCH AND ONLY THE
      *               BRANCHES WITH THIS FLAG SET WILL ACTUALLY PURGE
      *  BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      *  BAH 20200326 ADDED BR-EMAIL AND BR-MANAGER-EMAIL
      *  BAH 20210421 REMOVED FUTURE FIEKDS. MOVE BR-NO TO BE GROUP LEVEL
      *  BAH 20220921 ADDED BR-VERITEC-LICENSE FOR "IL" VERITEC EXTRACTS #1570
      *  BAH 20230419 ADDED BR-TEST-PP-AUTO-INSCOVR #1594A
      **************************************************************************
       78  BR-SIZE                      VALUE 1215.
       01  BR-REC.
           03  BR-KEY.
               05  BR-NO                PIC 9(4).
      $XFD DATE=YYYYMMDD
           03  BR-LTOUCH-DATE           PIC 9(8)     COMP-3.
           03  BR-NAME                  PIC X(40).
           03  BR-ADR1                  PIC X(24).
           03  BR-ADR2                  PIC X(24).
           03  BR-CITY                  PIC X(15).
           03  BR-STATE                 PIC XX.
           03  BR-ZIP                   PIC X(10).
           03  BR-PHONE                 PIC 9(10)    COMP-3.
           03  BR-RETAIN                PIC 9(11)    COMP-3.
           03  BR-NEXT-CHECK            PIC 9(6)     COMP-3.
           03  BR-PRINTERS.
               05  BR-PRINTTAB         OCCURS 4.
                   07  BR-PRINTER       PIC 99.
           03  BR-FEE-GL-BRNO           PIC 9(4).
           03  BR-REMIT1                PIC X(30).
           03  BR-REMIT2                PIC X(30).
           03  BR-REMIT3                PIC X(30).
           03  BR-STMNT1                PIC X(60).
           03  BR-STMNT2                PIC X(60).
           03  BR-INS-PRODUCER-NUM      PIC 9(8).
           03  BR-DLRCHK                PIC X.
           03  BR-FRONT-END-FG          PIC X.
           03  BR-EMLNRG-PHONE          PIC X.
           03  BR-POST-GL               PIC X.
           03  BR-ALLOTMNTS             PIC X.
           03  BR-EXTRACT-AUTO-UPD      PIC X.
           03  BR-PCNI-LEDGER           PIC X.
           03  BR-PRINTERFGS.
               05  BR-PRINTFGTAB       OCCURS 4.
                   07  BR-PRINTERFG     PIC X.
                       88 BR-PRINTERFG-CENTRONICS   VALUE "C".
                       88 BR-PRINTERFG-NEC          VALUE "N".
                       88 BR-PRINTERFG-OKIDATA      VALUE "O".
                       88 BR-PRINTERFG-OKIDATA-IBM  VALUE "I".
                       88 BR-PRINTERFG-LASER        VALUE "L".
                       88 BR-PRINTERFG-LASER-IIP    VALUE "2".
                       88 BR-PRINTERFG-PANASONIC    VALUE "P".
                       88 BR-PRINTERFG-DEFAULT      VALUE " ".
                       88 BR-PRINTERFG-VALID VALUE "C" "N" "O" "L"
                                                   "2" "P" " " "I".
           03  BR-NEGDLRSV              PIC X.
           03  BR-CPCORP-ORDER          PIC X.
           03  BR-PRIO-MEMO-FLAG        PIC X.
           03  BR-PREBILL-PL-JD-OPT     PIC X.
           03  BR-LOAN-NUM-FLAG         PIC X.
           03  BR-DOWNPY-FLAG           PIC X.
           03  BR-AUTO-INS-EARN         PIC X.
           03  BR-OTH-INS-LEVELS        PIC X.
           03  BR-ALLOW-INTERBR         PIC X.
           03  BR-UNER-DLANRP           PIC X.
           03  BR-SECOND-PROMISE        PIC X.
           03  BR-AVAIL-CREDIT          PIC X.
           03  BR-RECP1                 PIC X(30).
           03  BR-RECP2                 PIC X(30).
           03  BR-INC-WAIVERS           PIC X.
           03  BR-INC-INS               PIC X.

      *     BATCH PAYMENT FIELDS:
           03  BR-BP-NEXT-BATCH         PIC 9(2). 
           03  BR-BP-NOPAYOFF           PIC X.
           03  BR-BP-ODDPAY             PIC X.
           03  BR-BP-LC                 PIC X.
           03  BR-BP-NOPRIN             PIC X.
           03  BR-BP-FLASH              PIC X.
           03  BR-BP-ACTIONCD           PIC X.
           03  BR-BP-MAXCON             PIC 9(3)     COMP-3.
           03  BR-BP-MAXREC             PIC 9(3)     COMP-3.
           03  BR-BP-PL                 PIC X.
           03  BR-BP-JD                 PIC X.
           03  BR-BP-BRANCH             PIC 9(5)     COMP-3.

           03  BR-AGE-ERN               PIC X.
           03  BR-OTHER-BRANCH-GL       PIC X.
           03  BR-SALES-SMALL           PIC X.
           03  BR-NEW-COLL-INQUIRY      PIC X.
           03  BR-CHANGE-MEMO           PIC X.
           03  BR-INS-ADDON             PIC X.
               88  BR-INS-ADDON-VALID  VALUE "O" "Y" "N".
               88  BR-INS-ADDON-ORIG   VALUE "O".
               88  BR-INS-ADDON-NEW    VALUE "Y".
           03  BR-TRAILER-REC           PIC X.
           03  BR-ALDEL-FLAG            PIC X.
           03  BR-IB-LCHG-MESS          PIC X.
           03  BR-COL-QUEUE-PAST-DUE    PIC X.
           03  BR-LI-DLCOMM-CMSI-PP-MAX PIC 9(4)V99 COMP-3.
           03  BR-ADDON-FEECD-O2        PIC XX OCCURS 2 TIMES.
           03  BR-AUTO-CHECK            PIC X.
           03  BR-RECEIPT-PROMDATE      PIC X.
           03  BR-RECEIPT-INTPD         PIC X.
           03  BR-COLL-ID-SCAN          PIC X.
           03  BR-STOP-ON-COLL          PIC X.
           03  BR-DUEDATE-CHANGE-CD     PIC X.
           03  BR-CHECK-EOM-FG          PIC X.
           03  BR-VOLUME-DLANRP         PIC X.
           03  BR-PDTH-NXDUE            PIC X.
           03  BR-LC-CODE               PIC X.
           03  BR-VALIDATE-PRUS         PIC X.
           03  BR-AUTO-OPEN-BAL         PIC X.
           03  BR-OPEN-BALS.
               05  BR-OPEN-BAL          PIC S9(7)V99 COMP-3 OCCURS 20.
           03  BR-TENDER-CD             PIC X(5)     OCCURS 3.
           03  BR-DLPARV-ADJ-FG         PIC X.

           03  BR-OVERRIDE-BR-STATE     PIC X.
           03  BR-DEFAULT-SC-CLASS      PIC 9(3).
           03  BR-RECDEL-NOT-REC        PIC X.
           03  BR-POST-AHEAD-FG         PIC X.
           03  BR-CAN-LOANS-FG          PIC X.
               88  BR-CAN-LOANS-YES                  VALUE "Y".
               88  BR-CAN-LOANS-NO                   VALUE "N".
               88  BR-CAN-LOANS-ONLY                 VALUE "O".
               88  BR-CAN-LOANS-VALID                VALUE "Y"
                                                           "N" "O".
J043       03  BR-NON-MONTHLY-CAN-LOANS      PIC X.
0070       03  BR-BP-ALLOW-PMT-ACCT-OTHBAL   PIC X.
0070       03  BR-LBOX-ALLOW-PMT-ACCT-OTHBAL PIC X.
           03  BR-REGION-NUMBER              PIC 9(2).
           03  BR-OUR-FEE                    PIC X.

      *=================================================================
      *   THIS IS HOW IT IS USED IN A15.
      *    CL(DEC)  -  1      O1      - 4
      *    AH       -  2      O2      - 5
      *    PP       -  3
      *---------------------------------------------------------------
      *    ONLY METHOD2(1) AND METHOD2(4) ARE USED
      *    O3       -  1      CL(LEV) - 4
      *    O4       -  2      N/A     - 5
      *    O5       -  3
      * THE LOAN RECORD ONLY HAS O1 - O5, SO I USED O6 FOR CREDIT LIFE
      * LEVEL, REGWAN WANTS CREDIT LIFE DEC AND CREDIT LIFE LEV
      *---------------------------------------------------------------
      *   IN A30 
      *    CL(DEC)  -  1      O2       - 5     
      *    AH       -  2      O3       - 6     O5     -  9 
      *    PP       -  3      CL(LEV)  - 7     O6     - 10      
      *    O1       -  4      O4       - 8     O7     - 11
      *---------------------------------------------------------------

           03  BR-REB-METHOD            PIC 99       OCCURS 11.
           03  BR-REBDAYS-MONTH         PIC 99       OCCURS 11.
           03  BR-ACCRUAL-MONTHS        PIC 999      COMP-3 OCCURS 11.

      *=================================================================
           03  BR-GLOVERSHORT           PIC 9(11)    COMP-3.
           03  BR-GLBANK                PIC 9(11)    COMP-3.
           03  BR-GLONHAND              PIC 9(11)    COMP-3.
           03  BR-BANKBAL               PIC S9(9)V99 COMP-3.
           03  BR-RETAIN-AMT            PIC S9(9)V99 COMP-3.
           03  BR-LCARDFG               PIC X.

           03  BR-DATA-PATH             PIC X(6).
           03  BR-MACHINE               PIC XX.
           03  BR-MANAGER               PIC X(25).

           03  BR-EOD-PROCEDURE         PIC XXX.
           03  BR-BP-PAYOFF-NONCASH     PIC X.
           03  BR-BP-PAYOFF-OV          PIC X.
           03  BR-LI-DLCOMM-CMSI-O3-MAX PIC 9(4)V99 COMP-3.
           03  BR-LR-GROUP-COLLID       PIC X.

           03  BR-UNITPER-FG            PIC X.
           03  BR-UNITPER-Y-D-FG        PIC X.

           03  BR-NO-PROMPT-FOR-OTHBR    PIC X.
           03  BR-WORLD-BRANCH-TYPE      PIC X.
           03  BR-COL-QUEUE-BEHAVIOR-FG  PIC X.
           03  BR-CONVERT-AUTO-ACCTNO    PIC X.
           03  BR-TS-REPO-INS-FG         PIC X.
           03  BR-BOOK-SF-BR-ONLY        PIC X.
           03  BR-FORCE-VALID-CITY-COUNTY PIC X.
           03  BR-SET-COLLCD-DL-MFGCD    PIC X.
           03  BR-DEFAULT-SF-UNITPER-CD  PIC X.
           03  BR-DEFAULT-SF-PURCD       PIC X(3).
           03  BR-DEFAULT-LN-UNITPER-CD  PIC X.
           03  BR-DEFAULT-LN-PURCD       PIC X(3).
           03  BR-TAP-OTHER-DL           PIC X.
           03  BR-AH-AND-CL              PIC X.
           03  BR-NO-CASH-PROCEEDS       PIC X.
               88  BR-NO-CASH-PROCEEDS-VALID  VALUE "Y" "N" "B".
               88  BR-CASH-PB-ONLY       VALUE "B".
           03  BR-RI-BEFORE-REPO         PIC X.
           03  BR-RI-BEFORE-P4P5         PIC X.
           03  BR-NOT-OURS-REFUND        PIC X.
           03  BR-SFDLO3-WINDOW-FG       PIC X.
           03  BR-CALC-INS-ADJ           PIC X.
           03  BR-AC-BEFORE-REPO         PIC X.
           03  BR-AC-BEFORE-P4P5         PIC X.
           03  BR-BANK-ROUTING-NO        PIC 9(9)   COMP-3.
           03  BR-BANK-CLEARING-GL       PIC 9(11)  COMP-3.
           03  BR-GLBANK-DEPOSIT         PIC 9(11)  COMP-3.
           03  BR-BANK-ACCTNO            PIC X(17).
           03  BR-DEFAULT-SC-SUBCLASS    PIC 9(3).
           03  BR-NO-OUT-DLR-RSV         PIC X.
           03  BR-COLLECTOR-QUEUE        PIC X.
           03  BR-COLL-QUEUE-CON-REC     PIC X.
           03  BR-JE-ALLOCATION-SKIP     PIC X.
           03  BR-EXPAND-APP-INPUT       PIC X.
           03  BR-A50-CPCORP             PIC X.
           03  BR-INBR-BILNSC-FG         PIC X.
               88  BR-INBR-BILNSC-FG-VALID   VALUE "Y" "N" "M".
      *        ---------------------------------------------------------
      *         "Y" : USE GLOBAL SCAN WINDOW FOR INTER BRANCH SCAN.
      *         "N" : DO NOT USE GLOBAL SCAN WITH INTERBRANCH POSTING
      *               POP WINDOE TO ENTER BRNO AND GO TO THAT BRANCH.
      *         "M" : SAME AS "N" ABOVE PLUS THE FOLLOWING: WHEN IN
      *               COLLECTIONS OR F10 INT. GLOBAL SCAN WILL BE
      *               REPLACED WITH AN ENTRY BOX FOR BRNO ACCTNO TO VIEW.
      *        ---------------------------------------------------------
           03  BR-INBR-BRIEF-FG          PIC X.
           03  BR-LEDGER-JUST-INTCHG     PIC X.
           03  BR-BYINQ-START-PAGE       PIC 99.
           03  BR-DSINQ-START-PAGE       PIC 99.
           03  BR-APP-REQUIRED-FOR-INPUT PIC X.
           03  BR-CREDIT-SCORING-FG      PIC X.
           03  BR-COL-QUEUE-LOW-TO-HI    PIC X.
           03  BR-CAN-GROUP-ID           PIC X(4).
           03  BR-AUTO-LNSTAT-WINDOW     PIC X.
           03  BR-TIME-ZONE-FACTOR       PIC S9.
           03  BR-LONPF-OPT-DISPLAY      PIC X.
           03  BR-GIFTNET                PIC X.
           03  BR-GIFTNET-LEDGER-LINE    PIC X.
           03  BR-NO-FT-EMP              PIC 9(3)  COMP-3.
           03  BR-NO-PT-EMP              PIC 99V99 COMP-3.
           03  BR-BALLOON-LOANS-FG       PIC X.
           03  BR-GL-TYPE-ALLOWED        PIC X.
           03  BR-GL-ENTRY-DEFAULT       PIC X.
           03  BR-MIDSTREAM-1STPAY-FG    PIC X.
           03  BR-NODEF-1ST-X-MONTHS     PIC 9.
           03  BR-ADD-INS-FEECODE        PIC XX.
           03  BR-ROAD-G-SGL-FEECODE     PIC XX.
           03  BR-ROAD-G-FAM-FEECODE     PIC XX.
           03  BR-NODEF-IF-ZERO-AMT      PIC X.
           03  BR-ASSESS-LATE-CHGS       PIC X.
           03  BR-NEXT-SOLICIT-NO        PIC 9(6) COMP-3.
           03  BR-ADDON-FEECD-O3         PIC XX OCCURS 2 TIMES.
           03  BR-IMMEDIATE-RECEIPT      PIC X.
           03  BR-IMMEDIATE-DOC-PRINT    PIC X.
           03  BR-AUTO-PL-REF-AC         PIC X.
               88  BR-AUTO-PL-REF-AC-VALID  VALUE "Y" "2" "3".
           03  BR-FAX-NUMBER             PIC 9(10)    COMP-3.
           03  BR-ALPHA-DRAWERS          PIC X.
           03  BR-LATECHG-ON-ALLOTMENTS  PIC X.
           03  BR-ALLOW-SYS-AS-EFFDATE   PIC X(01).
           03  BR-TAX-PREBOOK-CUST       PIC 9(4) COMP-3.
           03  BR-TAX-PREBOOK-NONCUST    PIC 9(4) COMP-3.
           03  BR-PASSWORD-DRAWER        PIC X.
           03  BR-ONESTEP-RN-FG          PIC X.
           03  BR-LI-LISCAN-FG           PIC X.
           03  BR-HIDE-END-BAL           PIC X(01).
      *     BATCH PAYMENT FIELDS (LOCK BOX PROCESSING):
           03  BR-LBOX-NOPAYOFF          PIC X.
           03  BR-LBOX-ODDPAY            PIC X.
           03  BR-LBOX-LC                PIC X.
           03  BR-LBOX-NOPRIN            PIC X.
           03  BR-LBOX-FLASH             PIC X.
           03  BR-LBOX-ACTIONCD          PIC X.
           03  BR-LBOX-MAXCON            PIC 9(3)     COMP-3.
           03  BR-LBOX-MAXREC            PIC 9(3)     COMP-3.
           03  BR-LBOX-PL                PIC X.
           03  BR-LBOX-JD                PIC X.
           03  BR-LBOX                   PIC X(01).
           03  BR-LATECHG-ON-LBOX        PIC X(01).
           03  BR-ALLOW-OV-TRCD          PIC X.
           03  BR-GLOBAL-QUEUE           PIC X(01).
           03  BR-LBOX-PAYOFF-NONCASH    PIC X.
           03  BR-LBOX-PAYOFF-OV         PIC X.

           03  BR-COLLECT-MANAGER      PIC X(25).
           03  BR-COLLECT-PHONE        PIC 9(10) COMP-3.
           03  BR-COLLECT-EXT          PIC 9(4)  COMP-3.

           03  BR-ALT-INTO-THIS-BRANCH PIC X.
           03  BR-POFF-TITLE-WINDOW    PIC X.
           03  BR-COUPON-STATEMENT     PIC X.
           03  BR-SUPERVISOR           PIC X(25).
           03  BR-AREA-MANAGER         PIC X(25).

           03  BR-MIN-MONTHS-DEF       PIC 99.

      * COLLECTION CENTER INFO, PR# 164 REGACC
           03  BR-CC-FLAG              PIC X.
           03  BR-CC-ADR1              PIC X(24).
           03  BR-CC-CITY              PIC X(15).
           03  BR-CC-STATE             PIC XX.
           03  BR-CC-ZIP               PIC X(10).
      *USED TO INTERRUPT GRADE CODES COMING FROM CMSI TO POPULATE 
      *PARTIC. HELD
           03  BR-MIN-MONTHS-D2          PIC 99. 
           03  BR-SKIP-MIDSTREAM-TRW     PIC X.
           03  BR-GL-ESCROW-BALANCE      PIC 9(11) COMP-3.
           03  BR-GL-ESCROW-PAYABLE      PIC 9(11) COMP-3.
           03  BR-ALLOW-FORCE-OPEN-FG    PIC X.
           03  BR-ACH-BANK-NO            PIC 9.
           03  BR-WIMENU-CHECK-WRITING   PIC X.
           03  BR-ADP-PAY-GROUP          PIC X(3).
           03  BR-ALLOW-SCANNED-CKS      PIC X.
           03  BR-CHECK-DEPOSIT-GL       PIC 9(11) COMP-3.
           03  BR-OPTION-PST-LASER       PIC X.
           03  BR-USE-DEBIT-CARDS        PIC X.
           03  BR-GL-DEBIT-CARD-PROCEEDS PIC 9(11) COMP-3.
           03  BR-WKDAY-BANKID           PIC 9(4) COMP-3.
           03  BR-PURGE-NOW-FLAG         PIC X.
           03  BR-EMAIL                  PIC X(50).
           03  BR-MANAGER-EMAIL          PIC X(50).
           03  BR-VERITEC-LICENSE        PIC 9(7).
      *     03  BR-TEST-PP-AUTO-INSCOVR   PIC X.
	  

      * COPY "LIBGB/GB01BR_SQL.CPY".
      * COPYMEMBER: LIBGB/GB01BR_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - BRANCH MASTER FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0430 ***NEW***
      * BAH 20220923 ADDED QBR-VERITEC-LICENSE #1570
      * BAH 20230419 ADDED QBR-TEST-PP-AUTO-INSCOVR #1594A
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  BR-CURSOR-STAT               PIC X(2)  VALUE SPACES.
           88  BR-CURSOR-STAT-GOOD                VALUE "00".
           88  BR-CURSOR-STAT-BAD                 VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  QBR-WHERE-BEG-KEY.
           03  QBR-WBEG-NO               PIC 9(4).
 
       01  QBR-WHERE-END-KEY.
           03  QBR-WEND-NO               PIC 9(4).
 
       01  QBR-REC.
           03  QBR-NO                                 PIC 9(4).
           03  QBR-LTOUCH-DATE                        PIC X(10).
           03  QBR-NAME                               PIC X(40).
           03  QBR-ADR1                               PIC X(24).
           03  QBR-ADR2                               PIC X(24).
           03  QBR-CITY                               PIC X(15).
           03  QBR-STATE                              PIC XX.
           03  QBR-ZIP                                PIC X(10).
           03  QBR-PHONE                              PIC 9(10).
           03  QBR-RETAIN                             PIC 9(11).
           03  QBR-NEXT-CHECK                         PIC 9(6).
           03  QBR-PRINTER-1                          PIC 99.
           03  QBR-PRINTER-2                          PIC 99.
           03  QBR-PRINTER-3                          PIC 99.
           03  QBR-PRINTER-4                          PIC 99.
           03  QBR-FEE-GL-BRNO                        PIC 9(4).
           03  QBR-REMIT1                             PIC X(30).
           03  QBR-REMIT2                             PIC X(30).
           03  QBR-REMIT3                             PIC X(30).
           03  QBR-STMNT1                             PIC X(60).
           03  QBR-STMNT2                             PIC X(60).
           03  QBR-INS-PRODUCER-NUM                   PIC 9(8).
           03  QBR-DLRCHK                             PIC X.
           03  QBR-FRONT-END-FG                       PIC X.
           03  QBR-EMLNRG-PHONE                       PIC X.
           03  QBR-POST-GL                            PIC X.
           03  QBR-ALLOTMNTS                          PIC X.
           03  QBR-EXTRACT-AUTO-UPD                   PIC X.
           03  QBR-PCNI-LEDGER                        PIC X.
           03  QBR-PRINTERFG-1                        PIC X.
           03  QBR-PRINTERFG-2                        PIC X.
           03  QBR-PRINTERFG-3                        PIC X.
           03  QBR-PRINTERFG-4                        PIC X.
           03  QBR-NEGDLRSV                           PIC X.
           03  QBR-CPCORP-ORDER                       PIC X.
           03  QBR-PRIO-MEMO-FLAG                     PIC X.
           03  QBR-PREBILL-PL-JD-OPT                  PIC X.
           03  QBR-LOAN-NUM-FLAG                      PIC X.
           03  QBR-DOWNPY-FLAG                        PIC X.
           03  QBR-AUTO-INS-EARN                      PIC X.
           03  QBR-OTH-INS-LEVELS                     PIC X.
           03  QBR-ALLOW-INTERBR                      PIC X.
           03  QBR-UNER-DLANRP                        PIC X.
           03  QBR-SECOND-PROMISE                     PIC X.
           03  QBR-AVAIL-CREDIT                       PIC X.
           03  QBR-RECP1                              PIC X(30).
           03  QBR-RECP2                              PIC X(30).
           03  QBR-INC-WAIVERS                        PIC X.
           03  QBR-INC-INS                            PIC X.
           03  QBR-BP-NEXT-BATCH                      PIC 9(2).
           03  QBR-BP-NOPAYOFF                        PIC X.
           03  QBR-BP-ODDPAY                          PIC X.
           03  QBR-BP-LC                              PIC X.
           03  QBR-BP-NOPRIN                          PIC X.
           03  QBR-BP-FLASH                           PIC X.
           03  QBR-BP-ACTIONCD                        PIC X.
           03  QBR-BP-MAXCON                          PIC 9(3).
           03  QBR-BP-MAXREC                          PIC 9(3).
           03  QBR-BP-PL                              PIC X.
           03  QBR-BP-JD                              PIC X.
           03  QBR-BP-BRANCH                          PIC 9(5).
           03  QBR-AGE-ERN                            PIC X.
           03  QBR-OTHER-BRANCH-GL                    PIC X.
           03  QBR-SALES-SMALL                        PIC X.
           03  QBR-NEW-COLL-INQUIRY                   PIC X.
           03  QBR-CHANGE-MEMO                        PIC X.
           03  QBR-INS-ADDON                          PIC X.
           03  QBR-TRAILER-REC                        PIC X.
           03  QBR-ALDEL-FLAG                         PIC X.
           03  QBR-IB-LCHG-MESS                       PIC X.
           03  QBR-COL-QUEUE-PAST-DUE                 PIC X.
           03  QBR-LI-DLCOMM-CMSI-PP-MAX              PIC 9(4)V99.
           03  QBR-ADDON-FEECD-O2-1                   PIC XX.
           03  QBR-ADDON-FEECD-O2-2                   PIC XX.
           03  QBR-AUTO-CHECK                         PIC X.
           03  QBR-RECEIPT-PROMDATE                   PIC X.
           03  QBR-RECEIPT-INTPD                      PIC X.
           03  QBR-COLL-ID-SCAN                       PIC X.
           03  QBR-STOP-ON-COLL                       PIC X.
           03  QBR-DUEDATE-CHANGE-CD                  PIC X.
           03  QBR-CHECK-EOM-FG                       PIC X.
           03  QBR-VOLUME-DLANRP                      PIC X.
           03  QBR-PDTH-NXDUE                         PIC X.
           03  QBR-LC-CODE                            PIC X.
           03  QBR-VALIDATE-PRUS                      PIC X.
           03  QBR-AUTO-OPEN-BAL                      PIC X.
           03  QBR-OPEN-BAL-1                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-2                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-3                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-4                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-5                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-6                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-7                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-8                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-9                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-10                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-11                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-12                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-13                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-14                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-15                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-16                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-17                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-18                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-19                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-20                        PIC S9(7)V99.
           03  QBR-TENDER-CD-1                        PIC X(5).
           03  QBR-TENDER-CD-2                        PIC X(5).
           03  QBR-TENDER-CD-3                        PIC X(5).
           03  QBR-DLPARV-ADJ-FG                      PIC X.
           03  QBR-OVERRIDE-BR-STATE                  PIC X.
           03  QBR-DEFAULT-SC-CLASS                   PIC 9(3).
           03  QBR-RECDEL-NOT-REC                     PIC X.
           03  QBR-POST-AHEAD-FG                      PIC X.
           03  QBR-CAN-LOANS-FG                       PIC X.
           03  QBR-NON-MONTHLY-CAN-LOANS              PIC X.
           03  QBR-BP-ALLOW-PMT-ACCT-OTHBAL           PIC X.
           03  QBR-LBOX-ALLOW-PMT-ACCT-OTHBAL         PIC X.
           03  QBR-REGION-NUMBER                      PIC 9(2).
           03  QBR-OUR-FEE                            PIC X.
           03  QBR-REB-METHOD-1                       PIC 99.
           03  QBR-REB-METHOD-2                       PIC 99.
           03  QBR-REB-METHOD-3                       PIC 99.
           03  QBR-REB-METHOD-4                       PIC 99.
           03  QBR-REB-METHOD-5                       PIC 99.
           03  QBR-REB-METHOD-6                       PIC 99.
           03  QBR-REB-METHOD-7                       PIC 99.
           03  QBR-REB-METHOD-8                       PIC 99.
           03  QBR-REB-METHOD-9                       PIC 99.
           03  QBR-REB-METHOD-10                      PIC 99.
           03  QBR-REB-METHOD-11                      PIC 99.
           03  QBR-REBDAYS-MONTH-1                    PIC 99.
           03  QBR-REBDAYS-MONTH-2                    PIC 99.
           03  QBR-REBDAYS-MONTH-3                    PIC 99.
           03  QBR-REBDAYS-MONTH-4                    PIC 99.
           03  QBR-REBDAYS-MONTH-5                    PIC 99.
           03  QBR-REBDAYS-MONTH-6                    PIC 99.
           03  QBR-REBDAYS-MONTH-7                    PIC 99.
           03  QBR-REBDAYS-MONTH-8                    PIC 99.
           03  QBR-REBDAYS-MONTH-9                    PIC 99.
           03  QBR-REBDAYS-MONTH-10                   PIC 99.
           03  QBR-REBDAYS-MONTH-11                   PIC 99.
           03  QBR-ACCRUAL-MONTHS-1                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-2                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-3                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-4                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-5                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-6                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-7                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-8                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-9                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-10                  PIC 999.
           03  QBR-ACCRUAL-MONTHS-11                  PIC 999.
           03  QBR-GLOVERSHORT                        PIC 9(11).
           03  QBR-GLBANK                             PIC 9(11).
           03  QBR-GLONHAND                           PIC 9(11).
           03  QBR-BANKBAL                            PIC S9(9)V99.
           03  QBR-RETAIN-AMT                         PIC S9(9)V99.
           03  QBR-LCARDFG                            PIC X.
           03  QBR-DATA-PATH                          PIC X(6).
           03  QBR-MACHINE                            PIC XX.
           03  QBR-MANAGER                            PIC X(25).
           03  QBR-EOD-PROCEDURE                      PIC XXX.
           03  QBR-BP-PAYOFF-NONCASH                  PIC X.
           03  QBR-BP-PAYOFF-OV                       PIC X.
           03  QBR-LI-DLCOMM-CMSI-O3-MAX              PIC 9(4)V99.
           03  QBR-LR-GROUP-COLLID                    PIC X.
           03  QBR-UNITPER-FG                         PIC X.
           03  QBR-UNITPER-Y-D-FG                     PIC X.
           03  QBR-NO-PROMPT-FOR-OTHBR                PIC X.
           03  QBR-WORLD-BRANCH-TYPE                  PIC X.
           03  QBR-COL-QUEUE-BEHAVIOR-FG              PIC X.
           03  QBR-CONVERT-AUTO-ACCTNO                PIC X.
           03  QBR-TS-REPO-INS-FG                     PIC X.
           03  QBR-BOOK-SF-BR-ONLY                    PIC X.
           03  QBR-FORCE-VALID-CITY-COUNTY            PIC X.
           03  QBR-SET-COLLCD-DL-MFGCD                PIC X.
           03  QBR-DEFAULT-SF-UNITPER-CD              PIC X.
           03  QBR-DEFAULT-SF-PURCD                   PIC X(3).
           03  QBR-DEFAULT-LN-UNITPER-CD              PIC X.
           03  QBR-DEFAULT-LN-PURCD                   PIC X(3).
           03  QBR-TAP-OTHER-DL                       PIC X.
           03  QBR-AH-AND-CL                          PIC X.
           03  QBR-NO-CASH-PROCEEDS                   PIC X.
           03  QBR-RI-BEFORE-REPO                     PIC X.
           03  QBR-RI-BEFORE-P4P5                     PIC X.
           03  QBR-NOT-OURS-REFUND                    PIC X.
           03  QBR-SFDLO3-WINDOW-FG                   PIC X.
           03  QBR-CALC-INS-ADJ                       PIC X.
           03  QBR-AC-BEFORE-REPO                     PIC X.
           03  QBR-AC-BEFORE-P4P5                     PIC X.
           03  QBR-BANK-ROUTING-NO                    PIC 9(9).
           03  QBR-BANK-CLEARING-GL                   PIC 9(11).
           03  QBR-GLBANK-DEPOSIT                     PIC 9(11).
           03  QBR-BANK-ACCTNO                        PIC X(17).
           03  QBR-DEFAULT-SC-SUBCLASS                PIC 9(3).
           03  QBR-NO-OUT-DLR-RSV                     PIC X.
           03  QBR-COLLECTOR-QUEUE                    PIC X.
           03  QBR-COLL-QUEUE-CON-REC                 PIC X.
           03  QBR-JE-ALLOCATION-SKIP                 PIC X.
           03  QBR-EXPAND-APP-INPUT                   PIC X.
           03  QBR-A50-CPCORP                         PIC X.
           03  QBR-INBR-BILNSC-FG                     PIC X.
           03  QBR-INBR-BRIEF-FG                      PIC X.
           03  QBR-LEDGER-JUST-INTCHG                 PIC X.
           03  QBR-BYINQ-START-PAGE                   PIC 99.
           03  QBR-DSINQ-START-PAGE                   PIC 99.
           03  QBR-APP-REQUIRED-FOR-INPUT             PIC X.
           03  QBR-CREDIT-SCORING-FG                  PIC X.
           03  QBR-COL-QUEUE-LOW-TO-HI                PIC X.
           03  QBR-CAN-GROUP-ID                       PIC X(4).
           03  QBR-AUTO-LNSTAT-WINDOW                 PIC X.
           03  QBR-TIME-ZONE-FACTOR                   PIC S9.
           03  QBR-LONPF-OPT-DISPLAY                  PIC X.
           03  QBR-GIFTNET                            PIC X.
           03  QBR-GIFTNET-LEDGER-LINE                PIC X.
           03  QBR-NO-FT-EMP                          PIC 9(3).
           03  QBR-NO-PT-EMP                          PIC 99V99.
           03  QBR-BALLOON-LOANS-FG                   PIC X.
           03  QBR-GL-TYPE-ALLOWED                    PIC X.
           03  QBR-GL-ENTRY-DEFAULT                   PIC X.
           03  QBR-MIDSTREAM-1STPAY-FG                PIC X.
           03  QBR-NODEF-1ST-X-MONTHS                 PIC 9.
           03  QBR-ADD-INS-FEECODE                    PIC XX.
           03  QBR-ROAD-G-SGL-FEECODE                 PIC XX.
           03  QBR-ROAD-G-FAM-FEECODE                 PIC XX.
           03  QBR-NODEF-IF-ZERO-AMT                  PIC X.
           03  QBR-ASSESS-LATE-CHGS                   PIC X.
           03  QBR-NEXT-SOLICIT-NO                    PIC 9(6).
           03  QBR-ADDON-FEECD-O3-1                   PIC XX.
           03  QBR-ADDON-FEECD-O3-2                   PIC XX.
           03  QBR-IMMEDIATE-RECEIPT                  PIC X.
           03  QBR-IMMEDIATE-DOC-PRINT                PIC X.
           03  QBR-AUTO-PL-REF-AC                     PIC X.
           03  QBR-FAX-NUMBER                         PIC 9(10).
           03  QBR-ALPHA-DRAWERS                      PIC X.
           03  QBR-LATECHG-ON-ALLOTMENTS              PIC X.
           03  QBR-ALLOW-SYS-AS-EFFDATE               PIC X(01).
           03  QBR-TAX-PREBOOK-CUST                   PIC 9(4).
           03  QBR-TAX-PREBOOK-NONCUST                PIC 9(4).
           03  QBR-PASSWORD-DRAWER                    PIC X.
           03  QBR-ONESTEP-RN-FG                      PIC X.
           03  QBR-LI-LISCAN-FG                       PIC X.
           03  QBR-HIDE-END-BAL                       PIC X(01).
           03  QBR-LBOX-NOPAYOFF                      PIC X.
           03  QBR-LBOX-ODDPAY                        PIC X.
           03  QBR-LBOX-LC                            PIC X.
           03  QBR-LBOX-NOPRIN                        PIC X.
           03  QBR-LBOX-FLASH                         PIC X.
           03  QBR-LBOX-ACTIONCD                      PIC X.
           03  QBR-LBOX-MAXCON                        PIC 9(3).
           03  QBR-LBOX-MAXREC                        PIC 9(3).
           03  QBR-LBOX-PL                            PIC X.
           03  QBR-LBOX-JD                            PIC X.
           03  QBR-LBOX                               PIC X(01).
           03  QBR-LATECHG-ON-LBOX                    PIC X(01).
           03  QBR-ALLOW-OV-TRCD                      PIC X.
           03  QBR-GLOBAL-QUEUE                       PIC X(01).
           03  QBR-LBOX-PAYOFF-NONCASH                PIC X.
           03  QBR-LBOX-PAYOFF-OV                     PIC X.
           03  QBR-COLLECT-MANAGER                    PIC X(25).
           03  QBR-COLLECT-PHONE                      PIC 9(10).
           03  QBR-COLLECT-EXT                        PIC 9(4).
           03  QBR-ALT-INTO-THIS-BRANCH               PIC X.
           03  QBR-POFF-TITLE-WINDOW                  PIC X.
           03  QBR-COUPON-STATEMENT                   PIC X.
           03  QBR-SUPERVISOR                         PIC X(25).
           03  QBR-AREA-MANAGER                       PIC X(25).
           03  QBR-MIN-MONTHS-DEF                     PIC 99.
           03  QBR-CC-FLAG                            PIC X.
           03  QBR-CC-ADR1                            PIC X(24).
           03  QBR-CC-CITY                            PIC X(15).
           03  QBR-CC-STATE                           PIC XX.
           03  QBR-CC-ZIP                             PIC X(10).
           03  QBR-MIN-MONTHS-D2                      PIC 99.
           03  QBR-SKIP-MIDSTREAM-TRW                 PIC X.
           03  QBR-GL-ESCROW-BALANCE                  PIC 9(11).
           03  QBR-GL-ESCROW-PAYABLE                  PIC 9(11).
           03  QBR-ALLOW-FORCE-OPEN-FG                PIC X.
           03  QBR-ACH-BANK-NO                        PIC 9.
           03  QBR-WIMENU-CHECK-WRITING               PIC X.
           03  QBR-ADP-PAY-GROUP                      PIC X(3).
           03  QBR-ALLOW-SCANNED-CKS                  PIC X.
           03  QBR-CHECK-DEPOSIT-GL                   PIC 9(11).
           03  QBR-OPTION-PST-LASER                   PIC X.
           03  QBR-USE-DEBIT-CARDS                    PIC X.
           03  QBR-GL-DEBIT-CARD-PROCEEDS             PIC 9(11).
           03  QBR-WKDAY-BANKID                       PIC 9(4).
           03  QBR-PURGE-NOW-FLAG                     PIC X.
           03  QBR-EMAIL                              PIC X(50).
           03  QBR-MANAGER-EMAIL                      PIC X(50).
           03  QBR-VERITEC-LICENSE                    PIC 9(7).
      *     03  QBR-TEST-PP-AUTO-INSCOVR               PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       01  PR-IFN        PIC X(2)    VALUE "PR".
       01  WK1-IFN       PIC X(3)    VALUE "WK1".
       01  WK2-IFN       PIC X(3)    VALUE "WK2".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED          PIC 99       VALUE 0.
       01  LN-COUNT         PIC 999      VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  DISPLAY-MESS.
           03  FILLER           PIC X(25)    VALUE
           "REPORT IN PROGRESS.......".
           03  DISPLAY-MESS-BR  PIC 9(4).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  SUMMARY-FG           PIC X        VALUE "N".
       01  BR-TOTALS            PIC X        VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).

       01  HOLD-LN-OWNBR        PIC 9(4)     VALUE 0.
       01  HOLD-WK-BRANCH       REDEFINES HOLD-LN-OWNBR
                                PIC 9(4).

       01  HOLD-LN-CLASS        PIC 9(3)     VALUE 0.
       01  HOLD-WK-CLASS        REDEFINES HOLD-LN-CLASS
                                PIC 9(3).
							
       01  OPEN-LN3-SW          PIC X     VALUE "N".
       01  START-LN3-SW         PIC X     VALUE "N".

       01  ACCERN               PIC S9(9)V99  COMP VALUE 0.
       01  ORIG-ANTIC           PIC S9(9)V99  COMP VALUE 0.
       01  WS-INT               PIC S9(9)V99  COMP VALUE 0.
       01  ADDON                PIC S9(9)V99  COMP VALUE 0.
       01  SRV                  PIC S9(9)V99  COMP VALUE 0.
       01  MFEE                 PIC S9(9)V99  COMP VALUE 0.
       01  WS-DISC              PIC S9(9)V99  COMP VALUE 0.
       01  W-OTHER              PIC S9(9)V99  COMP VALUE 0.
       01  ENDING               PIC S9(9)V99  COMP VALUE 0.
       01  SUB                  PIC 9(6) COMP-3.
       01  CLSUB                PIC 9(3) COMP-3.
       01  W-LOAN-NUM.
           03  FILLER           PIC XXX.
           03  LOAN-NUM         PIC X(13).

      *   TOTALS SUB 1 = CLASS
      *              2 = BRANCH
       01  TOTALS-TABLE.
           03  TOTAL-TBL          OCCURS 2.
               05  T-NOACCTS   PIC 9(6)         COMP.
               05  T-ACCERN    PIC S9(9)V99     COMP.
               05  T-INT       PIC S9(9)V99     COMP.
               05  T-ADDON     PIC S9(9)V99     COMP.
               05  T-SRV       PIC S9(9)V99     COMP.
               05  T-MFEE      PIC S9(9)V99     COMP.
               05  T-DISC      PIC S9(9)V99     COMP.
               05  T-OTHER     PIC S9(9)V99     COMP.
               05  T-ENDING    PIC S9(9)V99     COMP.

      ******************************************************************
      *    W S - G T T - T A B L E
      ******************************************************************
       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL       OCCURS 10 TIMES.
               05  WS-GTT-CLASS-TBL           OCCURS 101.
                   07  WS-GTT-NOACCTS         PIC 9(7)     COMP.
                   07  WS-GTT-ACCERN          PIC S9(9)V99 COMP.
                   07  WS-GTT-INT             PIC S9(9)V99 COMP.
                   07  WS-GTT-ADDON           PIC S9(9)V99 COMP.
                   07  WS-GTT-SRV             PIC S9(9)V99 COMP.
                   07  WS-GTT-MFEE            PIC S9(9)V99 COMP.
                   07  WS-GTT-DISC            PIC S9(9)V99 COMP.
                   07  WS-GTT-OTHER           PIC S9(9)V99 COMP.
                   07  WS-GTT-ENDING          PIC S9(9)V99 COMP.

       01  CLASS-FG            PIC X(3)      VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 999.
           03  END-CLASS       PIC 999.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  SUM-ONLY        PIC X.
           03  DEALER          PIC 9999V99.
           03  GTT-ISO         PIC X.
           03  TOTAL-LEV-TABLE.
               05 TOTAL-LEVEL-TABLE     PIC X(1)     OCCURS 10.

       01  VERSION-CONTROL     PIC X VALUE "C".

       01  SV-BEG-BR           PIC 9(4)       VALUE 9999.
       01  SV-END-BR           PIC 9(4)       VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(30)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(38)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(17)    VALUE " ".
           03  FILLER           PIC X(41)    VALUE
           "                 UNEARNED INCOME REPORT  ".
           03  FILLER           PIC X(53) VALUE " ".

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-BRNO      PIC 9(4).
           03  FILLER      PIC X        VALUE " ".
           03  H-BRDESC    PIC X(20).
           03  FILLER      PIC X(97)    VALUE " ".

       01  HEAD4.
           03  FILLER      PIC X(8)     VALUE "CLASS  :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-LCNO      PIC 99.
           03  FILLER      PIC X        VALUE " ".
           03  H-LCDESC    PIC X(20).
           03  FILLER      PIC X(99) VALUE " ".

       01  HEAD5.
           03  FILLER      PIC X(29)    VALUE SPACE.
           03  FILLER      PIC X(22)    VALUE
               "ANTICIPATED  ADJUST TO".
           03  FILLER      PIC X(44)    VALUE
               "   ------------------- UNEARNED INCOME -----".
           03  FILLER      PIC X(20)    VALUE
               "---------------     ".
           03  FILLER      PIC X(28)    VALUE
               " TOTAL     TOTAL            ".

       01  HEAD6.
           03  FILLER      PIC X(23)    VALUE
               "ACCOUNT NO.   BORROWER ".
           03  FILLER      PIC X(5)    VALUE SPACE.
           03  FILLER      PIC X(45)    VALUE
               "TERM EARNINGS    ANTIC.  INTEREST     ADDON  ".
           03  FILLER      PIC X(48)    VALUE
           "  SERCHG  MAINTFEE   DL DISC     OTHER  UNEARNED".
           03  FILLER      PIC X(10)    VALUE
               "    EARNED".

       01  NEW-BRANCH.
           03  FILLER           PIC X(8) VALUE "BRANCH  ".
           03  D-T-BRNO         PIC 9(4).
           03  FILLER           PIC XX.
           03  D-T-BRDESC       PIC X(20).

       01  DETAIL-LINE     PIC X(132) VALUE SPACES.
       01  D-LINE1         REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM  PIC X(13).
           03  FILLER      PIC X.
           03  D-BWNAME    PIC X(14).
           03  FILLER      PIC X.
           03  D-ANTERM    PIC ZZ9            BLANK ZERO.
           03  D-EARN      PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-ADJUST    PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-INT       PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-ADDON     PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-SRV       PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-MFEE      PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-DISC      PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-OTHER     PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-ENDING    PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-ACCERN    PIC ZZZZZ9.99-     BLANK ZERO.

       01  CLASS-TOTAL REDEFINES DETAIL-LINE.
           03  D-TAG            PIC X(24).
           03  FILLER           PIC X.
           03  D-T-NOACCTS-T    PIC ZZZZZ9.
           03  D-T-TAG-T        PIC X.
           03  FILLER           PIC X(17).
           03  D-T-INT-T        PIC ZZZZZZZZ9.99-   BLANK ZERO.
           03  FILLER           PIC X(7).
           03  D-T-SRV-T        PIC ZZZZZZZZ9.99-   BLANK ZERO.
           03  FILLER           PIC X(7).
           03  D-T-DISC-T       PIC ZZZZZZZZ9.99-   BLANK ZERO.
           03  FILLER           PIC X(7).
           03  D-T-ENDING-T     PIC ZZZZZZZZ9.99-   BLANK ZERO.

       01  CLASS-TOTAL-2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(59).
           03  D-T-ADDON-T      PIC ZZZZZZZZ9.99-   BLANK ZERO.
           03  FILLER           PIC X(7).
           03  D-T-MFEE-T       PIC ZZZZZZZZ9.99-   BLANK ZERO.
           03  FILLER           PIC X(7).
           03  D-T-OTHER-T      PIC ZZZZZZZZ9.99-   BLANK ZERO.
           03  FILLER           PIC X(7).
           03  D-T-ACCERN-T     PIC ZZZZZZZZ9.99-   BLANK ZERO.

       01  TOTAL-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC XXX.
           03  D-T-CLASS        PIC 99.
           03  FILLER           PIC X(12).
           03  D-T-NOACCTS      PIC ZZZZZZ9.
           03  D-T-TAG          PIC X.
           03  D-T-INT          PIC Z(8)9.99-   BLANK ZERO.
           03  D-T-ADDON        PIC Z(8)9.99-   BLANK ZERO.
           03  D-T-SRV          PIC Z(8)9.99-   BLANK ZERO.
           03  D-T-MFEE         PIC Z(8)9.99-   BLANK ZERO.
           03  D-T-DISC         PIC Z(8)9.99-   BLANK ZERO.
           03  D-T-OTHER        PIC Z(8)9.99-   BLANK ZERO.
           03  D-T-ENDING       PIC Z(8)9.99-   BLANK ZERO.
           03  D-T-ACCERN       PIC Z(8)9.99-   BLANK ZERO.
       01  BRANCH-GROUPID       REDEFINES DETAIL-LINE.
           03  D-TYPE           PIC X(10).
           03  D-GRID.
               05  D-BRNO       PIC 9(4).
           03  FILLER           PIC XX.
           03  D-BRDESC         PIC X(20).
           03  FILLER           PIC X(96).

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(112).
           03  FILLER           REDEFINES RANGE-SELECTION.
               05  RANGE-SELECTION-NUM  PIC 9(4).99.

       01  SUMMARY1.
           03  FILLER           PIC X(11) VALUE
               "**SUMMARY**".
           03  FILLER           PIC X(121) VALUE " ".

       01  SUMMARY2.
           03  FILLER           PIC X(22) VALUE SPACES.
           03  FILLER           PIC X(48) VALUE
               "NO     ------------------------ UNEARNED INCOME ".
           03  FILLER           PIC X(45) VALUE
               "-------------------------               TOTAL".
           03  FILLER           PIC X(13) VALUE "        TOTAL".

       01  SUMMARY3.
           03  FILLER           PIC X(29) VALUE
           "   CLASS           ACCTS     ".
           03  FILLER           PIC X(50) VALUE
           "INTEREST        ADDON       SERCHG     MAINTFEE   ".
           03  FILLER           PIC X(49) VALUE
           "   DL DISC        OTHER     UNEARNED       EARNED".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".

       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/UERNRP_DEF.CPY".
       COPY "LIBLP/UERNRP_WKS.CPY".

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/UERNRP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
      *     PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "UERNRP" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE  ENT-GROUP TO GR-ID.
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
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
             IF EXT-ROUTE-BUF = "00"
                MOVE BR-NO TO DISPLAY-MESS-BR
                MOVE DISPLAY-MESS TO MESS
                PERFORM SEND-LEGEND.

           PERFORM OPEN-BW1-FILE.
		   
           IF OPEN-LN3-SW = "N"
               MOVE "Y" TO OPEN-LN3-SW
               PERFORM OPEN-LN3-FILE.
      *    PERFORM OPEN-LN3-FILE.

      *    MOVE LN-PATH-OWNBR TO LN-OWNBR
           MOVE BR-NO         TO LN-OWNBR
                                 LN-PATH-OWNBR
                                 QLN3-WBEG-OWNBR
                                 QLN3-WEND-OWNBR.
           MOVE BEG-CLASS     TO LN-CLASS
                                 QLN3-WBEG-CLASS.
           MOVE END-CLASS     TO QLN3-WEND-CLASS.
           MOVE 0             TO LN-ACCTNO
                                 QLN3-WBEG-ACCTNO.
           MOVE ALL "9"       TO QLN3-WEND-ACCTNO.

      D    STOP MESS.

           PERFORM START-LN3-FILE.

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              GO TO NEXT-LN.

           IF DEALER NOT = 0
              IF LN-DLNO NOT = DEALER
                 GO TO NEXT-LN.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *       PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           PERFORM COMPUTE-EARNINGS.

      * NEED THE "AND" FOR REGENCY
      * ACCOUNTS WITH JUST UNEARN(6) WERE BEING SKIPPED BECAUSE THE
      * GP-INCL-OTH-IN-EARNED = "N" WHICH CAUSES EARN-TOT-UNEARN TO BE 0
      * BUT THEY NEED TO SEE THESE ACCOUNTS, PL#  253
           IF EARN-TOT-UNEARN = 0 AND EARN-UNEARN(6) = 0
             AND EARN-UNEARN(4) = 0
                GO TO NEXT-LN.

           IF SUM-ONLY = "Y"
              GO TO BYPASS-DETAIL.

           IF FIRST-TIME = "Y"
              MOVE LN-OWNBR TO HOLD-LN-OWNBR
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF LN-OWNBR NOT = HOLD-LN-OWNBR
              MOVE "Y" TO BR-TOTALS
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE LN-OWNBR TO HOLD-LN-OWNBR
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF LN-CLASS NOT = HOLD-LN-CLASS
              PERFORM CLASS-TOTALS
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS.

       BYPASS-DETAIL.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN3-FILE.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           MOVE "Y" TO BR-TOTALS.
           IF SUM-ONLY NOT = "Y"
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS.
           IF (GTT-ISO = "I" OR GTT-ISO = "O")
              MOVE 99 TO LN-COUNT
              MOVE "Y" TO SUMMARY-FG
              PERFORM GROUP-TOTAL-TABLE.
           IF GTT-ISO NOT = "O"
              PERFORM SUMMARY-TOTALS
              PERFORM CORPORATE-TOTALS.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      *************************************************
       INITIALIZATION SECTION.
      *************************************************

           MOVE UERNRP-QUEUE-POS TO Q-POS.
           MOVE UERNRP-COPIES-POS TO C-POS.
           MOVE UERNRP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.

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

           MOVE 0 TO T-NOACCTS(1) T-NOACCTS(2) T-ACCERN(1) T-ACCERN(2)
                     T-INT(1)     T-INT(2)     T-ADDON(1)  T-ADDON (2)
                     T-SRV(1)     T-SRV(2)     T-MFEE(1)   T-MFEE(2)
                     T-DISC(1)    T-DISC(2)    T-OTHER(1)  T-OTHER (2)
                     T-ENDING(1)  T-ENDING(2).

           MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS...................." TO MESS
              PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *************************************************
       EO-EXEC-AUDIT SECTION.
      *************************************************

      *************************************************
       ENTER-PARAMETERS SECTION.
      *************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO GTT-ISO-PARAGRAPH.

       BR-01.
           MOVE "E  A040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.

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

           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-01.

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
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CL-01.
      *----MOVE VDUNUM0 TO BEG-BRANCH.
      *----------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE " INVALID BRANCH RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE "S  A160DISPLAY-USER." TO VDU
                 MOVE "USER BRANCH ONLY" TO VDUBUF
                 PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CL-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              MOVE " INVALID LOAN CLASS RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO DL-NO.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       DL-NO.
           MOVE "ENNN042DLNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-NO.
           MOVE VDUNUM2 TO DEALER.
           GO TO SUMYN.
       ALL-DL.
           MOVE "S  A070DLNO." TO VDU.
           MOVE "****.**" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE 0 TO DEALER.
       SUMYN.
           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DL-NO.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SUMYN.
           MOVE VDUBUF TO SUM-ONLY.
           IF SUM-ONLY NOT = "N" AND NOT = "Y" GO TO SUMYN.
      *-----------------------------------------------------------------
       GTT-ISO-PARAGRAPH.

           IF ( GROUP-FLAG NOT = "Y" )
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "S  A010GTT-ISO." TO VDU
              MOVE "S" TO VDUBUF GTT-ISO
              PERFORM SCREENIO
              GO TO EXIT-PARM.

           MOVE "E  A010GTT-ISO." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO SUMYN.
           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO GTT-ISO-PARAGRAPH.

           IF NOT ( VDUBUF = "I" OR "S" OR "O" )
              GO TO GTT-ISO-PARAGRAPH.

           MOVE VDUBUF TO GTT-ISO.

           IF ( GTT-ISO = "I" OR "O" )
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE.


       EXIT-PARM.
           EXIT.

      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************

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

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "SN N020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           IF DEALER = 0
              MOVE "S  A070DLNO." TO VDU
              MOVE "****.**" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN042DLNO." TO VDU
              MOVE DEALER TO VDUNUM2
              PERFORM SCREENIO.
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO           TO VDUBUF.
           PERFORM SCREENIO.

      *************************************************
       DETAIL-LINE-PROCESS SECTION.
      *************************************************
           IF FIRST-TIME = "Y"
              MOVE 1 TO LN-FEED
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE "N" TO FIRST-TIME.

           PERFORM COMPUTE-EARNINGS.

           MOVE EARN-TOT-ACCERN TO ACCERN.
           MOVE EARN-TOT-UNEARN TO ENDING.

           MOVE EARN-UNEARN(1)  TO WS-INT.
           MOVE EARN-UNEARN(2)  TO ADDON.
           MOVE EARN-UNEARN(3)  TO SRV.
           MOVE EARN-UNEARN(4)  TO MFEE.
           MOVE EARN-UNEARN(5)  TO WS-DISC.
           MOVE EARN-UNEARN(6)  TO W-OTHER.

           ADD EARN-TOT-ANTICERN EARN-TOT-ANTICADJ
                                        GIVING ORIG-ANTIC.
           MOVE ORIG-ANTIC TO D-EARN.
           MOVE EARN-TOT-ANTICADJ TO D-ADJUST.

           PERFORM PROCESS-BORROWER.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO W-LOAN-NUM.
           MOVE LOAN-NUM         TO D-LOAN-NUM.
           MOVE LN-ANTICTERM TO D-ANTERM.
           MOVE ACCERN       TO D-ACCERN.
           MOVE WS-INT       TO D-INT.
           MOVE ADDON        TO D-ADDON.
           MOVE SRV          TO D-SRV.
           MOVE MFEE         TO D-MFEE.
           MOVE WS-DISC      TO D-DISC.
           MOVE W-OTHER      TO D-OTHER.
           MOVE ENDING       TO D-ENDING.

           IF SUM-ONLY NOT = "Y"
              PERFORM WRITE-DETAIL-LINE.

           PERFORM UPDATE-WK-FILE.
           ADD 1       TO T-NOACCTS(1) T-NOACCTS(2).
           ADD ACCERN  TO T-ACCERN(1)  T-ACCERN(2).
           ADD WS-INT  TO T-INT(1)     T-INT(2).
           ADD ADDON   TO T-ADDON(1)   T-ADDON(2).
           ADD SRV     TO T-SRV(1)     T-SRV(2).
           ADD MFEE    TO T-MFEE(1)    T-MFEE(2).
           ADD WS-DISC TO T-DISC(1)    T-DISC(2).
           ADD W-OTHER TO T-OTHER(1)   T-OTHER(2).
           ADD ENDING  TO T-ENDING(1)  T-ENDING(2).

       END-DETAIL-LN-PROCESS.
           EXIT.

      *************************************************
       UPDATE-WK-FILE SECTION.
      *************************************************

       UPDATE-WK.

           MOVE LN-OWNBR TO WK-BRANCH.
           MOVE LN-CLASS TO WK-CLASS.
           PERFORM READ-WK1-FILE.
           IF IO-FG = 0
              GO TO UPDATE-WK-FIELDS.

           MOVE SPACES   TO WK-REC.
           MOVE LN-OWNBR TO WK-BRANCH.
           MOVE LN-CLASS TO WK-CLASS.
           MOVE 0 TO WK-NOACCTS WK-ACCERN WK-INT   WK-ADDON WK-SRV
                     WK-MFEE    WK-DISC   WK-OTHER WK-ENDING.
           PERFORM WRITE-WK1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO UPDATE-WK.

       UPDATE-WK-FIELDS.
           ADD 1       TO WK-NOACCTS.
           ADD ACCERN  TO WK-ACCERN.
           ADD WS-INT  TO WK-INT.
           ADD ADDON   TO WK-ADDON.
           ADD SRV     TO WK-SRV.
           ADD MFEE    TO WK-MFEE.
           ADD WS-DISC TO WK-DISC.
           ADD W-OTHER TO WK-OTHER.
           ADD ENDING  TO WK-ENDING.
           PERFORM REWRITE-WK1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
       END-UPDATE-WK-FILE.
           EXIT.

      ******************************************
       PROCESS-BORROWER SECTION.
      ******************************************
           MOVE LN-SSNO(1) TO BW-SSNO.
      *    PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "**NOT ON FILE**" TO D-BWNAME
              GO TO END-PROCESS-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
               BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-BWNAME.
       END-PROCESS-BORROWER.
           EXIT.

      ******************************************
       HEAD-PAGE SECTION.
      ******************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           IF SUMMARY-FG = "Y"
              GO TO HEAD-SUMMARY.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           IF SUMMARY-FG = "Y"
              GO TO HEAD-SUMMARY.
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE HEAD5 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE HEAD6 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 6 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
           GO TO END-HEAD-PAGE.
       HEAD-SUMMARY.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE SUMMARY1 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE SUMMARY2 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE SUMMARY3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ******************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************

           MOVE 4 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEALER NO          :" TO RANGE-LINE.
           IF DEALER = 0
              MOVE "****.**"           TO RANGE-SELECTION
           ELSE
              MOVE DEALER              TO RANGE-SELECTION-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SUMMARY ONLY       :" TO RANGE-LINE.
           MOVE SUM-ONLY               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "GROUP TOTALS       :" TO RANGE-LINE.
           MOVE GTT-ISO                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************
       SET-BRANCH-HEADINGS SECTION.
      ******************************************
           MOVE HOLD-WK-BRANCH TO BR-NO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.

      *********************************************
       SET-CLASS-HEADINGS SECTION.
      *********************************************
           MOVE HOLD-WK-CLASS TO H-LCNO LC-CODE.
           PERFORM GET-LC.
           MOVE LC-DESC TO H-LCDESC.

      *********************************************
       CLASS-TOTALS SECTION.
      *********************************************
           MOVE 3 TO LN-FEED.
           MOVE "CLASS TOTALS:" TO D-TAG.
           MOVE 1 TO SUB.
           PERFORM PRINT-TOTALS.
           IF BR-TOTALS = "N"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED.
       END-CLASS-TOTALS.
           MOVE 0 TO T-NOACCTS(1) T-ACCERN(1) T-INT(1) T-ADDON(1)
                     T-SRV(1)     T-MFEE(1)   T-DISC(1) T-OTHER(1)
                     T-ENDING(1).

      *********************************************
       BRANCH-TOTALS SECTION.
      *********************************************
           MOVE 2 TO LN-FEED.
           MOVE "BRANCH TOTALS:" TO D-TAG.
           MOVE 2 TO SUB.
           IF SUMMARY-FG = "N"
              PERFORM PRINT-TOTALS
           ELSE
              PERFORM PRINT-SUM-BRANCH.
           IF SUMMARY-FG = "N"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED.
           MOVE "N" TO BR-TOTALS.
       END-BRANCH-TOTALS.
           MOVE 0 TO T-NOACCTS(2) T-ACCERN(2) T-INT(2) T-ADDON(2)
                     T-SRV(2)     T-MFEE(2)   T-DISC(2) T-OTHER(2)
                     T-ENDING(2).

      *********************************************
       PRINT-CORPORATE-TOTS SECTION.
      *********************************************
           MOVE 2 TO LN-FEED.
           MOVE "CORPORATE TOTALS:" TO D-TAG.
           MOVE 1 TO SUB.
           PERFORM PRINT-SUM-BRANCH.
       END-CORP-TOTALS.
           MOVE 0 TO T-NOACCTS(2) T-ACCERN(2) T-INT(2) T-ADDON(2)
                     T-SRV(2)     T-MFEE(2)   T-DISC(2) T-OTHER(2)
                     T-ENDING(2).

      *********************************************
       PRINT-TOTALS SECTION.
      *********************************************
           MOVE T-NOACCTS(SUB) TO D-T-NOACCTS-T.
           MOVE "#" TO D-T-TAG-T.
           MOVE T-ACCERN(SUB) TO D-T-ACCERN-T.
           MOVE T-ADDON(SUB) TO D-T-ADDON-T.
           MOVE T-MFEE(SUB) TO D-T-MFEE-T.
           MOVE T-OTHER(SUB) TO D-T-OTHER-T.
           PERFORM WRITE-DETAIL-LINE.
           MOVE T-INT(SUB) TO D-T-INT-T.
           MOVE T-SRV(SUB) TO D-T-SRV-T.
           MOVE T-DISC(SUB) TO D-T-DISC-T.
           MOVE T-ENDING(SUB) TO D-T-ENDING-T.
           PERFORM WRITE-DETAIL-LINE.

      *********************************************
       PRINT-SUM-BRANCH SECTION.
      *********************************************
           MOVE T-NOACCTS(SUB) TO D-T-NOACCTS.
           MOVE "#" TO D-T-TAG.
           MOVE T-ACCERN(SUB) TO D-T-ACCERN.
           MOVE T-ADDON(SUB) TO D-T-ADDON.
           MOVE T-MFEE(SUB) TO D-T-MFEE.
           MOVE T-OTHER(SUB) TO D-T-OTHER.
           MOVE T-INT(SUB) TO D-T-INT.
           MOVE T-SRV(SUB) TO D-T-SRV.
           MOVE T-DISC(SUB) TO D-T-DISC.
           MOVE T-ENDING(SUB) TO D-T-ENDING.
           PERFORM WRITE-DETAIL-LINE.

      *******************************************
       SUMMARY-TOTALS SECTION.
      *******************************************

           MOVE "Y" TO SUMMARY-FG FIRST-TIME.
           MOVE 0 TO T-NOACCTS(2) T-ACCERN(2) T-INT(2) T-ADDON(2)
                     T-SRV(2)     T-MFEE(2)   T-DISC(2) T-OTHER(2)
                     T-ENDING(2).
           PERFORM CLOSE-WK1-FILE.

           PERFORM OPEN-WK1-FILE.
           MOVE 0 TO WK-BRANCH WK-CLASS.
           PERFORM START-WK1-FILE.
       SUMMARY-NEXT-WK.
           PERFORM READ-WK1-FILE-NEXT.
           IF IO-FG = 9
              PERFORM BRANCH-TOTALS
              GO TO END-SUMMARY-TOTALS.

           IF FIRST-TIME = "Y"
              MOVE WK-BRANCH TO HOLD-WK-BRANCH BR-NO
              PERFORM READ-BR-FILE
              MOVE BR-NO TO D-T-BRNO
              MOVE BR-NAME TO D-T-BRDESC
              MOVE NEW-BRANCH TO DETAIL-LINE.

           IF WK-BRANCH NOT = HOLD-WK-BRANCH
              PERFORM BRANCH-TOTALS
              MOVE 2 TO LN-FEED
              MOVE WK-BRANCH TO HOLD-WK-BRANCH BR-NO
              PERFORM READ-BR-FILE
              MOVE BR-NO TO D-T-BRNO
              MOVE BR-NAME TO D-T-BRDESC
              MOVE NEW-BRANCH TO DETAIL-LINE
              MOVE 3 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           IF FIRST-TIME = "Y"
              MOVE 1 TO LN-FEED
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              PERFORM WRITE-DETAIL-LINE
              MOVE "N" TO FIRST-TIME.

           MOVE WK-CLASS TO LC-CODE.
           PERFORM GET-LC.
           MOVE LC-CODE TO D-T-CLASS.
           MOVE WK-NOACCTS TO D-T-NOACCTS.
           MOVE WK-ACCERN TO D-T-ACCERN.
           MOVE WK-INT TO D-T-INT.
           MOVE WK-ADDON TO D-T-ADDON.
           MOVE WK-SRV TO D-T-SRV.
           MOVE WK-MFEE TO D-T-MFEE.
           MOVE WK-DISC TO D-T-DISC.
           MOVE WK-OTHER TO D-T-OTHER.
           MOVE WK-ENDING TO D-T-ENDING.

           PERFORM WRITE-DETAIL-LINE.

           ADD WK-NOACCTS TO T-NOACCTS(2).
           ADD WK-ACCERN TO T-ACCERN(2).
           ADD WK-INT TO T-INT(2).
           ADD WK-ADDON TO T-ADDON(2).
           ADD WK-SRV TO T-SRV(2).
           ADD WK-MFEE TO T-MFEE(2).
           ADD WK-DISC TO T-DISC(2).
           ADD WK-OTHER TO T-OTHER(2).
           ADD WK-ENDING TO T-ENDING(2).
           GO TO SUMMARY-NEXT-WK.

       END-SUMMARY-TOTALS.
           EXIT.

      *********************************************
       CORPORATE-TOTALS SECTION.
      *********************************************
           MOVE "Y" TO SUMMARY-FG FIRST-TIME.
           MOVE 0 TO T-NOACCTS(1) T-ACCERN(1) T-INT(1)
                     T-ADDON(1) T-SRV(1) T-MFEE(1) T-DISC(1)
                     T-OTHER(1) T-ENDING(1).
           MOVE 0 TO T-NOACCTS(2) T-ACCERN(2) T-INT(2)
                     T-ADDON(2) T-SRV(2) T-MFEE(2) T-DISC(2)
                     T-OTHER(2) T-ENDING(2).
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.
           MOVE 0 TO WK-CLASS.
           PERFORM START-WK2-FILE.
       CORPORATE-NEXT-WK.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              PERFORM PRINT-CORPORATE
              PERFORM PRINT-CORPORATE-TOTS
              GO TO END-CORPORATE-TOTALS.
           IF FIRST-TIME = "Y"
              MOVE 3 TO LN-FEED
              MOVE "CORPORATE TOTALS:" TO D-T-BRDESC
              MOVE WK-CLASS TO HOLD-WK-CLASS
              MOVE "N" TO FIRST-TIME.
           IF WK-CLASS NOT = HOLD-WK-CLASS
              PERFORM PRINT-CORPORATE
              MOVE WK-CLASS TO HOLD-WK-CLASS.

           ADD WK-NOACCTS TO T-NOACCTS(2).
           ADD WK-ACCERN TO T-ACCERN(2).
           ADD WK-INT TO T-INT(2).
           ADD WK-ADDON TO T-ADDON(2).
           ADD WK-SRV TO T-SRV(2).
           ADD WK-MFEE TO T-MFEE(2).
           ADD WK-DISC TO T-DISC(2).
           ADD WK-OTHER TO T-OTHER(2).
           ADD WK-ENDING TO T-ENDING(2).
           GO TO CORPORATE-NEXT-WK.

       END-CORPORATE-TOTALS.
           PERFORM CLOSE-WK1-FILE.

      *********************************************
       PRINT-CORPORATE SECTION.
      *********************************************
           MOVE HOLD-WK-CLASS TO LC-CODE.
           PERFORM GET-LC.
           MOVE LC-CODE TO D-T-CLASS.
           MOVE T-NOACCTS(2) TO D-T-NOACCTS.
           MOVE "#" TO D-T-TAG.
           MOVE T-ACCERN(2) TO D-T-ACCERN.
           MOVE T-INT(2) TO D-T-INT.
           MOVE T-ADDON(2) TO D-T-ADDON.
           MOVE T-SRV(2) TO D-T-SRV.
           MOVE T-MFEE(2) TO D-T-MFEE.
           MOVE T-DISC(2) TO D-T-DISC.
           MOVE T-OTHER(2) TO D-T-OTHER.
           MOVE T-ENDING(2) TO D-T-ENDING.
           PERFORM WRITE-DETAIL-LINE.
           ADD T-NOACCTS(2) TO T-NOACCTS(1).
           ADD T-ACCERN(2) TO T-ACCERN(1).
           ADD T-INT(2) TO T-INT(1).
           ADD T-ADDON(2) TO T-ADDON(1).
           ADD T-SRV(2) TO T-SRV(1).
           ADD T-MFEE(2) TO T-MFEE(1).
           ADD T-DISC(2) TO T-DISC(1).
           ADD T-OTHER(2) TO T-OTHER(1).
           ADD T-ENDING(2) TO T-ENDING(1).
           MOVE 0 TO T-NOACCTS(2) T-ACCERN(2) T-INT(2)
                     T-ADDON(2) T-SRV(2) T-MFEE(2)
                     T-DISC(2) T-OTHER(2) T-ENDING(2).

      *********************************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      *********************************************

           MOVE 0 TO WK-CLASS.
           MOVE WGR-SRESULT TO WK-BRANCH.
           PERFORM START-WK1-FILE.

       GTT-NEXT-WK1.

           PERFORM READ-WK1-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           IF WK-BRANCH NOT = WGR-SRESULT
              GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           MOVE "N" TO GTT-FIRST-TIME.

       GTT-OK-TO-PROCESS.

           PERFORM GTT-CHECK-ADD-TOTALS.
           GO TO GTT-NEXT-WK1.

       EXIT-GTT-COMPILE-GROUP-TOTALS.
           EXIT.

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************
           IF WK-CLASS = 0
              GO TO EXIT-GTT-ADD-TOTALS.

           MOVE WK-CLASS TO CLSUB.
           PERFORM GTT-ADD-UNEARNED.
           MOVE 100 TO CLSUB.
           PERFORM GTT-ADD-UNEARNED.
           GO TO EXIT-GTT-ADD-TOTALS.
       GTT-ADD-UNEARNED.
           ADD WK-NOACCTS   TO WS-GTT-NOACCTS(GTT-TOTAL-SUB CLSUB).
           ADD WK-ACCERN    TO WS-GTT-ACCERN(GTT-TOTAL-SUB CLSUB).
           ADD WK-INT       TO WS-GTT-INT(GTT-TOTAL-SUB CLSUB).
           ADD WK-ADDON     TO WS-GTT-ADDON(GTT-TOTAL-SUB CLSUB).
           ADD WK-SRV       TO WS-GTT-SRV(GTT-TOTAL-SUB CLSUB).
           ADD WK-MFEE      TO WS-GTT-MFEE(GTT-TOTAL-SUB CLSUB).
           ADD WK-DISC      TO WS-GTT-DISC(GTT-TOTAL-SUB CLSUB).
           ADD WK-OTHER     TO WS-GTT-OTHER(GTT-TOTAL-SUB CLSUB).
           ADD WK-ENDING    TO WS-GTT-ENDING(GTT-TOTAL-SUB CLSUB).
       EXIT-GTT-ADD-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-BR-TOTALS SECTION.
      ******************************************************************

           MOVE WGR-SID(GTT-PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB              TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "BRANCH #&&&& NOT ON FILE" TO BR-NAME
              INSPECT BR-NAME REPLACING FIRST "&&&&" BY BR-NO.

           MOVE SPACES       TO DETAIL-LINE.
           MOVE 9            TO GTT-TOTAL-SUB.
           MOVE "BRANCH #: " TO D-TYPE.
           MOVE BR-NO        TO D-BRNO.
           MOVE BR-NAME      TO D-BRDESC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SPACES       TO DETAIL-LINE.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      ******************************************************************

           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP ID: " TO D-TYPE.
           MOVE GR-ID        TO D-GRID.
           MOVE GR-NAME      TO D-BRDESC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SPACES       TO DETAIL-LINE.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-TOTALS SECTION.
      ******************************************************************

           PERFORM VARYING CLSUB FROM 1 BY 1 UNTIL CLSUB > 100
              IF WS-GTT-NOACCTS(GTT-TOTAL-SUB, CLSUB) > 0
                 IF CLSUB < 100
                    MOVE CLSUB TO D-T-CLASS
                 ELSE
                    IF GTT-TOTAL-SUB = 9
                       MOVE "BRANCH TOTALS:" TO D-TAG
                    ELSE
                       MOVE "&&&& TOTALS:" TO D-TAG
                       INSPECT D-TAG REPLACING FIRST "&&&&" BY GR-ID
                    END-IF
                 END-IF
                 MOVE WS-GTT-NOACCTS(GTT-TOTAL-SUB CLSUB) TO D-T-NOACCTS
                 MOVE WS-GTT-INT(GTT-TOTAL-SUB CLSUB)     TO D-T-INT
                 MOVE WS-GTT-ADDON(GTT-TOTAL-SUB CLSUB)   TO D-T-ADDON
                 MOVE WS-GTT-SRV(GTT-TOTAL-SUB CLSUB)     TO D-T-SRV
                 MOVE WS-GTT-MFEE(GTT-TOTAL-SUB CLSUB)    TO D-T-MFEE
                 MOVE WS-GTT-DISC(GTT-TOTAL-SUB CLSUB)    TO D-T-DISC
                 MOVE WS-GTT-OTHER(GTT-TOTAL-SUB CLSUB)   TO D-T-OTHER
                 MOVE WS-GTT-ENDING(GTT-TOTAL-SUB, CLSUB) TO D-T-ENDING
                 MOVE WS-GTT-ACCERN(GTT-TOTAL-SUB CLSUB)  TO D-T-ACCERN
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           MOVE LOW-VALUES TO WS-GTT-TABLE-GROUP-LEVEL(GTT-TOTAL-SUB).

       EXIT-GTT-PRINT-TOTALS.
           EXIT.

      ***************************************************************
       CLASS-SCAN SECTION.
      ***************************************************************
           MOVE 0 TO LCSCAN-KEY.
           MOVE " " TO LCSCAN-VERIFY.
           MOVE LCSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF TO LCSCAN-BUF.
           IF SCAN-STATUS NOT = "00"
            OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SNNN020CL1." TO VDU
           ELSE
              MOVE "SNNN020CL2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.


      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO UERNRP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO UERNRP-SYSDATE.
           DISPLAY UERNRP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE UERNRP-FORM-FIELDS TO WR-BUF. 
           MOVE UERNRP-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM. 

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE UERNRP-HELP--FILE TO HELP-LINK-FILE.
           MOVE UERNRP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/UERNRP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      **************************************************
       MISC-PARAGRAPHS SECTION.
      **************************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       GET-LC.
      *    PERFORM READ-LC1-FILE.
           IF IO-FG = 9
              MOVE "*** NOT FOUND ***" TO LC-DESC.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************
       IO-ROUTINE SECTION.
      **************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
  
      * COPY "LIBGB/GBBRGS_SQL.CPY".
      * COPYMEMBER: LIBGB/GBBRGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - BRFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0507 ***NEW***
      * BAH 20220923 ADDED QBR-VERITEC-LICENSE #1570
      * BAH 20230419 ADDED QBR-TEST-PP-AUTO-INSCOVR #1594A
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD BR-REC FROM QBR-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-BR-FIELDS.
           MOVE QBR-NO TO BR-NO.

           IF ( QBR-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO BR-LTOUCH-DATE
           ELSE
              MOVE QBR-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BR-LTOUCH-DATE.

           MOVE QBR-NAME TO BR-NAME.
           MOVE QBR-ADR1 TO BR-ADR1.
           MOVE QBR-ADR2 TO BR-ADR2.
           MOVE QBR-CITY TO BR-CITY.
           MOVE QBR-STATE TO BR-STATE.
           MOVE QBR-ZIP TO BR-ZIP.
           MOVE QBR-PHONE TO BR-PHONE.
           MOVE QBR-RETAIN TO BR-RETAIN.
           MOVE QBR-NEXT-CHECK TO BR-NEXT-CHECK.
           MOVE QBR-PRINTER-1 TO BR-PRINTER(1).
           MOVE QBR-PRINTER-2 TO BR-PRINTER(2).
           MOVE QBR-PRINTER-3 TO BR-PRINTER(3).
           MOVE QBR-PRINTER-4 TO BR-PRINTER(4).
           MOVE QBR-FEE-GL-BRNO TO BR-FEE-GL-BRNO.
           MOVE QBR-REMIT1 TO BR-REMIT1.
           MOVE QBR-REMIT2 TO BR-REMIT2.
           MOVE QBR-REMIT3 TO BR-REMIT3.
           MOVE QBR-STMNT1 TO BR-STMNT1.
           MOVE QBR-STMNT2 TO BR-STMNT2.
           MOVE QBR-INS-PRODUCER-NUM TO BR-INS-PRODUCER-NUM.
           MOVE QBR-DLRCHK TO BR-DLRCHK.
           MOVE QBR-FRONT-END-FG TO BR-FRONT-END-FG.
           MOVE QBR-EMLNRG-PHONE TO BR-EMLNRG-PHONE.
           MOVE QBR-POST-GL TO BR-POST-GL.
           MOVE QBR-ALLOTMNTS TO BR-ALLOTMNTS.
           MOVE QBR-EXTRACT-AUTO-UPD TO BR-EXTRACT-AUTO-UPD.
           MOVE QBR-PCNI-LEDGER TO BR-PCNI-LEDGER.
           MOVE QBR-PRINTERFG-1 TO BR-PRINTERFG(1).
           MOVE QBR-PRINTERFG-2 TO BR-PRINTERFG(2).
           MOVE QBR-PRINTERFG-3 TO BR-PRINTERFG(3).
           MOVE QBR-PRINTERFG-4 TO BR-PRINTERFG(4).
           MOVE QBR-NEGDLRSV TO BR-NEGDLRSV.
           MOVE QBR-CPCORP-ORDER TO BR-CPCORP-ORDER.
           MOVE QBR-PRIO-MEMO-FLAG TO BR-PRIO-MEMO-FLAG.
           MOVE QBR-PREBILL-PL-JD-OPT TO BR-PREBILL-PL-JD-OPT.
           MOVE QBR-LOAN-NUM-FLAG TO BR-LOAN-NUM-FLAG.
           MOVE QBR-DOWNPY-FLAG TO BR-DOWNPY-FLAG.
           MOVE QBR-AUTO-INS-EARN TO BR-AUTO-INS-EARN.
           MOVE QBR-OTH-INS-LEVELS TO BR-OTH-INS-LEVELS.
           MOVE QBR-ALLOW-INTERBR TO BR-ALLOW-INTERBR.
           MOVE QBR-UNER-DLANRP TO BR-UNER-DLANRP.
           MOVE QBR-SECOND-PROMISE TO BR-SECOND-PROMISE.
           MOVE QBR-AVAIL-CREDIT TO BR-AVAIL-CREDIT.
           MOVE QBR-RECP1 TO BR-RECP1.
           MOVE QBR-RECP2 TO BR-RECP2.
           MOVE QBR-INC-WAIVERS TO BR-INC-WAIVERS.
           MOVE QBR-INC-INS TO BR-INC-INS.
           MOVE QBR-BP-NEXT-BATCH TO BR-BP-NEXT-BATCH.
           MOVE QBR-BP-NOPAYOFF TO BR-BP-NOPAYOFF.
           MOVE QBR-BP-ODDPAY TO BR-BP-ODDPAY.
           MOVE QBR-BP-LC TO BR-BP-LC.
           MOVE QBR-BP-NOPRIN TO BR-BP-NOPRIN.
           MOVE QBR-BP-FLASH TO BR-BP-FLASH.
           MOVE QBR-BP-ACTIONCD TO BR-BP-ACTIONCD.
           MOVE QBR-BP-MAXCON TO BR-BP-MAXCON.
           MOVE QBR-BP-MAXREC TO BR-BP-MAXREC.
           MOVE QBR-BP-PL TO BR-BP-PL.
           MOVE QBR-BP-JD TO BR-BP-JD.
           MOVE QBR-BP-BRANCH TO BR-BP-BRANCH.
           MOVE QBR-AGE-ERN TO BR-AGE-ERN.
           MOVE QBR-OTHER-BRANCH-GL TO BR-OTHER-BRANCH-GL.
           MOVE QBR-SALES-SMALL TO BR-SALES-SMALL.
           MOVE QBR-NEW-COLL-INQUIRY TO BR-NEW-COLL-INQUIRY.
           MOVE QBR-CHANGE-MEMO TO BR-CHANGE-MEMO.
           MOVE QBR-INS-ADDON TO BR-INS-ADDON.
           MOVE QBR-TRAILER-REC TO BR-TRAILER-REC.
           MOVE QBR-ALDEL-FLAG TO BR-ALDEL-FLAG.
           MOVE QBR-IB-LCHG-MESS TO BR-IB-LCHG-MESS.
           MOVE QBR-COL-QUEUE-PAST-DUE TO BR-COL-QUEUE-PAST-DUE.
           MOVE QBR-LI-DLCOMM-CMSI-PP-MAX TO BR-LI-DLCOMM-CMSI-PP-MAX.
           MOVE QBR-ADDON-FEECD-O2-1 TO BR-ADDON-FEECD-O2(1).
           MOVE QBR-ADDON-FEECD-O2-2 TO BR-ADDON-FEECD-O2(2).
           MOVE QBR-AUTO-CHECK TO BR-AUTO-CHECK.
           MOVE QBR-RECEIPT-PROMDATE TO BR-RECEIPT-PROMDATE.
           MOVE QBR-RECEIPT-INTPD TO BR-RECEIPT-INTPD.
           MOVE QBR-COLL-ID-SCAN TO BR-COLL-ID-SCAN.
           MOVE QBR-STOP-ON-COLL TO BR-STOP-ON-COLL.
           MOVE QBR-DUEDATE-CHANGE-CD TO BR-DUEDATE-CHANGE-CD.
           MOVE QBR-CHECK-EOM-FG TO BR-CHECK-EOM-FG.
           MOVE QBR-VOLUME-DLANRP TO BR-VOLUME-DLANRP.
           MOVE QBR-PDTH-NXDUE TO BR-PDTH-NXDUE.
           MOVE QBR-LC-CODE TO BR-LC-CODE.
           MOVE QBR-VALIDATE-PRUS TO BR-VALIDATE-PRUS.
           MOVE QBR-AUTO-OPEN-BAL TO BR-AUTO-OPEN-BAL.
           MOVE QBR-OPEN-BAL-1 TO BR-OPEN-BAL(1).
           MOVE QBR-OPEN-BAL-2 TO BR-OPEN-BAL(2).
           MOVE QBR-OPEN-BAL-3 TO BR-OPEN-BAL(3).
           MOVE QBR-OPEN-BAL-4 TO BR-OPEN-BAL(4).
           MOVE QBR-OPEN-BAL-5 TO BR-OPEN-BAL(5).
           MOVE QBR-OPEN-BAL-6 TO BR-OPEN-BAL(6).
           MOVE QBR-OPEN-BAL-7 TO BR-OPEN-BAL(7).
           MOVE QBR-OPEN-BAL-8 TO BR-OPEN-BAL(8).
           MOVE QBR-OPEN-BAL-9 TO BR-OPEN-BAL(9).
           MOVE QBR-OPEN-BAL-10 TO BR-OPEN-BAL(10).
           MOVE QBR-OPEN-BAL-11 TO BR-OPEN-BAL(11).
           MOVE QBR-OPEN-BAL-12 TO BR-OPEN-BAL(12).
           MOVE QBR-OPEN-BAL-13 TO BR-OPEN-BAL(13).
           MOVE QBR-OPEN-BAL-14 TO BR-OPEN-BAL(14).
           MOVE QBR-OPEN-BAL-15 TO BR-OPEN-BAL(15).
           MOVE QBR-OPEN-BAL-16 TO BR-OPEN-BAL(16).
           MOVE QBR-OPEN-BAL-17 TO BR-OPEN-BAL(17).
           MOVE QBR-OPEN-BAL-18 TO BR-OPEN-BAL(18).
           MOVE QBR-OPEN-BAL-19 TO BR-OPEN-BAL(19).
           MOVE QBR-OPEN-BAL-20 TO BR-OPEN-BAL(20).
           MOVE QBR-TENDER-CD-1 TO BR-TENDER-CD(1).
           MOVE QBR-TENDER-CD-2 TO BR-TENDER-CD(2).
           MOVE QBR-TENDER-CD-3 TO BR-TENDER-CD(3).
           MOVE QBR-DLPARV-ADJ-FG TO BR-DLPARV-ADJ-FG.
           MOVE QBR-OVERRIDE-BR-STATE TO BR-OVERRIDE-BR-STATE.
           MOVE QBR-DEFAULT-SC-CLASS TO BR-DEFAULT-SC-CLASS.
           MOVE QBR-RECDEL-NOT-REC TO BR-RECDEL-NOT-REC.
           MOVE QBR-POST-AHEAD-FG TO BR-POST-AHEAD-FG.
           MOVE QBR-CAN-LOANS-FG TO BR-CAN-LOANS-FG.
           MOVE QBR-NON-MONTHLY-CAN-LOANS TO BR-NON-MONTHLY-CAN-LOANS.
           MOVE QBR-BP-ALLOW-PMT-ACCT-OTHBAL
              TO BR-BP-ALLOW-PMT-ACCT-OTHBAL.
           MOVE QBR-LBOX-ALLOW-PMT-ACCT-OTHBAL
              TO BR-LBOX-ALLOW-PMT-ACCT-OTHBAL.
           MOVE QBR-REGION-NUMBER TO BR-REGION-NUMBER.
           MOVE QBR-OUR-FEE TO BR-OUR-FEE.
           MOVE QBR-REB-METHOD-1 TO BR-REB-METHOD(1).
           MOVE QBR-REB-METHOD-2 TO BR-REB-METHOD(2).
           MOVE QBR-REB-METHOD-3 TO BR-REB-METHOD(3).
           MOVE QBR-REB-METHOD-4 TO BR-REB-METHOD(4).
           MOVE QBR-REB-METHOD-5 TO BR-REB-METHOD(5).
           MOVE QBR-REB-METHOD-6 TO BR-REB-METHOD(6).
           MOVE QBR-REB-METHOD-7 TO BR-REB-METHOD(7).
           MOVE QBR-REB-METHOD-8 TO BR-REB-METHOD(8).
           MOVE QBR-REB-METHOD-9 TO BR-REB-METHOD(9).
           MOVE QBR-REB-METHOD-10 TO BR-REB-METHOD(10).
           MOVE QBR-REB-METHOD-11 TO BR-REB-METHOD(11).
           MOVE QBR-REBDAYS-MONTH-1 TO BR-REBDAYS-MONTH(1).
           MOVE QBR-REBDAYS-MONTH-2 TO BR-REBDAYS-MONTH(2).
           MOVE QBR-REBDAYS-MONTH-3 TO BR-REBDAYS-MONTH(3).
           MOVE QBR-REBDAYS-MONTH-4 TO BR-REBDAYS-MONTH(4).
           MOVE QBR-REBDAYS-MONTH-5 TO BR-REBDAYS-MONTH(5).
           MOVE QBR-REBDAYS-MONTH-6 TO BR-REBDAYS-MONTH(6).
           MOVE QBR-REBDAYS-MONTH-7 TO BR-REBDAYS-MONTH(7).
           MOVE QBR-REBDAYS-MONTH-8 TO BR-REBDAYS-MONTH(8).
           MOVE QBR-REBDAYS-MONTH-9 TO BR-REBDAYS-MONTH(9).
           MOVE QBR-REBDAYS-MONTH-10 TO BR-REBDAYS-MONTH(10).
           MOVE QBR-REBDAYS-MONTH-11 TO BR-REBDAYS-MONTH(11).
           MOVE QBR-ACCRUAL-MONTHS-1 TO BR-ACCRUAL-MONTHS(1).
           MOVE QBR-ACCRUAL-MONTHS-2 TO BR-ACCRUAL-MONTHS(2).
           MOVE QBR-ACCRUAL-MONTHS-3 TO BR-ACCRUAL-MONTHS(3).
           MOVE QBR-ACCRUAL-MONTHS-4 TO BR-ACCRUAL-MONTHS(4).
           MOVE QBR-ACCRUAL-MONTHS-5 TO BR-ACCRUAL-MONTHS(5).
           MOVE QBR-ACCRUAL-MONTHS-6 TO BR-ACCRUAL-MONTHS(6).
           MOVE QBR-ACCRUAL-MONTHS-7 TO BR-ACCRUAL-MONTHS(7).
           MOVE QBR-ACCRUAL-MONTHS-8 TO BR-ACCRUAL-MONTHS(8).
           MOVE QBR-ACCRUAL-MONTHS-9 TO BR-ACCRUAL-MONTHS(9).
           MOVE QBR-ACCRUAL-MONTHS-10 TO BR-ACCRUAL-MONTHS(10).
           MOVE QBR-ACCRUAL-MONTHS-11 TO BR-ACCRUAL-MONTHS(11).
           MOVE QBR-GLOVERSHORT TO BR-GLOVERSHORT.
           MOVE QBR-GLBANK TO BR-GLBANK.
           MOVE QBR-GLONHAND TO BR-GLONHAND.
           MOVE QBR-BANKBAL TO BR-BANKBAL.
           MOVE QBR-RETAIN-AMT TO BR-RETAIN-AMT.
           MOVE QBR-LCARDFG TO BR-LCARDFG.
           MOVE QBR-DATA-PATH TO BR-DATA-PATH.
           MOVE QBR-MACHINE TO BR-MACHINE.
           MOVE QBR-MANAGER TO BR-MANAGER.
           MOVE QBR-EOD-PROCEDURE TO BR-EOD-PROCEDURE.
           MOVE QBR-BP-PAYOFF-NONCASH TO BR-BP-PAYOFF-NONCASH.
           MOVE QBR-BP-PAYOFF-OV TO BR-BP-PAYOFF-OV.
           MOVE QBR-LI-DLCOMM-CMSI-O3-MAX TO BR-LI-DLCOMM-CMSI-O3-MAX.
           MOVE QBR-LR-GROUP-COLLID TO BR-LR-GROUP-COLLID.
           MOVE QBR-UNITPER-FG TO BR-UNITPER-FG.
           MOVE QBR-UNITPER-Y-D-FG TO BR-UNITPER-Y-D-FG.
           MOVE QBR-NO-PROMPT-FOR-OTHBR TO BR-NO-PROMPT-FOR-OTHBR.
           MOVE QBR-WORLD-BRANCH-TYPE TO BR-WORLD-BRANCH-TYPE.
           MOVE QBR-COL-QUEUE-BEHAVIOR-FG TO BR-COL-QUEUE-BEHAVIOR-FG.
           MOVE QBR-CONVERT-AUTO-ACCTNO TO BR-CONVERT-AUTO-ACCTNO.
           MOVE QBR-TS-REPO-INS-FG TO BR-TS-REPO-INS-FG.
           MOVE QBR-BOOK-SF-BR-ONLY TO BR-BOOK-SF-BR-ONLY.
           MOVE QBR-FORCE-VALID-CITY-COUNTY
              TO BR-FORCE-VALID-CITY-COUNTY.
           MOVE QBR-SET-COLLCD-DL-MFGCD TO BR-SET-COLLCD-DL-MFGCD.
           MOVE QBR-DEFAULT-SF-UNITPER-CD TO BR-DEFAULT-SF-UNITPER-CD.
           MOVE QBR-DEFAULT-SF-PURCD TO BR-DEFAULT-SF-PURCD.
           MOVE QBR-DEFAULT-LN-UNITPER-CD TO BR-DEFAULT-LN-UNITPER-CD.
           MOVE QBR-DEFAULT-LN-PURCD TO BR-DEFAULT-LN-PURCD.
           MOVE QBR-TAP-OTHER-DL TO BR-TAP-OTHER-DL.
           MOVE QBR-AH-AND-CL TO BR-AH-AND-CL.
           MOVE QBR-NO-CASH-PROCEEDS TO BR-NO-CASH-PROCEEDS.
           MOVE QBR-RI-BEFORE-REPO TO BR-RI-BEFORE-REPO.
           MOVE QBR-RI-BEFORE-P4P5 TO BR-RI-BEFORE-P4P5.
           MOVE QBR-NOT-OURS-REFUND TO BR-NOT-OURS-REFUND.
           MOVE QBR-SFDLO3-WINDOW-FG TO BR-SFDLO3-WINDOW-FG.
           MOVE QBR-CALC-INS-ADJ TO BR-CALC-INS-ADJ.
           MOVE QBR-AC-BEFORE-REPO TO BR-AC-BEFORE-REPO.
           MOVE QBR-AC-BEFORE-P4P5 TO BR-AC-BEFORE-P4P5.
           MOVE QBR-BANK-ROUTING-NO TO BR-BANK-ROUTING-NO.
           MOVE QBR-BANK-CLEARING-GL TO BR-BANK-CLEARING-GL.
           MOVE QBR-GLBANK-DEPOSIT TO BR-GLBANK-DEPOSIT.
           MOVE QBR-BANK-ACCTNO TO BR-BANK-ACCTNO.
           MOVE QBR-DEFAULT-SC-SUBCLASS TO BR-DEFAULT-SC-SUBCLASS.
           MOVE QBR-NO-OUT-DLR-RSV TO BR-NO-OUT-DLR-RSV.
           MOVE QBR-COLLECTOR-QUEUE TO BR-COLLECTOR-QUEUE.
           MOVE QBR-COLL-QUEUE-CON-REC TO BR-COLL-QUEUE-CON-REC.
           MOVE QBR-JE-ALLOCATION-SKIP TO BR-JE-ALLOCATION-SKIP.
           MOVE QBR-EXPAND-APP-INPUT TO BR-EXPAND-APP-INPUT.
           MOVE QBR-A50-CPCORP TO BR-A50-CPCORP.
           MOVE QBR-INBR-BILNSC-FG TO BR-INBR-BILNSC-FG.
           MOVE QBR-INBR-BRIEF-FG TO BR-INBR-BRIEF-FG.
           MOVE QBR-LEDGER-JUST-INTCHG TO BR-LEDGER-JUST-INTCHG.
           MOVE QBR-BYINQ-START-PAGE TO BR-BYINQ-START-PAGE.
           MOVE QBR-DSINQ-START-PAGE TO BR-DSINQ-START-PAGE.
           MOVE QBR-APP-REQUIRED-FOR-INPUT
              TO BR-APP-REQUIRED-FOR-INPUT.
           MOVE QBR-CREDIT-SCORING-FG TO BR-CREDIT-SCORING-FG.
           MOVE QBR-COL-QUEUE-LOW-TO-HI TO BR-COL-QUEUE-LOW-TO-HI.
           MOVE QBR-CAN-GROUP-ID TO BR-CAN-GROUP-ID.
           MOVE QBR-AUTO-LNSTAT-WINDOW TO BR-AUTO-LNSTAT-WINDOW.
           MOVE QBR-TIME-ZONE-FACTOR TO BR-TIME-ZONE-FACTOR.
           MOVE QBR-LONPF-OPT-DISPLAY TO BR-LONPF-OPT-DISPLAY.
           MOVE QBR-GIFTNET TO BR-GIFTNET.
           MOVE QBR-GIFTNET-LEDGER-LINE TO BR-GIFTNET-LEDGER-LINE.
           MOVE QBR-NO-FT-EMP TO BR-NO-FT-EMP.
           MOVE QBR-NO-PT-EMP TO BR-NO-PT-EMP.
           MOVE QBR-BALLOON-LOANS-FG TO BR-BALLOON-LOANS-FG.
           MOVE QBR-GL-TYPE-ALLOWED TO BR-GL-TYPE-ALLOWED.
           MOVE QBR-GL-ENTRY-DEFAULT TO BR-GL-ENTRY-DEFAULT.
           MOVE QBR-MIDSTREAM-1STPAY-FG TO BR-MIDSTREAM-1STPAY-FG.
           MOVE QBR-NODEF-1ST-X-MONTHS TO BR-NODEF-1ST-X-MONTHS.
           MOVE QBR-ADD-INS-FEECODE TO BR-ADD-INS-FEECODE.
           MOVE QBR-ROAD-G-SGL-FEECODE TO BR-ROAD-G-SGL-FEECODE.
           MOVE QBR-ROAD-G-FAM-FEECODE TO BR-ROAD-G-FAM-FEECODE.
           MOVE QBR-NODEF-IF-ZERO-AMT TO BR-NODEF-IF-ZERO-AMT.
           MOVE QBR-ASSESS-LATE-CHGS TO BR-ASSESS-LATE-CHGS.
           MOVE QBR-NEXT-SOLICIT-NO TO BR-NEXT-SOLICIT-NO.
           MOVE QBR-ADDON-FEECD-O3-1 TO BR-ADDON-FEECD-O3(1).
           MOVE QBR-ADDON-FEECD-O3-2 TO BR-ADDON-FEECD-O3(2).
           MOVE QBR-IMMEDIATE-RECEIPT TO BR-IMMEDIATE-RECEIPT.
           MOVE QBR-IMMEDIATE-DOC-PRINT TO BR-IMMEDIATE-DOC-PRINT.
           MOVE QBR-AUTO-PL-REF-AC TO BR-AUTO-PL-REF-AC.
           MOVE QBR-FAX-NUMBER TO BR-FAX-NUMBER.
           MOVE QBR-ALPHA-DRAWERS TO BR-ALPHA-DRAWERS.
           MOVE QBR-LATECHG-ON-ALLOTMENTS TO BR-LATECHG-ON-ALLOTMENTS.
           MOVE QBR-ALLOW-SYS-AS-EFFDATE TO BR-ALLOW-SYS-AS-EFFDATE.
           MOVE QBR-TAX-PREBOOK-CUST TO BR-TAX-PREBOOK-CUST.
           MOVE QBR-TAX-PREBOOK-NONCUST TO BR-TAX-PREBOOK-NONCUST.
           MOVE QBR-PASSWORD-DRAWER TO BR-PASSWORD-DRAWER.
           MOVE QBR-ONESTEP-RN-FG TO BR-ONESTEP-RN-FG.
           MOVE QBR-LI-LISCAN-FG TO BR-LI-LISCAN-FG.
           MOVE QBR-HIDE-END-BAL TO BR-HIDE-END-BAL.
           MOVE QBR-LBOX-NOPAYOFF TO BR-LBOX-NOPAYOFF.
           MOVE QBR-LBOX-ODDPAY TO BR-LBOX-ODDPAY.
           MOVE QBR-LBOX-LC TO BR-LBOX-LC.
           MOVE QBR-LBOX-NOPRIN TO BR-LBOX-NOPRIN.
           MOVE QBR-LBOX-FLASH TO BR-LBOX-FLASH.
           MOVE QBR-LBOX-ACTIONCD TO BR-LBOX-ACTIONCD.
           MOVE QBR-LBOX-MAXCON TO BR-LBOX-MAXCON.
           MOVE QBR-LBOX-MAXREC TO BR-LBOX-MAXREC.
           MOVE QBR-LBOX-PL TO BR-LBOX-PL.
           MOVE QBR-LBOX-JD TO BR-LBOX-JD.
           MOVE QBR-LBOX TO BR-LBOX.
           MOVE QBR-LATECHG-ON-LBOX TO BR-LATECHG-ON-LBOX.
           MOVE QBR-ALLOW-OV-TRCD TO BR-ALLOW-OV-TRCD.
           MOVE QBR-GLOBAL-QUEUE TO BR-GLOBAL-QUEUE.
           MOVE QBR-LBOX-PAYOFF-NONCASH TO BR-LBOX-PAYOFF-NONCASH.
           MOVE QBR-LBOX-PAYOFF-OV TO BR-LBOX-PAYOFF-OV.
           MOVE QBR-COLLECT-MANAGER TO BR-COLLECT-MANAGER.
           MOVE QBR-COLLECT-PHONE TO BR-COLLECT-PHONE.
           MOVE QBR-COLLECT-EXT TO BR-COLLECT-EXT.
           MOVE QBR-ALT-INTO-THIS-BRANCH TO BR-ALT-INTO-THIS-BRANCH.
           MOVE QBR-POFF-TITLE-WINDOW TO BR-POFF-TITLE-WINDOW.
           MOVE QBR-COUPON-STATEMENT TO BR-COUPON-STATEMENT.
           MOVE QBR-SUPERVISOR TO BR-SUPERVISOR.
           MOVE QBR-AREA-MANAGER TO BR-AREA-MANAGER.
           MOVE QBR-MIN-MONTHS-DEF TO BR-MIN-MONTHS-DEF.
           MOVE QBR-CC-FLAG TO BR-CC-FLAG.
           MOVE QBR-CC-ADR1 TO BR-CC-ADR1.
           MOVE QBR-CC-CITY TO BR-CC-CITY.
           MOVE QBR-CC-STATE TO BR-CC-STATE.
           MOVE QBR-CC-ZIP TO BR-CC-ZIP.
           MOVE QBR-MIN-MONTHS-D2 TO BR-MIN-MONTHS-D2.
           MOVE QBR-SKIP-MIDSTREAM-TRW TO BR-SKIP-MIDSTREAM-TRW.
           MOVE QBR-GL-ESCROW-BALANCE TO BR-GL-ESCROW-BALANCE.
           MOVE QBR-GL-ESCROW-PAYABLE TO BR-GL-ESCROW-PAYABLE.
           MOVE QBR-ALLOW-FORCE-OPEN-FG TO BR-ALLOW-FORCE-OPEN-FG.
           MOVE QBR-ACH-BANK-NO TO BR-ACH-BANK-NO.
           MOVE QBR-WIMENU-CHECK-WRITING TO BR-WIMENU-CHECK-WRITING.
           MOVE QBR-ADP-PAY-GROUP TO BR-ADP-PAY-GROUP.
           MOVE QBR-ALLOW-SCANNED-CKS TO BR-ALLOW-SCANNED-CKS.
           MOVE QBR-CHECK-DEPOSIT-GL TO BR-CHECK-DEPOSIT-GL.
           MOVE QBR-OPTION-PST-LASER TO BR-OPTION-PST-LASER.
           MOVE QBR-USE-DEBIT-CARDS TO BR-USE-DEBIT-CARDS.
           MOVE QBR-GL-DEBIT-CARD-PROCEEDS
                                    TO BR-GL-DEBIT-CARD-PROCEEDS.
           MOVE QBR-WKDAY-BANKID    TO BR-WKDAY-BANKID.
           MOVE QBR-PURGE-NOW-FLAG  TO BR-PURGE-NOW-FLAG.
           MOVE QBR-EMAIL           TO BR-EMAIL.
           MOVE QBR-MANAGER-EMAIL   TO BR-MANAGER-EMAIL.
           MOVE QBR-VERITEC-LICENSE TO BR-VERITEC-LICENSE.
      *     MOVE QBR-TEST-PP-AUTO-INSCOVR TO BR-TEST-PP-AUTO-INSCOVR.
 
      *-----------------------------------------------------------------
      *    LOAD QBR-REC FROM BR-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-BR-FIELDS.
           MOVE BR-NO TO QBR-NO.

           IF ( BR-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QBR-LTOUCH-DATE
           ELSE
              MOVE BR-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBR-LTOUCH-DATE.

           MOVE BR-NAME TO QBR-NAME.
           MOVE BR-ADR1 TO QBR-ADR1.
           MOVE BR-ADR2 TO QBR-ADR2.
           MOVE BR-CITY TO QBR-CITY.
           MOVE BR-STATE TO QBR-STATE.
           MOVE BR-ZIP TO QBR-ZIP.
           MOVE BR-PHONE TO QBR-PHONE.
           MOVE BR-RETAIN TO QBR-RETAIN.
           MOVE BR-NEXT-CHECK TO QBR-NEXT-CHECK.
           MOVE BR-PRINTER(1) TO QBR-PRINTER-1.
           MOVE BR-PRINTER(2) TO QBR-PRINTER-2.
           MOVE BR-PRINTER(3) TO QBR-PRINTER-3.
           MOVE BR-PRINTER(4) TO QBR-PRINTER-4.
           MOVE BR-FEE-GL-BRNO TO QBR-FEE-GL-BRNO.
           MOVE BR-REMIT1 TO QBR-REMIT1.
           MOVE BR-REMIT2 TO QBR-REMIT2.
           MOVE BR-REMIT3 TO QBR-REMIT3.
           MOVE BR-STMNT1 TO QBR-STMNT1.
           MOVE BR-STMNT2 TO QBR-STMNT2.
           MOVE BR-INS-PRODUCER-NUM TO QBR-INS-PRODUCER-NUM.
           MOVE BR-DLRCHK TO QBR-DLRCHK.
           MOVE BR-FRONT-END-FG TO QBR-FRONT-END-FG.
           MOVE BR-EMLNRG-PHONE TO QBR-EMLNRG-PHONE.
           MOVE BR-POST-GL TO QBR-POST-GL.
           MOVE BR-ALLOTMNTS TO QBR-ALLOTMNTS.
           MOVE BR-EXTRACT-AUTO-UPD TO QBR-EXTRACT-AUTO-UPD.
           MOVE BR-PCNI-LEDGER TO QBR-PCNI-LEDGER.
           MOVE BR-PRINTERFG(1) TO QBR-PRINTERFG-1.
           MOVE BR-PRINTERFG(2) TO QBR-PRINTERFG-2.
           MOVE BR-PRINTERFG(3) TO QBR-PRINTERFG-3.
           MOVE BR-PRINTERFG(4) TO QBR-PRINTERFG-4.
           MOVE BR-NEGDLRSV TO QBR-NEGDLRSV.
           MOVE BR-CPCORP-ORDER TO QBR-CPCORP-ORDER.
           MOVE BR-PRIO-MEMO-FLAG TO QBR-PRIO-MEMO-FLAG.
           MOVE BR-PREBILL-PL-JD-OPT TO QBR-PREBILL-PL-JD-OPT.
           MOVE BR-LOAN-NUM-FLAG TO QBR-LOAN-NUM-FLAG.
           MOVE BR-DOWNPY-FLAG TO QBR-DOWNPY-FLAG.
           MOVE BR-AUTO-INS-EARN TO QBR-AUTO-INS-EARN.
           MOVE BR-OTH-INS-LEVELS TO QBR-OTH-INS-LEVELS.
           MOVE BR-ALLOW-INTERBR TO QBR-ALLOW-INTERBR.
           MOVE BR-UNER-DLANRP TO QBR-UNER-DLANRP.
           MOVE BR-SECOND-PROMISE TO QBR-SECOND-PROMISE.
           MOVE BR-AVAIL-CREDIT TO QBR-AVAIL-CREDIT.
           MOVE BR-RECP1 TO QBR-RECP1.
           MOVE BR-RECP2 TO QBR-RECP2.
           MOVE BR-INC-WAIVERS TO QBR-INC-WAIVERS.
           MOVE BR-INC-INS TO QBR-INC-INS.
           MOVE BR-BP-NEXT-BATCH TO QBR-BP-NEXT-BATCH.
           MOVE BR-BP-NOPAYOFF TO QBR-BP-NOPAYOFF.
           MOVE BR-BP-ODDPAY TO QBR-BP-ODDPAY.
           MOVE BR-BP-LC TO QBR-BP-LC.
           MOVE BR-BP-NOPRIN TO QBR-BP-NOPRIN.
           MOVE BR-BP-FLASH TO QBR-BP-FLASH.
           MOVE BR-BP-ACTIONCD TO QBR-BP-ACTIONCD.
           MOVE BR-BP-MAXCON TO QBR-BP-MAXCON.
           MOVE BR-BP-MAXREC TO QBR-BP-MAXREC.
           MOVE BR-BP-PL TO QBR-BP-PL.
           MOVE BR-BP-JD TO QBR-BP-JD.
           MOVE BR-BP-BRANCH TO QBR-BP-BRANCH.
           MOVE BR-AGE-ERN TO QBR-AGE-ERN.
           MOVE BR-OTHER-BRANCH-GL TO QBR-OTHER-BRANCH-GL.
           MOVE BR-SALES-SMALL TO QBR-SALES-SMALL.
           MOVE BR-NEW-COLL-INQUIRY TO QBR-NEW-COLL-INQUIRY.
           MOVE BR-CHANGE-MEMO TO QBR-CHANGE-MEMO.
           MOVE BR-INS-ADDON TO QBR-INS-ADDON.
           MOVE BR-TRAILER-REC TO QBR-TRAILER-REC.
           MOVE BR-ALDEL-FLAG TO QBR-ALDEL-FLAG.
           MOVE BR-IB-LCHG-MESS TO QBR-IB-LCHG-MESS.
           MOVE BR-COL-QUEUE-PAST-DUE TO QBR-COL-QUEUE-PAST-DUE.
           MOVE BR-LI-DLCOMM-CMSI-PP-MAX TO QBR-LI-DLCOMM-CMSI-PP-MAX.
           MOVE BR-ADDON-FEECD-O2(1) TO QBR-ADDON-FEECD-O2-1.
           MOVE BR-ADDON-FEECD-O2(2) TO QBR-ADDON-FEECD-O2-2.
           MOVE BR-AUTO-CHECK TO QBR-AUTO-CHECK.
           MOVE BR-RECEIPT-PROMDATE TO QBR-RECEIPT-PROMDATE.
           MOVE BR-RECEIPT-INTPD TO QBR-RECEIPT-INTPD.
           MOVE BR-COLL-ID-SCAN TO QBR-COLL-ID-SCAN.
           MOVE BR-STOP-ON-COLL TO QBR-STOP-ON-COLL.
           MOVE BR-DUEDATE-CHANGE-CD TO QBR-DUEDATE-CHANGE-CD.
           MOVE BR-CHECK-EOM-FG TO QBR-CHECK-EOM-FG.
           MOVE BR-VOLUME-DLANRP TO QBR-VOLUME-DLANRP.
           MOVE BR-PDTH-NXDUE TO QBR-PDTH-NXDUE.
           MOVE BR-LC-CODE TO QBR-LC-CODE.
           MOVE BR-VALIDATE-PRUS TO QBR-VALIDATE-PRUS.
           MOVE BR-AUTO-OPEN-BAL TO QBR-AUTO-OPEN-BAL.
           MOVE BR-OPEN-BAL(1) TO QBR-OPEN-BAL-1.
           MOVE BR-OPEN-BAL(2) TO QBR-OPEN-BAL-2.
           MOVE BR-OPEN-BAL(3) TO QBR-OPEN-BAL-3.
           MOVE BR-OPEN-BAL(4) TO QBR-OPEN-BAL-4.
           MOVE BR-OPEN-BAL(5) TO QBR-OPEN-BAL-5.
           MOVE BR-OPEN-BAL(6) TO QBR-OPEN-BAL-6.
           MOVE BR-OPEN-BAL(7) TO QBR-OPEN-BAL-7.
           MOVE BR-OPEN-BAL(8) TO QBR-OPEN-BAL-8.
           MOVE BR-OPEN-BAL(9) TO QBR-OPEN-BAL-9.
           MOVE BR-OPEN-BAL(10) TO QBR-OPEN-BAL-10.
           MOVE BR-OPEN-BAL(11) TO QBR-OPEN-BAL-11.
           MOVE BR-OPEN-BAL(12) TO QBR-OPEN-BAL-12.
           MOVE BR-OPEN-BAL(13) TO QBR-OPEN-BAL-13.
           MOVE BR-OPEN-BAL(14) TO QBR-OPEN-BAL-14.
           MOVE BR-OPEN-BAL(15) TO QBR-OPEN-BAL-15.
           MOVE BR-OPEN-BAL(16) TO QBR-OPEN-BAL-16.
           MOVE BR-OPEN-BAL(17) TO QBR-OPEN-BAL-17.
           MOVE BR-OPEN-BAL(18) TO QBR-OPEN-BAL-18.
           MOVE BR-OPEN-BAL(19) TO QBR-OPEN-BAL-19.
           MOVE BR-OPEN-BAL(20) TO QBR-OPEN-BAL-20.
           MOVE BR-TENDER-CD(1) TO QBR-TENDER-CD-1.
           MOVE BR-TENDER-CD(2) TO QBR-TENDER-CD-2.
           MOVE BR-TENDER-CD(3) TO QBR-TENDER-CD-3.
           MOVE BR-DLPARV-ADJ-FG TO QBR-DLPARV-ADJ-FG.
           MOVE BR-OVERRIDE-BR-STATE TO QBR-OVERRIDE-BR-STATE.
           MOVE BR-DEFAULT-SC-CLASS TO QBR-DEFAULT-SC-CLASS.
           MOVE BR-RECDEL-NOT-REC TO QBR-RECDEL-NOT-REC.
           MOVE BR-POST-AHEAD-FG TO QBR-POST-AHEAD-FG.
           MOVE BR-CAN-LOANS-FG TO QBR-CAN-LOANS-FG.
           MOVE BR-NON-MONTHLY-CAN-LOANS TO QBR-NON-MONTHLY-CAN-LOANS.
           MOVE BR-BP-ALLOW-PMT-ACCT-OTHBAL
            TO QBR-BP-ALLOW-PMT-ACCT-OTHBAL.
           MOVE BR-LBOX-ALLOW-PMT-ACCT-OTHBAL
            TO QBR-LBOX-ALLOW-PMT-ACCT-OTHBAL.
           MOVE BR-REGION-NUMBER TO QBR-REGION-NUMBER.
           MOVE BR-OUR-FEE TO QBR-OUR-FEE.
           MOVE BR-REB-METHOD(1) TO QBR-REB-METHOD-1.
           MOVE BR-REB-METHOD(2) TO QBR-REB-METHOD-2.
           MOVE BR-REB-METHOD(3) TO QBR-REB-METHOD-3.
           MOVE BR-REB-METHOD(4) TO QBR-REB-METHOD-4.
           MOVE BR-REB-METHOD(5) TO QBR-REB-METHOD-5.
           MOVE BR-REB-METHOD(6) TO QBR-REB-METHOD-6.
           MOVE BR-REB-METHOD(7) TO QBR-REB-METHOD-7.
           MOVE BR-REB-METHOD(8) TO QBR-REB-METHOD-8.
           MOVE BR-REB-METHOD(9) TO QBR-REB-METHOD-9.
           MOVE BR-REB-METHOD(10) TO QBR-REB-METHOD-10.
           MOVE BR-REB-METHOD(11) TO QBR-REB-METHOD-11.
           MOVE BR-REBDAYS-MONTH(1) TO QBR-REBDAYS-MONTH-1.
           MOVE BR-REBDAYS-MONTH(2) TO QBR-REBDAYS-MONTH-2.
           MOVE BR-REBDAYS-MONTH(3) TO QBR-REBDAYS-MONTH-3.
           MOVE BR-REBDAYS-MONTH(4) TO QBR-REBDAYS-MONTH-4.
           MOVE BR-REBDAYS-MONTH(5) TO QBR-REBDAYS-MONTH-5.
           MOVE BR-REBDAYS-MONTH(6) TO QBR-REBDAYS-MONTH-6.
           MOVE BR-REBDAYS-MONTH(7) TO QBR-REBDAYS-MONTH-7.
           MOVE BR-REBDAYS-MONTH(8) TO QBR-REBDAYS-MONTH-8.
           MOVE BR-REBDAYS-MONTH(9) TO QBR-REBDAYS-MONTH-9.
           MOVE BR-REBDAYS-MONTH(10) TO QBR-REBDAYS-MONTH-10.
           MOVE BR-REBDAYS-MONTH(11) TO QBR-REBDAYS-MONTH-11.
           MOVE BR-ACCRUAL-MONTHS(1) TO QBR-ACCRUAL-MONTHS-1.
           MOVE BR-ACCRUAL-MONTHS(2) TO QBR-ACCRUAL-MONTHS-2.
           MOVE BR-ACCRUAL-MONTHS(3) TO QBR-ACCRUAL-MONTHS-3.
           MOVE BR-ACCRUAL-MONTHS(4) TO QBR-ACCRUAL-MONTHS-4.
           MOVE BR-ACCRUAL-MONTHS(5) TO QBR-ACCRUAL-MONTHS-5.
           MOVE BR-ACCRUAL-MONTHS(6) TO QBR-ACCRUAL-MONTHS-6.
           MOVE BR-ACCRUAL-MONTHS(7) TO QBR-ACCRUAL-MONTHS-7.
           MOVE BR-ACCRUAL-MONTHS(8) TO QBR-ACCRUAL-MONTHS-8.
           MOVE BR-ACCRUAL-MONTHS(9) TO QBR-ACCRUAL-MONTHS-9.
           MOVE BR-ACCRUAL-MONTHS(10) TO QBR-ACCRUAL-MONTHS-10.
           MOVE BR-ACCRUAL-MONTHS(11) TO QBR-ACCRUAL-MONTHS-11.
           MOVE BR-GLOVERSHORT TO QBR-GLOVERSHORT.
           MOVE BR-GLBANK TO QBR-GLBANK.
           MOVE BR-GLONHAND TO QBR-GLONHAND.
           MOVE BR-BANKBAL TO QBR-BANKBAL.
           MOVE BR-RETAIN-AMT TO QBR-RETAIN-AMT.
           MOVE BR-LCARDFG TO QBR-LCARDFG.
           MOVE BR-DATA-PATH TO QBR-DATA-PATH.
           MOVE BR-MACHINE TO QBR-MACHINE.
           MOVE BR-MANAGER TO QBR-MANAGER.
           MOVE BR-EOD-PROCEDURE TO QBR-EOD-PROCEDURE.
           MOVE BR-BP-PAYOFF-NONCASH TO QBR-BP-PAYOFF-NONCASH.
           MOVE BR-BP-PAYOFF-OV TO QBR-BP-PAYOFF-OV.
           MOVE BR-LI-DLCOMM-CMSI-O3-MAX TO QBR-LI-DLCOMM-CMSI-O3-MAX.
           MOVE BR-LR-GROUP-COLLID TO QBR-LR-GROUP-COLLID.
           MOVE BR-UNITPER-FG TO QBR-UNITPER-FG.
           MOVE BR-UNITPER-Y-D-FG TO QBR-UNITPER-Y-D-FG.
           MOVE BR-NO-PROMPT-FOR-OTHBR TO QBR-NO-PROMPT-FOR-OTHBR.
           MOVE BR-WORLD-BRANCH-TYPE TO QBR-WORLD-BRANCH-TYPE.
           MOVE BR-COL-QUEUE-BEHAVIOR-FG TO QBR-COL-QUEUE-BEHAVIOR-FG.
           MOVE BR-CONVERT-AUTO-ACCTNO TO QBR-CONVERT-AUTO-ACCTNO.
           MOVE BR-TS-REPO-INS-FG TO QBR-TS-REPO-INS-FG.
           MOVE BR-BOOK-SF-BR-ONLY TO QBR-BOOK-SF-BR-ONLY.
           MOVE BR-FORCE-VALID-CITY-COUNTY
            TO QBR-FORCE-VALID-CITY-COUNTY.
           MOVE BR-SET-COLLCD-DL-MFGCD TO QBR-SET-COLLCD-DL-MFGCD.
           MOVE BR-DEFAULT-SF-UNITPER-CD TO QBR-DEFAULT-SF-UNITPER-CD.
           MOVE BR-DEFAULT-SF-PURCD TO QBR-DEFAULT-SF-PURCD.
           MOVE BR-DEFAULT-LN-UNITPER-CD TO QBR-DEFAULT-LN-UNITPER-CD.
           MOVE BR-DEFAULT-LN-PURCD TO QBR-DEFAULT-LN-PURCD.
           MOVE BR-TAP-OTHER-DL TO QBR-TAP-OTHER-DL.
           MOVE BR-AH-AND-CL TO QBR-AH-AND-CL.
           MOVE BR-NO-CASH-PROCEEDS TO QBR-NO-CASH-PROCEEDS.
           MOVE BR-RI-BEFORE-REPO TO QBR-RI-BEFORE-REPO.
           MOVE BR-RI-BEFORE-P4P5 TO QBR-RI-BEFORE-P4P5.
           MOVE BR-NOT-OURS-REFUND TO QBR-NOT-OURS-REFUND.
           MOVE BR-SFDLO3-WINDOW-FG TO QBR-SFDLO3-WINDOW-FG.
           MOVE BR-CALC-INS-ADJ TO QBR-CALC-INS-ADJ.
           MOVE BR-AC-BEFORE-REPO TO QBR-AC-BEFORE-REPO.
           MOVE BR-AC-BEFORE-P4P5 TO QBR-AC-BEFORE-P4P5.
           MOVE BR-BANK-ROUTING-NO TO QBR-BANK-ROUTING-NO.
           MOVE BR-BANK-CLEARING-GL TO QBR-BANK-CLEARING-GL.
           MOVE BR-GLBANK-DEPOSIT TO QBR-GLBANK-DEPOSIT.
           MOVE BR-BANK-ACCTNO TO QBR-BANK-ACCTNO.
           MOVE BR-DEFAULT-SC-SUBCLASS TO QBR-DEFAULT-SC-SUBCLASS.
           MOVE BR-NO-OUT-DLR-RSV TO QBR-NO-OUT-DLR-RSV.
           MOVE BR-COLLECTOR-QUEUE TO QBR-COLLECTOR-QUEUE.
           MOVE BR-COLL-QUEUE-CON-REC TO QBR-COLL-QUEUE-CON-REC.
           MOVE BR-JE-ALLOCATION-SKIP TO QBR-JE-ALLOCATION-SKIP.
           MOVE BR-EXPAND-APP-INPUT TO QBR-EXPAND-APP-INPUT.
           MOVE BR-A50-CPCORP TO QBR-A50-CPCORP.
           MOVE BR-INBR-BILNSC-FG TO QBR-INBR-BILNSC-FG.
           MOVE BR-INBR-BRIEF-FG TO QBR-INBR-BRIEF-FG.
           MOVE BR-LEDGER-JUST-INTCHG TO QBR-LEDGER-JUST-INTCHG.
           MOVE BR-BYINQ-START-PAGE TO QBR-BYINQ-START-PAGE.
           MOVE BR-DSINQ-START-PAGE TO QBR-DSINQ-START-PAGE.
           MOVE BR-APP-REQUIRED-FOR-INPUT
            TO QBR-APP-REQUIRED-FOR-INPUT.
           MOVE BR-CREDIT-SCORING-FG TO QBR-CREDIT-SCORING-FG.
           MOVE BR-COL-QUEUE-LOW-TO-HI TO QBR-COL-QUEUE-LOW-TO-HI.
           MOVE BR-CAN-GROUP-ID TO QBR-CAN-GROUP-ID.
           MOVE BR-AUTO-LNSTAT-WINDOW TO QBR-AUTO-LNSTAT-WINDOW.
           MOVE BR-TIME-ZONE-FACTOR TO QBR-TIME-ZONE-FACTOR.
           MOVE BR-LONPF-OPT-DISPLAY TO QBR-LONPF-OPT-DISPLAY.
           MOVE BR-GIFTNET TO QBR-GIFTNET.
           MOVE BR-GIFTNET-LEDGER-LINE TO QBR-GIFTNET-LEDGER-LINE.
           MOVE BR-NO-FT-EMP TO QBR-NO-FT-EMP.
           MOVE BR-NO-PT-EMP TO QBR-NO-PT-EMP.
           MOVE BR-BALLOON-LOANS-FG TO QBR-BALLOON-LOANS-FG.
           MOVE BR-GL-TYPE-ALLOWED TO QBR-GL-TYPE-ALLOWED.
           MOVE BR-GL-ENTRY-DEFAULT TO QBR-GL-ENTRY-DEFAULT.
           MOVE BR-MIDSTREAM-1STPAY-FG TO QBR-MIDSTREAM-1STPAY-FG.
           MOVE BR-NODEF-1ST-X-MONTHS TO QBR-NODEF-1ST-X-MONTHS.
           MOVE BR-ADD-INS-FEECODE TO QBR-ADD-INS-FEECODE.
           MOVE BR-ROAD-G-SGL-FEECODE TO QBR-ROAD-G-SGL-FEECODE.
           MOVE BR-ROAD-G-FAM-FEECODE TO QBR-ROAD-G-FAM-FEECODE.
           MOVE BR-NODEF-IF-ZERO-AMT TO QBR-NODEF-IF-ZERO-AMT.
           MOVE BR-ASSESS-LATE-CHGS TO QBR-ASSESS-LATE-CHGS.
           MOVE BR-NEXT-SOLICIT-NO TO QBR-NEXT-SOLICIT-NO.
           MOVE BR-ADDON-FEECD-O3(1) TO QBR-ADDON-FEECD-O3-1.
           MOVE BR-ADDON-FEECD-O3(2) TO QBR-ADDON-FEECD-O3-2.
           MOVE BR-IMMEDIATE-RECEIPT TO QBR-IMMEDIATE-RECEIPT.
           MOVE BR-IMMEDIATE-DOC-PRINT TO QBR-IMMEDIATE-DOC-PRINT.
           MOVE BR-AUTO-PL-REF-AC TO QBR-AUTO-PL-REF-AC.
           MOVE BR-FAX-NUMBER TO QBR-FAX-NUMBER.
           MOVE BR-ALPHA-DRAWERS TO QBR-ALPHA-DRAWERS.
           MOVE BR-LATECHG-ON-ALLOTMENTS TO QBR-LATECHG-ON-ALLOTMENTS.
           MOVE BR-ALLOW-SYS-AS-EFFDATE TO QBR-ALLOW-SYS-AS-EFFDATE.
           MOVE BR-TAX-PREBOOK-CUST TO QBR-TAX-PREBOOK-CUST.
           MOVE BR-TAX-PREBOOK-NONCUST TO QBR-TAX-PREBOOK-NONCUST.
           MOVE BR-PASSWORD-DRAWER TO QBR-PASSWORD-DRAWER.
           MOVE BR-ONESTEP-RN-FG TO QBR-ONESTEP-RN-FG.
           MOVE BR-LI-LISCAN-FG TO QBR-LI-LISCAN-FG.
           MOVE BR-HIDE-END-BAL TO QBR-HIDE-END-BAL.
           MOVE BR-LBOX-NOPAYOFF TO QBR-LBOX-NOPAYOFF.
           MOVE BR-LBOX-ODDPAY TO QBR-LBOX-ODDPAY.
           MOVE BR-LBOX-LC TO QBR-LBOX-LC.
           MOVE BR-LBOX-NOPRIN TO QBR-LBOX-NOPRIN.
           MOVE BR-LBOX-FLASH TO QBR-LBOX-FLASH.
           MOVE BR-LBOX-ACTIONCD TO QBR-LBOX-ACTIONCD.
           MOVE BR-LBOX-MAXCON TO QBR-LBOX-MAXCON.
           MOVE BR-LBOX-MAXREC TO QBR-LBOX-MAXREC.
           MOVE BR-LBOX-PL TO QBR-LBOX-PL.
           MOVE BR-LBOX-JD TO QBR-LBOX-JD.
           MOVE BR-LBOX TO QBR-LBOX.
           MOVE BR-LATECHG-ON-LBOX TO QBR-LATECHG-ON-LBOX.
           MOVE BR-ALLOW-OV-TRCD TO QBR-ALLOW-OV-TRCD.
           MOVE BR-GLOBAL-QUEUE TO QBR-GLOBAL-QUEUE.
           MOVE BR-LBOX-PAYOFF-NONCASH TO QBR-LBOX-PAYOFF-NONCASH.
           MOVE BR-LBOX-PAYOFF-OV TO QBR-LBOX-PAYOFF-OV.
           MOVE BR-COLLECT-MANAGER TO QBR-COLLECT-MANAGER.
           MOVE BR-COLLECT-PHONE TO QBR-COLLECT-PHONE.
           MOVE BR-COLLECT-EXT TO QBR-COLLECT-EXT.
           MOVE BR-ALT-INTO-THIS-BRANCH TO QBR-ALT-INTO-THIS-BRANCH.
           MOVE BR-POFF-TITLE-WINDOW TO QBR-POFF-TITLE-WINDOW.
           MOVE BR-COUPON-STATEMENT TO QBR-COUPON-STATEMENT.
           MOVE BR-SUPERVISOR TO QBR-SUPERVISOR.
           MOVE BR-AREA-MANAGER TO QBR-AREA-MANAGER.
           MOVE BR-MIN-MONTHS-DEF TO QBR-MIN-MONTHS-DEF.
           MOVE BR-CC-FLAG TO QBR-CC-FLAG.
           MOVE BR-CC-ADR1 TO QBR-CC-ADR1.
           MOVE BR-CC-CITY TO QBR-CC-CITY.
           MOVE BR-CC-STATE TO QBR-CC-STATE.
           MOVE BR-CC-ZIP TO QBR-CC-ZIP.
           MOVE BR-MIN-MONTHS-D2 TO QBR-MIN-MONTHS-D2.
           MOVE BR-SKIP-MIDSTREAM-TRW TO QBR-SKIP-MIDSTREAM-TRW.
           MOVE BR-GL-ESCROW-BALANCE TO QBR-GL-ESCROW-BALANCE.
           MOVE BR-GL-ESCROW-PAYABLE TO QBR-GL-ESCROW-PAYABLE.
           MOVE BR-ALLOW-FORCE-OPEN-FG TO QBR-ALLOW-FORCE-OPEN-FG.
           MOVE BR-ACH-BANK-NO TO QBR-ACH-BANK-NO.
           MOVE BR-WIMENU-CHECK-WRITING TO QBR-WIMENU-CHECK-WRITING.
           MOVE BR-ADP-PAY-GROUP TO QBR-ADP-PAY-GROUP.
           MOVE BR-ALLOW-SCANNED-CKS TO QBR-ALLOW-SCANNED-CKS.
           MOVE BR-CHECK-DEPOSIT-GL TO QBR-CHECK-DEPOSIT-GL.
           MOVE BR-OPTION-PST-LASER TO QBR-OPTION-PST-LASER.
           MOVE BR-USE-DEBIT-CARDS TO QBR-USE-DEBIT-CARDS.
           MOVE BR-GL-DEBIT-CARD-PROCEEDS
                                   TO QBR-GL-DEBIT-CARD-PROCEEDS.
           MOVE BR-WKDAY-BANKID    TO QBR-WKDAY-BANKID.
           MOVE BR-PURGE-NOW-FLAG  TO QBR-PURGE-NOW-FLAG.
           MOVE BR-EMAIL           TO QBR-EMAIL.
           MOVE BR-MANAGER-EMAIL   TO QBR-MANAGER-EMAIL.
           MOVE BR-VERITEC-LICENSE TO QBR-VERITEC-LICENSE.
      *     MOVE BR-TEST-PP-AUTO-INSCOVR TO QBR-TEST-PP-AUTO-INSCOVR.
 
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
                   ISNULL(CAST(BRFILE.BR_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BRFILE.BR_NAME,''),
                   ISNULL(BRFILE.BR_ADR1,''),
                   ISNULL(BRFILE.BR_ADR2,''),
                   ISNULL(BRFILE.BR_CITY,''),
                   ISNULL(BRFILE.BR_STATE,''),
                   ISNULL(BRFILE.BR_ZIP,''),
                   BRFILE.BR_PHONE,
                   BRFILE.BR_RETAIN,
                   BRFILE.BR_NEXT_CHECK,
                   BRFILE.BR_PRINTER_1,
                   BRFILE.BR_PRINTER_2,
                   BRFILE.BR_PRINTER_3,
                   BRFILE.BR_PRINTER_4,
                   BRFILE.BR_FEE_GL_BRNO,
                   ISNULL(BRFILE.BR_REMIT1,''),
                   ISNULL(BRFILE.BR_REMIT2,''),
                   ISNULL(BRFILE.BR_REMIT3,''),
                   ISNULL(BRFILE.BR_STMNT1,''),
                   ISNULL(BRFILE.BR_STMNT2,''),
                   BRFILE.BR_INS_PRODUCER_NUM,
                   ISNULL(BRFILE.BR_DLRCHK,''),
                   ISNULL(BRFILE.BR_FRONT_END_FG,''),
                   ISNULL(BRFILE.BR_EMLNRG_PHONE,''),
                   ISNULL(BRFILE.BR_POST_GL,''),
                   ISNULL(BRFILE.BR_ALLOTMNTS,''),
                   ISNULL(BRFILE.BR_EXTRACT_AUTO_UPD,''),
                   ISNULL(BRFILE.BR_PCNI_LEDGER,''),
                   ISNULL(BRFILE.BR_PRINTERFG_1,''),
                   ISNULL(BRFILE.BR_PRINTERFG_2,''),
                   ISNULL(BRFILE.BR_PRINTERFG_3,''),
                   ISNULL(BRFILE.BR_PRINTERFG_4,''),
                   ISNULL(BRFILE.BR_NEGDLRSV,''),
                   ISNULL(BRFILE.BR_CPCORP_ORDER,''),
                   ISNULL(BRFILE.BR_PRIO_MEMO_FLAG,''),
                   ISNULL(BRFILE.BR_PREBILL_PL_JD_OPT,''),
                   ISNULL(BRFILE.BR_LOAN_NUM_FLAG,''),
                   ISNULL(BRFILE.BR_DOWNPY_FLAG,''),
                   ISNULL(BRFILE.BR_AUTO_INS_EARN,''),
                   ISNULL(BRFILE.BR_OTH_INS_LEVELS,''),
                   ISNULL(BRFILE.BR_ALLOW_INTERBR,''),
                   ISNULL(BRFILE.BR_UNER_DLANRP,''),
                   ISNULL(BRFILE.BR_SECOND_PROMISE,''),
                   ISNULL(BRFILE.BR_AVAIL_CREDIT,''),
                   ISNULL(BRFILE.BR_RECP1,''),
                   ISNULL(BRFILE.BR_RECP2,''),
                   ISNULL(BRFILE.BR_INC_WAIVERS,''),
                   ISNULL(BRFILE.BR_INC_INS,''),
                   BRFILE.BR_BP_NEXT_BATCH,
                   ISNULL(BRFILE.BR_BP_NOPAYOFF,''),
                   ISNULL(BRFILE.BR_BP_ODDPAY,''),
                   ISNULL(BRFILE.BR_BP_LC,''),
                   ISNULL(BRFILE.BR_BP_NOPRIN,''),
                   ISNULL(BRFILE.BR_BP_FLASH,''),
                   ISNULL(BRFILE.BR_BP_ACTIONCD,''),
                   BRFILE.BR_BP_MAXCON,
                   BRFILE.BR_BP_MAXREC,
                   ISNULL(BRFILE.BR_BP_PL,''),
                   ISNULL(BRFILE.BR_BP_JD,''),
                   BRFILE.BR_BP_BRANCH,
                   ISNULL(BRFILE.BR_AGE_ERN,''),
                   ISNULL(BRFILE.BR_OTHER_BRANCH_GL,''),
                   ISNULL(BRFILE.BR_SALES_SMALL,''),
                   ISNULL(BRFILE.BR_NEW_COLL_INQUIRY,''),
                   ISNULL(BRFILE.BR_CHANGE_MEMO,''),
                   ISNULL(BRFILE.BR_INS_ADDON,''),
                   ISNULL(BRFILE.BR_TRAILER_REC,''),
                   ISNULL(BRFILE.BR_ALDEL_FLAG,''),
                   ISNULL(BRFILE.BR_IB_LCHG_MESS,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_PAST_DUE,''),
                   BRFILE.BR_LI_DLCOMM_CMSI_PP_MAX,
                   ISNULL(BRFILE.BR_ADDON_FEECD_O2_1,''),
                   ISNULL(BRFILE.BR_ADDON_FEECD_O2_2,''),
                   ISNULL(BRFILE.BR_AUTO_CHECK,''),
                   ISNULL(BRFILE.BR_RECEIPT_PROMDATE,''),
                   ISNULL(BRFILE.BR_RECEIPT_INTPD,''),
                   ISNULL(BRFILE.BR_COLL_ID_SCAN,''),
                   ISNULL(BRFILE.BR_STOP_ON_COLL,''),
                   ISNULL(BRFILE.BR_DUEDATE_CHANGE_CD,''),
                   ISNULL(BRFILE.BR_CHECK_EOM_FG,''),
                   ISNULL(BRFILE.BR_VOLUME_DLANRP,''),
                   ISNULL(BRFILE.BR_PDTH_NXDUE,''),
                   ISNULL(BRFILE.BR_LC_CODE,''),
                   ISNULL(BRFILE.BR_VALIDATE_PRUS,''),
                   ISNULL(BRFILE.BR_AUTO_OPEN_BAL,''),
                   BRFILE.BR_OPEN_BAL_1,
                   BRFILE.BR_OPEN_BAL_2,
                   BRFILE.BR_OPEN_BAL_3,
                   BRFILE.BR_OPEN_BAL_4,
                   BRFILE.BR_OPEN_BAL_5,
                   BRFILE.BR_OPEN_BAL_6,
                   BRFILE.BR_OPEN_BAL_7,
                   BRFILE.BR_OPEN_BAL_8,
                   BRFILE.BR_OPEN_BAL_9,
                   BRFILE.BR_OPEN_BAL_10,
                   BRFILE.BR_OPEN_BAL_11,
                   BRFILE.BR_OPEN_BAL_12,
                   BRFILE.BR_OPEN_BAL_13,
                   BRFILE.BR_OPEN_BAL_14,
                   BRFILE.BR_OPEN_BAL_15,
                   BRFILE.BR_OPEN_BAL_16,
                   BRFILE.BR_OPEN_BAL_17,
                   BRFILE.BR_OPEN_BAL_18,
                   BRFILE.BR_OPEN_BAL_19,
                   BRFILE.BR_OPEN_BAL_20,
                   ISNULL(BRFILE.BR_TENDER_CD_1,''),
                   ISNULL(BRFILE.BR_TENDER_CD_2,''),
                   ISNULL(BRFILE.BR_TENDER_CD_3,''),
                   ISNULL(BRFILE.BR_DLPARV_ADJ_FG,''),
                   ISNULL(BRFILE.BR_OVERRIDE_BR_STATE,''),
                   BRFILE.BR_DEFAULT_SC_CLASS,
                   ISNULL(BRFILE.BR_RECDEL_NOT_REC,''),
                   ISNULL(BRFILE.BR_POST_AHEAD_FG,''),
                   ISNULL(BRFILE.BR_CAN_LOANS_FG,''),
                   ISNULL(BRFILE.BR_NON_MONTHLY_CAN_LOANS,''),
                   ISNULL(BRFILE.BR_BP_ALLOW_PMT_ACCT_OTHBAL,''),
                   ISNULL(BRFILE.BR_LBOX_ALLOW_PMT_ACCT_OTHBAL,''),
                   BRFILE.BR_REGION_NUMBER,
                   ISNULL(BRFILE.BR_OUR_FEE,''),
                   BRFILE.BR_REB_METHOD_1,
                   BRFILE.BR_REB_METHOD_2,
                   BRFILE.BR_REB_METHOD_3,
                   BRFILE.BR_REB_METHOD_4,
                   BRFILE.BR_REB_METHOD_5,
                   BRFILE.BR_REB_METHOD_6,
                   BRFILE.BR_REB_METHOD_7,
                   BRFILE.BR_REB_METHOD_8,
                   BRFILE.BR_REB_METHOD_9,
                   BRFILE.BR_REB_METHOD_10,
                   BRFILE.BR_REB_METHOD_11,
                   BRFILE.BR_REBDAYS_MONTH_1,
                   BRFILE.BR_REBDAYS_MONTH_2,
                   BRFILE.BR_REBDAYS_MONTH_3,
                   BRFILE.BR_REBDAYS_MONTH_4,
                   BRFILE.BR_REBDAYS_MONTH_5,
                   BRFILE.BR_REBDAYS_MONTH_6,
                   BRFILE.BR_REBDAYS_MONTH_7,
                   BRFILE.BR_REBDAYS_MONTH_8,
                   BRFILE.BR_REBDAYS_MONTH_9,
                   BRFILE.BR_REBDAYS_MONTH_10,
                   BRFILE.BR_REBDAYS_MONTH_11,
                   BRFILE.BR_ACCRUAL_MONTHS_1,
                   BRFILE.BR_ACCRUAL_MONTHS_2,
                   BRFILE.BR_ACCRUAL_MONTHS_3,
                   BRFILE.BR_ACCRUAL_MONTHS_4,
                   BRFILE.BR_ACCRUAL_MONTHS_5,
                   BRFILE.BR_ACCRUAL_MONTHS_6,
                   BRFILE.BR_ACCRUAL_MONTHS_7,
                   BRFILE.BR_ACCRUAL_MONTHS_8,
                   BRFILE.BR_ACCRUAL_MONTHS_9,
                   BRFILE.BR_ACCRUAL_MONTHS_10,
                   BRFILE.BR_ACCRUAL_MONTHS_11,
                   BRFILE.BR_GLOVERSHORT,
                   BRFILE.BR_GLBANK,
                   BRFILE.BR_GLONHAND,
                   BRFILE.BR_BANKBAL,
                   BRFILE.BR_RETAIN_AMT,
                   ISNULL(BRFILE.BR_LCARDFG,''),
                   ISNULL(BRFILE.BR_DATA_PATH,''),
                   ISNULL(BRFILE.BR_MACHINE,''),
                   ISNULL(BRFILE.BR_MANAGER,''),
                   ISNULL(BRFILE.BR_EOD_PROCEDURE,''),
                   ISNULL(BRFILE.BR_BP_PAYOFF_NONCASH,''),
                   ISNULL(BRFILE.BR_BP_PAYOFF_OV,''),
                   BRFILE.BR_LI_DLCOMM_CMSI_O3_MAX,
                   ISNULL(BRFILE.BR_LR_GROUP_COLLID,''),
                   ISNULL(BRFILE.BR_UNITPER_FG,''),
                   ISNULL(BRFILE.BR_UNITPER_Y_D_FG,''),
                   ISNULL(BRFILE.BR_NO_PROMPT_FOR_OTHBR,''),
                   ISNULL(BRFILE.BR_WORLD_BRANCH_TYPE,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_BEHAVIOR_FG,''),
                   ISNULL(BRFILE.BR_CONVERT_AUTO_ACCTNO,''),
                   ISNULL(BRFILE.BR_TS_REPO_INS_FG,''),
                   ISNULL(BRFILE.BR_BOOK_SF_BR_ONLY,''),
                   ISNULL(BRFILE.BR_FORCE_VALID_CITY_COUNTY,''),
                   ISNULL(BRFILE.BR_SET_COLLCD_DL_MFGCD,''),
                   ISNULL(BRFILE.BR_DEFAULT_SF_UNITPER_CD,''),
                   ISNULL(BRFILE.BR_DEFAULT_SF_PURCD,''),
                   ISNULL(BRFILE.BR_DEFAULT_LN_UNITPER_CD,''),
                   ISNULL(BRFILE.BR_DEFAULT_LN_PURCD,''),
                   ISNULL(BRFILE.BR_TAP_OTHER_DL,''),
                   ISNULL(BRFILE.BR_AH_AND_CL,''),
                   ISNULL(BRFILE.BR_NO_CASH_PROCEEDS,''),
                   ISNULL(BRFILE.BR_RI_BEFORE_REPO,''),
                   ISNULL(BRFILE.BR_RI_BEFORE_P4P5,''),
                   ISNULL(BRFILE.BR_NOT_OURS_REFUND,''),
                   ISNULL(BRFILE.BR_SFDLO3_WINDOW_FG,''),
                   ISNULL(BRFILE.BR_CALC_INS_ADJ,''),
                   ISNULL(BRFILE.BR_AC_BEFORE_REPO,''),
                   ISNULL(BRFILE.BR_AC_BEFORE_P4P5,''),
                   BRFILE.BR_BANK_ROUTING_NO,
                   BRFILE.BR_BANK_CLEARING_GL,
                   BRFILE.BR_GLBANK_DEPOSIT,
                   ISNULL(BRFILE.BR_BANK_ACCTNO,''),
                   BRFILE.BR_DEFAULT_SC_SUBCLASS,
                   ISNULL(BRFILE.BR_NO_OUT_DLR_RSV,''),
                   ISNULL(BRFILE.BR_COLLECTOR_QUEUE,''),
                   ISNULL(BRFILE.BR_COLL_QUEUE_CON_REC,''),
                   ISNULL(BRFILE.BR_JE_ALLOCATION_SKIP,''),
                   ISNULL(BRFILE.BR_EXPAND_APP_INPUT,''),
                   ISNULL(BRFILE.BR_A50_CPCORP,''),
                   ISNULL(BRFILE.BR_INBR_BILNSC_FG,''),
                   ISNULL(BRFILE.BR_INBR_BRIEF_FG,''),
                   ISNULL(BRFILE.BR_LEDGER_JUST_INTCHG,''),
                   BRFILE.BR_BYINQ_START_PAGE,
                   BRFILE.BR_DSINQ_START_PAGE,
                   ISNULL(BRFILE.BR_APP_REQUIRED_FOR_INPUT,''),
                   ISNULL(BRFILE.BR_CREDIT_SCORING_FG,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_LOW_TO_HI,''),
                   ISNULL(BRFILE.BR_CAN_GROUP_ID,''),
                   ISNULL(BRFILE.BR_AUTO_LNSTAT_WINDOW,''),
                   BRFILE.BR_TIME_ZONE_FACTOR,
                   ISNULL(BRFILE.BR_LONPF_OPT_DISPLAY,''),
                   ISNULL(BRFILE.BR_GIFTNET,''),
                   ISNULL(BRFILE.BR_GIFTNET_LEDGER_LINE,''),
                   BRFILE.BR_NO_FT_EMP,
                   BRFILE.BR_NO_PT_EMP,
                   ISNULL(BRFILE.BR_BALLOON_LOANS_FG,''),
                   ISNULL(BRFILE.BR_GL_TYPE_ALLOWED,''),
                   ISNULL(BRFILE.BR_GL_ENTRY_DEFAULT,''),
                   ISNULL(BRFILE.BR_MIDSTREAM_1STPAY_FG,''),
                   BRFILE.BR_NODEF_1ST_X_MONTHS,
                   ISNULL(BRFILE.BR_ADD_INS_FEECODE,''),
                   ISNULL(BRFILE.BR_ROAD_G_SGL_FEECODE,''),
                   ISNULL(BRFILE.BR_ROAD_G_FAM_FEECODE,''),
                   ISNULL(BRFILE.BR_NODEF_IF_ZERO_AMT,''),
                   ISNULL(BRFILE.BR_ASSESS_LATE_CHGS,''),
                   BRFILE.BR_NEXT_SOLICIT_NO,
                   ISNULL(BRFILE.BR_ADDON_FEECD_O3_1,''),
                   ISNULL(BRFILE.BR_ADDON_FEECD_O3_2,''),
                   ISNULL(BRFILE.BR_IMMEDIATE_RECEIPT,''),
                   ISNULL(BRFILE.BR_IMMEDIATE_DOC_PRINT,''),
                   ISNULL(BRFILE.BR_AUTO_PL_REF_AC,''),
                   BRFILE.BR_FAX_NUMBER,
                   ISNULL(BRFILE.BR_ALPHA_DRAWERS,''),
                   ISNULL(BRFILE.BR_LATECHG_ON_ALLOTMENTS,''),
                   ISNULL(BRFILE.BR_ALLOW_SYS_AS_EFFDATE,''),
                   BRFILE.BR_TAX_PREBOOK_CUST,
                   BRFILE.BR_TAX_PREBOOK_NONCUST,
                   ISNULL(BRFILE.BR_PASSWORD_DRAWER,''),
                   ISNULL(BRFILE.BR_ONESTEP_RN_FG,''),
                   ISNULL(BRFILE.BR_LI_LISCAN_FG,''),
                   ISNULL(BRFILE.BR_HIDE_END_BAL,''),
                   ISNULL(BRFILE.BR_LBOX_NOPAYOFF,''),
                   ISNULL(BRFILE.BR_LBOX_ODDPAY,''),
                   ISNULL(BRFILE.BR_LBOX_LC,''),
                   ISNULL(BRFILE.BR_LBOX_NOPRIN,''),
                   ISNULL(BRFILE.BR_LBOX_FLASH,''),
                   ISNULL(BRFILE.BR_LBOX_ACTIONCD,''),
                   BRFILE.BR_LBOX_MAXCON,
                   BRFILE.BR_LBOX_MAXREC,
                   ISNULL(BRFILE.BR_LBOX_PL,''),
                   ISNULL(BRFILE.BR_LBOX_JD,''),
                   ISNULL(BRFILE.BR_LBOX,''),
                   ISNULL(BRFILE.BR_LATECHG_ON_LBOX,''),
                   ISNULL(BRFILE.BR_ALLOW_OV_TRCD,''),
                   ISNULL(BRFILE.BR_GLOBAL_QUEUE,''),
                   ISNULL(BRFILE.BR_LBOX_PAYOFF_NONCASH,''),
                   ISNULL(BRFILE.BR_LBOX_PAYOFF_OV,''),
                   ISNULL(BRFILE.BR_COLLECT_MANAGER,''),
                   BRFILE.BR_COLLECT_PHONE,
                   BRFILE.BR_COLLECT_EXT,
                   ISNULL(BRFILE.BR_ALT_INTO_THIS_BRANCH,''),
                   ISNULL(BRFILE.BR_POFF_TITLE_WINDOW,''),
                   ISNULL(BRFILE.BR_COUPON_STATEMENT,''),
                   ISNULL(BRFILE.BR_SUPERVISOR,''),
                   ISNULL(BRFILE.BR_AREA_MANAGER,''),
                   BRFILE.BR_MIN_MONTHS_DEF,
                   ISNULL(BRFILE.BR_CC_FLAG,''),
                   ISNULL(BRFILE.BR_CC_ADR1,''),
                   ISNULL(BRFILE.BR_CC_CITY,''),
                   ISNULL(BRFILE.BR_CC_STATE,''),
                   ISNULL(BRFILE.BR_CC_ZIP,''),
                   BRFILE.BR_MIN_MONTHS_D2,
                   ISNULL(BRFILE.BR_SKIP_MIDSTREAM_TRW,''),
                   BRFILE.BR_GL_ESCROW_BALANCE,
                   BRFILE.BR_GL_ESCROW_PAYABLE,
                   ISNULL(BRFILE.BR_ALLOW_FORCE_OPEN_FG,''),
                   BRFILE.BR_ACH_BANK_NO,
                   ISNULL(BRFILE.BR_WIMENU_CHECK_WRITING,''),
                   ISNULL(BRFILE.BR_ADP_PAY_GROUP,''),
                   ISNULL(BRFILE.BR_ALLOW_SCANNED_CKS,''),
                   BRFILE.BR_CHECK_DEPOSIT_GL,
                   ISNULL(BRFILE.BR_OPTION_PST_LASER,''),
                   ISNULL(BRFILE.BR_USE_DEBIT_CARDS,''),
                   BRFILE.BR_GL_DEBIT_CARD_PROCEEDS,
                   BRFILE.BR_WKDAY_BANKID,
                   ISNULL(BRFILE.BR_PURGE_NOW_FLAG,''),
                   ISNULL(BRFILE.BR_EMAIL,''),
                   ISNULL(BRFILE.BR_MANAGER_EMAIL,''),
                   BRFILE.BR_VERITEC_LICENSE
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO >= :QBR-WBEG-NO
              AND BRFILE.BR_NO <= :QBR-WEND-NO
            ORDER BY BR_NO
           END-EXEC.

           EXEC SQL
               OPEN BR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

      *            ISNULL(BRFILE.BR_TEST_PP_AUTO_INSCOVR,'')

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
                   ISNULL(CAST(BRFILE.BR_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BRFILE.BR_NAME,''),
                   ISNULL(BRFILE.BR_ADR1,''),
                   ISNULL(BRFILE.BR_ADR2,''),
                   ISNULL(BRFILE.BR_CITY,''),
                   ISNULL(BRFILE.BR_STATE,''),
                   ISNULL(BRFILE.BR_ZIP,''),
                   BRFILE.BR_PHONE,
                   BRFILE.BR_RETAIN,
                   BRFILE.BR_NEXT_CHECK,
                   BRFILE.BR_PRINTER_1,
                   BRFILE.BR_PRINTER_2,
                   BRFILE.BR_PRINTER_3,
                   BRFILE.BR_PRINTER_4,
                   BRFILE.BR_FEE_GL_BRNO,
                   ISNULL(BRFILE.BR_REMIT1,''),
                   ISNULL(BRFILE.BR_REMIT2,''),
                   ISNULL(BRFILE.BR_REMIT3,''),
                   ISNULL(BRFILE.BR_STMNT1,''),
                   ISNULL(BRFILE.BR_STMNT2,''),
                   BRFILE.BR_INS_PRODUCER_NUM,
                   ISNULL(BRFILE.BR_DLRCHK,''),
                   ISNULL(BRFILE.BR_FRONT_END_FG,''),
                   ISNULL(BRFILE.BR_EMLNRG_PHONE,''),
                   ISNULL(BRFILE.BR_POST_GL,''),
                   ISNULL(BRFILE.BR_ALLOTMNTS,''),
                   ISNULL(BRFILE.BR_EXTRACT_AUTO_UPD,''),
                   ISNULL(BRFILE.BR_PCNI_LEDGER,''),
                   ISNULL(BRFILE.BR_PRINTERFG_1,''),
                   ISNULL(BRFILE.BR_PRINTERFG_2,''),
                   ISNULL(BRFILE.BR_PRINTERFG_3,''),
                   ISNULL(BRFILE.BR_PRINTERFG_4,''),
                   ISNULL(BRFILE.BR_NEGDLRSV,''),
                   ISNULL(BRFILE.BR_CPCORP_ORDER,''),
                   ISNULL(BRFILE.BR_PRIO_MEMO_FLAG,''),
                   ISNULL(BRFILE.BR_PREBILL_PL_JD_OPT,''),
                   ISNULL(BRFILE.BR_LOAN_NUM_FLAG,''),
                   ISNULL(BRFILE.BR_DOWNPY_FLAG,''),
                   ISNULL(BRFILE.BR_AUTO_INS_EARN,''),
                   ISNULL(BRFILE.BR_OTH_INS_LEVELS,''),
                   ISNULL(BRFILE.BR_ALLOW_INTERBR,''),
                   ISNULL(BRFILE.BR_UNER_DLANRP,''),
                   ISNULL(BRFILE.BR_SECOND_PROMISE,''),
                   ISNULL(BRFILE.BR_AVAIL_CREDIT,''),
                   ISNULL(BRFILE.BR_RECP1,''),
                   ISNULL(BRFILE.BR_RECP2,''),
                   ISNULL(BRFILE.BR_INC_WAIVERS,''),
                   ISNULL(BRFILE.BR_INC_INS,''),
                   BRFILE.BR_BP_NEXT_BATCH,
                   ISNULL(BRFILE.BR_BP_NOPAYOFF,''),
                   ISNULL(BRFILE.BR_BP_ODDPAY,''),
                   ISNULL(BRFILE.BR_BP_LC,''),
                   ISNULL(BRFILE.BR_BP_NOPRIN,''),
                   ISNULL(BRFILE.BR_BP_FLASH,''),
                   ISNULL(BRFILE.BR_BP_ACTIONCD,''),
                   BRFILE.BR_BP_MAXCON,
                   BRFILE.BR_BP_MAXREC,
                   ISNULL(BRFILE.BR_BP_PL,''),
                   ISNULL(BRFILE.BR_BP_JD,''),
                   BRFILE.BR_BP_BRANCH,
                   ISNULL(BRFILE.BR_AGE_ERN,''),
                   ISNULL(BRFILE.BR_OTHER_BRANCH_GL,''),
                   ISNULL(BRFILE.BR_SALES_SMALL,''),
                   ISNULL(BRFILE.BR_NEW_COLL_INQUIRY,''),
                   ISNULL(BRFILE.BR_CHANGE_MEMO,''),
                   ISNULL(BRFILE.BR_INS_ADDON,''),
                   ISNULL(BRFILE.BR_TRAILER_REC,''),
                   ISNULL(BRFILE.BR_ALDEL_FLAG,''),
                   ISNULL(BRFILE.BR_IB_LCHG_MESS,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_PAST_DUE,''),
                   BRFILE.BR_LI_DLCOMM_CMSI_PP_MAX,
                   ISNULL(BRFILE.BR_ADDON_FEECD_O2_1,''),
                   ISNULL(BRFILE.BR_ADDON_FEECD_O2_2,''),
                   ISNULL(BRFILE.BR_AUTO_CHECK,''),
                   ISNULL(BRFILE.BR_RECEIPT_PROMDATE,''),
                   ISNULL(BRFILE.BR_RECEIPT_INTPD,''),
                   ISNULL(BRFILE.BR_COLL_ID_SCAN,''),
                   ISNULL(BRFILE.BR_STOP_ON_COLL,''),
                   ISNULL(BRFILE.BR_DUEDATE_CHANGE_CD,''),
                   ISNULL(BRFILE.BR_CHECK_EOM_FG,''),
                   ISNULL(BRFILE.BR_VOLUME_DLANRP,''),
                   ISNULL(BRFILE.BR_PDTH_NXDUE,''),
                   ISNULL(BRFILE.BR_LC_CODE,''),
                   ISNULL(BRFILE.BR_VALIDATE_PRUS,''),
                   ISNULL(BRFILE.BR_AUTO_OPEN_BAL,''),
                   BRFILE.BR_OPEN_BAL_1,
                   BRFILE.BR_OPEN_BAL_2,
                   BRFILE.BR_OPEN_BAL_3,
                   BRFILE.BR_OPEN_BAL_4,
                   BRFILE.BR_OPEN_BAL_5,
                   BRFILE.BR_OPEN_BAL_6,
                   BRFILE.BR_OPEN_BAL_7,
                   BRFILE.BR_OPEN_BAL_8,
                   BRFILE.BR_OPEN_BAL_9,
                   BRFILE.BR_OPEN_BAL_10,
                   BRFILE.BR_OPEN_BAL_11,
                   BRFILE.BR_OPEN_BAL_12,
                   BRFILE.BR_OPEN_BAL_13,
                   BRFILE.BR_OPEN_BAL_14,
                   BRFILE.BR_OPEN_BAL_15,
                   BRFILE.BR_OPEN_BAL_16,
                   BRFILE.BR_OPEN_BAL_17,
                   BRFILE.BR_OPEN_BAL_18,
                   BRFILE.BR_OPEN_BAL_19,
                   BRFILE.BR_OPEN_BAL_20,
                   ISNULL(BRFILE.BR_TENDER_CD_1,''),
                   ISNULL(BRFILE.BR_TENDER_CD_2,''),
                   ISNULL(BRFILE.BR_TENDER_CD_3,''),
                   ISNULL(BRFILE.BR_DLPARV_ADJ_FG,''),
                   ISNULL(BRFILE.BR_OVERRIDE_BR_STATE,''),
                   BRFILE.BR_DEFAULT_SC_CLASS,
                   ISNULL(BRFILE.BR_RECDEL_NOT_REC,''),
                   ISNULL(BRFILE.BR_POST_AHEAD_FG,''),
                   ISNULL(BRFILE.BR_CAN_LOANS_FG,''),
                   ISNULL(BRFILE.BR_NON_MONTHLY_CAN_LOANS,''),
                   ISNULL(BRFILE.BR_BP_ALLOW_PMT_ACCT_OTHBAL,''),
                   ISNULL(BRFILE.BR_LBOX_ALLOW_PMT_ACCT_OTHBAL,''),
                   BRFILE.BR_REGION_NUMBER,
                   ISNULL(BRFILE.BR_OUR_FEE,''),
                   BRFILE.BR_REB_METHOD_1,
                   BRFILE.BR_REB_METHOD_2,
                   BRFILE.BR_REB_METHOD_3,
                   BRFILE.BR_REB_METHOD_4,
                   BRFILE.BR_REB_METHOD_5,
                   BRFILE.BR_REB_METHOD_6,
                   BRFILE.BR_REB_METHOD_7,
                   BRFILE.BR_REB_METHOD_8,
                   BRFILE.BR_REB_METHOD_9,
                   BRFILE.BR_REB_METHOD_10,
                   BRFILE.BR_REB_METHOD_11,
                   BRFILE.BR_REBDAYS_MONTH_1,
                   BRFILE.BR_REBDAYS_MONTH_2,
                   BRFILE.BR_REBDAYS_MONTH_3,
                   BRFILE.BR_REBDAYS_MONTH_4,
                   BRFILE.BR_REBDAYS_MONTH_5,
                   BRFILE.BR_REBDAYS_MONTH_6,
                   BRFILE.BR_REBDAYS_MONTH_7,
                   BRFILE.BR_REBDAYS_MONTH_8,
                   BRFILE.BR_REBDAYS_MONTH_9,
                   BRFILE.BR_REBDAYS_MONTH_10,
                   BRFILE.BR_REBDAYS_MONTH_11,
                   BRFILE.BR_ACCRUAL_MONTHS_1,
                   BRFILE.BR_ACCRUAL_MONTHS_2,
                   BRFILE.BR_ACCRUAL_MONTHS_3,
                   BRFILE.BR_ACCRUAL_MONTHS_4,
                   BRFILE.BR_ACCRUAL_MONTHS_5,
                   BRFILE.BR_ACCRUAL_MONTHS_6,
                   BRFILE.BR_ACCRUAL_MONTHS_7,
                   BRFILE.BR_ACCRUAL_MONTHS_8,
                   BRFILE.BR_ACCRUAL_MONTHS_9,
                   BRFILE.BR_ACCRUAL_MONTHS_10,
                   BRFILE.BR_ACCRUAL_MONTHS_11,
                   BRFILE.BR_GLOVERSHORT,
                   BRFILE.BR_GLBANK,
                   BRFILE.BR_GLONHAND,
                   BRFILE.BR_BANKBAL,
                   BRFILE.BR_RETAIN_AMT,
                   ISNULL(BRFILE.BR_LCARDFG,''),
                   ISNULL(BRFILE.BR_DATA_PATH,''),
                   ISNULL(BRFILE.BR_MACHINE,''),
                   ISNULL(BRFILE.BR_MANAGER,''),
                   ISNULL(BRFILE.BR_EOD_PROCEDURE,''),
                   ISNULL(BRFILE.BR_BP_PAYOFF_NONCASH,''),
                   ISNULL(BRFILE.BR_BP_PAYOFF_OV,''),
                   BRFILE.BR_LI_DLCOMM_CMSI_O3_MAX,
                   ISNULL(BRFILE.BR_LR_GROUP_COLLID,''),
                   ISNULL(BRFILE.BR_UNITPER_FG,''),
                   ISNULL(BRFILE.BR_UNITPER_Y_D_FG,''),
                   ISNULL(BRFILE.BR_NO_PROMPT_FOR_OTHBR,''),
                   ISNULL(BRFILE.BR_WORLD_BRANCH_TYPE,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_BEHAVIOR_FG,''),
                   ISNULL(BRFILE.BR_CONVERT_AUTO_ACCTNO,''),
                   ISNULL(BRFILE.BR_TS_REPO_INS_FG,''),
                   ISNULL(BRFILE.BR_BOOK_SF_BR_ONLY,''),
                   ISNULL(BRFILE.BR_FORCE_VALID_CITY_COUNTY,''),
                   ISNULL(BRFILE.BR_SET_COLLCD_DL_MFGCD,''),
                   ISNULL(BRFILE.BR_DEFAULT_SF_UNITPER_CD,''),
                   ISNULL(BRFILE.BR_DEFAULT_SF_PURCD,''),
                   ISNULL(BRFILE.BR_DEFAULT_LN_UNITPER_CD,''),
                   ISNULL(BRFILE.BR_DEFAULT_LN_PURCD,''),
                   ISNULL(BRFILE.BR_TAP_OTHER_DL,''),
                   ISNULL(BRFILE.BR_AH_AND_CL,''),
                   ISNULL(BRFILE.BR_NO_CASH_PROCEEDS,''),
                   ISNULL(BRFILE.BR_RI_BEFORE_REPO,''),
                   ISNULL(BRFILE.BR_RI_BEFORE_P4P5,''),
                   ISNULL(BRFILE.BR_NOT_OURS_REFUND,''),
                   ISNULL(BRFILE.BR_SFDLO3_WINDOW_FG,''),
                   ISNULL(BRFILE.BR_CALC_INS_ADJ,''),
                   ISNULL(BRFILE.BR_AC_BEFORE_REPO,''),
                   ISNULL(BRFILE.BR_AC_BEFORE_P4P5,''),
                   BRFILE.BR_BANK_ROUTING_NO,
                   BRFILE.BR_BANK_CLEARING_GL,
                   BRFILE.BR_GLBANK_DEPOSIT,
                   ISNULL(BRFILE.BR_BANK_ACCTNO,''),
                   BRFILE.BR_DEFAULT_SC_SUBCLASS,
                   ISNULL(BRFILE.BR_NO_OUT_DLR_RSV,''),
                   ISNULL(BRFILE.BR_COLLECTOR_QUEUE,''),
                   ISNULL(BRFILE.BR_COLL_QUEUE_CON_REC,''),
                   ISNULL(BRFILE.BR_JE_ALLOCATION_SKIP,''),
                   ISNULL(BRFILE.BR_EXPAND_APP_INPUT,''),
                   ISNULL(BRFILE.BR_A50_CPCORP,''),
                   ISNULL(BRFILE.BR_INBR_BILNSC_FG,''),
                   ISNULL(BRFILE.BR_INBR_BRIEF_FG,''),
                   ISNULL(BRFILE.BR_LEDGER_JUST_INTCHG,''),
                   BRFILE.BR_BYINQ_START_PAGE,
                   BRFILE.BR_DSINQ_START_PAGE,
                   ISNULL(BRFILE.BR_APP_REQUIRED_FOR_INPUT,''),
                   ISNULL(BRFILE.BR_CREDIT_SCORING_FG,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_LOW_TO_HI,''),
                   ISNULL(BRFILE.BR_CAN_GROUP_ID,''),
                   ISNULL(BRFILE.BR_AUTO_LNSTAT_WINDOW,''),
                   BRFILE.BR_TIME_ZONE_FACTOR,
                   ISNULL(BRFILE.BR_LONPF_OPT_DISPLAY,''),
                   ISNULL(BRFILE.BR_GIFTNET,''),
                   ISNULL(BRFILE.BR_GIFTNET_LEDGER_LINE,''),
                   BRFILE.BR_NO_FT_EMP,
                   BRFILE.BR_NO_PT_EMP,
                   ISNULL(BRFILE.BR_BALLOON_LOANS_FG,''),
                   ISNULL(BRFILE.BR_GL_TYPE_ALLOWED,''),
                   ISNULL(BRFILE.BR_GL_ENTRY_DEFAULT,''),
                   ISNULL(BRFILE.BR_MIDSTREAM_1STPAY_FG,''),
                   BRFILE.BR_NODEF_1ST_X_MONTHS,
                   ISNULL(BRFILE.BR_ADD_INS_FEECODE,''),
                   ISNULL(BRFILE.BR_ROAD_G_SGL_FEECODE,''),
                   ISNULL(BRFILE.BR_ROAD_G_FAM_FEECODE,''),
                   ISNULL(BRFILE.BR_NODEF_IF_ZERO_AMT,''),
                   ISNULL(BRFILE.BR_ASSESS_LATE_CHGS,''),
                   BRFILE.BR_NEXT_SOLICIT_NO,
                   ISNULL(BRFILE.BR_ADDON_FEECD_O3_1,''),
                   ISNULL(BRFILE.BR_ADDON_FEECD_O3_2,''),
                   ISNULL(BRFILE.BR_IMMEDIATE_RECEIPT,''),
                   ISNULL(BRFILE.BR_IMMEDIATE_DOC_PRINT,''),
                   ISNULL(BRFILE.BR_AUTO_PL_REF_AC,''),
                   BRFILE.BR_FAX_NUMBER,
                   ISNULL(BRFILE.BR_ALPHA_DRAWERS,''),
                   ISNULL(BRFILE.BR_LATECHG_ON_ALLOTMENTS,''),
                   ISNULL(BRFILE.BR_ALLOW_SYS_AS_EFFDATE,''),
                   BRFILE.BR_TAX_PREBOOK_CUST,
                   BRFILE.BR_TAX_PREBOOK_NONCUST,
                   ISNULL(BRFILE.BR_PASSWORD_DRAWER,''),
                   ISNULL(BRFILE.BR_ONESTEP_RN_FG,''),
                   ISNULL(BRFILE.BR_LI_LISCAN_FG,''),
                   ISNULL(BRFILE.BR_HIDE_END_BAL,''),
                   ISNULL(BRFILE.BR_LBOX_NOPAYOFF,''),
                   ISNULL(BRFILE.BR_LBOX_ODDPAY,''),
                   ISNULL(BRFILE.BR_LBOX_LC,''),
                   ISNULL(BRFILE.BR_LBOX_NOPRIN,''),
                   ISNULL(BRFILE.BR_LBOX_FLASH,''),
                   ISNULL(BRFILE.BR_LBOX_ACTIONCD,''),
                   BRFILE.BR_LBOX_MAXCON,
                   BRFILE.BR_LBOX_MAXREC,
                   ISNULL(BRFILE.BR_LBOX_PL,''),
                   ISNULL(BRFILE.BR_LBOX_JD,''),
                   ISNULL(BRFILE.BR_LBOX,''),
                   ISNULL(BRFILE.BR_LATECHG_ON_LBOX,''),
                   ISNULL(BRFILE.BR_ALLOW_OV_TRCD,''),
                   ISNULL(BRFILE.BR_GLOBAL_QUEUE,''),
                   ISNULL(BRFILE.BR_LBOX_PAYOFF_NONCASH,''),
                   ISNULL(BRFILE.BR_LBOX_PAYOFF_OV,''),
                   ISNULL(BRFILE.BR_COLLECT_MANAGER,''),
                   BRFILE.BR_COLLECT_PHONE,
                   BRFILE.BR_COLLECT_EXT,
                   ISNULL(BRFILE.BR_ALT_INTO_THIS_BRANCH,''),
                   ISNULL(BRFILE.BR_POFF_TITLE_WINDOW,''),
                   ISNULL(BRFILE.BR_COUPON_STATEMENT,''),
                   ISNULL(BRFILE.BR_SUPERVISOR,''),
                   ISNULL(BRFILE.BR_AREA_MANAGER,''),
                   BRFILE.BR_MIN_MONTHS_DEF,
                   ISNULL(BRFILE.BR_CC_FLAG,''),
                   ISNULL(BRFILE.BR_CC_ADR1,''),
                   ISNULL(BRFILE.BR_CC_CITY,''),
                   ISNULL(BRFILE.BR_CC_STATE,''),
                   ISNULL(BRFILE.BR_CC_ZIP,''),
                   BRFILE.BR_MIN_MONTHS_D2,
                   ISNULL(BRFILE.BR_SKIP_MIDSTREAM_TRW,''),
                   BRFILE.BR_GL_ESCROW_BALANCE,
                   BRFILE.BR_GL_ESCROW_PAYABLE,
                   ISNULL(BRFILE.BR_ALLOW_FORCE_OPEN_FG,''),
                   BRFILE.BR_ACH_BANK_NO,
                   ISNULL(BRFILE.BR_WIMENU_CHECK_WRITING,''),
                   ISNULL(BRFILE.BR_ADP_PAY_GROUP,''),
                   ISNULL(BRFILE.BR_ALLOW_SCANNED_CKS,''),
                   BRFILE.BR_CHECK_DEPOSIT_GL,
                   ISNULL(BRFILE.BR_OPTION_PST_LASER,''),
                   ISNULL(BRFILE.BR_USE_DEBIT_CARDS,''),
                   BRFILE.BR_GL_DEBIT_CARD_PROCEEDS,
                   BRFILE.BR_WKDAY_BANKID,
                   ISNULL(BRFILE.BR_PURGE_NOW_FLAG,''),
                   ISNULL(BRFILE.BR_EMAIL,''),
                   ISNULL(BRFILE.BR_MANAGER_EMAIL,''),
                   BRFILE.BR_VERITEC_LICENSE 
            INTO :QBR-REC
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

      *            ISNULL(BRFILE.BR_TEST_PP_AUTO_INSCOVR,'')

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
                   FETCH BR_CURSOR INTO :QBR-REC
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
	  
       COPY "LIBGB/GBPRINT.CPY".
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LNFILE NOT = "S" )
              MOVE LN-PATH-BEG             TO LN-SELECT-PATH-BEG
              MOVE LN-PATH-FILE            TO LN-SELECT-PATH-FILE
           ELSE
              MOVE LN-PATH-SQL             TO LN-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-SELECT-PATH TO E-FILE.

      *    EXEC SQL
      *         CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT
      *               USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
      *    END-EXEC.


      *    ACCEPT SQL-CONNECT-SERVER FROM ENVIRONMENT
      *                 "OTIS_DSN_NAME".
      *    ACCEPT SQL-CONNECT-USERID 
      *      FROM ENVIRONMENT "OTIS_ACUSQL_LOGIN".
      *    ACCEPT SQL-CONNECT-PSWD FROM ENVIRONMENT "OTIS_ACUSQL_PSWD".


           EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.


      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
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

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :RET-CODE = EXEC SP_RZRNRP_LNFILE_SELECT
              ( :QLN3-WBEG-OWNBR, :QLN3-WEND-OWNBR,  :QLN3-WBEG-CLASS,
                :QLN3-WEND-CLASS, :QLN3-WBEG-ACCTNO, :QLN3-WEND-ACCTNO )
           END-EXEC.

 
           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

      *    EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

      *    EXEC SQL
      *     DECLARE LN3_CURSOR CURSOR FOR DYNAMIC_SQL
      *    END-EXEC.

      *    MOVE '{CALL SP_RZAGED_LNFILE_SELECT(?, ?, ?, ?, ?, ?)}'
      *                               TO LN-COMMAND.
      *    EXEC SQL
      *         PREPARE DYNAMIC_SQL FROM :LN-COMMAND
      *    END-EXEC.

      *    EXEC SQL
      *     OPEN LN3_CURSOR USING :QLN3-WBEG-OWNBR, :QLN3-WEND-OWNBR,
      *                           :QLN3-WBEG-CLASS, :QLN3-WEND-CLASS,
      *                           :QLN3-WBEG-ACCTNO, :QLN3-WEND-ACCTNO
      *    END-EXEC.


      *    PERFORM SQL-IO-VALIDATION.

      *    MOVE ZEROES TO QLN3-WBEG-OWNBR
      *                   QLN3-WEND-OWNBR
      *                   QLN3-WBEG-CLASS
      *                   QLN3-WEND-CLASS
      *                   QLN3-WBEG-ACCTNO
      *                   QLN3-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-LN3-FILE.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           MOVE LN-OWNBR  TO QLN-OWNBR.
           MOVE LN-CLASS  TO QLN-CLASS.
           MOVE LN-ACCTNO TO QLN-ACCTNO.

           EXEC SQL
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
            INTO :QLN-REC
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
              AND LNFILE.LN_CLASS  = :QLN-CLASS
              AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

          EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

              EXEC SQL
                   FETCH NEXT LN3_CURSOR INTO
                         :QLN-OWNBR                              
                        ,:QLN-ACCTNO
		        ,:QLN-CLASS
                        ,:QLN-SUBCLASS
                        ,:QLN-LAWCODE
                        ,:QLN-SSNO1
                        ,:QLN-MAKERCD1
                        ,:QLN-SSNO2
                        ,:QLN-MAKERCD2
                        ,:QLN-SSNO3
                        ,:QLN-MAKERCD3
                        ,:QLN-SSNO4
                        ,:QLN-MAKERCD4
                        ,:QLN-ORGST
                        ,:QLN-SPRCLASS
                        ,:QLN-POCD
                        ,:QLN-PLCD
                        ,:QLN-SERVACCT
                        ,:QLN-STATFLAGS
                        ,:QLN-ORIG-1STPYDATE
                        ,:QLN-1STPYDATE
                        ,:QLN-ORGTERM
                        ,:QLN-DLNO
                        ,:QLN-CURBAL
                        ,:QLN-ANTICTERM
                        ,:QLN-ANTICERN-1
                        ,:QLN-ANTICERN-2
                        ,:QLN-ANTICERN-3
                        ,:QLN-ANTICERN-4
                        ,:QLN-ANTICERN-5
                        ,:QLN-ANTICERN-6
                        ,:QLN-ANTICERN-7
                        ,:QLN-ANTICERN-8
                        ,:QLN-ANTICERN-9
                        ,:QLN-ANTICERN-10
                        ,:QLN-ANTICADJ-1
                        ,:QLN-ANTICADJ-2
                        ,:QLN-ANTICADJ-3
                        ,:QLN-ANTICADJ-4
                        ,:QLN-ANTICADJ-5
                        ,:QLN-ANTICADJ-6
                        ,:QLN-ANTICADJ-7
                        ,:QLN-ANTICADJ-8
                        ,:QLN-ANTICADJ-9
                        ,:QLN-ANTICADJ-10
                        ,:QLN-UFERN-1
                        ,:QLN-UFERN-2
                        ,:QLN-UFERN-3
                        ,:QLN-UFERN-4
                        ,:QLN-UFERN-5
                        ,:QLN-UFERN-6
                        ,:QLN-UFERN-7
                        ,:QLN-UFERN-8
                        ,:QLN-UFERN-9
                        ,:QLN-UFERN-10
                        ,:QLN-ACCERN-1
                        ,:QLN-ACCERN-2
                        ,:QLN-ACCERN-3
                        ,:QLN-ACCERN-4
                        ,:QLN-ACCERN-5
                        ,:QLN-ACCERN-6
                        ,:QLN-ACCERN-7
                        ,:QLN-ACCERN-8
                        ,:QLN-ACCERN-9
                        ,:QLN-ACCERN-10
                        ,:QLN-TOTNODEF
                        ,:QLN-SUM-ORGTERM
                        ,:QLN-UNITPER-CD
                        ,:QLN-UNITPER-FREQ
                        ,:QBW-LNAME
                        ,:QBW-FNAME
                        ,:QBW-SOLICIT
                        ,:QLC-CODE
                        ,:QLC-DESC
                        ,:QSPA-EXPD-FRMLA

              END-EXEC.


      *    IF ( LN3-CURSOR-STAT-GOOD )
      *       EXEC SQL
      *            FETCH LN3_CURSOR INTO :QLN-REC
      *       END-EXEC
      *    ELSE
      *    IF ( LN3-CURSOR-GR-STAT-GOOD )
      *       EXEC SQL
      *            FETCH LN3_CURSOR_GR INTO :QLN-REC
      *       END-EXEC
      *    ELSE
      *       MOVE 9        TO SQLCODE
      *       MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
			  PERFORM GET-BW-FIELDS
			  PERFORM GET-LC-FIELDS
			  PERFORM GET-SPA-FIELDS.

          EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC. 
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *    IF ( LN3-CURSOR-GR-STAT-GOOD )
      *       EXEC SQL
      *            CLOSE LN3_CURSOR_GR
      *       END-EXEC
      *       MOVE STAT---BAD TO LN3-CURSOR-GR-STAT.	
	  
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
      * COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       OPEN-WK1-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK1-IFN TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK1-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK1-FILE-OUTPUT.
           UNLOCK WK-FILE.
       OPEN-WK1-FILE.
           PERFORM OPEN-IT.
           MOVE WK1-IFN TO E-FILE.
           OPEN I-O WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK1-FILE.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK1-FILE.
           UNLOCK WK-FILE.
       READ-WK1-FILE.
           PERFORM READ-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           READ WK-FILE.
           IF IO-FG = 8
              GO TO READ-WK1-FILE.
       READ-WK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           READ WK-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-WK1-FILE-NEXT.
       START-WK1-FILE.
           PERFORM START-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           START WK-FILE KEY NOT < WK1-KEY.
           UNLOCK WK-FILE.
       WRITE-WK1-FILE.
           PERFORM WRITE-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           WRITE WK-REC.
           IF IO-FG = 8
              GO TO WRITE-WK1-FILE.
           UNLOCK WK-FILE.
       REWRITE-WK1-FILE.
           PERFORM REWRITE-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           REWRITE WK-REC.
           IF IO-FG = 8
              GO TO REWRITE-WK1-FILE.
           UNLOCK WK-FILE.
       CLOSE-WK1-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.
       READ-WK2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WK2-IFN TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           READ WK-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-WK2-FILE-NEXT.
       START-WK2-FILE.
           PERFORM START-IT.
           MOVE WK2-IFN TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           START WK-FILE KEY NOT < WK2-KEY.
           UNLOCK WK-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      **************************************************
       END-PROGRAM SECTION.
      **************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
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
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY UERNRP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
