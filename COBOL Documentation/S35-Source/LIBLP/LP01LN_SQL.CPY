      * COPYMEMBER: LIBLP/LP01LN_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN MASTER FILE
      *
      *=================================================================
      * REV:
      * JKC 2021-0430 ***NEW***
      * JKC 2023-0713 ADDED LN1-RETURN-CODE THRU LN7-RETURN-CODE.
      * BAH 20230714 COMMENT OUT LN6 AND LN7
      * JKC 20240404 ADDED QLN-LOS-APP-ID PIC 9(18).
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

       01  LN1-RETURN-CODE           PIC S9(4)   COMP-5.
       01  LN3-RETURN-CODE           PIC S9(4)   COMP-5.
       01  LN4-RETURN-CODE           PIC S9(4)   COMP-5.
       01  LN5-RETURN-CODE           PIC S9(4)   COMP-5.

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
           03  QLN-LOS-APP-ID                        PIC 9(18).

       EXEC SQL END DECLARE SECTION END-EXEC.
