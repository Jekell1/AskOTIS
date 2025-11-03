       IDENTIFICATION DIVISION.
       PROGRAM-ID.                         LNTEST.
       AUTHOR.                             RMZ.
       REMARKS.
	   SHOW THE EXECUTION OF A STORED PROCEDURE USING A CURSOR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/LNTESS S35 04/18/23-13:49:45 >".

      *=================================================================
      * P-TIMER-LINK-BUF IS AN EXACT COPY IN LIBGB/TIMER_LNK.CPY
      * P-TIMER-LINK-BUF IS AN EXACT COPY IN LIBGB/TIMEWK.CPY
      * ANY CHANGE HERE MUST BE MADE IN THE OTHER COPY MEMBERS!!!!!
      *=================================================================
       01  P-TIMER.
           03  P-TIMER-TYPE-SET             PIC X(1).
               88  P-TIMER-TYPE-SET-VALID   VALUE "E" "R" "U".

           03  P-TIMER-LINK-BUF.
               05  P-TIMER-STAT             PIC X(02).
               05  P-TIMER-DAYSEC-BEG       PIC 99V9(7).
               05  P-TIMER-DAYSEC-END       PIC 99V9(7).
               05  P-TIMER-DAYSEC-ELAPSED   PIC 99V9(7).
               05  P-TIMER-BEG-EDIT.
                   07  P-TIMER-BEG-HHMMSS   PIC 99/99/99.
                   07  FILLER               PIC X(01).
                   07  P-TIMER-BEG-AMPM     PIC X(02).
               05  P-TIMER-END-EDIT.
                   07  P-TIMER-END-HHMMSS   PIC 99/99/99.
                   07  FILLER               PIC X(01).
                   07  P-TIMER-END-AMPM     PIC X(02).
               05  P-TIMER-ELAPSED-EDIT     PIC 99/99/99/99.
               05  P-TIMER-TYPE             PIC X(1).
                   88  P-TIMER-TYPE-EXCEL   VALUE "E".
                   88  P-TIMER-TYPE-REPORT  VALUE "R".
                   88  P-TIMER-TYPE-UTILITY VALUE "U".
                   88  P-TIMER-TYPE-VALID   VALUE "E" "R" "U".

       EXEC SQL INCLUDE SQLCA END-EXEC.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  SQLSERVER     PIC X(64) VALUE SPACES.
       01  SQLUSER       PIC X(32) VALUE SPACES.
       01  SQLPASSWORD   PIC X(32) VALUE SPACES.

       01  QLN3-WBEG-OWNBR    PIC 9(4)  VALUE ZEROS.
       01  QLN3-WEND-OWNBR    PIC 9(4)  VALUE ZEROS.
       01  QLN3-WBEG-CLASS    PIC 9(3)  VALUE ZEROS.
       01  QLN3-WEND-CLASS    PIC 9(3)  VALUE ZEROS.
       01  QLN3-WBEG-ACCTNO   PIC 9(8)  VALUE ZEROS.
       01  QLN3-WEND-ACCTNO   PIC 9(8)  VALUE ZEROS. 

       01  QBW-REC.
           03  QBW-OWNBR                              PIC 9(4).
           03  QBW-SSNO                               PIC 9(9).
           03  QBW-LNAME                              PIC X(35).
           03  QBW-FNAME                              PIC X(35).

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

       01  QLP-REC.
           03  QLP-BRNO                              PIC  9(4).
           03  QLP-ACCTNO                            PIC  9(8).
           03  QLP-SEQNO                             PIC  9(5).
           03  QLP-LTOUCH-DATE                       PIC X(10).
           03  QLP-USERID                            PIC X(10).
           03  QLP-POSTDATE                          PIC X(10).
           03  QLP-POSTTIME                          PIC 9(6).
           03  QLP-TRDATE                            PIC X(10).
           03  QLP-PAYDATE                           PIC X(10).
           03  QLP-TRCD                              PIC XX.
           03  QLP-REFNO                             PIC X(5).
           03  QLP-TRAMT                             PIC S9(7)V99.
           03  QLP-APOTH                             PIC S9(7)V99.
           03  QLP-APOT2                             PIC S9(7)V99.
           03  QLP-APINT                             PIC S9(7)V99.
           03  QLP-APLC                              PIC S9(7)V99.
           03  QLP-APCUR                             PIC S9(7)V99.
           03  QLP-OTHBAL                            PIC S9(7)V99.
           03  QLP-OT2BAL                            PIC S9(7)V99.
           03  QLP-INTBAL                            PIC S9(7)V99.
           03  QLP-LCBAL                             PIC S9(7)V99.
           03  QLP-CURBAL                            PIC S9(7)V99.
           03  QLP-INTDUE                            PIC S9(7)V99.
           03  QLP-DLPROC                            PIC S9(7)V99.
           03  QLP-EARNED-1                          PIC S9(7)V99.
           03  QLP-EARNED-2                          PIC S9(7)V99.
           03  QLP-EARNED-3                          PIC S9(7)V99.
           03  QLP-INTPAID                           PIC S9(7)V99.
           03  QLP-SSNO                              PIC 9(10).
           03  QLP-PDTH-DATE                         PIC X(10).
           03  QLP-LCPDTH-DATE                       PIC X(10).
           03  QLP-INTPDTH-DATE                      PIC X(10).
           03  QLP-TILLNO                            PIC 99.
           03  QLP-APINTOWE                          PIC S9(7)V99.
           03  QLP-DLPARVHELD-ADJ                    PIC S9(7)V99.
           03  QLP-LT-SEQNO                          PIC 9(3).
           03  QLP-IBPC                              PIC X.
           03  QLP-MANUAL-RB                         PIC X.
           03  QLP-INSTYPES                          PIC X.
           03  QLP-REV                               PIC X.
           03  QLP-REVTRCD                           PIC XX.
           03  QLP-REVSEQNO                          PIC 9(5).
           03  QLP-STATUSFG                          PIC X.
           03  QLP-NEWSTAT-RATE                      PIC 999V999.
           03  QLP-LCPARTIALS                        PIC S9(7)V99.
           03  QLP-ADDON-ORIG-REGPY                  PIC S9(7)V99.
           03  QLP-NEW1ST-PYDATE                     PIC X(10).
           03  QLP-DEF-DUEDATE                       PIC X(10).
           03  QLP-LCPAID                            PIC S9(7)V99.
           03  QLP-OLD1ST-PYDATE                     PIC X(10).
           03  QLP-DEF-TERM                          PIC 9(3).
           03  QLP-ADDON-LAST-PYAMT                  PIC S9(7)V99.
           03  QLP-DLRVCHGS-ADJ-1                    PIC S9(7)V99.
           03  QLP-DLRVCHGS-ADJ-2                    PIC S9(7)V99.
           03  QLP-DLRVCHGS-ADJ-3                    PIC S9(7)V99.
           03  QLP-DLPARVPAID-ADJ                    PIC S9(7)V99.
           03  QLP-DLNO                              PIC 9(4)V99.
           03  QLP-PREPAY-PENALTY                    PIC S9(7)V99.
           03  QLP-PRORATED-INS-REB                  PIC S9(7)V99.
           03  QLP-POSTING-BR                        PIC 9(4).
           03  QLP-NOLCHG                            PIC S9(3).
           03  QLP-COLLECTOR                         PIC X(3).
           03  QLP-EXTRA-PRIN-LFP                    PIC S9(7)V99.
           03  QLP-REMAIN-INT-SINCE-LFP              PIC S9(7)V99.
           03  QLP-REMAIN-PRIN-SINCE-LFP             PIC S9(7)V99.
           03  QLP-APPLIED-TO-ESCROW                 PIC S9(7)V99.
           03  QLP-ESCROW-PREPAID                    PIC S9(7)V99.
           03  QLP-PRIOR-POOLID                      PIC 9(5).
           03  QLP-ADDON-INTPDTH-DATE                PIC X(10).
           03  QLP-FROMCL                            PIC  999.
           03  QLP-TOCL                              PIC  999.
           03  QLP-ALT-PREPAY-FG                     PIC X.
                  
       01  SUB           PIC 9(6)              VALUE ZEROS.
       01  LOOP-CTR      PIC 9(6)              VALUE ZEROS.

       01  EOF-SW                              PIC X VALUE "N".
       01  RET-CODE                            PIC S9(4) COMP-5.
       01  DSN-NAME                            PIC X(12).
       01  USERID                              PIC X(8).
       01  PASSWD.
           49 PASSWD-LENGTH                    PIC S9(4) COMP-5 VALUE 0.
           49 PASSWD-NAME                      PIC X(18).

       EXEC SQL END DECLARE SECTION END-EXEC.

       COPY "LIBGB/GETFMW.CPY".

      ******************************************************************
       PROCEDURE DIVISION.
       MAIN SECTION.
           EXEC SQL WHENEVER SQLERROR GO TO ERROR-EXIT END-EXEC.
           DISPLAY "THIS PROGRAM DEMONSTRATES CONNECTING TO A SQL SERVER
      -    " INSTANCE AND EXECUTING".
           DISPLAY "A STORED PROCEDURE (SP_LNFILE_BLOCK).  HOPEFULLY YOU 
      -    " HAVE EXECUTED 'CREATE' TO".
           DISPLAY "CREATE AND POPULATE THE TABLE, AND INSTALLED THE STO
      -    "RED PROCEDURE IN".
           DISPLAY "'SP_LNFILE_BLOB.SQL' INTO A LOCATION THAT IT CAN BE 
      -    "FOUND.".
           PERFORM INITIALIZATION-ROUTINE.
           PERFORM MAKE-CONNECTION.

      D    STOP QLN-OWNBR.

           ADD 1          TO LOOP-CTR.
           MOVE 0010      TO QLN-OWNBR.
           MOVE 00000111  TO QLN-ACCTNO.
           MOVE 252251769 TO QLN-SSNO1.

           PERFORM SELECT-BRANCH-ROUTINE.

           DISPLAY "END OF FIRST BW SELECT. ". 
           ACCEPT QLN-OWNBR.
      *    STOP RUN.          
 
           PERFORM EXECUTE-IT.
           PERFORM PROCESS-LNFILE.
           PERFORM DISCONNECT-CONNECTION.
           DISPLAY "PROGRAM COMPLETE. PRESS ENTER TO EXIT."
           ACCEPT OMITTED.
           STOP RUN.

       EXECUTE-IT.
           DISPLAY "ENTER THE BEGINNING OWNBR: ", NO.
           ACCEPT QLN3-WBEG-OWNBR.
           DISPLAY "ENTER THE ENDING OWNBR: ", NO.
           ACCEPT QLN3-WEND-OWNBR.
           DISPLAY "ENTER THE BEGINNING CLASS: ", NO.
           ACCEPT QLN3-WBEG-CLASS.
           DISPLAY "ENTER THE ENDING CLASS: ", NO.
           ACCEPT QLN3-WEND-CLASS.
           DISPLAY "ENTER THE BEGINNING ACCTNO: ", NO.
           ACCEPT QLN3-WBEG-ACCTNO.
           DISPLAY "ENTER THE ENDING ACCTNO: ", NO.
           ACCEPT QLN3-WEND-ACCTNO.
           EXEC SQL WHENEVER SQLERROR GO TO ERROR-EXIT END-EXEC.


       PROCESS-LNFILE. 

           MOVE 0 TO SUB

           EXEC SQL SET CONNECTION SP1 END-EXEC.
      
      *    EXEC SQL DECLARE LN3_CURSOR CURSOR FOR
      *             :RET-CODE = EXEC SP_LNFILE_SELECT ( :WBEG-OWNBR,
      *             :WEND-OWNBR )
      *    END-EXEC 

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :RET-CODE = EXEC SP_RZAGED_LNFILE_SELECT
              ( :QLN3-WBEG-OWNBR, :QLN3-WEND-OWNBR,  :QLN3-WBEG-CLASS,
                :QLN3-WEND-CLASS, :QLN3-WBEG-ACCTNO, :QLN3-WEND-ACCTNO )
           END-EXEC  

	   EXEC SQL
	       OPEN LN3_CURSOR
	   END-EXEC 

      *    DISPLAY "EXITING OPEN CURSOR.".

      *    DISPLAY "ENTERING FETCH.".

	   PERFORM UNTIL SQLCODE NOT = 0
    
               EXEC SQL SET CONNECTION SP1 END-EXEC

	       EXEC SQL
	           FETCH LN3_CURSOR INTO
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
                   ,:QLN-ORGBR                         
                   ,:QLN-OWNST                         
                   ,:QLN-SPRCLASS                      
                   ,:QLN-CONVERCD                      
                   ,:QLN-MIDSTREAM                     
                   ,:QLN-PURCD                         
                   ,:QLN-SRCD                          
                   ,:QLN-COLLCD                        
                   ,:QLN-COLLID                        
                   ,:QLN-POCD                          
                   ,:QLN-PLCD                          
                   ,:QLN-PLTYPE                        
                   ,:QLN-CONFIDENTCD                   
                   ,:QLN-ALLOTCD                       
                   ,:QLN-RECASTCD                      
                   ,:QLN-SKIP-TRW                      
                   ,:QLN-STATUSFG                      
                   ,:QLN-SMALLOAN                      
                   ,:QLN-SALEFIN                       
                   ,:QLN-SERVACCT                      
                   ,:QLN-LOANTYPE                      
                   ,:QLN-ADDON-FG                      
                   ,:QLN-PAY-SCHLD-FG                  
                   ,:QLN-LC-CODE                       
                   ,:QLN-CAN-LOAN-NO                   
                   ,:QLN-STATFLAGS                     
                   ,:QLN-ENTDATE                       
                   ,:QLN-LOANDATE                      
                   ,:QLN-PURDATE                       
                   ,:QLN-INTDATE                       
                   ,:QLN-CASHDATE                      
                   ,:QLN-FILEDATE                      
                   ,:QLN-RESCISSION-DATE               
                   ,:QLN-PART-RECOURSE-DATE            
                   ,:QLN-ORIG-1STPYDATE                
                   ,:QLN-1STPYDATE                     
                   ,:QLN-JDDATE                        
                   ,:QLN-PLDATE                        
                   ,:QLN-PL-TRDATE                     
                   ,:QLN-ACCELDATE                     
                   ,:QLN-REDUDATE                      
                   ,:QLN-POFFDATE                      
                   ,:QLN-PO-TRDATE                     
                   ,:QLN-LEGALDATE                     
                   ,:QLN-BNKRPTDATE                    
                   ,:QLN-BNKRPT-DISMISS-DATE           
                   ,:QLN-BNKRPT-DISCHARGE-DATE         
                   ,:QLN-BNKRPT-REAFFIRM-DATE          
                   ,:QLN-DEBTAIDATE                    
                   ,:QLN-REPODATE                      
                   ,:QLN-REDEEMDATE                    
                   ,:QLN-SOLDDATE                      
                   ,:QLN-INS-CONFIRM-DATE              
                   ,:QLN-PRIOR-RN-FOR10-DATE           
                   ,:QLN-LAST-RN-FOR10-DATE            
                   ,:QLN-TRW-DOFD-DATE                 
                   ,:QLN-LAST-PYPA-DATE                
                   ,:QLN-BT-TRDATE                     
                   ,:QLN-METRO2-LAST-PMT-DATE          
                   ,:QLN-FOLLOWUP-DATE                 
                   ,:QLN-LAST-ACCRUAL-DATE             
                   ,:QLN-APRATE                        
                   ,:QLN-EFFRATE                       
                   ,:QLN-SMPRATE                       
                   ,:QLN-YLDRATE                       
                   ,:QLN-OVRDRATE                      
                   ,:QLN-ENDORATE                      
                   ,:QLN-JDRATE                        
                   ,:QLN-ACCRATE                       
                   ,:QLN-REDURATE                      
                   ,:QLN-CONRATE                       
                   ,:QLN-SERVPER                       
                   ,:QLN-SELLPR                        
                   ,:QLN-DOWNPY                        
                   ,:QLN-TRADEIN                       
                   ,:QLN-LNAMT                         
                   ,:QLN-FINCHG                        
                   ,:QLN-INTCHG                        
                   ,:QLN-SERCHG                        
                   ,:QLN-EXTCHG                        
                   ,:QLN-MAINTFEE                      
                   ,:QLN-POINTS                        
                   ,:QLN-PROCEEDS                      
                   ,:QLN-ORGTERM                       
                   ,:QLN-1STPYAMT                      
                   ,:QLN-NOREGPY                       
                   ,:QLN-REGPYAMT                      
                   ,:QLN-LASTPYAMT                     
                   ,:QLN-PROCASH                       
                   ,:QLN-TILLNO                        
                   ,:QLN-STATE-FLAG                    
                   ,:QLN-STATE-WORKER                  
                   ,:QLN-STATE-WORKER2                 
                   ,:QLN-FEECODE-1                     
                   ,:QLN-FEEAMT-1                      
                   ,:QLN-FEEOURS-1                     
                   ,:QLN-FEEFINCHG-1                   
                   ,:QLN-FEECODE-2                     
                   ,:QLN-FEEAMT-2                      
                   ,:QLN-FEEOURS-2                     
                   ,:QLN-FEEFINCHG-2                   
                   ,:QLN-FEECODE-3                     
                   ,:QLN-FEEAMT-3                      
                   ,:QLN-FEEOURS-3                     
                   ,:QLN-FEEFINCHG-3                   
                   ,:QLN-FEECODE-4                     
                   ,:QLN-FEEAMT-4                      
                   ,:QLN-FEEOURS-4                     
                   ,:QLN-FEEFINCHG-4                   
                   ,:QLN-FEECODE-5                     
                   ,:QLN-FEEAMT-5                      
                   ,:QLN-FEEOURS-5                     
                   ,:QLN-FEEFINCHG-5                   
                   ,:QLN-FEECODE-6                     
                   ,:QLN-FEEAMT-6                      
                   ,:QLN-FEEOURS-6                     
                   ,:QLN-FEEFINCHG-6                   
                   ,:QLN-FEECODE-7                     
                   ,:QLN-FEEAMT-7                      
                   ,:QLN-FEEOURS-7                     
                   ,:QLN-FEEFINCHG-7                   
                   ,:QLN-FEECODE-8                     
                   ,:QLN-FEEAMT-8                      
                   ,:QLN-FEEOURS-8                     
                   ,:QLN-FEEFINCHG-8                   
                   ,:QLN-FEECODE-9                     
                   ,:QLN-FEEAMT-9                      
                   ,:QLN-FEEOURS-9                     
                   ,:QLN-FEEFINCHG-9                   
                   ,:QLN-FEECODE-10                    
                   ,:QLN-FEEAMT-10                     
                   ,:QLN-FEEOURS-10                    
                   ,:QLN-FEEFINCHG-10                  
                   ,:QLN-FEECODE-11                    
                   ,:QLN-FEEAMT-11                     
                   ,:QLN-FEEOURS-11                    
                   ,:QLN-FEEFINCHG-11                  
                   ,:QLN-FEECODE-12                    
                   ,:QLN-FEEAMT-12                     
                   ,:QLN-FEEOURS-12                    
                   ,:QLN-FEEFINCHG-12                  
                   ,:QLN-FEECODE-13                    
                   ,:QLN-FEEAMT-13                     
                   ,:QLN-FEEOURS-13                    
                   ,:QLN-FEEFINCHG-13                  
                   ,:QLN-FEECODE-14                    
                   ,:QLN-FEEAMT-14                     
                   ,:QLN-FEEOURS-14                    
                   ,:QLN-FEEFINCHG-14                  
                   ,:QLN-FEECODE-15                    
                   ,:QLN-FEEAMT-15                     
                   ,:QLN-FEEOURS-15                    
                   ,:QLN-FEEFINCHG-15                  
                   ,:QLN-FEECODE-16                    
                   ,:QLN-FEEAMT-16                     
                   ,:QLN-FEEOURS-16                    
                   ,:QLN-FEEFINCHG-16                  
                   ,:QLN-FEECODE-17                    
                   ,:QLN-FEEAMT-17                     
                   ,:QLN-FEEOURS-17                    
                   ,:QLN-FEEFINCHG-17                  
                   ,:QLN-FEECODE-18                    
                   ,:QLN-FEEAMT-18                     
                   ,:QLN-FEEOURS-18                    
                   ,:QLN-FEEFINCHG-18                  
                   ,:QLN-FEECODE-19                    
                   ,:QLN-FEEAMT-19                     
                   ,:QLN-FEEOURS-19                    
                   ,:QLN-FEEFINCHG-19                  
                   ,:QLN-FEECODE-20                    
                   ,:QLN-FEEAMT-20                     
                   ,:QLN-FEEOURS-20                    
                   ,:QLN-FEEFINCHG-20                  
                   ,:QLN-INSURED                       
                   ,:QLN-INSTYPES                      
                   ,:QLN-INSELIGIBLE                   
                   ,:QLN-INSCOMP-1                     
                   ,:QLN-INSOURS-1                     
                   ,:QLN-INSEFF-DATE-1                 
                   ,:QLN-INSEXP-DATE-1                 
                   ,:QLN-INSPREM-1                      
                   ,:QLN-INSCOVR-1                      
                   ,:QLN-INSCOMM-1                      
                   ,:QLN-INSANTICERN-1                  
                   ,:QLN-INSANTICADJ-1                  
                   ,:QLN-INSUFERN-1                     
                   ,:QLN-INSACCERN-1                    
                   ,:QLN-INSEARN-ORDER-1                
                   ,:QLN-INSFINCHG-1                    
                   ,:QLN-INSCOMP-2                      
                   ,:QLN-INSOURS-2                      
                   ,:QLN-INSEFF-DATE-2                  
                   ,:QLN-INSEXP-DATE-2                  
                   ,:QLN-INSPREM-2                      
                   ,:QLN-INSCOVR-2                      
                   ,:QLN-INSCOMM-2                      
                   ,:QLN-INSANTICERN-2                  
                   ,:QLN-INSANTICADJ-2                  
                   ,:QLN-INSUFERN-2                     
                   ,:QLN-INSACCERN-2                    
                   ,:QLN-INSEARN-ORDER-2                
                   ,:QLN-INSFINCHG-2                    
                   ,:QLN-INSCOMP-3                      
                   ,:QLN-INSOURS-3                      
                   ,:QLN-INSEFF-DATE-3                  
                   ,:QLN-INSEXP-DATE-3                  
                   ,:QLN-INSPREM-3                      
                   ,:QLN-INSCOVR-3                      
                   ,:QLN-INSCOMM-3                      
                   ,:QLN-INSANTICERN-3                  
                   ,:QLN-INSANTICADJ-3                  
                   ,:QLN-INSUFERN-3                     
                   ,:QLN-INSACCERN-3                    
                   ,:QLN-INSEARN-ORDER-3                
                   ,:QLN-INSFINCHG-3                    
                   ,:QLN-INSCOMP-4                      
                   ,:QLN-INSOURS-4                      
                   ,:QLN-INSEFF-DATE-4                  
                   ,:QLN-INSEXP-DATE-4                  
                   ,:QLN-INSPREM-4                      
                   ,:QLN-INSCOVR-4                      
                   ,:QLN-INSCOMM-4                      
                   ,:QLN-INSANTICERN-4                  
                   ,:QLN-INSANTICADJ-4                  
                   ,:QLN-INSUFERN-4                     
                   ,:QLN-INSACCERN-4                    
                   ,:QLN-INSEARN-ORDER-4                
                   ,:QLN-INSFINCHG-4                    
                   ,:QLN-INSCOMP-5                      
                   ,:QLN-INSOURS-5                      
                   ,:QLN-INSEFF-DATE-5                  
                   ,:QLN-INSEXP-DATE-5                  
                   ,:QLN-INSPREM-5                      
                   ,:QLN-INSCOVR-5                      
                   ,:QLN-INSCOMM-5                      
                   ,:QLN-INSANTICERN-5                  
                   ,:QLN-INSANTICADJ-5                  
                   ,:QLN-INSUFERN-5                     
                   ,:QLN-INSACCERN-5                    
                   ,:QLN-INSEARN-ORDER-5                
                   ,:QLN-INSFINCHG-5                    
                   ,:QLN-INSCOMP-6                      
                   ,:QLN-INSOURS-6                      
                   ,:QLN-INSEFF-DATE-6                  
                   ,:QLN-INSEXP-DATE-6                  
                   ,:QLN-INSPREM-6                      
                   ,:QLN-INSCOVR-6                      
                   ,:QLN-INSCOMM-6                      
                   ,:QLN-INSANTICERN-6                  
                   ,:QLN-INSANTICADJ-6                  
                   ,:QLN-INSUFERN-6                     
                   ,:QLN-INSACCERN-6                    
                   ,:QLN-INSEARN-ORDER-6                
                   ,:QLN-INSFINCHG-6                    
                   ,:QLN-INSCOMP-7                      
                   ,:QLN-INSOURS-7                      
                   ,:QLN-INSEFF-DATE-7                  
                   ,:QLN-INSEXP-DATE-7                  
                   ,:QLN-INSPREM-7                      
                   ,:QLN-INSCOVR-7                      
                   ,:QLN-INSCOMM-7                      
                   ,:QLN-INSANTICERN-7                  
                   ,:QLN-INSANTICADJ-7                  
                   ,:QLN-INSUFERN-7                     
                   ,:QLN-INSACCERN-7                    
                   ,:QLN-INSEARN-ORDER-7                
                   ,:QLN-INSFINCHG-7                    
                   ,:QLN-INSCOMP-8                      
                   ,:QLN-INSOURS-8                      
                   ,:QLN-INSEFF-DATE-8                  
                   ,:QLN-INSEXP-DATE-8                  
                   ,:QLN-INSPREM-8                      
                   ,:QLN-INSCOVR-8                      
                   ,:QLN-INSCOMM-8                      
                   ,:QLN-INSANTICERN-8                  
                   ,:QLN-INSANTICADJ-8                  
                   ,:QLN-INSUFERN-8                     
                   ,:QLN-INSACCERN-8                    
                   ,:QLN-INSEARN-ORDER-8                
                   ,:QLN-INSFINCHG-8                    
                   ,:QLN-INSCOMP-9                      
                   ,:QLN-INSOURS-9                      
                   ,:QLN-INSEFF-DATE-9                  
                   ,:QLN-INSEXP-DATE-9                  
                   ,:QLN-INSPREM-9                      
                   ,:QLN-INSCOVR-9                      
                   ,:QLN-INSCOMM-9                      
                   ,:QLN-INSANTICERN-9                  
                   ,:QLN-INSANTICADJ-9                  
                   ,:QLN-INSUFERN-9                     
                   ,:QLN-INSACCERN-9                    
                   ,:QLN-INSEARN-ORDER-9                
                   ,:QLN-INSFINCHG-9                    
                   ,:QLN-INSCOMP-10                     
                   ,:QLN-INSOURS-10                     
                   ,:QLN-INSEFF-DATE-10                 
                   ,:QLN-INSEXP-DATE-10                 
                   ,:QLN-INSPREM-10                     
                   ,:QLN-INSCOVR-10                     
                   ,:QLN-INSCOMM-10                     
                   ,:QLN-INSANTICERN-10                 
                   ,:QLN-INSANTICADJ-10                 
                   ,:QLN-INSUFERN-10                    
                   ,:QLN-INSACCERN-10                   
                   ,:QLN-INSEARN-ORDER-10               
                   ,:QLN-INSFINCHG-10                   
                   ,:QLN-PY-YYYYMM-RECDEL               
                   ,:QLN-MTD-PAYS-RECDEL                
                   ,:QLN-GARNISH-EXTR                   
                   ,:QLN-SHIP-SAME-FG                   
                   ,:QLN-AD-D-INS-COVR                  
                   ,:QLN-TOT-PARTIAL-CHGOFF             
                   ,:QLN-LPO                            
                   ,:QLN-REP-CD                         
                   ,:QLN-EMP-ID                         
                   ,:QLN-PROCESS-CD                     
                   ,:QLN-WCBC-CREDITLIM                 
                   ,:QLN-FOLLOWUP-CD                    
                   ,:QLN-MIP-FG                         
                   ,:QLN-MIP-MTD-CLAHPAID               
                   ,:QLN-MIP-CLPDTH-DATE                
                   ,:QLN-MIP-TOT-CLPAID                 
                   ,:QLN-MIP-AHPDTH-DATE                
                   ,:QLN-MIP-TOT-AHPAID                 
                   ,:QLN-IL-GROSS-INCOME                
                   ,:QLN-IL-GROSS-INCOME2               
                   ,:QLN-RESCISSION                     
                   ,:QLN-INTERIM-TRW                    
                   ,:QLN-DLNO                           
                   ,:QLN-DLPOOL                         
                   ,:QLN-DLRECOURSE                     
                   ,:QLN-DLPARTRCAMT                    
                   ,:QLN-ORIGPURAMT                     
                   ,:QLN-PURBAL                         
                   ,:QLN-DLRVPRIN-1                     
                   ,:QLN-DLRVPRIN-2                     
                   ,:QLN-DLRVPRIN-3                     
                   ,:QLN-DLRVCHGS-1                     
                   ,:QLN-DLRVCHGS-2                     
                   ,:QLN-DLRVCHGS-3                     
                   ,:QLN-DLPARVHELD                     
                   ,:QLN-DLPARVPAID                     
                   ,:QLN-DLDISC                         
                   ,:QLN-LCPDTH-DATE                    
                   ,:QLN-INTPDTH-DATE                   
                   ,:QLN-CURBAL                         
                   ,:QLN-INTBAL                         
                   ,:QLN-LCBAL                          
                   ,:QLN-OTHBAL                         
                   ,:QLN-OT2BAL                         
                   ,:QLN-LCPAID                         
                   ,:QLN-LCPARTIALS                     
                   ,:QLN-STMT-COUNTER                   
                   ,:QLN-PRIOR-STMT-COUNTER             
                   ,:QLN-FROZEN-REASON                  
                   ,:QLN-COUPON-STATEMENT               
                   ,:QLN-MONTHEND-BAL                   
                   ,:QLN-ANTICTERM                      
                   ,:QLN-ANTICERN-1                     
                   ,:QLN-ANTICADJ-1                     
                   ,:QLN-UFERN-1                        
                   ,:QLN-ACCERN-1                       
                   ,:QLN-EARN-ORDER-1                   
                   ,:QLN-ANTICERN-2                     
                   ,:QLN-ANTICADJ-2                     
                   ,:QLN-UFERN-2                        
                   ,:QLN-ACCERN-2                       
                   ,:QLN-EARN-ORDER-2                   
                   ,:QLN-ANTICERN-3                     
                   ,:QLN-ANTICADJ-3                     
                   ,:QLN-UFERN-3                        
                   ,:QLN-ACCERN-3                       
                   ,:QLN-EARN-ORDER-3                   
                   ,:QLN-ANTICERN-4                     
                   ,:QLN-ANTICADJ-4                     
                   ,:QLN-UFERN-4                        
                   ,:QLN-ACCERN-4                       
                   ,:QLN-EARN-ORDER-4                   
                   ,:QLN-ANTICERN-5                     
                   ,:QLN-ANTICADJ-5                     
                   ,:QLN-UFERN-5                        
                   ,:QLN-ACCERN-5                       
                   ,:QLN-EARN-ORDER-5                   
                   ,:QLN-ANTICERN-6                     
                   ,:QLN-ANTICADJ-6                     
                   ,:QLN-UFERN-6                        
                   ,:QLN-ACCERN-6                       
                   ,:QLN-EARN-ORDER-6                   
                   ,:QLN-ANTICERN-7                     
                   ,:QLN-ANTICADJ-7                     
                   ,:QLN-UFERN-7                        
                   ,:QLN-ACCERN-7                       
                   ,:QLN-EARN-ORDER-7                   
                   ,:QLN-ANTICERN-8                     
                   ,:QLN-ANTICADJ-8                     
                   ,:QLN-UFERN-8                        
                   ,:QLN-ACCERN-8                       
                   ,:QLN-EARN-ORDER-8                   
                   ,:QLN-ANTICERN-9                     
                   ,:QLN-ANTICADJ-9                     
                   ,:QLN-UFERN-9                        
                   ,:QLN-ACCERN-9                       
                   ,:QLN-EARN-ORDER-9                   
                   ,:QLN-ANTICERN-10                    
                   ,:QLN-ANTICADJ-10                    
                   ,:QLN-UFERN-10                       
                   ,:QLN-ACCERN-10                      
                   ,:QLN-EARN-ORDER-10                  
                   ,:QLN-LMO-PRIOR-INTEREST             
                   ,:QLN-PRIOR-UNEARNED                 
                   ,:QLN-ERNDELQ-STOP                   
                   ,:QLN-NONACRERN                      
                   ,:QLN-PRIOR-NONACRERN                
                   ,:QLN-ERN-IB-PDTH-DATE               
                   ,:QLN-ERN-IB-PRINBAL                 
                   ,:QLN-ERN-IB-INTOWE                  
                   ,:QLN-TOTERN                         
                   ,:QLN-TOTOTH                         
                   ,:QLN-TOTINT                         
                   ,:QLN-TOTLCHG                        
                   ,:QLN-TOTDEF                         
                   ,:QLN-TOTSERACCT                     
                   ,:QLN-LCWAIVERS                      
                   ,:QLN-INTWAIVERS                     
                   ,:QLN-TOTPAYMNTD                     
                   ,:QLN-TOTEXCPAYMNTD                  
                   ,:QLN-TOTNODEF                       
                   ,:QLN-YTDNODEF                       
                   ,:QLN-TOTNODEFACT                    
                   ,:QLN-YTDNODEFACT                    
                   ,:QLN-TOTNOALDEL                     
                   ,:QLN-YTDNOALDEL                     
                   ,:QLN-YTDINTPD                       
                   ,:QLN-LYRINTPD                       
                   ,:QLN-TOTINTPD                       
                   ,:QLN-RECASTINT                      
                   ,:QLN-RECASTPRIN                     
                   ,:QLN-CL-REBATE                      
                   ,:QLN-AH-REBATE                      
                   ,:QLN-PP-REBATE                      
                   ,:QLN-INT-REBATE                     
                   ,:QLN-SC-REBATE                      
                   ,:QLN-MF-REBATE                      
                   ,:QLN-DF-REBATE                      
                   ,:QLN-EX-REBATE                      
                   ,:QLN-O1-REBATE                      
                   ,:QLN-O2-REBATE                      
                   ,:QLN-O3-REBATE                      
                   ,:QLN-O4-REBATE                      
                   ,:QLN-O5-REBATE                      
                   ,:QLN-O6-REBATE                      
                   ,:QLN-O7-REBATE                      
                   ,:QLN-UN-O7-REB                      
                   ,:QLN-UN-O8-REB                      
                   ,:QLN-UN-O9-REB                      
                   ,:QLN-UN-O10-REB                     
                   ,:QLN-PLREASON                       
                   ,:QLN-COLLECTOR                      
                   ,:QLN-ACTIONCD                       
                   ,:QLN-ACTIONCD2                      
                   ,:QLN-ATCODE                         
                   ,:QLN-RECODE                         
                   ,:QLN-ESCROW-FG                      
                   ,:QLN-PROMDAYS-PASTDUE               
                   ,:QLN-PROMID                         
                   ,:QLN-PROMDATE                       
                   ,:QLN-PROMAMT                        
                   ,:QLN-PROMDATE2                      
                   ,:QLN-DATE-PAID-LAST                 
                   ,:QLN-AMT-PAID-LAST                  
                   ,:QLN-LASTCFORMNO                    
                   ,:QLN-LASTCFORMDATE                  
                   ,:QLN-DELCNT                         
                   ,:QLN-DELCNT2                        
                   ,:QLN-CREDITLIM                      
                   ,:QLN-CRSCORE                        
                   ,:QLN-LASTSFORMNO                    
                   ,:QLN-LASTSFORMDATE                  
                   ,:QLN-SOLCNT                         
                   ,:QLN-MESSAGE                        
                   ,:QLN-OLDACCTNO                      
                   ,:QLN-PRLNNO                         
                   ,:QLN-PRBAL                          
                   ,:QLN-PRINTDUE                       
                   ,:QLN-PRFINCHGRB                     
                   ,:QLN-PRINSRB                        
                   ,:QLN-PRPOCD                         
                   ,:QLN-PRPODATE                       
                   ,:QLN-PRDELINQAMT                    
                   ,:QLN-PRLNAMT                        
                   ,:QLN-PRPDUE-DAYS                    
                   ,:QLN-PRDLDISC-UNEARN                
                   ,:QLN-PRCONPROFILE                   
                   ,:QLN-SUM-ORIGPURAMT                 
                   ,:QLN-SUM-LNAMT                      
                   ,:QLN-SUM-FINCHG                     
                   ,:QLN-SUM-INTCHG                     
                   ,:QLN-SUM-ORGTERM                    
                   ,:QLN-SUM-REGPYAMT                   
                   ,:QLN-SUM-EXTCHG                     
                   ,:QLN-SUM-SERCHG                     
                   ,:QLN-SUM-MAINTFEE                   
                   ,:QLN-UNITPER-CD                     
                   ,:QLN-UNITPER-FREQ                   
                   ,:QLN-DAYS-IN-UNITPER                
                   ,:QLN-UNITPER-PER-YEAR               
                   ,:QLN-REPOCD                         
                   ,:QLN-BR-TFR-FG                      
                   ,:QLN-O3INS-DLPROC                   
                   ,:QLN-INSPREM-DLADJ                  
                   ,:QLN-PLAMT                          
                   ,:QLN-MTD-PRIN                       
                   ,:QLN-MTD-LC                         
                   ,:QLN-MTD-INT                        
                   ,:QLN-MTD-OT                         
                   ,:QLN-MTD-EX                         
                   ,:QLN-BNKRPT-CHAPTER                 
                   ,:QLN-BMO-CON-STATUS                 
                   ,:QLN-BMO-REC-STATUS                 
                   ,:QLN-APP-NUMBER                     
                   ,:QLN-GIFTNET                        
                   ,:QLN-GIFTNET-OUR-PO                 
                   ,:QLN-PROC-DISBURSED                 
                   ,:QLN-NOODDPY                        
                   ,:QLN-CONPROFILE-1                   
                   ,:QLN-CONPROFILE-2                   
                   ,:QLN-CONPROFILE-3                   
                   ,:QLN-CONPROFILE-4                   
                   ,:QLN-CONPROFILE-5                   
                   ,:QLN-RECPROFILE-1                   
                   ,:QLN-RECPROFILE-2                   
                   ,:QLN-RECPROFILE-3                   
                   ,:QLN-RECPROFILE-4                   
                   ,:QLN-RECPROFILE-5                   
                   ,:QLN-TRW-STATUS                     
                   ,:QLN-ORIGIN                         
                   ,:QLN-PO-TRCD                        
                   ,:QLN-NEXIS-CRSCORE                  
                   ,:QLN-TRW-B-CONSUMER-IND             
                   ,:QLN-PARVHELD-PAID-FG               
                   ,:QLN-ACCT-FROZEN                    
                   ,:QLN-TRW-SPECIAL-CODE               
                   ,:QLN-CREATION-STATUS                
                   ,:QLN-TOTNOLCHG                      
                   ,:QLN-LBOX                           
                   ,:QLN-EXTRA-PRIN-LFP                 
                   ,:QLN-REMAIN-INT-SINCE-LFP           
                   ,:QLN-REMAIN-PRIN-SINCE-LFP          
                   ,:QLN-INTERIM-COUPON                 
                   ,:QLN-DLCOMM-O3-CMSI                 
                   ,:QLN-DLCOMM-PP-CMSI                 
                   ,:QLN-TRW-J-CONSUMER-IND             
                   ,:QLN-ACH                            
                   ,:QLN-1099-C-FG                      
                   ,:QLN-TRW-COMPLIANCE-CODE            
                   ,:QLN-UNWIND-REASON-CODE             
                   ,:QLN-PBI-PBD-FG                     
                   ,:QLN-OLD-ACCT-FORMAT-FG             
                   ,:QLN-APPROVED-BY-USER               
                   ,:QLN-APPROVED-BY-CODE               
                   ,:QLN-APPROVED-BY-DATE               
                   ,:QLN-APPROVED-BY-TIME               
                   ,:QLN-LTOUCH-DATE                    
                   ,:QLN-LTOUCH-TIME                    
	       END-EXEC

               IF SQLCODE < 0
                   PERFORM ERROR-EXIT
               END-IF
	       IF SQLCODE = 0
                   ADD 1 TO SUB
                   IF  (SUB = 1 ) 
      *                  SUB = 1000 OR
      *                  SUB = 2000 OR
      *                  SUB = 3000 OR
      *                  SUB = 4000 OR
      *                  SUB = 5000 OR
      *                  SUB = 6000 OR
      *                  SUB = 7000)
                       DISPLAY "REC: ", LOOP-CTR, SUB, QLN-OWNBR, 
                               QLN-ACCTNO
                       PERFORM SELECT-BRANCH-ROUTINE
                       PERFORM PROCESS-LOAN-PAYMNET-ROUTINE
                   END-IF
	       END-IF
	   END-PERFORM.

           DISPLAY "EXITING FETCH.".


           EXEC SQL SET CONNECTION SP1 END-EXEC.

           EXEC SQL
               CLOSE LN3_CURSOR
           END-EXEC.

      *    EXEC SQL COMMIT END-EXEC.
      *    EXEC SQL DISCONNECT SP1 END-EXEC.
      *    IF SQLCODE < 0 PERFORM ERROR-EXIT.

      *    DISPLAY "PAST DISCONNECT OF SP1 CURSOR.".


      *    ACCEPT SQLSERVER FROM ENVIRONMENT
      *                 "OTIS_DSN_NAME".
      *    ACCEPT USERID FROM ENVIRONMENT "OTIS_ACUSQL_LOGIN".
      *    ACCEPT PASSWD-NAME FROM ENVIRONMENT "OTIS_ACUSQL_PSWD".


      *    EXEC SQL CONNECT TO :SQLSERVER AS SP1
      *                   USER :USERID USING :PASSWD-NAME
      *    END-EXEC.

      *    EXEC SQL WHENEVER SQLERROR GO TO ERROR-EXIT END-EXEC.

      *    IF SQLCODE < 0 PERFORM ERROR-EXIT.

      *    DISPLAY "PAST RECONNECT OF SP1 CURSOR.".


           IF LOOP-CTR   < 6
               IF QLN3-WBEG-OWNBR = 0010
                   MOVE 0102 TO QLN3-WBEG-OWNBR
                   MOVE 0102 TO QLN3-WEND-OWNBR
                   GO TO PROCESS-LNFILE 
               else
                   ADD 1     TO LOOP-CTR
                   MOVE 0010 TO QLN3-WBEG-OWNBR
                   MOVE 0010 TO QLN3-WEND-OWNBR
                   GO TO PROCESS-LNFILE.


       INITIALIZATION-ROUTINE.


      *     DISPLAY "ENTER THE SERVER NAME: ", NO ADVANCING.
      *     ACCEPT DSN-NAME.
      *     DISPLAY "ENTER YOUR USER ID (OR PRESS RETURN TO USE WINDOWS A
      *-           "UTHENTICATION): ", NO.
      *     ACCEPT USERID.
      *     IF USERID NOT = SPACES
      *         DISPLAY "ENTER YOUR PASSWORD : ", NO
      *         ACCEPT PASSWD-NAME

      * PASSWORDS IN A CONNECT STATEMENT MUST BE ENTERED IN A VARCHAR FORMAT
      * WITH THE LENGTH OF THE INPUT STRING.
               INSPECT PASSWD-NAME TALLYING PASSWD-LENGTH FOR CHARACTERS
                  BEFORE INITIAL " "
      *     END-IF.

           MOVE "R"    TO P-TIMER-TYPE-SET.
           MOVE ZEROES TO P-TIMER-DAYSEC-BEG.

           IF ( P-TIMER-TYPE-SET-VALID )
               MOVE P-TIMER-TYPE-SET TO P-TIMER-TYPE
           ELSE
               MOVE "R" TO P-TIMER-TYPE.     *> REPORT OPTION
 
           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.

           DISPLAY "RUN START: " P-TIMER-BEG-EDIT.


       MAKE-CONNECTION.

           ACCEPT SQLSERVER FROM ENVIRONMENT
                        "OTIS_DSN_NAME".
           ACCEPT USERID FROM ENVIRONMENT "OTIS_ACUSQL_LOGIN".
           ACCEPT PASSWD-NAME FROM ENVIRONMENT "OTIS_ACUSQL_PSWD".

           EXEC SQL CONNECT TO :SQLSERVER AS C1
                          USER :USERID USING :PASSWD-NAME
           END-EXEC.

           EXEC SQL WHENEVER SQLERROR GO TO ERROR-EXIT END-EXEC.

           IF SQLCODE < 0 PERFORM ERROR-EXIT.

      *-----

           ACCEPT SQLSERVER FROM ENVIRONMENT
                        "OTIS_DSN_NAME".
           ACCEPT USERID FROM ENVIRONMENT "OTIS_ACUSQL_LOGIN".
           ACCEPT PASSWD-NAME FROM ENVIRONMENT "OTIS_ACUSQL_PSWD".


           EXEC SQL CONNECT TO :SQLSERVER AS SP1
                          USER :USERID USING :PASSWD-NAME
           END-EXEC.

           EXEC SQL WHENEVER SQLERROR GO TO ERROR-EXIT END-EXEC.

           IF SQLCODE < 0 PERFORM ERROR-EXIT.

      *-----

           ACCEPT SQLSERVER FROM ENVIRONMENT
                        "OTIS_DSN_NAME".
           ACCEPT USERID FROM ENVIRONMENT "OTIS_ACUSQL_LOGIN".
           ACCEPT PASSWD-NAME FROM ENVIRONMENT "OTIS_ACUSQL_PSWD".


           EXEC SQL CONNECT TO :SQLSERVER AS SP2
                          USER :USERID USING :PASSWD-NAME
           END-EXEC.

           EXEC SQL WHENEVER SQLERROR GO TO ERROR-EXIT END-EXEC.

           IF SQLCODE < 0 PERFORM ERROR-EXIT.


       SELECT-BRANCH-ROUTINE.

           DISPLAY "ENTERING SELECT-BRANCH-ROUTINE."

           EXEC SQL SET CONNECTION C1 END-EXEC.

           EXEC SQL          
             SELECT BW_OWNBR 
                   ,BW_SSNO
                   ,BW_LNAME
                   ,BW_FNAME
               INTO                       
                    :QBW-OWNBR                                         
                   ,:QBW-SSNO                                            
                   ,:QBW-LNAME                                          
                   ,:QBW-FNAME                                           
             FROM DBO.BWFILE
            WHERE BW_OWNBR = :QLN-OWNBR 
              AND BW_SSNO  = :QLN-SSNO1
           END-EXEC.

           IF SQLCODE NOT = 0 
               DISPLAY "BW-ABENDED - SQLCODE = " SQLCODE
               PERFORM ERROR-EXIT.

           DISPLAY "BW-DATA: " LOOP-CTR QBW-OWNBR, QBW-SSNO, 
                               QBW-LNAME, QBW-FNAME.


       PROCESS-LOAN-PAYMNET-ROUTINE. 
	   
           DISPLAY "ENTERING PROCESS-LOAN-PAYMNET-ROUTINE."

           EXEC SQL SET CONNECTION SP2 END-EXEC.

           EXEC SQL DECLARE LP1_CURSOR CURSOR FOR
                    :RET-CODE = EXEC SP_LNFILE_BLOB ( :QLN-OWNBR,
                    :QLN-ACCTNO )
           END-EXEC 


           EXEC SQL
               OPEN LP1_CURSOR
           END-EXEC.

           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH LP1_CURSOR INTO
                      :QLP-BRNO                 
                     ,:QLP-ACCTNO               
                     ,:QLP-SEQNO                
                     ,:QLP-LTOUCH-DATE          
                     ,:QLP-USERID               
                     ,:QLP-POSTDATE             
                     ,:QLP-POSTTIME             
                     ,:QLP-TRDATE               
                     ,:QLP-PAYDATE              
                     ,:QLP-TRCD                 
                     ,:QLP-REFNO                
                     ,:QLP-TRAMT                
                     ,:QLP-APOTH                
                     ,:QLP-APOT2                
                     ,:QLP-APINT                
                     ,:QLP-APLC                 
                     ,:QLP-APCUR                
                     ,:QLP-OTHBAL               
                     ,:QLP-OT2BAL               
                     ,:QLP-INTBAL               
                     ,:QLP-LCBAL                
                     ,:QLP-CURBAL               
                     ,:QLP-INTDUE               
                     ,:QLP-DLPROC               
                     ,:QLP-EARNED-1             
                     ,:QLP-EARNED-2             
                     ,:QLP-EARNED-3             
                     ,:QLP-INTPAID              
                     ,:QLP-SSNO                 
                     ,:QLP-PDTH-DATE            
                     ,:QLP-LCPDTH-DATE          
                     ,:QLP-INTPDTH-DATE         
                     ,:QLP-TILLNO               
                     ,:QLP-APINTOWE             
                     ,:QLP-DLPARVHELD-ADJ       
                     ,:QLP-LT-SEQNO             
                     ,:QLP-IBPC                 
                     ,:QLP-MANUAL-RB            
                     ,:QLP-INSTYPES             
                     ,:QLP-REV                  
                     ,:QLP-REVTRCD              
                     ,:QLP-REVSEQNO             
                     ,:QLP-STATUSFG             
                     ,:QLP-NEWSTAT-RATE         
                     ,:QLP-LCPARTIALS           
                     ,:QLP-ADDON-ORIG-REGPY     
                     ,:QLP-NEW1ST-PYDATE        
                     ,:QLP-DEF-DUEDATE          
                     ,:QLP-LCPAID               
                     ,:QLP-OLD1ST-PYDATE        
                     ,:QLP-DEF-TERM             
                     ,:QLP-ADDON-LAST-PYAMT     
                     ,:QLP-DLRVCHGS-ADJ-1       
                     ,:QLP-DLRVCHGS-ADJ-2       
                     ,:QLP-DLRVCHGS-ADJ-3       
                     ,:QLP-DLPARVPAID-ADJ       
                     ,:QLP-DLNO                 
                     ,:QLP-PREPAY-PENALTY       
                     ,:QLP-PRORATED-INS-REB     
                     ,:QLP-POSTING-BR           
                     ,:QLP-NOLCHG               
                     ,:QLP-COLLECTOR            
                     ,:QLP-EXTRA-PRIN-LFP       
                     ,:QLP-REMAIN-INT-SINCE-LFP 
                     ,:QLP-REMAIN-PRIN-SINCE-LFP
                     ,:QLP-APPLIED-TO-ESCROW    
                     ,:QLP-ESCROW-PREPAID       
                     ,:QLP-PRIOR-POOLID         
                     ,:QLP-ADDON-INTPDTH-DATE   
                     ,:QLP-FROMCL               
                     ,:QLP-TOCL                 
                     ,:QLP-ALT-PREPAY-FG        

              END-EXEC

               IF SQLCODE < 0
                   PERFORM ERROR-EXIT
               END-IF

               IF  (SUB = 1 )
                   DISPLAY "LPFILE REC: ", LOOP-CTR QLP-BRNO,  
                                           QLP-ACCTNO,     
                                           QLP-SEQNO, QLP-PAYDATE     
               END-IF  
 

           END-PERFORM.

              EXEC SQL
                   CLOSE LP1_CURSOR
              END-EXEC.


           EXEC SQL SET CONNECTION C1 END-EXEC.

      *    EXEC SQL COMMIT END-EXEC.
      *    EXEC SQL DISCONNECT C1 END-EXEC.
      *    IF SQLCODE < 0 PERFORM ERROR-EXIT.

      *    DISPLAY "PAST DISCONNECT OF C1 CURSOR.".


      *    ACCEPT SQLSERVER FROM ENVIRONMENT
      *                 "OTIS_DSN_NAME".
      *    ACCEPT USERID FROM ENVIRONMENT "OTIS_ACUSQL_LOGIN".
      *    ACCEPT PASSWD-NAME FROM ENVIRONMENT "OTIS_ACUSQL_PSWD".


      *    EXEC SQL WHENEVER SQLERROR GO TO ERROR-EXIT END-EXEC.

      *    EXEC SQL CONNECT TO :SQLSERVER AS C1
      *                   USER :USERID USING :PASSWD-NAME
      *    END-EXEC.

      *    EXEC SQL WHENEVER SQLERROR GO TO ERROR-EXIT END-EXEC.

      *    IF SQLCODE < 0 PERFORM ERROR-EXIT.


       DISCONNECT-CONNECTION.
           EXEC SQL COMMIT END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.
           IF SQLCODE < 0 PERFORM ERROR-EXIT.

           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.

      *    PERFORM TIMER-STOP.
      *    MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.
           DISPLAY "RUN STOP: " P-TIMER-END-EDIT.
           INSPECT P-TIMER-ELAPSED-EDIT REPLACING ALL "/" BY "-".
           DISPLAY "RUN TIME: " P-TIMER-ELAPSED-EDIT.

       ERROR-EXIT.

           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.

      *    PERFORM TIMER-STOP.
      *    MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.
           DISPLAY "RUN STOP: " P-TIMER-END-EDIT.
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
           DISPLAY "SQL ERROR: SQLCODE  " SQLCODE OF SQLCA.
           DISPLAY "           SQLSTATE " SQLSTATE OF SQLCA.
           DISPLAY SQLERRMC OF SQLCA.
           ACCEPT OMITTED.
           EXEC SQL DISCONNECT ALL END-EXEC.
           STOP RUN.
