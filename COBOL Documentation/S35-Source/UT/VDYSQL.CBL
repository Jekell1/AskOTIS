      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLTEST.
       REMARKS.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/VDYSQL S35 03/31/23-09:24:39 >".
       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL INCLUDE SQLDA END-EXEC.
       77   WS-THREAD-1          USAGE HANDLE OF THREAD VALUE NULL.
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  SQL-GREATER          PIC XX VALUE ">".
       01  SQL-EQUAL-GREATER    PIC XX VALUE ">=".
       01  SQL-LESS             PIC XX VALUE "<".
       01  SQL-EQUAL-LESS       PIC XX VALUE "<=".
       01  SQL-EQUAL            PIC XX VALUE "=".
      
       01 RET-CODE       PIC S9(4) COMP-5.
       01  WS-CURSOR     PIC X(64) VALUE SPACES.
       01 WS-RETURN      PIC 9(8) COMP-5.
       01 WS-NO          PIC 9(8) VALUE ZERO.
       01 BW-COMMAND     PIC X(8192) VALUE SPACES.
       01 LN-COMMAND     PIC X(8192) VALUE SPACES.
       01 WS-COMMAND     PIC X(8192) VALUE SPACES.
       01 WS-ZERO PIC 9(8) COMP-5 VALUE ZERO.
       01 I               PIC 9(8) COMP-5.
      
      
       01  SQLSERVER     PIC X(64) VALUE SPACES.
       01  SQLUSER       PIC X(32) VALUE SPACES.
       01  SQLPASSWORD   PIC X(32) VALUE SPACES.
       01  USERID                          PIC X(16).
       01  PASSWD.
      *KC  03 PASSWD-LENGTH                PIC S9(4) COMP-5 VALUE 0.
           03 PASSWD-NAME                  PIC X(18).
       01  PARM1         PIC S9(9) COMP-5.
       01  PARM2         PIC S9(9) COMP-5.
       01  RETCODE       PIC S9(9) COMP-5.
       01  WS-SSNO       PIC 9(9).
      
       01  LN-REC.
               03 LN1-KEY  .
                   05 LN-OWNBR   PIC 9(4).
                   05 LN-ACCTNO   PIC 9(8).
               03 LN-CLASS   PIC 9(3).
               03 LN-SUBCLASS   PIC 99.
               03 LN-LAWCODE   PIC 99.
               03 LN-BWTABLEX  .
                   05 LN-SSNO1   PIC 9(9).
                   05 LN-MAKERCD1   PIC X.
                   05 LN-SSNO2   PIC 9(9).
                   05 LN-MAKERCD2   PIC X.
                   05 LN-SSNO3   PIC 9(9).
                   05 LN-MAKERCD3   PIC X.
                   05 LN-SSNO4   PIC 9(9).
                   05 LN-MAKERCD4   PIC X.
               03 LN-BWTABLE  REDEFINES LN-BWTABLEX .
                   05 LN-BORRTBL   OCCURS 4.
                       07 LN-SSNO   PIC 9(9).
                       07 LN-MAKERCD   PIC X.
               03 LN-ORGST   PIC XX.
               03 LN-ORGBR   PIC 9(4) COMP-3.
               03 LN-OWNST   PIC XX.
               03 LN-SPRCLASS   PIC 9(3).
               03 LN-CONVERCD   PIC X.
                   88 BULK-PURCHASE   VALUE "B".
                   88 CONVERSION-NORMAL   VALUE "Y".
               03 LN-MIDSTREAM   PIC X.
               03 LN-PURCD   PIC XXX.
               03 LN-SRCD   PIC XX.
               03 LN-COLLCD   PIC X(2).
               03 LN-COLLID   PIC X(20).
               03 LN-POCD   PIC XX.
                   88 LN-POCD-NONLOAN   VALUE "**" "*I" "*R" "*U".
                   88 LN-POCD-OPEN   VALUE "  ".
                   88 LN-POCD-POFF   VALUE " 1" THRU "ZZ".
                   88 LN-POCD-INRESCISSION   VALUE "*I".
                   88 LN-POCD-RESCIND   VALUE "*R".
                   88 LN-POCD-PB-RESC   VALUE "*P".
                   88 LN-POCD-UNWIND   VALUE "*U".
                   88 LN-POCD-VOID   VALUE "**".
                   88 LN-POCD-TRANSFERRED   VALUE "BT".
               03 LN-PLCD   PIC X.
               03 LN-PLTYPE   PIC X.
               03 LN-CONFIDENTCD   PIC X.
               03 LN-ALLOTCD   PIC X.
               03 LN-RECASTCD   PIC X.
               03 LN-SKIP-TRW   PIC X.
               03 LN-STATUSFG   PIC X.
               03 LN-SMALLOAN   PIC X.
               03 LN-SALEFIN   PIC X.
               03 LN-SERVACCT   PIC X.
               03 LN-LOANTYPE   PIC X.
               03 LN-ADDON-FG   PIC X.
               03 LN-PAY-SCHLD-FG   PIC X.
               03 LN-LC-CODE   PIC 99.
               03 LN-CAN-LOAN-NO   PIC XX.
               03 LN-STATFLAGS   PIC X(20).
               03 LN-STATUSFLAGS  REDEFINES LN-STATFLAGS  OCCURS 20.
                   05 LN-STATFG   PIC X.
               03 LN-ENTDATE   PIC 9(8) COMP-3.
               03 LN-LOANDATE   PIC 9(8) COMP-3.
               03 LN-PURDATE   PIC 9(8) COMP-3.
               03 LN-INTDATE   PIC 9(8) COMP-3.
               03 LN-CASHDATE   PIC 9(8) COMP-3.
               03 LN-FILEDATE   PIC 9(8) COMP-3.
               03 LN-RESCISSION-DATE   PIC 9(8) COMP-3.
               03 LN-PART-RECOURSE-DATE   PIC 9(8) COMP-3.
               03 LN-ORIG-1STPYDATE   PIC 9(8) COMP-3.
               03 LN-1STPYDATE   PIC 9(8).
               03 LN-1STPYDATEX  REDEFINES LN-1STPYDATE .
                   05 LN-1STPY-YYYYMM   PIC 9(6).
                   05 LN-1STPYDA   PIC 99.
               03 LN-JDDATE   PIC 9(8) COMP-3.
               03 LN-PLDATE   PIC 9(8) COMP-3.
               03 LN-PL-TRDATE   PIC 9(8) COMP-3.
               03 LN-ACCELDATE   PIC 9(8) COMP-3.
               03 LN-REDUDATE   PIC 9(8) COMP-3.
               03 LN-POFFDATE   PIC 9(8) COMP-3.
               03 LN-PO-TRDATE   PIC 9(8) COMP-3.
               03 LN-LEGALDATE   PIC 9(8) COMP-3.
               03 LN-BNKRPTDATE   PIC 9(8) COMP-3.
               03 LN-BNKRPT-DISMISS-DATE   PIC 9(8) COMP-3.
               03 LN-BNKRPT-DISCHARGE-DATE   PIC 9(8) COMP-3.
               03 LN-BNKRPT-REAFFIRM-DATE   PIC 9(8) COMP-3.
               03 LN-DEBTAIDATE   PIC 9(8) COMP-3.
               03 LN-REPODATE   PIC 9(8) COMP-3.
               03 LN-REDEEMDATE   PIC 9(8) COMP-3.
               03 LN-SOLDDATE   PIC 9(8) COMP-3.
               03 LN-INS-CONFIRM-DATE   PIC 9(8) COMP-3.
               03 LN-PRIOR-RN-FOR10-DATE   PIC 9(8) COMP-3.
               03 LN-LAST-RN-FOR10-DATE   PIC 9(8) COMP-3.
               03 LN-TRW-DOFD-DATE   PIC 9(8) COMP-3.
               03 LN-LAST-PYPA-DATE   PIC 9(8) COMP-3.
               03 LN-BT-TRDATE   PIC 9(8) COMP-3.
               03 LN-METRO2-LAST-PMT-DATE   PIC 9(8) COMP-3.
               03 LN-FOLLOWUP-DATE   PIC 9(8) COMP-3.
               03 LN-LAST-ACCRUAL-DATE   PIC 9(8) COMP-3.
               03 LN-APRATE   PIC 999V999 COMP-3.
               03 LN-EFFRATE   PIC 999V999 COMP-3.
               03 LN-SMPRATE   PIC 999V999 COMP-3.
               03 LN-YLDRATE   PIC 999V999 COMP-3.
               03 LN-OVRDRATE   PIC 999V999 COMP-3.
               03 LN-ENDORATE   PIC 999V999 COMP-3.
               03 LN-JDRATE   PIC 999V999 COMP-3.
               03 LN-ACCRATE   PIC 999V999 COMP-3.
               03 LN-REDURATE   PIC 999V999 COMP-3.
               03 LN-CONRATE   PIC 999V999 COMP-3.
               03 LN-SERVPER   PIC 9(3)V99 COMP-3.
               03 LN-SELLPR   PIC 9(7)V99 COMP-3.
               03 LN-DOWNPY   PIC 9(7)V99 COMP-3.
               03 LN-TRADEIN   PIC S9(7)V99 COMP-3.
               03 LN-LNAMT   PIC 9(7)V99 COMP-3.
               03 LN-FINCHG   PIC 9(7)V99 COMP-3.
               03 LN-INTCHG   PIC 9(7)V99 COMP-3.
               03 LN-SERCHG   PIC 9(5)V99 COMP-3.
               03 LN-EXTCHG   PIC 9(5)V99 COMP-3.
               03 LN-MAINTFEE   PIC 9(5)V99 COMP-3.
               03 LN-POINTS   PIC 99V99 COMP-3.
               03 LN-PROCEEDS   PIC S9(7)V99 COMP-3.
               03 LN-ORGTERM   PIC 9(3) COMP-3.
               03 LN-1STPYAMT   PIC 9(7)V99 COMP-3.
               03 LN-NOREGPY   PIC 9(3) COMP-3.
               03 LN-REGPYAMT   PIC 9(7)V99 COMP-3.
               03 LN-LASTPYAMT   PIC S9(7)V99 COMP-3.
               03 LN-PROCASH   PIC S9(7)V99 COMP-3.
               03 LN-TILLNO   PIC 9(2).
               03 LN-STATE-FLAG   PIC X.
               03 LN-STATE-WORKER   PIC S9(7)V99 COMP-3.
               03 LN-STATE-WORKER2   PIC S9(5)V99 COMP-3.
               03 LN-FEETBL   OCCURS 20.
                   05 LN-FEECODE   PIC XX.
                   05 LN-FEEAMT   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS   PIC X.
                   05 LN-FEEFINCHG   PIC X.
               03 LN-FEETBL-REDEF  REDEFINES LN-FEETBL .
                   05 LN-FEECODE-1   PIC XX.
                   05 LN-FEEAMT-1   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-1   PIC X.
                   05 LN-FEEFINCHG-1   PIC X.
                   05 LN-FEECODE-2   PIC XX.
                   05 LN-FEEAMT-2   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-2   PIC X.
                   05 LN-FEEFINCHG-2   PIC X.
                   05 LN-FEECODE-3   PIC XX.
                   05 LN-FEEAMT-3   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-3   PIC X.
                   05 LN-FEEFINCHG-3   PIC X.
                   05 LN-FEECODE-4   PIC XX.
                   05 LN-FEEAMT-4   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-4   PIC X.
                   05 LN-FEEFINCHG-4   PIC X.
                   05 LN-FEECODE-5   PIC XX.
                   05 LN-FEEAMT-5   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-5   PIC X.
                   05 LN-FEEFINCHG-5   PIC X.
                   05 LN-FEECODE-6   PIC XX.
                   05 LN-FEEAMT-6   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-6   PIC X.
                   05 LN-FEEFINCHG-6   PIC X.
                   05 LN-FEECODE-7   PIC XX.
                   05 LN-FEEAMT-7   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-7   PIC X.
                   05 LN-FEEFINCHG-7   PIC X.
                   05 LN-FEECODE-8   PIC XX.
                   05 LN-FEEAMT-8   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-8   PIC X.
                   05 LN-FEEFINCHG-8   PIC X.
                   05 LN-FEECODE-9   PIC XX.
                   05 LN-FEEAMT-9   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-9   PIC X.
                   05 LN-FEEFINCHG-9   PIC X.
                   05 LN-FEECODE-10   PIC XX.
                   05 LN-FEEAMT-10   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-10   PIC X.
                   05 LN-FEEFINCHG-10   PIC X.
                   05 LN-FEECODE-11   PIC XX.
                   05 LN-FEEAMT-11   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-11   PIC X.
                   05 LN-FEEFINCHG-11   PIC X.
                   05 LN-FEECODE-12   PIC XX.
                   05 LN-FEEAMT-12   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-12   PIC X.
                   05 LN-FEEFINCHG-12   PIC X.
                   05 LN-FEECODE-13   PIC XX.
                   05 LN-FEEAMT-13   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-13   PIC X.
                   05 LN-FEEFINCHG-13   PIC X.
                   05 LN-FEECODE-14   PIC XX.
                   05 LN-FEEAMT-14   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-14   PIC X.
                   05 LN-FEEFINCHG-14   PIC X.
                   05 LN-FEECODE-15   PIC XX.
                   05 LN-FEEAMT-15   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-15   PIC X.
                   05 LN-FEEFINCHG-15   PIC X.
                   05 LN-FEECODE-16   PIC XX.
                   05 LN-FEEAMT-16   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-16   PIC X.
                   05 LN-FEEFINCHG-16   PIC X.
                   05 LN-FEECODE-17   PIC XX.
                   05 LN-FEEAMT-17   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-17   PIC X.
                   05 LN-FEEFINCHG-17   PIC X.
                   05 LN-FEECODE-18   PIC XX.
                   05 LN-FEEAMT-18   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-18   PIC X.
                   05 LN-FEEFINCHG-18   PIC X.
                   05 LN-FEECODE-19   PIC XX.
                   05 LN-FEEAMT-19   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-19   PIC X.
                   05 LN-FEEFINCHG-19   PIC X.
                   05 LN-FEECODE-20   PIC XX.
                   05 LN-FEEAMT-20   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-20   PIC X.
                   05 LN-FEEFINCHG-20   PIC X.
               03 LN-INSCODES  .
                   05 LN-INSURED   PIC X.
                       88 LN-INS-NO-ONE   VALUE "N".
                       88 LN-INS-SINGLE   VALUE "M" "S".
                       88 LN-INS-JOINT   VALUE "J".
                   05 LN-INSTYPES   PIC X.
                       88 LN-INSTYPES-VALID   VALUE "A" THRU "Z" "0" "1"
                           "2" "3" "4" "5" "6" "7" "8" "9".
                       88 LN-INSTYPES-DL   VALUE "C" "G" "I" "J" "M" "P"
                           "T" "X" "Y" "5" "6" "7".
                       88 LN-INSTYPES-LL   VALUE "B" "A" "F" "H" "L" "O"
                           "S" "U" "W" "2" "3" "4".
                       88 LN-INSTYPES-AH   VALUE "D" "F" "G" "K" "L" "M"
                           "O" "P" "Q" "V" "W" "X" "3" "4" "6" "7" "9"
                           "0".
                       88 LN-INSTYPES-PP   VALUE "E" "H" "J" "K" "L" "M"
                           "U" "Y" "Z" "3" "6" "9".
                       88 LN-INSTYPES-PP2   VALUE "R" "S" "T" "V" "W"
                           "X" "2" "4" "5" "7" "8" "0".
                       88 LN-INSTYPES-O1   VALUE "1" "A" "I" "O" "P" "Q"
                           "U" "Y" "Z" "2" "3" "4" "5" "6" "7" "8" "9"
                           "0".
                   05 LN-INSELIGIBLE   PIC X.
                       88 LN-INSELIGIBLE-VALID   VALUE "A" THRU "Z" "0"
                           "1" "2" "3" "4" "5" "6" "7" "8" "9".
                       88 LN-INSELIGIBLE-DL   VALUE "C" "G" "I" "J" "M"
                           "P" "T" "X" "Y" "5" "6" "7".
                       88 LN-INSELIGIBLE-LL   VALUE "B" "A" "F" "H" "L"
                           "O" "S" "U" "W" "2" "3" "4".
                       88 LN-INSELIGIBLE-AH   VALUE "D" "F" "G" "K" "L"
                           "M" "O" "P" "Q" "V" "W" "X" "3" "4" "6" "7"
                           "9" "0".
                       88 LN-INSELIGIBLE-PP   VALUE "E" "H" "J" "K" "L"
                           "M" "U" "Y" "Z" "3" "6" "9".
                       88 LN-INSELIGIBLE-PP2   VALUE "R" "S" "T" "V" "W"
                           "X" "2" "4" "5" "7" "8" "0".
                       88 LN-INSELIGIBLE-O1   VALUE "1" "A" "I" "O" "P"
                           "Q" "U" "Y" "Z" "2" "3" "4" "5" "6" "7" "8"
                           "9" "0".
               03 LN-INSTBL   OCCURS 10.
                   05 LN-INSCOMP   PIC 99.
                   05 LN-INSOURS   PIC X.
                   05 LN-INSEFF-DATE   PIC 9(8) COMP-3.
                   05 LN-INSTRM  REDEFINES LN-INSEFF-DATE  PIC 9(8)
                       COMP-3.
                   05 LN-INSEXP-DATE   PIC 9(8) COMP-3.
                   05 LN-INSPREM   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER   PIC 99.
                   05 LN-INSFINCHG   PIC X.
               03 LN-INSTBL-REDEF  REDEFINES LN-INSTBL .
                   05 LN-FEECODE-1   PIC XX.
                   05 LN-FEEAMT-1   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-1   PIC X.
                   05 LN-FEEFINCHG-1   PIC X.
                   05 LN-INSCOMP-1   PIC 99.
                   05 LN-INSOURS-1   PIC X.
                   05 LN-INSEFF-DATE-1   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-1   PIC 9(8) COMP-3.
                   05 LN-INSPREM-1   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-1   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-1   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-1   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-1   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-1   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-1   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-1   PIC 99.
                   05 LN-INSFINCHG-1   PIC X.
                   05 LN-FEECODE-2   PIC XX.
                   05 LN-FEEAMT-2   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-2   PIC X.
                   05 LN-FEEFINCHG-2   PIC X.
                   05 LN-INSCOMP-2   PIC 99.
                   05 LN-INSOURS-2   PIC X.
                   05 LN-INSEFF-DATE-2   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-2   PIC 9(8) COMP-3.
                   05 LN-INSPREM-2   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-2   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-2   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-2   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-2   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-2   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-2   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-2   PIC 99.
                   05 LN-INSFINCHG-2   PIC X.
                   05 LN-FEECODE-3   PIC XX.
                   05 LN-FEEAMT-3   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-3   PIC X.
                   05 LN-FEEFINCHG-3   PIC X.
                   05 LN-INSCOMP-3   PIC 99.
                   05 LN-INSOURS-3   PIC X.
                   05 LN-INSEFF-DATE-3   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-3   PIC 9(8) COMP-3.
                   05 LN-INSPREM-3   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-3   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-3   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-3   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-3   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-3   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-3   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-3   PIC 99.
                   05 LN-INSFINCHG-3   PIC X.
                   05 LN-FEECODE-4   PIC XX.
                   05 LN-FEEAMT-4   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-4   PIC X.
                   05 LN-FEEFINCHG-4   PIC X.
                   05 LN-INSCOMP-4   PIC 99.
                   05 LN-INSOURS-4   PIC X.
                   05 LN-INSEFF-DATE-4   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-4   PIC 9(8) COMP-3.
                   05 LN-INSPREM-4   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-4   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-4   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-4   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-4   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-4   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-4   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-4   PIC 99.
                   05 LN-INSFINCHG-4   PIC X.
                   05 LN-FEECODE-5   PIC XX.
                   05 LN-FEEAMT-5   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-5   PIC X.
                   05 LN-FEEFINCHG-5   PIC X.
                   05 LN-INSCOMP-5   PIC 99.
                   05 LN-INSOURS-5   PIC X.
                   05 LN-INSEFF-DATE-5   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-5   PIC 9(8) COMP-3.
                   05 LN-INSPREM-5   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-5   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-5   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-5   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-5   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-5   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-5   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-5   PIC 99.
                   05 LN-INSFINCHG-5   PIC X.
                   05 LN-FEECODE-6   PIC XX.
                   05 LN-FEEAMT-6   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-6   PIC X.
                   05 LN-FEEFINCHG-6   PIC X.
                   05 LN-INSCOMP-6   PIC 99.
                   05 LN-INSOURS-6   PIC X.
                   05 LN-INSEFF-DATE-6   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-6   PIC 9(8) COMP-3.
                   05 LN-INSPREM-6   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-6   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-6   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-6   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-6   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-6   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-6   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-6   PIC 99.
                   05 LN-INSFINCHG-6   PIC X.
                   05 LN-FEECODE-7   PIC XX.
                   05 LN-FEEAMT-7   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-7   PIC X.
                   05 LN-FEEFINCHG-7   PIC X.
                   05 LN-INSCOMP-7   PIC 99.
                   05 LN-INSOURS-7   PIC X.
                   05 LN-INSEFF-DATE-7   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-7   PIC 9(8) COMP-3.
                   05 LN-INSPREM-7   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-7   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-7   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-7   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-7   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-7   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-7   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-7   PIC 99.
                   05 LN-INSFINCHG-7   PIC X.
                   05 LN-FEECODE-8   PIC XX.
                   05 LN-FEEAMT-8   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-8   PIC X.
                   05 LN-FEEFINCHG-8   PIC X.
                   05 LN-INSCOMP-8   PIC 99.
                   05 LN-INSOURS-8   PIC X.
                   05 LN-INSEFF-DATE-8   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-8   PIC 9(8) COMP-3.
                   05 LN-INSPREM-8   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-8   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-8   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-8   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-8   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-8   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-8   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-8   PIC 99.
                   05 LN-INSFINCHG-8   PIC X.
                   05 LN-FEECODE-9   PIC XX.
                   05 LN-FEEAMT-9   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-9   PIC X.
                   05 LN-FEEFINCHG-9   PIC X.
                   05 LN-INSCOMP-9   PIC 99.
                   05 LN-INSOURS-9   PIC X.
                   05 LN-INSEFF-DATE-9   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-9   PIC 9(8) COMP-3.
                   05 LN-INSPREM-9   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-9   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-9   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-9   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-9   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-9   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-9   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-9   PIC 99.
                   05 LN-INSFINCHG-9   PIC X.
                   05 LN-FEECODE-10   PIC XX.
                   05 LN-FEEAMT-10   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-10   PIC X.
                   05 LN-FEEFINCHG-10   PIC X.
                   05 LN-INSCOMP-10   PIC 99.
                   05 LN-INSOURS-10   PIC X.
                   05 LN-INSEFF-DATE-10   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-10   PIC 9(8) COMP-3.
                   05 LN-INSPREM-10   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-10   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-10   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-10   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-10   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-10   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-10   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-10   PIC 99.
                   05 LN-INSFINCHG-10   PIC X.
                   05 LN-FEECODE-11   PIC XX.
                   05 LN-FEEAMT-11   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-11   PIC X.
                   05 LN-FEEFINCHG-11   PIC X.
                   05 LN-INSCOMP-11   PIC 99.
                   05 LN-INSOURS-11   PIC X.
                   05 LN-INSEFF-DATE-11   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-11   PIC 9(8) COMP-3.
                   05 LN-INSPREM-11   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-11   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-11   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-11   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-11   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-11   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-11   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-11   PIC 99.
                   05 LN-INSFINCHG-11   PIC X.
                   05 LN-FEECODE-12   PIC XX.
                   05 LN-FEEAMT-12   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-12   PIC X.
                   05 LN-FEEFINCHG-12   PIC X.
                   05 LN-INSCOMP-12   PIC 99.
                   05 LN-INSOURS-12   PIC X.
                   05 LN-INSEFF-DATE-12   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-12   PIC 9(8) COMP-3.
                   05 LN-INSPREM-12   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-12   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-12   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-12   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-12   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-12   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-12   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-12   PIC 99.
                   05 LN-INSFINCHG-12   PIC X.
                   05 LN-FEECODE-13   PIC XX.
                   05 LN-FEEAMT-13   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-13   PIC X.
                   05 LN-FEEFINCHG-13   PIC X.
                   05 LN-INSCOMP-13   PIC 99.
                   05 LN-INSOURS-13   PIC X.
                   05 LN-INSEFF-DATE-13   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-13   PIC 9(8) COMP-3.
                   05 LN-INSPREM-13   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-13   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-13   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-13   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-13   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-13   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-13   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-13   PIC 99.
                   05 LN-INSFINCHG-13   PIC X.
                   05 LN-FEECODE-14   PIC XX.
                   05 LN-FEEAMT-14   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-14   PIC X.
                   05 LN-FEEFINCHG-14   PIC X.
                   05 LN-INSCOMP-14   PIC 99.
                   05 LN-INSOURS-14   PIC X.
                   05 LN-INSEFF-DATE-14   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-14   PIC 9(8) COMP-3.
                   05 LN-INSPREM-14   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-14   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-14   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-14   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-14   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-14   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-14   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-14   PIC 99.
                   05 LN-INSFINCHG-14   PIC X.
                   05 LN-FEECODE-15   PIC XX.
                   05 LN-FEEAMT-15   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-15   PIC X.
                   05 LN-FEEFINCHG-15   PIC X.
                   05 LN-INSCOMP-15   PIC 99.
                   05 LN-INSOURS-15   PIC X.
                   05 LN-INSEFF-DATE-15   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-15   PIC 9(8) COMP-3.
                   05 LN-INSPREM-15   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-15   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-15   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-15   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-15   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-15   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-15   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-15   PIC 99.
                   05 LN-INSFINCHG-15   PIC X.
                   05 LN-FEECODE-16   PIC XX.
                   05 LN-FEEAMT-16   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-16   PIC X.
                   05 LN-FEEFINCHG-16   PIC X.
                   05 LN-INSCOMP-16   PIC 99.
                   05 LN-INSOURS-16   PIC X.
                   05 LN-INSEFF-DATE-16   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-16   PIC 9(8) COMP-3.
                   05 LN-INSPREM-16   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-16   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-16   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-16   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-16   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-16   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-16   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-16   PIC 99.
                   05 LN-INSFINCHG-16   PIC X.
                   05 LN-FEECODE-17   PIC XX.
                   05 LN-FEEAMT-17   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-17   PIC X.
                   05 LN-FEEFINCHG-17   PIC X.
                   05 LN-INSCOMP-17   PIC 99.
                   05 LN-INSOURS-17   PIC X.
                   05 LN-INSEFF-DATE-17   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-17   PIC 9(8) COMP-3.
                   05 LN-INSPREM-17   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-17   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-17   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-17   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-17   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-17   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-17   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-17   PIC 99.
                   05 LN-INSFINCHG-17   PIC X.
                   05 LN-FEECODE-18   PIC XX.
                   05 LN-FEEAMT-18   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-18   PIC X.
                   05 LN-FEEFINCHG-18   PIC X.
                   05 LN-INSCOMP-18   PIC 99.
                   05 LN-INSOURS-18   PIC X.
                   05 LN-INSEFF-DATE-18   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-18   PIC 9(8) COMP-3.
                   05 LN-INSPREM-18   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-18   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-18   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-18   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-18   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-18   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-18   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-18   PIC 99.
                   05 LN-INSFINCHG-18   PIC X.
                   05 LN-FEECODE-19   PIC XX.
                   05 LN-FEEAMT-19   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-19   PIC X.
                   05 LN-FEEFINCHG-19   PIC X.
                   05 LN-INSCOMP-19   PIC 99.
                   05 LN-INSOURS-19   PIC X.
                   05 LN-INSEFF-DATE-19   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-19   PIC 9(8) COMP-3.
                   05 LN-INSPREM-19   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-19   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-19   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-19   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-19   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-19   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-19   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-19   PIC 99.
                   05 LN-INSFINCHG-19   PIC X.
                   05 LN-FEECODE-20   PIC XX.
                   05 LN-FEEAMT-20   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-20   PIC X.
                   05 LN-FEEFINCHG-20   PIC X.
                   05 LN-INSCOMP-20   PIC 99.
                   05 LN-INSOURS-20   PIC X.
                   05 LN-INSEFF-DATE-20   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-20   PIC 9(8) COMP-3.
                   05 LN-INSPREM-20   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-20   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-20   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-20   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-20   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-20   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-20   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-20   PIC 99.
                   05 LN-INSFINCHG-20   PIC X.
               03 LN-PY-YYYYMM-RECDEL   PIC 9(6) COMP-3.
               03 LN-MTD-PAYS-RECDEL   PIC S9(7)V99 COMP-3.
               03 LN-GARNISH-EXTR   PIC X.
               03 LN-SHIP-SAME-FG   PIC X.
               03 LN-AD-D-INS-COVR   PIC 9(7) COMP-3.
               03 LN-TOT-PARTIAL-CHGOFF   PIC S9(7)V99 COMP-3.
               03 LN-LPO   PIC X(4).
               03 LN-REP-CD   PIC X(8).
               03 LN-EMP-ID   PIC 9(8) COMP-3.
               03 LN-PROCESS-CD   PIC X.
               03 LN-WCBC-CREDITLIM   PIC S9(5) COMP-3.
               03 LN-FOLLOWUP-CD   PIC X(4).
               03 LN-MIP-FG   PIC X.
               03 LN-MIP-MTD-CLAHPAID   PIC S9(5)V99 COMP-3.
               03 LN-MIP-CLPDTH-DATE   PIC 9(8) COMP-3.
               03 LN-MIP-TOT-CLPAID   PIC S9(7)V99 COMP-3.
               03 LN-MIP-AHPDTH-DATE   PIC 9(8) COMP-3.
               03 LN-MIP-TOT-AHPAID   PIC S9(7)V99 COMP-3.
               03 LN-IL-GROSS-INCOME   PIC S9(7)V99 COMP-3.
               03 LN-IL-GROSS-INCOME2   PIC S9(7)V99 COMP-3.
               03 LN-RESCISSION   PIC X(1).
               03 LN-INTERIM-TRW   PIC X(1).
               03 LN-DLNO   PIC 9(4)V99 COMP-3.
               03 LN-DLPOOL   PIC 9(5) COMP-3.
               03 LN-DLRECOURSE   PIC X.
               03 LN-DLPARTRCAMT   PIC 9(7)V99 COMP-3.
               03 LN-ORIGPURAMT   PIC 9(7)V99 COMP-3.
               03 LN-PURBAL   PIC S9(7)V99 COMP-3.
               03 LN-DLRVPRIN   PIC 9(7)V99 COMP-3 OCCURS 3.
               03 LN-DLRVPRIN-REDEF  REDEFINES LN-DLRVPRIN .
                 04 LN-DLRVPRIN-1   PIC 9(7)V99 COMP-3.
                 04 LN-DLRVPRIN-2   PIC 9(7)V99 COMP-3.
                 04 LN-DLRVPRIN-3   PIC 9(7)V99 COMP-3.
               03 LN-DLRVCHGS   PIC 9(7)V99 COMP-3 OCCURS 3.
               03 LN-DLRVCHGS-REDEF  REDEFINES LN-DLRVCHGS .
                 04 LN-DLRVCHGS-1   PIC 9(7)V99 COMP-3.
                 04 LN-DLRVCHGS-2   PIC 9(7)V99 COMP-3.
                 04 LN-DLRVCHGS-3   PIC 9(7)V99 COMP-3.
               03 LN-DLPARVHELD   PIC 9(7)V99 COMP-3.
               03 LN-DLPARVPAID   PIC 9(7)V99 COMP-3.
               03 LN-DLDISC   PIC 9(7)V99 COMP-3.
               03 LN-LCPDTH-DATE   PIC 9(8) COMP-3.
               03 LN-INTPDTH-DATE   PIC 9(8) COMP-3.
               03 LN-CURBAL   PIC S9(7)V99 COMP-3.
               03 LN-INTBAL   PIC S9(7)V99 COMP-3.
               03 LN-LCBAL   PIC S9(7)V99 COMP-3.
               03 LN-OTHBAL   PIC S9(7)V99 COMP-3.
               03 LN-OT2BAL   PIC S9(7)V99 COMP-3.
               03 LN-LCPAID   PIC S9(7)V99 COMP-3.
               03 LN-LCPARTIALS   PIC S9(7)V99 COMP-3.
               03 LN-STMT-COUNTER   PIC 9(5) COMP-3.
               03 LN-PRIOR-STMT-COUNTER   PIC 9(5) COMP-3.
               03 LN-FROZEN-REASON   PIC X(2).
               03 LN-COUPON-STATEMENT   PIC X.
               03 LN-MONTHEND-BAL   PIC 9(7)V99 COMP-3.
               03 LN-ANTICTERM   PIC 9(3) COMP-3.
               03 LN-ANTICTBL   OCCURS 10.
                   05 LN-ANTICERN   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER   PIC 99.
               03 LN-ANTICTBL-REDEF  REDEFINES LN-ANTICTBL .
                   05 LN-FEECODE-1   PIC XX.
                   05 LN-FEEAMT-1   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-1   PIC X.
                   05 LN-FEEFINCHG-1   PIC X.
                   05 LN-INSCOMP-1   PIC 99.
                   05 LN-INSOURS-1   PIC X.
                   05 LN-INSEFF-DATE-1   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-1   PIC 9(8) COMP-3.
                   05 LN-INSPREM-1   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-1   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-1   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-1   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-1   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-1   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-1   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-1   PIC 99.
                   05 LN-INSFINCHG-1   PIC X.
                   05 LN-ANTICERN-1   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-1   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-1   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-1   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-1   PIC 99.
                   05 LN-FEECODE-2   PIC XX.
                   05 LN-FEEAMT-2   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-2   PIC X.
                   05 LN-FEEFINCHG-2   PIC X.
                   05 LN-INSCOMP-2   PIC 99.
                   05 LN-INSOURS-2   PIC X.
                   05 LN-INSEFF-DATE-2   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-2   PIC 9(8) COMP-3.
                   05 LN-INSPREM-2   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-2   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-2   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-2   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-2   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-2   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-2   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-2   PIC 99.
                   05 LN-INSFINCHG-2   PIC X.
                   05 LN-ANTICERN-2   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-2   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-2   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-2   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-2   PIC 99.
                   05 LN-FEECODE-3   PIC XX.
                   05 LN-FEEAMT-3   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-3   PIC X.
                   05 LN-FEEFINCHG-3   PIC X.
                   05 LN-INSCOMP-3   PIC 99.
                   05 LN-INSOURS-3   PIC X.
                   05 LN-INSEFF-DATE-3   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-3   PIC 9(8) COMP-3.
                   05 LN-INSPREM-3   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-3   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-3   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-3   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-3   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-3   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-3   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-3   PIC 99.
                   05 LN-INSFINCHG-3   PIC X.
                   05 LN-ANTICERN-3   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-3   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-3   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-3   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-3   PIC 99.
                   05 LN-FEECODE-4   PIC XX.
                   05 LN-FEEAMT-4   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-4   PIC X.
                   05 LN-FEEFINCHG-4   PIC X.
                   05 LN-INSCOMP-4   PIC 99.
                   05 LN-INSOURS-4   PIC X.
                   05 LN-INSEFF-DATE-4   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-4   PIC 9(8) COMP-3.
                   05 LN-INSPREM-4   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-4   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-4   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-4   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-4   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-4   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-4   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-4   PIC 99.
                   05 LN-INSFINCHG-4   PIC X.
                   05 LN-ANTICERN-4   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-4   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-4   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-4   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-4   PIC 99.
                   05 LN-FEECODE-5   PIC XX.
                   05 LN-FEEAMT-5   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-5   PIC X.
                   05 LN-FEEFINCHG-5   PIC X.
                   05 LN-INSCOMP-5   PIC 99.
                   05 LN-INSOURS-5   PIC X.
                   05 LN-INSEFF-DATE-5   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-5   PIC 9(8) COMP-3.
                   05 LN-INSPREM-5   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-5   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-5   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-5   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-5   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-5   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-5   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-5   PIC 99.
                   05 LN-INSFINCHG-5   PIC X.
                   05 LN-ANTICERN-5   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-5   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-5   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-5   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-5   PIC 99.
                   05 LN-FEECODE-6   PIC XX.
                   05 LN-FEEAMT-6   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-6   PIC X.
                   05 LN-FEEFINCHG-6   PIC X.
                   05 LN-INSCOMP-6   PIC 99.
                   05 LN-INSOURS-6   PIC X.
                   05 LN-INSEFF-DATE-6   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-6   PIC 9(8) COMP-3.
                   05 LN-INSPREM-6   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-6   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-6   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-6   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-6   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-6   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-6   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-6   PIC 99.
                   05 LN-INSFINCHG-6   PIC X.
                   05 LN-ANTICERN-6   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-6   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-6   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-6   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-6   PIC 99.
                   05 LN-FEECODE-7   PIC XX.
                   05 LN-FEEAMT-7   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-7   PIC X.
                   05 LN-FEEFINCHG-7   PIC X.
                   05 LN-INSCOMP-7   PIC 99.
                   05 LN-INSOURS-7   PIC X.
                   05 LN-INSEFF-DATE-7   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-7   PIC 9(8) COMP-3.
                   05 LN-INSPREM-7   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-7   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-7   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-7   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-7   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-7   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-7   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-7   PIC 99.
                   05 LN-INSFINCHG-7   PIC X.
                   05 LN-ANTICERN-7   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-7   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-7   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-7   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-7   PIC 99.
                   05 LN-FEECODE-8   PIC XX.
                   05 LN-FEEAMT-8   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-8   PIC X.
                   05 LN-FEEFINCHG-8   PIC X.
                   05 LN-INSCOMP-8   PIC 99.
                   05 LN-INSOURS-8   PIC X.
                   05 LN-INSEFF-DATE-8   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-8   PIC 9(8) COMP-3.
                   05 LN-INSPREM-8   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-8   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-8   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-8   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-8   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-8   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-8   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-8   PIC 99.
                   05 LN-INSFINCHG-8   PIC X.
                   05 LN-ANTICERN-8   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-8   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-8   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-8   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-8   PIC 99.
                   05 LN-FEECODE-9   PIC XX.
                   05 LN-FEEAMT-9   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-9   PIC X.
                   05 LN-FEEFINCHG-9   PIC X.
                   05 LN-INSCOMP-9   PIC 99.
                   05 LN-INSOURS-9   PIC X.
                   05 LN-INSEFF-DATE-9   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-9   PIC 9(8) COMP-3.
                   05 LN-INSPREM-9   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-9   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-9   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-9   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-9   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-9   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-9   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-9   PIC 99.
                   05 LN-INSFINCHG-9   PIC X.
                   05 LN-ANTICERN-9   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-9   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-9   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-9   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-9   PIC 99.
                   05 LN-FEECODE-10   PIC XX.
                   05 LN-FEEAMT-10   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-10   PIC X.
                   05 LN-FEEFINCHG-10   PIC X.
                   05 LN-INSCOMP-10   PIC 99.
                   05 LN-INSOURS-10   PIC X.
                   05 LN-INSEFF-DATE-10   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-10   PIC 9(8) COMP-3.
                   05 LN-INSPREM-10   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-10   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-10   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-10   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-10   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-10   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-10   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-10   PIC 99.
                   05 LN-INSFINCHG-10   PIC X.
                   05 LN-ANTICERN-10   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-10   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-10   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-10   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-10   PIC 99.
                   05 LN-FEECODE-11   PIC XX.
                   05 LN-FEEAMT-11   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-11   PIC X.
                   05 LN-FEEFINCHG-11   PIC X.
                   05 LN-INSCOMP-11   PIC 99.
                   05 LN-INSOURS-11   PIC X.
                   05 LN-INSEFF-DATE-11   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-11   PIC 9(8) COMP-3.
                   05 LN-INSPREM-11   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-11   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-11   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-11   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-11   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-11   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-11   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-11   PIC 99.
                   05 LN-INSFINCHG-11   PIC X.
                   05 LN-ANTICERN-11   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-11   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-11   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-11   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-11   PIC 99.
                   05 LN-FEECODE-12   PIC XX.
                   05 LN-FEEAMT-12   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-12   PIC X.
                   05 LN-FEEFINCHG-12   PIC X.
                   05 LN-INSCOMP-12   PIC 99.
                   05 LN-INSOURS-12   PIC X.
                   05 LN-INSEFF-DATE-12   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-12   PIC 9(8) COMP-3.
                   05 LN-INSPREM-12   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-12   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-12   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-12   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-12   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-12   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-12   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-12   PIC 99.
                   05 LN-INSFINCHG-12   PIC X.
                   05 LN-ANTICERN-12   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-12   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-12   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-12   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-12   PIC 99.
                   05 LN-FEECODE-13   PIC XX.
                   05 LN-FEEAMT-13   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-13   PIC X.
                   05 LN-FEEFINCHG-13   PIC X.
                   05 LN-INSCOMP-13   PIC 99.
                   05 LN-INSOURS-13   PIC X.
                   05 LN-INSEFF-DATE-13   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-13   PIC 9(8) COMP-3.
                   05 LN-INSPREM-13   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-13   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-13   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-13   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-13   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-13   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-13   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-13   PIC 99.
                   05 LN-INSFINCHG-13   PIC X.
                   05 LN-ANTICERN-13   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-13   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-13   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-13   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-13   PIC 99.
                   05 LN-FEECODE-14   PIC XX.
                   05 LN-FEEAMT-14   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-14   PIC X.
                   05 LN-FEEFINCHG-14   PIC X.
                   05 LN-INSCOMP-14   PIC 99.
                   05 LN-INSOURS-14   PIC X.
                   05 LN-INSEFF-DATE-14   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-14   PIC 9(8) COMP-3.
                   05 LN-INSPREM-14   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-14   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-14   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-14   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-14   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-14   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-14   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-14   PIC 99.
                   05 LN-INSFINCHG-14   PIC X.
                   05 LN-ANTICERN-14   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-14   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-14   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-14   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-14   PIC 99.
                   05 LN-FEECODE-15   PIC XX.
                   05 LN-FEEAMT-15   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-15   PIC X.
                   05 LN-FEEFINCHG-15   PIC X.
                   05 LN-INSCOMP-15   PIC 99.
                   05 LN-INSOURS-15   PIC X.
                   05 LN-INSEFF-DATE-15   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-15   PIC 9(8) COMP-3.
                   05 LN-INSPREM-15   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-15   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-15   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-15   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-15   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-15   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-15   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-15   PIC 99.
                   05 LN-INSFINCHG-15   PIC X.
                   05 LN-ANTICERN-15   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-15   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-15   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-15   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-15   PIC 99.
                   05 LN-FEECODE-16   PIC XX.
                   05 LN-FEEAMT-16   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-16   PIC X.
                   05 LN-FEEFINCHG-16   PIC X.
                   05 LN-INSCOMP-16   PIC 99.
                   05 LN-INSOURS-16   PIC X.
                   05 LN-INSEFF-DATE-16   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-16   PIC 9(8) COMP-3.
                   05 LN-INSPREM-16   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-16   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-16   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-16   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-16   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-16   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-16   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-16   PIC 99.
                   05 LN-INSFINCHG-16   PIC X.
                   05 LN-ANTICERN-16   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-16   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-16   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-16   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-16   PIC 99.
                   05 LN-FEECODE-17   PIC XX.
                   05 LN-FEEAMT-17   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-17   PIC X.
                   05 LN-FEEFINCHG-17   PIC X.
                   05 LN-INSCOMP-17   PIC 99.
                   05 LN-INSOURS-17   PIC X.
                   05 LN-INSEFF-DATE-17   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-17   PIC 9(8) COMP-3.
                   05 LN-INSPREM-17   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-17   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-17   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-17   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-17   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-17   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-17   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-17   PIC 99.
                   05 LN-INSFINCHG-17   PIC X.
                   05 LN-ANTICERN-17   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-17   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-17   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-17   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-17   PIC 99.
                   05 LN-FEECODE-18   PIC XX.
                   05 LN-FEEAMT-18   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-18   PIC X.
                   05 LN-FEEFINCHG-18   PIC X.
                   05 LN-INSCOMP-18   PIC 99.
                   05 LN-INSOURS-18   PIC X.
                   05 LN-INSEFF-DATE-18   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-18   PIC 9(8) COMP-3.
                   05 LN-INSPREM-18   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-18   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-18   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-18   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-18   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-18   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-18   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-18   PIC 99.
                   05 LN-INSFINCHG-18   PIC X.
                   05 LN-ANTICERN-18   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-18   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-18   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-18   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-18   PIC 99.
                   05 LN-FEECODE-19   PIC XX.
                   05 LN-FEEAMT-19   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-19   PIC X.
                   05 LN-FEEFINCHG-19   PIC X.
                   05 LN-INSCOMP-19   PIC 99.
                   05 LN-INSOURS-19   PIC X.
                   05 LN-INSEFF-DATE-19   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-19   PIC 9(8) COMP-3.
                   05 LN-INSPREM-19   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-19   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-19   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-19   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-19   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-19   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-19   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-19   PIC 99.
                   05 LN-INSFINCHG-19   PIC X.
                   05 LN-ANTICERN-19   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-19   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-19   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-19   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-19   PIC 99.
                   05 LN-FEECODE-20   PIC XX.
                   05 LN-FEEAMT-20   PIC 9(5)V99 COMP-3.
                   05 LN-FEEOURS-20   PIC X.
                   05 LN-FEEFINCHG-20   PIC X.
                   05 LN-INSCOMP-20   PIC 99.
                   05 LN-INSOURS-20   PIC X.
                   05 LN-INSEFF-DATE-20   PIC 9(8) COMP-3.
                   05 LN-INSEXP-DATE-20   PIC 9(8) COMP-3.
                   05 LN-INSPREM-20   PIC 9(5)V99 COMP-3.
                   05 LN-INSCOVR-20   PIC 9(7)V99 COMP-3.
                   05 LN-INSCOMM-20   PIC 9(5)V99 COMP-3.
                   05 LN-INSANTICERN-20   PIC S9(7)V99 COMP-3.
                   05 LN-INSANTICADJ-20   PIC S9(7)V99 COMP-3.
                   05 LN-INSUFERN-20   PIC S9(7)V99 COMP-3.
                   05 LN-INSACCERN-20   PIC S9(7)V99 COMP-3.
                   05 LN-INSEARN-ORDER-20   PIC 99.
                   05 LN-INSFINCHG-20   PIC X.
                   05 LN-ANTICERN-20   PIC S9(7)V99 COMP-3.
                   05 LN-ANTICADJ-20   PIC S9(7)V99 COMP-3.
                   05 LN-UFERN-20   PIC S9(7)V99 COMP-3.
                   05 LN-ACCERN-20   PIC S9(7)V99 COMP-3.
                   05 LN-EARN-ORDER-20   PIC 99.
               03 LN-LMO-PRIOR-INTEREST   PIC S9(7)V99 COMP-3.
               03 LN-PRIOR-UNEARNED   PIC S9(7)V99 COMP-3.
               03 LN-ERNDELQ-STOP   PIC X.
               03 LN-NONACRERN   PIC S9(7)V99 COMP-3.
               03 LN-PRIOR-NONACRERN   PIC S9(7)V99 COMP-3.
               03 LN-ERN-IB-PDTH-DATE   PIC 9(8) COMP-3.
               03 LN-ERN-IB-PRINBAL   PIC S9(7)V99 COMP-3.
               03 LN-ERN-IB-INTOWE   PIC S9(7)V99 COMP-3.
               03 LN-TOTERN   PIC S9(7)V99 COMP-3.
               03 LN-TOTOTH   PIC S9(7)V99 COMP-3.
               03 LN-TOTINT   PIC S9(7)V99 COMP-3.
               03 LN-TOTLCHG   PIC S9(7)V99 COMP-3.
               03 LN-TOTDEF   PIC S9(7)V99 COMP-3.
               03 LN-TOTSERACCT   PIC S9(7)V99 COMP-3.
               03 LN-LCWAIVERS   PIC S9(5)V99 COMP-3.
               03 LN-INTWAIVERS   PIC S9(5)V99 COMP-3.
               03 LN-TOTPAYMNTD   PIC S9(7)V99 COMP-3.
               03 LN-TOTEXCPAYMNTD   PIC S9(7)V99 COMP-3.
               03 LN-TOTNODEF   PIC S99 COMP-3.
               03 LN-YTDNODEF   PIC S99 COMP-3.
               03 LN-TOTNODEFACT   PIC S99 COMP-3.
               03 LN-YTDNODEFACT   PIC S99 COMP-3.
               03 LN-TOTNOALDEL   PIC S99 COMP-3.
               03 LN-YTDNOALDEL   PIC S99 COMP-3.
               03 LN-YTDINTPD   PIC S9(7)V99 COMP-3.
               03 LN-LYRINTPD   PIC S9(7)V99 COMP-3.
               03 LN-TOTINTPD   PIC S9(7)V99 COMP-3.
               03 LN-RECASTINT   PIC 9(7)V99 COMP-3.
               03 LN-RECASTPRIN   PIC 9(7)V99 COMP-3.
               03 LN-REBATE-TAB  .
                   05 LN-CL-REBATE   PIC X.
                   05 LN-AH-REBATE   PIC X.
                   05 LN-PP-REBATE   PIC X.
                   05 LN-INT-REBATE   PIC X.
                   05 LN-SC-REBATE   PIC X.
                   05 LN-MF-REBATE   PIC X.
                   05 LN-DF-REBATE   PIC X.
                   05 LN-EX-REBATE   PIC X.
                   05 LN-O1-REBATE   PIC X.
                   05 LN-O2-REBATE   PIC X.
                   05 LN-O3-REBATE   PIC X.
                   05 LN-O4-REBATE   PIC X.
                   05 LN-O5-REBATE   PIC X.
                   05 LN-O6-REBATE   PIC X.
                   05 LN-O7-REBATE   PIC X.
                   05 LN-UN-O7-REB   PIC X.
                   05 LN-UN-O8-REB   PIC X.
                   05 LN-UN-O9-REB   PIC X.
                   05 LN-UN-O10-REB   PIC X.
               03 LN-REBATE-TABX  REDEFINES LN-REBATE-TAB .
                   05 LN-REBATE   PIC X OCCURS 19.
               03 LN-PLREASON   PIC XX.
               03 LN-COLLECTOR   PIC X(3).
               03 LN-ACTIONCD   PIC X(4).
               03 LN-ACTIONCD2   PIC X(4).
               03 LN-ATCODE   PIC X(3).
               03 LN-RECODE   PIC X(3).
               03 LN-ESCROW-FG   PIC X.
               03 LN-PROMDAYS-PASTDUE   PIC S9(3) COMP-3.
               03 LN-PROMID   PIC X(3).
               03 LN-PROMDATE   PIC 9(8) COMP-3.
               03 LN-PROMAMT   PIC 9(7)V99 COMP-3.
               03 LN-PROMDATE2   PIC 9(8) COMP-3.
               03 LN-DATE-PAID-LAST   PIC 9(8) COMP-3.
               03 LN-AMT-PAID-LAST   PIC 9(7)V99 COMP-3.
               03 LN-LASTCFORMNO   PIC 9(5) COMP-3.
               03 LN-LASTCFORMDATE   PIC 9(8) COMP-3.
               03 LN-DELCNT   PIC 99.
               03 LN-DELCNT2   PIC 99.
               03 LN-CREDITLIM   PIC 9(7) COMP-3.
               03 LN-CRSCORE   PIC S9(3) COMP-3.
               03 LN-LASTSFORMNO   PIC 9(5) COMP-3.
               03 LN-LASTSFORMDATE   PIC 9(8) COMP-3.
               03 LN-SOLCNT   PIC 99.
               03 LN-MESSAGE   PIC X(30).
               03 LN-OLDACCTNO   PIC 9(8) COMP-3.
               03 LN-PRLNNO   PIC 9(8) COMP-3.
               03 LN-PRBAL   PIC S9(7)V99 COMP-3.
               03 LN-PRINTDUE   PIC S9(7)V99 COMP-3.
               03 LN-PRFINCHGRB   PIC S9(7)V99 COMP-3.
               03 LN-PRINSRB   PIC S9(7)V99 COMP-3.
               03 LN-PRPOCD   PIC XX.
               03 LN-PRPODATE   PIC 9(8) COMP-3.
               03 LN-PRDELINQAMT   PIC S9(7)V99 COMP-3.
               03 LN-PRLNAMT   PIC 9(7)V99 COMP-3.
               03 LN-PRPDUE-DAYS   PIC S9(5) COMP-3.
               03 LN-PRDLDISC-UNEARN   PIC S9(7)V99 COMP-3.
               03 LN-PRCONPROFILE   PIC 9(12) COMP-3.
               03 LN-SUM-ORIGPURAMT   PIC 9(7)V99 COMP-3.
               03 LN-SUM-LNAMT   PIC 9(7)V99 COMP-3.
               03 LN-SUM-FINCHG   PIC 9(7)V99 COMP-3.
               03 LN-SUM-INTCHG   PIC 9(7)V99 COMP-3.
               03 LN-SUM-ORGTERM   PIC 9(3) COMP-3.
               03 LN-SUM-REGPYAMT   PIC 9(7)V99 COMP-3.
               03 LN-SUM-EXTCHG   PIC 9(5)V99 COMP-3.
               03 LN-SUM-SERCHG   PIC 9(5)V99 COMP-3.
               03 LN-SUM-MAINTFEE   PIC 9(7)V99 COMP-3.
               03 LN-UNITPER-INFO  .
                   05 LN-UNITPER-CD   PIC X.
                       88 LN-UNITPER-CD-DAYS   VALUE "D" "S" "W" "B".
                       88 LN-UNITPER-CD-MONTHS   VALUE "M" "Y".
                   05 LN-UNITPER-FREQ   PIC 999 COMP-3.
               03 LN-DAYS-IN-UNITPER   PIC 9(3) COMP-3.
               03 LN-UNITPER-PER-YEAR   PIC 9(3)V9(12) COMP-3.
               03 LN-REPOCD   PIC X.
                   88 LN-IS-REPO-AR   VALUE "X" "S".
                   88 LN-REPO-ONHAND   VALUE "X".
                   88 LN-REPO-REDEEMED   VALUE "V".
                   88 LN-REPO-SOLD   VALUE "S".
               03 LN-BR-TFR-FG   PIC X.
               03 LN-O3INS-DLPROC   PIC 9(7)V99 COMP-3.
               03 LN-INSPREM-DLADJ   PIC S9(5)V99 COMP-3.
               03 LN-PLAMT   PIC 9(7)V99 COMP-3.
               03 LN-MTD-PRIN   PIC S9(7)V99 COMP-3.
               03 LN-MTD-LC   PIC S9(5)V99 COMP-3.
               03 LN-MTD-INT   PIC S9(5)V99 COMP-3.
               03 LN-MTD-OT   PIC S9(5)V99 COMP-3.
               03 LN-MTD-EX   PIC S9(7)V99 COMP-3.
               03 LN-BNKRPT-CHAPTER   PIC 9(2).
               03 LN-BMO-CON-STATUS   PIC X.
               03 LN-BMO-REC-STATUS   PIC X.
               03 LN-APP-NUMBER   PIC 9(09) COMP-3.
               03 LN-GIFTNET   PIC X.
               03 LN-GIFTNET-OUR-PO   PIC 9(7) COMP-3.
               03 LN-PROC-DISBURSED   PIC X.
               03 LN-NOODDPY   PIC X.
               03 LN-CONPROFILE   PIC 9(12) COMP-3 OCCURS 5.
               03 LN-CONPROFILE-REDEF  REDEFINES LN-CONPROFILE .
                 04 LN-CONPROFILE-1   PIC 9(12) COMP-3.
                 04 LN-CONPROFILE-2   PIC 9(12) COMP-3.
                 04 LN-CONPROFILE-3   PIC 9(12) COMP-3.
                 04 LN-CONPROFILE-4   PIC 9(12) COMP-3.
                 04 LN-CONPROFILE-5   PIC 9(12) COMP-3.
               03 LN-RECPROFILE   PIC 9(12) COMP-3 OCCURS 5.
               03 LN-RECPROFILE-REDEF  REDEFINES LN-RECPROFILE .
                 04 LN-RECPROFILE-1   PIC 9(12) COMP-3.
                 04 LN-RECPROFILE-2   PIC 9(12) COMP-3.
                 04 LN-RECPROFILE-3   PIC 9(12) COMP-3.
                 04 LN-RECPROFILE-4   PIC 9(12) COMP-3.
                 04 LN-RECPROFILE-5   PIC 9(12) COMP-3.
               03 LN-TRW-STATUS   PIC XX.
               03 LN-ORIGIN   PIC X(4).
               03 LN-PO-TRCD   PIC XX.
               03 LN-NEXIS-CRSCORE   PIC 9(3) COMP-3.
               03 LN-TRW-B-CONSUMER-IND   PIC X(2).
               03 LN-PARVHELD-PAID-FG   PIC X.
               03 LN-ACCT-FROZEN   PIC X.
               03 LN-TRW-SPECIAL-CODE   PIC X(2).
               03 LN-CREATION-STATUS   PIC X(1).
                   88 LN-CREATION-EXTERNAL   VALUE "L".
                   88 LN-CREATION-INTERNAL   VALUE "I" " ".
                   88 LN-CREATION-LI-VENDOR   VALUE "L".
               03 LN-TOTNOLCHG   PIC S9(3) COMP-3.
               03 LN-LBOX   PIC X(1).
               03 LN-EXTRA-PRIN-LFP   PIC S9(7)V99 COMP-3.
               03 LN-REMAIN-INT-SINCE-LFP   PIC S9(7)V99 COMP-3.
               03 LN-REMAIN-PRIN-SINCE-LFP   PIC S9(7)V99 COMP-3.
               03 LN-INTERIM-COUPON   PIC X.
               03 LN-DLCOMM-O3-CMSI   PIC 9(5)V99 COMP-3.
               03 LN-DLCOMM-PP-CMSI   PIC 9(5)V99 COMP-3.
               03 LN-TRW-J-CONSUMER-IND   PIC X(2).
               03 LN-ACH   PIC X.
               03 LN-1099-C-FG   PIC X.
               03 LN-TRW-COMPLIANCE-CODE   PIC XX.
                   88 LN-TRW-COMPLIANCE-VALID   VALUE "XA" THRU "XH"
                       "XJ" "XR".
               03 LN-UNWIND-REASON-CODE   PIC X(2).
               03 LN-PBI-PBD-FG   PIC X.
               03 LN-OLD-ACCT-FORMAT-FG   PIC X.
               03 LN-APPROVED-BY-USER   PIC X(10).
               03 LN-APPROVED-BY-CODE   PIC X(3).
               03 LN-APPROVED-BY-DATE   PIC 9(8) COMP-3.
               03 LN-APPROVED-BY-TIME   PIC 9(6) COMP-3.
               03 LN-FUTURE-AMT1   PIC S9(7)V99 COMP-3.
               03 LN-FUTURE-AMT2   PIC S9(7)V99 COMP-3.
               03 LN-FUTURE-AMT3   PIC S9(7)V99 COMP-3.
               03 LN-FUTURE-AMT4   PIC S9(7)V99 COMP-3.
               03 LN-FUTURE-AMT5   PIC S9(7)V99 COMP-3.
               03 LN-FUTURE-AMT6   PIC S9(7)V99 COMP-3.
               03 LN-FUTURE-AMT7   PIC S9(7)V99 COMP-3.
               03 LN-FUTURE-AMT8   PIC S9(7)V99 COMP-3.
               03 LN-FUTURE-AMT9   PIC S9(7)V99 COMP-3.
               03 LN-FUTURE-AMT10   PIC S9(7)V99 COMP-3.
               03 LN-LTOUCH-DATE   PIC 9(8) COMP-3.
               03 LN-LTOUCH-TIME   PIC 9(6) COMP-3.
      
       01 BW-REC  .
           03 BW1-KEY  .
               05 BW-OWNBR   PIC 9(4).
               05 BW5-KEY  .
                   07 BW-SSNO   PIC 9(9).
           03 BW-NAME  .
               05 BW-LNAME   PIC X(35).
               05 BW-FNAME   PIC X(35).
           03 BW-MNAME   PIC X(35).
           03 BW-NICKNAME   PIC X(35).
           03 BW-GENERATION   PIC X(7).
           03 BW-ADR1   PIC X(30).
           03 BW-ADR2   PIC X(30).
           03 BW-CITY   PIC X(30).
           03 BW-STATE   PIC XX.
           03 BW-ZIP   PIC X(10).
           03 BW-LTOUCH-DATE   PIC 9(8) COMP-3.
           03 BW-REFDATE   PIC 9(8) .
           03 BW-BDATE   PIC 9(8) COMP-3.
           03 BW-PHONE   PIC 9(10) COMP-3.
           03 BW-CELL-PHONE   PIC 9(10) COMP-3.
           03 BW-NATIONALITY   PIC X(8).
           03 BW-GENDER   PIC X.
           03 BW-DEPEND   PIC 99.
           03 BW-RENTCD   PIC X(4).
           03 BW-TIMEADDR   PIC X(4).
           03 BW-CREDITCD   PIC X(4).
           03 BW-STRTGCD   PIC X(6).
           03 BW-DWELLCD   PIC XX.
           03 BW-COUNTY   PIC X(4).
           03 BW-ORIGIN   PIC X(4).
           03 BW-CENSUSCD   PIC X(10).
           03 BW-ORGSLSMN   PIC 9(4) COMP-3.
           03 BW-REPCODE   PIC X(4).
           03 BW-SPSSNO   PIC 9(9).
           03 BW-SPNAME  .
               05 BW-SPLNAME   PIC X(35).
               05 BW-SPFNAME   PIC X(35).
           03 BW-SP-BDATE   PIC 9(8) COMP-3.
           03 BW-EMPLOYER   PIC X(35).
           03 BW-SOLICIT   PIC X.
      
       EXEC SQL END DECLARE SECTION END-EXEC.
      
       01  WS-COUNT PIC S9(8) VALUE ZERO.
       01  WS-TOTAL        PIC 9(8).
       01  WS-CHAR PIC X VALUE SPACES.
       01  WS-TIME         PIC 9(8).
       01  WS-ERROR        PIC X(60) VALUE SPACES.
       01  EXTENDED-STATUS          PIC X(5).
       01  TEXT-MESSAGE             PIC X(50) VALUE SPACES.
       01  WS-START-TIME.
           03 WS-START-HH           PIC 99.
           03                       PIC X VALUE ":".
           03 WS-START-MM           PIC 99.
           03                       PIC X VALUE ":".
           03 WS-START-SS           PIC 99.
       01  WS-S-TIME                PIC 9(11).
       01  WS-END-TIME.
           03 WS-END-HH             PIC 99.
           03                       PIC X VALUE ":".
           03 WS-END-MM             PIC 99.
           03                       PIC X VALUE ":".
           03 WS-END-SS             PIC 99.
       01  WS-E-TIME                PIC 9(11).
       01  WS-PROCESS-TIME.
           03 WS-PROCESS-HH         PIC 99.
           03                       PIC X VALUE ":".
           03 WS-PROCESS-MM         PIC 99.
           03                       PIC X VALUE ":".
           03 WS-PROCESS-SS         PIC 99.
       01  WS-TEMP-HH               PIC 99.
       01  WS-TOTAL-TIME            PIC 9(11).
       SCREEN SECTION.
       01  SCREEN01.
           03 BLANK SCREEN.
           03 VALUE "WORLD ACCEPTANCE, ODBC/SQL STRESS TEST"
                         LINE 3 COL 32.
           03 LINE + 2 COL 5 VALUE "START TIME:  ".
           03 COL 30 FROM WS-START-TIME.
           03 LINE + 2 COL 5 VALUE "END TIME:  ".
           03 COL 30 FROM WS-END-TIME.
           03 LINE + 4 COL 5 VALUE "PROCESS TIME: ".
           03 COL 30 FROM WS-PROCESS-TIME.
           03 LINE + 5 COL 5 "LNFILE RECORDS PROCESSED:".
           03 COL 30 FROM WS-TOTAL.
           03 COL + 10 FROM LN-ACCTNO.
           03 COL + 5 FROM BW-LNAME.
           03 LINE 24 COL 2 FROM WS-ERROR.
      ******************************************************************
       PROCEDURE DIVISION.
      
      
            ACCEPT WS-TIME  FROM TIME.
            MOVE  WS-TIME(1:2) TO WS-START-HH.
            MOVE  WS-TIME(3:2) TO WS-START-MM.
            MOVE  WS-TIME(5:2) TO WS-START-SS.
      D     STOP WS-TIME.
            PERFORM MAKE-CONNECTION.
      
            MOVE 10 TO PARM1
            MOVE 0 TO PARM2
      
            EXEC SQL RELAXED SET CONNECTION CONNECT2 END-EXEC
            EXEC SQL
                DECLARE BWCURSOR CURSOR FOR DYNAMIC_SQL
            END-EXEC.
      
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *     STOP WS-TIME.
      *     EXEC SQL RELAXED SET CONNECTION CONNECT2 END-EXEC
      *     MOVE 10 TO BW-OWNBR.
      *     MOVE 001748052 TO BW-SSNO.
      *     PERFORM BW-READ.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      D     STOP WS-TIME.

            EXEC SQL RELAXED SET CONNECTION CONNECT1 END-EXEC

            STOP WS-TIME.
            EXEC SQL RELAXED
                DECLARE LNCURSOR CURSOR FOR
                    EXEC LN_FILE
            END-EXEC.

            MOVE 1 TO WS-NO

            EXEC SQL OPEN LNCURSOR END-EXEC.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
      * BELOW WORKS PROVIDED YOU COMMENT LOGIC FOR GETTING BW
      *VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
      *     STOP WS-TIME.
      *     EXEC SQL RELAXED SET CONNECTION CONNECT1 END-EXEC
      *     EXEC SQL
      *         DECLARE LNCURSOR CURSOR FOR DYNAMIC_SQL
      *     END-EXEC.
      *
      *     MOVE '{CALL LN_FILE}' TO LN-COMMAND  | YOU CAN ADD OUT TO THE
      *     EXEC SQL
      *          PREPARE DYNAMIC_SQL FROM :LN-COMMAND
      *     END-EXEC
      *
      *     EXEC SQL RELAXED
      *          OPEN LNCURSOR
      *     END-EXEC

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *    EXEC SQL RELAXED SET CONNECTION CONNECT1 END-EXEC
      *    EXEC SQL DECLARE LNCURSOR CURSOR FOR
      *             EXEC LN_FILE (:LN-ACCTNO, :LN-OWNBR OUT)
      *    END-EXEC.
      *    EXEC SQL RELAXED
      *         OPEN LNCURSOR
      *    END-EXEC.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
            PERFORM TEST AFTER
                    UNTIL LN-OWNBR > 10 OR SQLCODE NOT = 0
      *         EXEC SQL RELAXED SET CONNECTION CONNECT1 END-EXEC
                EXEC SQL RELAXED
                FETCH NEXT LNCURSOR
                INTO
                    :LN-OWNBR, :LN-CLASS, :LN-ACCTNO, :LN-SSNO1,
                    :LN-SSNO2, :LN-SSNO3, :LN-SSNO4, :LN-DLNO,
                    :LN-LOANDATE, :LN-ENTDATE,
                    :LN-SUM-LNAMT, :LN-PLDATE,
                    :LN-PLCD, :LN-PLTYPE, :LN-SALEFIN,
                    :LN-DLRECOURSE, :LN-SERVACCT,
                    :LN-JDDATE, :LN-REPOCD,
                    :LN-BNKRPTDATE, :LN-LEGALDATE,
                    :LN-DEBTAIDATE, :LN-CURBAL, :LN-INTBAL, :LN-LCBAL,
                    :LN-OTHBAL, :LN-ORGST, :LN-SPRCLASS, :LN-SUBCLASS,
                    :LN-LAWCODE,  :LN-DATE-PAID-LAST,
                    :LN-UNITPER-CD, :LN-UNITPER-FREQ,
                   :LN-RECPROFILE-1, :LN-RECPROFILE-2, :LN-RECPROFILE-3,
                   :LN-RECPROFILE-4, :LN-RECPROFILE-5, :LN-CONPROFILE-1,
                   :LN-CONPROFILE-2, :LN-CONPROFILE-3, :LN-CONPROFILE-4,
                    :LN-CONPROFILE-5, :LN-APRATE, :LN-OT2BAL,
                    :LN-1STPYDATE,:LN-REGPYAMT, :LN-ANTICERN-1,
                    :LN-ANTICERN-2, :LN-ANTICERN-3, :LN-ANTICERN-4,
                    :LN-ANTICERN-5, :LN-ANTICERN-6, :LN-ANTICERN-7,
                    :LN-ANTICERN-8, :LN-ANTICERN-9, :LN-ANTICERN-10,
                    :LN-ANTICADJ-1, :LN-ANTICADJ-2, :LN-ANTICADJ-3,
                    :LN-ANTICADJ-4, :LN-ANTICADJ-5, :LN-ANTICADJ-6,
                    :LN-ANTICADJ-7, :LN-ANTICADJ-8, :LN-ANTICADJ-9,
                    :LN-ANTICADJ-10, :LN-ACCERN-1, :LN-ACCERN-2,
                    :LN-ACCERN-3, :LN-ACCERN-4, :LN-ACCERN-5,
                    :LN-ACCERN-6, :LN-ACCERN-7, :LN-ACCERN-8,
                    :LN-ACCERN-9, :LN-ACCERN-10,
                    :LN-UFERN-1,
                    :LN-UFERN-2,
                    :LN-UFERN-3, :LN-UFERN-4, :LN-UFERN-5, :LN-UFERN-6,
                    :LN-UFERN-7, :LN-UFERN-8, :LN-UFERN-9, :LN-UFERN-10,
                    :LN-MTD-PAYS-RECDEL, :LN-PY-YYYYMM-RECDEL,
                    :LN-TOTPAYMNTD, :LN-1STPYAMT, :LN-LASTPYAMT
                END-EXEC
      
      *
                IF SQLCODE = 0
                    EXEC SQL RELAXED SET CONNECTION CONNECT2 END-EXEC
                    MOVE LN-OWNBR TO BW-OWNBR
                    MOVE LN-SSNO1 TO BW-SSNO
                    PERFORM BW-READ
      
      *             MOVE LN-SSNO2 TO BW-SSNO
      *             PERFORM BW-READ
      *
      *             MOVE LN-SSNO3 TO BW-SSNO
      *             PERFORM BW-READ
      *
      *             MOVE LN-SSNO4 TO BW-SSNO
      *             PERFORM BW-READ
      *
                    MOVE ZERO TO SQLCODE
                    ADD 1 TO WS-COUNT
                    ADD 1 TO WS-TOTAL
                    IF WS-COUNT > 99 OR WS-TOTAL = 1
                      DISPLAY SCREEN01
                      ACCEPT OMITTED  LINE 1 COL 1 BEFORE TIME 0
                      MOVE ZERO TO WS-COUNT
                      END-IF
                END-IF
      
            END-PERFORM.
      
      
            COMPUTE WS-S-TIME = WS-START-HH * 60
            COMPUTE WS-S-TIME = (WS-S-TIME + WS-START-MM) * 60
            COMPUTE WS-S-TIME = WS-S-TIME + WS-START-SS
      
            ACCEPT WS-TIME FROM TIME
            MOVE  WS-TIME(1:2) TO WS-END-HH.
            MOVE  WS-TIME(3:2) TO WS-END-MM.
            MOVE  WS-TIME(5:2) TO WS-END-SS.
      
            MOVE WS-END-HH TO WS-TEMP-HH
            IF WS-TEMP-HH < WS-START-HH
               ADD 24 TO WS-TEMP-HH
            END-IF
            COMPUTE WS-E-TIME = WS-TEMP-HH * 60
            COMPUTE WS-E-TIME = (WS-E-TIME + WS-END-MM) * 60
            COMPUTE WS-E-TIME = WS-E-TIME + WS-END-SS
            COMPUTE WS-TOTAL-TIME = WS-E-TIME - WS-S-TIME
      
            DIVIDE WS-TOTAL-TIME BY 60 GIVING WS-TOTAL-TIME
                                           REMAINDER WS-PROCESS-SS
            DIVIDE WS-TOTAL-TIME BY 60 GIVING WS-TOTAL-TIME
                                           REMAINDER WS-PROCESS-MM
            MOVE WS-TOTAL-TIME TO WS-PROCESS-HH
      
            MOVE "PROCESSING COMPLETED." TO WS-ERROR.
      
            DISPLAY SCREEN01.
            ACCEPT OMITTED.
      
      
            EXIT PROGRAM.
            STOP RUN.
      
      
        BW-READ.
      D     STOP WS-TIME.
             MOVE '{CALL BW_READ(?, ?)}' TO BW-COMMAND  | YOU CAN ADD OUT TO THE
             EXEC SQL
                 PREPARE DYNAMIC_SQL FROM :BW-COMMAND
             END-EXEC
       
            EXEC SQL RELAXED
              OPEN BWCURSOR USING :BW-OWNBR, :BW-SSNO
            END-EXEC.
        
           EXEC SQL
                FETCH NEXT FROM BWCURSOR INTO
                  :BW-OWNBR, :BW-LNAME, :BW-FNAME, :BW-SPSSNO,
                  :BW-SPLNAME, :BW-SPFNAME, :BW-SSNO,
                  :BW-SOLICIT
           END-EXEC.
      
      *    EXEC SQL CLOSE BWCURSOR END-EXEC.
           PERFORM BW-CLOSE-CURSOR.
      
      
       BW-CLOSE-CURSOR.
           EXEC SQL CLOSE BWCURSOR END-EXEC.


      
      
       MAKE-CONNECTION.
      *    ACCEPT SQLSERVER FROM ENVIRONMENT
      *                 "A_MSSQL_DEFAULT_CONNECTION".
           ACCEPT SQLSERVER FROM ENVIRONMENT
                        "OTIS_ACUSQL_ODBC_NM".
      *    ACCEPT USERID FROM ENVIRONMENT "A_MSSQL_LOGIN".
           ACCEPT USERID FROM ENVIRONMENT "OTIS_ACUSQL_LOGIN".
      *    ACCEPT PASSWD-NAME FROM ENVIRONMENT "A_MSSQL_PASSWD".
           ACCEPT PASSWD-NAME FROM ENVIRONMENT "OTIS_ACUSQL_PSWD".
      *KC  PERFORM VARYING PASSWD-LENGTH FROM 1 BY 1
      *KC          UNTIL PASSWD-NAME(PASSWD-LENGTH:1) = SPACES
      *KC  END-PERFORM.
      *KC  SUBTRACT 1 FROM PASSWD-LENGTH
      
      *KC  EXEC SQL CONNECT TO :SQLSERVER AS CONNECT1
      *KC                 USER :USERID USING :PASSWD
      *KC  END-EXEC.
      
           EXEC SQL CONNECT TO :SQLSERVER AS CONNECT1
                          USER :USERID USING :PASSWD-NAME
           END-EXEC.

           EXEC SQL WHENEVER SQLERROR GO TO ERROR-EXIT END-EXEC.
      
           IF SQLCODE < 0 PERFORM ERROR-EXIT.
      
      
      *KC  EXEC SQL CONNECT TO :SQLSERVER AS CONNECT2
      *KC                 USER :USERID USING :PASSWD
      *KC  END-EXEC.
      
           EXEC SQL CONNECT TO :SQLSERVER AS CONNECT2
                          USER :USERID USING :PASSWD-NAME
           END-EXEC.
      
      
           IF SQLCODE < 0 PERFORM ERROR-EXIT.
      
       END-MAKE-CONNECTION.
      
       BW-CLOSE.
           EXEC SQL CLOSE BWCURSOR END-EXEC.
      
       ERROR-EXIT.
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
           DISPLAY "SQL ERROR: SQLCODE  " SQLCODE OF SQLCA.
           DISPLAY "           SQLSTATE " SQLSTATE OF SQLCA.
           DISPLAY SQLERRMC OF SQLCA.
           MOVE SQLERRMC TO WS-ERROR.
           DISPLAY SCREEN01
           ACCEPT OMITTED
           EXIT PROGRAM
           EXEC SQL DISCONNECT ALL END-EXEC.
           STOP RUN.
      
