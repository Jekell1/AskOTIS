      * COPYMEMBER: LIBLP/LPWKCOPL
      **************************************************************************
      *   LPWKCOPL
      *
      *  THIS IS THE AREA WHERE DATA IS PASSED BACK FROM THE
      *    CALCULATION PROGRAM.
      *
      * REV:
      *  JTG 110788 INCREASED SIZE OF PRIOR LOAN FIELDS
      *  JTG 062089     "       "       "     "    "
      *  JTG 081889 COP-PRPAID-AHEAD FOR (GA)
      *             COP-RESTRICT-ADJ
      *  JTG 101889 COP-STATE-WORKER FOR (GA)
      *  JTG 102489 ADDED COP-PRLOAN-RESTRICTED (GA)
      *  JTG 011990 ADDED COP-OTINS-COV-ERR COP-PPINS-ERR (GA)
      *  JTG 051790 ADDED COP-DUAL-LOAN-ADJ  (GA)
      *             COP-SMALLOAN, COP-SERVACCT, COP-SALEFIN
      *  JTG 062195 EXPANDED FOR 10 INSURANCES
      *  JTG 062395 ADDED CODES FOR CALC OF O1
      *  JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  JTG 072397 ADDED COP-INSFINCHG & COP-GL-AMOUNT-TOTAL-FINCHG
      *  JTG 082097 ADDED FOR WORLD A90, COP-PRIOR-CASHADV,
      *                   COP-RESTRICT-SERCHG, COP-RESTRICT-HIGH-CASHADV
      *  JTG 020498 ADDED COP-CAN-LOAN-NO, RE: LOGIC TO FORCE ONE CALENDAR
      *             MONTH TO FIRST PAYMENT
      *  JTG 000112 ADDED COP-INS-REJECT-TERM RE: SP-INSREJECT-FG OPTION 'C',
      *             WHICH WILL CAUSE WRITTING NO OF MONTHS INSURANCE NOT IN
      *             VIOLATION, WISCONSIN #1032
      *  JTG 000316 CHANGED COP-UNKNOWN-ERROR VALUE " " TO VALUE " " "%"
      *             CALCULATIONS VIA: POINTS ENTERED CAUSE SERVICE CHARGE TO
      *             EXCEED SPR MAX DURING INTERMIM CALCULATION AND SET
      *             MAXIMUM SCHG INPUTED       WESTERN CREDIT PL: #243
      *  JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99    LENDMARK PR#J021
      *             ADDED 88  COP-LOAN-VALUES-OVER-MAX    VALUE "V".
      *  JTG 020201 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *             ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS  WORLD #288
      *             ADDED COP-INS-TAX               PIC S9(7)V99 COMP-3.
      *  BAH 020806 ADDED COP-MFEE-RESTRICT AND SOME FILLER, PR# 1867 REGWAN
      *  BAH 030618 ADDED COP-PRIOR-SUB-CLASS FOR SCFRMLA 46, PR#2101 LENDMARK
      *
      * KEC 2015.0605 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED THE USE OF 'INDEXED BY' ON OCCURS
      *         FOR USE WITH MICROFOCUS.
      *         COPIX DEFINED IN LIBLP/LPWKINDEX.CPY
      *  BAH 160819 CHANGED COP-PRIOR-LOAN-CLASS TO 9(3)
      *  BAH 170413 CHANGED 8 DIGIT DATES, PD0006
      **************************************************************************
       01  COP-CALCULATION-OUTPUT-AREA.
           05 COP-CONTROL-FIELDS.
             10  COP-NO-OF-TIMES-CALLED        PIC S999 COMP.
             10  COP-RETURN-CODE                 PIC X.
               88  COP-CALCULATION-OK            VALUE "A".
               88  COP-BAD-INPUT                 VALUE "B".
               88  COP-WONT-CONVERGE             VALUE "C".
               88  COP-PAYMENT-ERR               VALUE "D".
               88  COP-1ST-PAYMENT-ERR           VALUE "E".
               88  COP-LHINS-ERR                 VALUE "F".
               88  COP-AHINS-ERR                 VALUE "G".
               88  COP-PPINS-ERR                 VALUE "H".
               88  COP-MAINT-FEES-ERR            VALUE "I".
               88  COP-SERVICE-CHARGE-ERR        VALUE "J".
               88  COP-ANNUITY-FACTOR-ERR        VALUE "K".
               88  COP-AMOUNT-FINACED-ERR        VALUE "L".
               88  COP-FINCHG-ERR                VALUE "M".
               88  COP-OVR-RATE-ERR              VALUE "N".
               88  COP-APR-ERR                   VALUE "O".
               88  COP-MANY-SIMPLE-ERR           VALUE "P".
               88  COP-FTINS-ERR                 VALUE "Q".
               88  COP-OTINS-COV-ERR             VALUE "R".
               88  COP-BALLOON-ERR               VALUE "S".
               88  COP-PROCEEDS-ERR              VALUE "T".
               88  COP-LOAN-VALUES-OVER-MAX      VALUE "V".
               88  COP-UNKNOWN-ERROR             VALUE " " "%".
             10  COP-COUNTERS.
               15  COP-LOOP-COUNT                PIC S9(5) COMP.
               15  COP-LOOP-COUNT-MAX            PIC S9(5) COMP.
             10  COP-WORKING-AMOUNTS.
               15  COP-TOTAL-NOTE              PIC S9(7)V99 COMP-3.
               15  COP-INT-CHARGEABLE          PIC S9(7)V99 COMP-3.
               15  COP-NEW-LOAN-PROCEEDS       PIC S9(7)V99 COMP-3.
               15  COP-1ST-PMT-MTHS            PIC 999 COMP-3.
               15  COP-1ST-PMT-DAYS            PIC 999 COMP-3.
               15  COP-1ST-PMT-EXTENSION       PIC 999 COMP-3.
               15  COP-ETBL                    PIC 9.
           05 COP-OTHER-FILE-FIELDS.
             10 COP-LOANTYPE                   PIC X.
               88 COP-INTEREST-BEARING         VALUE "I".
               88 COP-PRECALCULATED            VALUE "P".
             10 COP-SMALLOAN                   PIC X.
             10 COP-SALEFIN                    PIC X.
             10 COP-SERVACCT                   PIC X.
             10 COP-CAN-LOAN-NO                PIC XX.
             10  COP-PRIOR-LOAN-INFO.
               15  COP-PRIOR-LOAN-STATE        PIC XX.
               15  COP-PRIOR-LOAN-CLASS        PIC 999.
               15  COP-PRIOR-LOAN.
                 20  COP-PRBAL                 PIC S9(7)V99  COMP-3.
                 20  COP-PRINTDUE              PIC S9(7)V99  COMP-3.
                 20  COP-PRFINCHGRB            PIC S9(7)V99  COMP-3.
                 20  COP-PRINSRB               PIC S9(7)V99  COMP-3.
                 20  COP-PRPAID-AHEAD          PIC S9(7)V99  COMP-3.
                 20  COP-PRLOAN-RESTRICTED     PIC X.
               15  COP-RESTRICT-ADJ            PIC S9(7)V99  COMP-3.
               15  COP-DUAL-LOAN-ADJ           PIC S9(7)V99  COMP-3.
               15  COP-PRIOR-CASHADV           PIC S9(7)V99  COMP-3.
               15  COP-RESTRICT-SERCHG         PIC S9(7)V99  COMP-3.
               15  COP-RESTRICT-HIGH-CASHADV   PIC S9(7)V99  COMP-3.
               15  COP-PRIOR-LOAN-DATE         PIC 9(8).
               15  COP-PRIOR-LOAN-DATE-X REDEFINES COP-PRIOR-LOAN-DATE.
                 20  CENTURY                   PIC 99.
                 20  YEAR                      PIC 99.
                 20  MONTH                     PIC 99.
                 20  DAY1                      PIC 99.
             10  COP-STATE-WORKER              PIC S9(7)V99 COMP-3.
             10  COP-FINCHG                    PIC 9(7)V99  COMP-3.
             10  COP-MFEE-RESTRICT             PIC S9(5)V99 COMP-3.
             10  COP-PRIOR-SUB-CLASS           PIC 99.
             10  FILLER                        PIC X(13).

           05  COP-SCREEN-FIELDS.
             10  COP-INPUT-FIELDS.
               15  COP-INPUT-DATES.
                 20  COP-LN-DT                 PIC 9(8).
                 20  COP-LN-DT-X    REDEFINES  COP-LN-DT.
                   25  CENTURY                 PIC 99.
                   25  YEAR                    PIC 99.
                   25  MONTH                   PIC 99.
                   25  DAY1                    PIC 99.
                 20  COP-IN-DT                 PIC 9(8).
                 20  COP-IN-DT-X    REDEFINES  COP-IN-DT.
                   25  CENTURY                 PIC 99.
                   25  YEAR                    PIC 99.
                   25  MONTH                   PIC 99.
                   25  DAY1                    PIC 99.
                 20  COP-FL-DT                 PIC 9(8).
                 20  COP-FL-DT-X    REDEFINES  COP-FL-DT.
                   25  CENTURY                 PIC 99.
                   25  YEAR                    PIC 99.
                   25  MONTH                   PIC 99.
                   25  DAY1                    PIC 99.
                 20  COP-PMT-DT                PIC 9(8).
                 20  COP-PMT-DT-X   REDEFINES  COP-PMT-DT.
                   25  CENTURY                 PIC 99.
                   25  YEAR                    PIC 99.
                   25  MONTH                   PIC 99.
                   25  DAY1                    PIC 99.
               15  COP-TERM                    PIC 9(4) COMP-3.
               15  COP-PURPOSE-CD              PIC XXX.
               15  COP-COLLTRL-CD              PIC XX.
               15  COP-SOURCE-CD               PIC XX.
               15  COP-PROCEEDS                PIC 9(7)V99 COMP-3.
               15  COP-PROCEEDS-SW             PIC X.
                 88  COP-PROCEEDS-ARE-INPUT    VALUE "I".
                 88  COP-PROCEEDS-ARE-CALCD    VALUE "C".
               15  COP-INS-CODES.
                 20  COP-INS-WHO               PIC X.
                   88  COP-INS-NO-ONE          VALUE
                                               "N".
                   88  COP-INS-SINGLE          VALUE
                                               "M" "S".
                   88  COP-INS-JOINT           VALUE
                                               "J".
                 20  COP-INS-TYPE              PIC X.
                   88  COP-INS-NONE            VALUE
                                               "N".
                   88  COP-INS-LL              VALUE "B" "F" "H" "S"
                                                     "L" "W" "A" "O"
                                                     "U" "2" "3" "4".
                   88  COP-INS-DL              VALUE "C" "G" "J" "T"
                                                     "M" "X" "I" "P"
                                                     "Y" "5" "6" "7".
                   88  COP-INS-AH              VALUE "F" "G" "D" "L"
                                                     "M" "K" "W" "X"
                                                     "V" "O" "P" "Q"
                                                     "0" "3" "4" "6"
                                                     "7" "9".
                   88  COP-INS-PP              VALUE "E" "H" "J" "K"
                                                     "L" "M" "U" "Y"
                                                     "Z" "3" "6" "9".
                   88  COP-INS-PP2             VALUE "R" "S" "T" "V"
                                                     "W" "X" "2" "4"
                                                     "5" "7" "8" "0".
                   88  COP-INS-O1              VALUE "A" "O" "U" "I"
                                                     "P" "Y" "Q" "Z"
                                                     "0" "1" "2" "3"
                                                     "4" "5" "6" "7"
                                                     "8" "9".
                 20  COP-INS-ELIG              PIC X.
               15  COP-POINTS                  PIC 99V99 COMP-3.
               15  COP-POINTS-SW               PIC X.
                 88  COP-POINTS-ARE-INPUT      VALUE "I".
                 88  COP-POINTS-ARE-CALCD      VALUE "C".
               15  COP-GL-AMOUNT-TOTAL         PIC 9(7)V99 COMP-3.
               15  COP-GL-AMOUNT-TOTAL-FINCHG  PIC 9(7)V99 COMP-3.
             10  COP-INTERMED-FIELDS.
               15  COP-RATES.
                 20  COP-OVR-RATE              PIC 9999V9999 COMP-3.
                 20  COP-OVR-RATE-SW           PIC X.
                   88  COP-OVR-RATE-IS-NULL    VALUE "C".
                   88  COP-OVR-RATE-IS-EFF     VALUE "E".
                   88  COP-OVR-RATE-IS-APR     VALUE "A".
                 20  COP-EFF-RATE              PIC 9999V9999 COMP-3.
                 20  COP-SIM-RATE              PIC 9999V9999 COMP-3.
                 20  COP-APR-RATE              PIC 9999V9999 COMP-3.
                 20  COP-END-RATE              PIC 9999V9999 COMP-3.
               15  COP-INSURANCES              OCCURS 8.
      *KEC 2015.0605-------------------------> INDEXED BY COPIX.
                 20  COP-INS-CO                PIC 99      COMP-3.
                 20  COP-INS-USE-SW            PIC X.
                   88  COP-INS-USE-IS-MEMO     VALUE "M".
                 20  COP-INS-TERM              PIC 9999    COMP-3.
                 20  COP-INS-TERM-SW           PIC X.
                   88  COP-TERM-CHANGED        VALUE "C".
                 20  COP-INS-COVERAGE          PIC 9(7)V99 COMP-3.
                 20  COP-COVERAGE-SW           PIC X.
                   88  COP-COVERAGE-CHANGED    VALUE "C".
                 20  COP-INS-PREMIUM           PIC 9(7)V99 COMP-3.
                 20  COP-PREMIUM-SW            PIC X.
                   88  COP-PREMIUM-CHANGED     VALUE "C".
                 20  COP-INS-SW                PIC X.
                   88  COP-INS-IS-INPUT        VALUE "I".
                   88  COP-INS-IS-CALCD        VALUE "C".
                 20  COP-INSFINCHG-SW          PIC X.
                   88  COP-INS-IS-FINCHG       VALUE "Y".
                 20  COP-INS-REJECT-TERM       PIC 9999     COMP-3.
                 20  COP-INS-TAX               PIC S9(7)V99 COMP-3.

             10  COP-OUTPUT-FIELDS.
               15  COP-SERVICE-CHG             PIC 9(7)V99 COMP-3.
               15  COP-SERVICE-CHG-SW          PIC X.
                 88  COP-SERVICE-CHG-IS-INPUT  VALUE "I".
                 88  COP-SERVICE-CHG-IS-CALCD  VALUE "C".
               15  COP-MAINT-FEES              PIC 9(7)V99 COMP-3.
               15  COP-MAINT-FEES-SW           PIC X.
                 88  COP-MAINT-FEES-ARE-INPUT  VALUE "I".
                 88  COP-MAINT-FEES-ARE-CALCD  VALUE "C".
               15  COP-1ST-PY-EXT              PIC 9(7)V99 COMP-3.
               15  COP-1ST-PY-EXT-SW           PIC X.
                 88  COP-1ST-PY-EXT-IS-INPUT  VALUE "I".
                 88  COP-1ST-PY-EXT-IS-CALCD  VALUE "C".
               15  COP-INT-CHARGE              PIC 9(7)V99 COMP-3.
               15  COP-AMT-FINANCD             PIC 9(7)V99 COMP-3.
               15  COP-APR-FINANCD             PIC 9(7)V99 COMP-3.
               15  COP-LOAN-AMOUNT             PIC 9(7)V99 COMP-3.
               15  COP-TOT-REPYBLE             PIC 9(7)V99 COMP-3.
               15  COP-ANTIC-EARNS             PIC 9(7)V99 COMP-3.
               15  COP-ODD-1ST-PMT             PIC 9(7)V99 COMP-3.
               15  COP-REG-PAYMENT             PIC 9(7)V99 COMP-3.
               15  COP-REG-PAYMENT-SW          PIC X.
                 88  COP-REG-PAYMENT-IS-INPUT  VALUE "I".
                 88  COP-REG-PAYMENT-IS-CALCD  VALUE "C".
      *-----------------------------------------------------------------
