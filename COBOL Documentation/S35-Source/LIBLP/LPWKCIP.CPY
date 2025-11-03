      * COPYMEMBER: LIBLP/LPWKCIP
      **************************************************************************
      *   LPWKCIP
      *
      *   LONPB0, CALCZ3, ZONPC0, BLQUOT, SFQUOT
      *
      *  THIS IS THE AREA WHERE DATA IS PASSED TO FROM THE
      *    CALCULATION PROGRAM.
      *
      * REV:
      *  JTG 110788 INCREASED SIZE OF PRIOR LOAN FIELDS
      *  JTG 062089     "       "       "     "    "
      *  JTG 081889 CIP-PRPAID-AHEAD FOR (GA)
      *             CIP-RESTRICT-ADJ
      *  JTG 101889 CIP-STATE-WORKER FOR (GA)
      *  JTG 102489 ADDED CIP-PRLOAN-RESTRICTED (GA)
      *  JTG 011990 ADDED CIP-OTINS-COV-ERR CIP-PPINS-ERR (GA)
      *  JTG 051790 ADDED CIP-DUAL-LOAN-ADJ  (GA)
      *             CIP-SMALLOAN, CIP-SERVACCT, CIP-SALEFIN
      *  JTG 030292 ADDED CIP-MATURITY-DT (FOR UPER)
      *  JTG 012493 MADE BALLOONS SIGNED
      *  JTG 041394 ADDED CIP-FEE-TABLE (FL) (BUY HERE PAY HERE)
      *                    AND SELLPR,DOWNPY,PERDOWN INFO
      *  JTG 062195 EXPANDED FOR 10 INSURANCES
      *  JTG 062395 ADDED CODES FOR CIP-INS-O1
      *  JTG 031396 MAKED CIP-FINCHG SIGNED
      *  JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  JTG 072397 ADDED CIP-INSFINCHG & CIP-GL-TOTAL-FINCHG
      *  JTG 082097 ADDED FOR WORLD A90, CIP-PRIOR-CASHADV,
      *                   CIP-RESTRICT-SERCHG, CIP-RESTRICT-HIGH-CASHADV
      *  JTG 020498 ADDED CIP-CAN-LOAN-NO, RE: LOGIC TO FORCE ONE CALENDAR
      *             MONTH TO FIRST PAYMENT
      *  JTG 000112 ADDED CIP-INS-REJECT-TERM RE: SP-INSREJECT-FG OPTION 'C',
      *             WHICH WILL CAUSE WRITTING NO OF MONTHS INSURANCE NOT IN
      *             VIOLATION, WISCONSIN #1032
      *  JTG 000316 CHANGED CIP-UNKNOWN-ERROR VALUE " " TO VALUE " " "%"
      *             CALCULATIONS VIA: POINTS ENTERED CAUSE SERVICE CHARGE TO
      *             EXCEED SPR MAX DURING INTERMIM CALCULATION AND SET
      *             MAXIMUM SCHG INPUTED WESTERN CREDIT PL: #243
      *  JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99    LENDMARK PR#J021
      *             ADDED 88  CIP-LOAN-VALUES-OVER-MAX    VALUE "V".
      *  JTG 020215 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *             ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS  WORLD #288
      *             ADDED CIP-INS-TAX               PIC S9(7)V99 COMP.
      *  BAH 020806 ADDED CIP-MFEE-RESTRICT AND SOME FILLER, PR# 1867 REGWAN
      *  BAH 030618 ADDED CIP-PRIOR-SUB-CLASS FOR SCFRMLA 46, PR#2101 LENDMARK
      *
      * KEC 2015.0605 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED THE USE OF 'INDEXED BY' ON OCCURS
      *         FOR USE WITH MICROFOCUS.
      *         CIPIX DEFINED IN LIBLP/LPWKINDEX.CPY
      *  BAH 160824 CHANGED CIP-PRIOR-LOAN-CLASS FROM 9(2) TO 9(3)
      *  BAH 170406 CHANGE 8 DIGIT DATES, PD0006
      **************************************************************************
       01  CIP-CALCULATION-INPUT-AREA.
           05 CIP-CONTROL-FIELDS.
             10  CIP-NO-OF-TIMES-CALLED        PIC S999 COMP.
             10  CIP-RETURN-CODE               PIC X.
               88  CIP-CALCULATION-OK          VALUE "A".
               88  CIP-BAD-INPUT               VALUE "B".
               88  CIP-WONT-CONVERGE           VALUE "C".
               88  CIP-PAYMENT-ERR             VALUE "D".
               88  CIP-1ST-PAYMENT-ERR         VALUE "E".
               88  CIP-LHINS-ERR               VALUE "F".
               88  CIP-AHINS-ERR               VALUE "G".
               88  CIP-PPINS-ERR               VALUE "H".
               88  CIP-MAINT-FEES-ERR          VALUE "I".
               88  CIP-SERVICE-CHARGE-ERR      VALUE "J".
               88  CIP-ANNUITY-FACTOR-ERR      VALUE "K".
               88  CIP-AMOUNT-FINACED-ERR      VALUE "L".
               88  CIP-FINCHG-ERR              VALUE "M".
               88  CIP-OVR-RATE-ERR            VALUE "N".
               88  CIP-APR-ERR                 VALUE "O".
               88  CIP-MANY-SIMPLE-ERR         VALUE "P".
               88  CIP-FTINS-ERR               VALUE "Q".
               88  CIP-OTINS-COV-ERR           VALUE "R".
               88  CIP-BALLOON-ERR             VALUE "S".
               88  CIP-PROCEEDS-ERR            VALUE "T".
               88  CIP-LOAN-VALUES-OVER-MAX    VALUE "V".
               88  CIP-UNKNOWN-ERROR           VALUE " " "%".
             10  CIP-COUNTERS.
               15  CIP-LOOP-COUNT              PIC S9(5) COMP.
               15  CIP-LOOP-COUNT-MAX          PIC S9(5) COMP.
             10  CIP-BALLOON-LAST-FG            PIC X.
               88  CIP-YES-BALLOON              VALUE "B".
               88  CIP-NO-BALLOON               VALUE "L".
             10  CIP-WORKING-AMOUNTS.
               15  CIP-TOTAL-NOTE              PIC S9(7)V99 COMP.
               15  CIP-INT-CHARGEABLE          PIC S9(7)V99 COMP.
               15  CIP-NEW-LOAN-PROCEEDS       PIC S9(7)V99 COMP.
               15  CIP-1ST-PMT-MTHS            PIC 999 COMP.
               15  CIP-1ST-PMT-DAYS            PIC 999 COMP.
               15  CIP-1ST-PMT-EXTENSION       PIC 999 COMP.
               15  CIP-ETBL                    PIC 9.
           05 CIP-OTHER-FILE-FIELDS.
             10 CIP-MATURITY-DATE              PIC 9(8)     COMP.
             10 CIP-LOANTYPE                   PIC X.
               88 CIP-INTEREST-BEARING         VALUE "I".
               88 CIP-PRECALCULATED            VALUE "P".
             10 CIP-SMALLOAN                   PIC X.
             10 CIP-SALEFIN                    PIC X.
             10 CIP-SERVACCT                   PIC X.
             10 CIP-CAN-LOAN-NO                PIC XX.
             10 CIP-UNITPER-CD                 PIC X.
             10 CIP-UNITPER-FREQ               PIC 999.
             10 CIP-PRIOR-LOAN-INFO.
               15  CIP-PRIOR-LOAN-STATE        PIC XX.
               15  CIP-PRIOR-LOAN-CLASS        PIC 9(3).
               15  CIP-PRIOR-LOAN.
                 20  CIP-PRBAL                 PIC S9(7)V99  COMP.
                 20  CIP-PRINTDUE              PIC S9(7)V99  COMP.
                 20  CIP-PRFINCHGRB            PIC S9(7)V99  COMP.
                 20  CIP-PRINSRB               PIC S9(7)V99  COMP.
                 20  CIP-PRPAID-AHEAD          PIC S9(7)V99  COMP.
                 20  CIP-PRLOAN-RESTRICTED     PIC X.
               15  CIP-RESTRICT-ADJ            PIC S9(7)V99  COMP.
               15  CIP-DUAL-LOAN-ADJ           PIC S9(7)V99  COMP.
               15  CIP-PRIOR-CASHADV           PIC S9(7)V99  COMP.
               15  CIP-RESTRICT-SERCHG         PIC S9(7)V99  COMP.
               15  CIP-RESTRICT-HIGH-CASHADV   PIC S9(7)V99  COMP.
               15  CIP-PRIOR-LOAN-DATE         PIC 9(8).
             10  CIP-STATE-WORKER              PIC S9(7)V99  COMP.
             10  CIP-FINCHG                    PIC S9(7)V99  COMP.
             10  CIP-MFEE-RESTRICT             PIC S9(5)V99  COMP.
             10  CIP-PRIOR-SUB-CLASS           PIC 99.
             10  FILLER                        PIC X(13).

           05  CIP-SCREEN-FIELDS.
             10  CIP-INPUT-FIELDS.
               15  CIP-INPUT-DATES.
                 20  CIP-LOANDATE              PIC 9(8).
                 20  CIP-INTDATE               PIC 9(8).
                 20  CIP-FILEDATE              PIC 9(8).
                 20  CIP-1STPYDATE             PIC 9(8).
               15  CIP-TERM                    PIC 9(4) COMP.
               15  CIP-PURPOSE-CD              PIC XXX.
               15  CIP-COLLTRL-CD              PIC XX.
               15  CIP-SOURCE-CD               PIC XX.
               15  CIP-PROCEEDS                PIC 9(7)V99 COMP.
               15  CIP-PROCEEDS-SW             PIC X.
                 88  CIP-PROCEEDS-ARE-INPUT    VALUE "I".
                 88  CIP-PROCEEDS-ARE-CALCD    VALUE "C".
               15  CIP-BALLOON                 PIC S9(7)V99 COMP.
               15  CIP-BALLOON-SW              PIC X.
                 88  CIP-BALLOON-IS-INPUT      VALUE "I".
                 88  CIP-BALLOON-IS-CALCD      VALUE "C".

               15  CIP-SELLPR-FG               PIC X.
               15  CIP-DOWNPY-FG               PIC X.
               15  CIP-PERDOWN-FG              PIC X.

               15  CIP-INS-CODES.
                 20  CIP-INS-WHO               PIC X.
                   88  CIP-INS-NO-ONE          VALUE
                                               "N".
                   88  CIP-INS-SINGLE          VALUE
                                               "M" "S".
                   88  CIP-INS-JOINT           VALUE
                                               "J".
                 20  CIP-INS-TYPE              PIC X.
                   88  CIP-INS-NONE            VALUE
                                               "N".
                   88  CIP-INS-LL              VALUE "B" "F" "H" "S"
                                                     "L" "W" "A" "O"
                                                     "U" "2" "3" "4".
                   88  CIP-INS-DL              VALUE "C" "G" "J" "T"
                                                     "M" "X" "I" "P"
                                                     "Y" "5" "6" "7".
                   88  CIP-INS-AH              VALUE "F" "G" "D" "L"
                                                     "M" "K" "W" "X"
                                                     "V" "O" "P" "Q"
                                                     "0" "3" "4" "6"
                                                     "7" "9".
                   88  CIP-INS-PP              VALUE "E" "H" "J" "K"
                                                     "L" "M" "U" "Y"
                                                     "Z" "3" "6" "9".
                   88  CIP-INS-PP2             VALUE "R" "S" "T" "V"
                                                     "W" "X" "2" "4"
                                                     "5" "7" "8" "0".
                   88  CIP-INS-O1              VALUE "A" "O" "U" "I"
                                                     "P" "Y" "Q" "Z"
                                                     "0" "1" "2" "3"
                                                     "4" "5" "6" "7"
                                                     "8" "9".
                 20  CIP-INS-ELIG              PIC X.
               15  CIP-POINTS                  PIC 99V99 COMP.
               15  CIP-POINTS-SW               PIC X.
                 88  CIP-POINTS-ARE-INPUT      VALUE "I".
                 88  CIP-POINTS-ARE-CALCD      VALUE "C".
               15  CIP-GL-AMOUNT-TOTAL         PIC 9(7)V99 COMP.
               15  CIP-GL-AMOUNT-TOTAL-FINCHG  PIC 9(7)V99 COMP.
             10  CIP-INTERMED-FIELDS.
               15  CIP-RATES.
                 20  CIP-OVR-RATE              PIC 9999V9999 COMP.
                 20  CIP-OVR-RATE-SW           PIC X.
                   88  CIP-OVR-RATE-IS-NULL    VALUE "C".
                   88  CIP-OVR-RATE-IS-EFF     VALUE "E".
                   88  CIP-OVR-RATE-IS-APR     VALUE "A".
                 20  CIP-EFF-RATE              PIC 9999V9999 COMP.
                 20  CIP-SIM-RATE              PIC 9999V9999 COMP.
                 20  CIP-APR-RATE              PIC 9999V9999 COMP.
                 20  CIP-END-RATE              PIC 9999V9999 COMP.
               15  CIP-INSURANCES              OCCURS 8.
      *KEC 2015.0605-------------------------> INDEXED BY CIPIX.
                 20  CIP-INS-CO                PIC 99 COMP.
                 20  CIP-INS-USE-SW            PIC X.
                   88  CIP-INS-USE-IS-MEMO     VALUE "M".
                 20  CIP-INS-TERM              PIC S9(4) COMP.
                 20  CIP-INS-TERM-SW           PIC X.
                   88  CIP-INS-TERM-CHANGED    VALUE "C".
                 20  CIP-INS-COVERAGE          PIC S9(7)V99 COMP.
                 20  CIP-COVERAGE-SW           PIC X.
                   88  CIP-COVERAGE-CHANGED    VALUE "C".
                 20  CIP-INS-PREMIUM           PIC S9(7)V99 COMP.
                 20  CIP-PREMIUM-SW            PIC X.
                   88  CIP-PREMIUM-CHANGED     VALUE "C".
                 20  CIP-INS-SW                PIC X.
                   88  CIP-INS-IS-INPUT        VALUE "I".
                   88  CIP-INS-IS-CALCD        VALUE "C".
                 20  CIP-INSFINCHG-SW          PIC X.
                   88  CIP-INS-IS-FINCHG       VALUE "Y".
                 20  CIP-INS-REJECT-TERM       PIC S9(4)    COMP.
                 20  CIP-INS-TAX               PIC S9(7)V99 COMP.

             10  CIP-OUTPUT-FIELDS.
               15  CIP-SELLPR                  PIC S9(7)V99 COMP.
               15  CIP-DOWNPY                  PIC S9(7)V99 COMP.
               15  CIP-PERDOWN                 PIC S999V99  COMP.
               15  CIP-FEE-TABLE               OCCURS 2.
                   17  CIP-FEE-CHG             PIC S9(7)V99 COMP.
                   17  CIP-FEE-CHG-SW          PIC X.
                     88  CIP-FEE-CHG-IS-INPUT  VALUE "I".
                     88  CIP-FEE-CHG-IS-CALCD  VALUE "C".
               15  CIP-SERVICE-CHG             PIC S9(7)V99 COMP.
               15  CIP-SERVICE-CHG-SW          PIC X.
                 88  CIP-SERVICE-CHG-IS-INPUT  VALUE "I".
                 88  CIP-SERVICE-CHG-IS-CALCD  VALUE "C".
               15  CIP-MAINT-FEES              PIC S9(7)V99 COMP.
               15  CIP-MAINT-FEES-SW           PIC X.
                 88  CIP-MAINT-FEES-ARE-INPUT  VALUE "I".
                 88  CIP-MAINT-FEES-ARE-CALCD  VALUE "C".
               15  CIP-1ST-PY-EXT              PIC S9(7)V99 COMP.
               15  CIP-1ST-PY-EXT-SW           PIC X.
                 88  CIP-1ST-PY-EXT-IS-INPUT   VALUE "I".
                 88  CIP-1ST-PY-EXT-IS-CALCD   VALUE "C".
               15  CIP-INT-CHARGE              PIC S9(7)V99  COMP.
               15  CIP-AMT-FINANCD             PIC S9(7)V99  COMP.
               15  CIP-APR-FINANCD             PIC S9(7)V99  COMP.
               15  CIP-LOAN-AMOUNT             PIC S9(7)V99  COMP.
               15  CIP-TOT-REPYBLE             PIC S9(7)V99  COMP.
               15  CIP-ANTIC-EARNS             PIC S9(7)V99  COMP.
               15  CIP-ODD-1ST-PMT             PIC S9(7)V99  COMP.
               15  CIP-REGPYAMT                PIC S9(7)V99  COMP.
               15  CIP-REGPYAMT-SW             PIC X.
                 88  CIP-REGPYAMT-IS-INPUT     VALUE "I".
                 88  CIP-REGPYAMT-IS-CALCD     VALUE "C".
      *-----------------------------------------------------------------
