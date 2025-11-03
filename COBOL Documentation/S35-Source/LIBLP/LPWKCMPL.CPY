      * COPYMEMBER: LIBLP/LPWKCMPL
      **************************************************************************
      *                   LPWKCMPL
      *    INTERMEDIATE DIRECT LOAN CALCULATION AREA
      * REV:
      *  JTG 062195 EXPANDED FOR 10 INSURANCES
      *  JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  JTG 072397 ADDED CMP-INSFINCHG & CMP-GL-TOTAL-FINCHG
      *                    AND CMP-APR-FINANCD
      *  JTG 000112 ADDED CMP-INS-REJECT-TERM RE: SP-INSREJECT-FG OPTION 'C',
      *             WHICH WILL CAUSE WRITTING NO OF MONTHS INSURANCE NOT IN
      *             VIOLATION, WISCONSIN #1032
      *  JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99    LENDMARK PR#J021
      * JTG 020201 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS   WORLD #288
      *            ADDED CMP-INS-TAX               PIC S9(7)V99 COMP-3.
      *
      * KEC 2015.0604 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED THE USE OF 'INDEXED BY' ON OCCURS SO TO 
      *         PROPERLY WORK FOR USE WITH MICROFOCUS.
      *         CMPIX DEFINED IN LIBLP/LPWKINDEX.CPY
      * BAH 170413 8 DIGIT DATES, PD0006
      *
      **************************************************************************
       01  CMP-CALCULATION-INTERMED-AREA.
           05  CMP-COUNTERS.
               10  CMP-LOOP-COUNT            PIC S9(5) COMP.
               10  CMP-LOOP-COUNT-MAX        PIC S9(5) COMP VALUE +72.
           05  CMP-WORKING-AMOUNTS.
               10  CMP-TOTAL-NOTE            PIC S9(7)V99 COMP-3.
               10  CMP-INT-CHARGEABLE        PIC S9(7)V99 COMP-3.
               10  CMP-NEW-LOAN-PROCEEDS     PIC S9(7)V99 COMP-3.
               10  CMP-1ST-PMT-MTHS          PIC 999 COMP-3.
               10  CMP-1ST-PMT-DAYS          PIC 999 COMP-3.
               10  CMP-1ST-PMT-EXTENSION     PIC 999 COMP-3.
               10  CMP-ETBL                  PIC 9.
           05  CMP-SCREEN-FIELDS.
               10  CMP-INPUT-FIELDS.
                   15  CMP-INPUT-DATES.
                       20  CMP-LN-DT         PIC 9(8).
                       20  CMP-LN-DT-X    REDEFINES  CMP-LN-DT.
                           25  CENTURY       PIC 99.
                           25  YEAR          PIC 99.
                           25  MONTH         PIC 99.
                           25  DAY1          PIC 99.
                       20  CMP-IN-DT         PIC 9(8).
                       20  CMP-IN-DT-X    REDEFINES  CMP-IN-DT.
                           25  CENTURY       PIC 99.
                           25  YEAR          PIC 99.
                           25  MONTH         PIC 99.
                           25  DAY1          PIC 99.
                       20  CMP-FL-DT         PIC 9(8).
                       20  CMP-FL-DT-X    REDEFINES  CMP-FL-DT.
                           25  CENTURY       PIC 99.
                           25  YEAR          PIC 99.
                           25  MONTH         PIC 99.
                           25  DAY1          PIC 99.
                       20  CMP-PMT-DT        PIC 9(8).
                       20  CMP-PMT-DT-X   REDEFINES  CMP-PMT-DT.
                           25  CENTURY       PIC 99.
                           25  YEAR          PIC 99.
                           25  MONTH         PIC 99.
                           25  DAY1          PIC 99.
                   15  CMP-TERM              PIC 9(4) COMP-3.
                   15  CMP-PURPOSE-CD        PIC XXX.
                   15  CMP-COLLTRL-CD        PIC XX.
                   15  CMP-SOURCE-CD         PIC XX.
                   15  CMP-PROCEEDS          PIC 9(7)V99 COMP-3.
                   15  CMP-PROCEEDS-SW       PIC X.
                       88  CMP-PROCEEDS-ARE-INPUT    VALUE "I".
                       88  CMP-PROCEEDS-ARE-CALCD    VALUE "C".
                   15  CMP-INS-CODES.
                       20  CMP-INS-WHO           PIC X.
                       20  CMP-INS-TYPE          PIC X.
                       20  CMP-INS-ELIG          PIC X.
                   15  CMP-POINTS                PIC 99V99 COMP-3.
                   15  CMP-POINTS-SW             PIC X.
                       88  CMP-POINTS-ARE-INPUT      VALUE "I".
                       88  CMP-POINTS-ARE-CALCD      VALUE "C".
                   15  CMP-GL-AMOUNT-TOTAL        PIC 9(7)V99 COMP-3.
                   15  CMP-GL-AMOUNT-TOTAL-FINCHG PIC 9(7)V99 COMP-3.
               10  CMP-INTERMED-FIELDS.
                   15  CMP-RATES.
                       20  CMP-OVR-RATE         PIC 9999V9999 COMP-3.
                       20  CMP-OVR-RATE-SW      PIC X.
                           88  CMP-OVR-RATE-IS-NULL  VALUE "C".
                           88  CMP-OVR-RATE-IS-EFF   VALUE "E".
                           88  CMP-OVR-RATE-IS-APR   VALUE "A".
                       20  CMP-EFF-RATE         PIC 9999V9999 COMP-3.
                       20  CMP-SIM-RATE         PIC 9999V9999 COMP-3.
                       20  CMP-APR-RATE         PIC 9999V9999 COMP-3.
                       20  CMP-END-RATE         PIC 9999V9999 COMP-3.
                   15  CMP-INSURANCES          OCCURS 8.
      *KEC 2015.0604 ------------------------> INDEXED BY CMPIX.
                       20  CMP-INS-CO           PIC 99      COMP-3.
                       20  CMP-INS-USE-SW       PIC X.
                           88  CMP-INS-USE-IS-MEMO  VALUE "M".
                       20  CMP-INS-TERM         PIC 9999    COMP-3.
                       20  CMP-TERM-SW          PIC X.
                           88  CMP-TERM-CHANGED     VALUE "C".
                       20  CMP-INS-COVERAGE     PIC 9(7)V99 COMP-3.
                       20  CMP-COVERAGE-SW      PIC X.
                           88  CMP-COVERAGE-CHANGED VALUE "C".
                       20  CMP-INS-PREMIUM      PIC 9(7)V99 COMP-3.
                       20  CMP-PREMIUM-SW       PIC X.
                           88  CMP-PREMIUM-CHANGED  VALUE "C".
                       20  CMP-INS-SW           PIC X.
                           88  CMP-INS-IS-INPUT     VALUE "I".
                           88  CMP-INS-IS-CALCD     VALUE "C".
                       20  CMP-INSFINCHG-SW     PIC X.
                           88  CMP-INS-IS-FINCHG    VALUE "Y".
                       20  CMP-INS-REJECT-TERM  PIC 9999     COMP-3.
                       20  CMP-INS-TAX          PIC S9(7)V99 COMP-3.
               10  CMP-OUTPUT-FIELDS.
                   15  CMP-SERVICE-CHG         PIC 9(7)V99 COMP-3.
                   15  CMP-SERVICE-CHG-SW      PIC X.
                 88  CMP-SERVICE-CHG-IS-INPUT  VALUE "I".
                 88  CMP-SERVICE-CHG-IS-CALCD  VALUE "C".
               15  CMP-MAINT-FEES              PIC 9(7)V99 COMP-3.
               15  CMP-MAINT-FEES-SW           PIC X.
                 88  CMP-MAINT-FEES-ARE-INPUT  VALUE "I".
                 88  CMP-MAINT-FEES-ARE-CALCD  VALUE "C".
               15  CMP-1ST-PY-EXT              PIC 9(7)V99 COMP-3.
               15  CMP-1ST-PY-EXT-SW           PIC X.
                 88  CMP-1ST-PY-EXT-IS-INPUT  VALUE "I".
                 88  CMP-1ST-PY-EXT-IS-CALCD  VALUE "C".
               15  CMP-INT-CHARGE              PIC 9(7)V99 COMP-3.
               15  CMP-AMT-FINANCD             PIC 9(7)V99 COMP-3.
               15  CMP-APR-FINANCD             PIC 9(7)V99 COMP-3.
               15  CMP-LOAN-AMOUNT             PIC 9(7)V99 COMP-3.
               15  CMP-TOT-REPYBLE             PIC 9(7)V99 COMP-3.
               15  CMP-ANTIC-EARNS             PIC 9(7)V99 COMP-3.
               15  CMP-ODD-1ST-PMT             PIC 9(7)V99 COMP-3.
               15  CMP-REG-PAYMENT             PIC 9(7)V99 COMP-3.
               15  CMP-REG-PAYMENT-SW          PIC X.
                 88  CMP-REG-PAYMENT-IS-INPUT  VALUE "I".
                 88  CMP-REG-PAYMENT-IS-CALCD  VALUE "C".
      *-----------------------------------------------------------------
