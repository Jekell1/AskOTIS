      * COPYMEMBER: LIBLP/LPAPRZW
      **************************************************************************
      *    LPAPRZW
      *    APR CALCULATION WORKERS
      * REV:
      *  JTG 041690 ADDED APR-METHOD FOR U.S. RULE
      *                 VALUES: A - ACTUARIAL
      *                         U - U.S. RULE
      *  JTG 060590 ADDED APR-SPAY-FG (PACESETER) SINGLE PAYS
      *  JTG 120691 ADDED UNITPERIOD FIELDS
      *  JTG 121291 ADDED APR-LASTPYAMT APR-DISFRMLA
      *  JTG 052193 CHANGED APR-NOREGPY TO APR-ORGTERM
      *             REMOVED APR-NOODDPY
      *  JTG 100998 ALLOWED  APR-FINANCED, APR-REGPYAMT, APR-1STPYAMT
      *             AND APR-1ST-PAYMENT TO BE NEGATIVE TO HANDLE SMALL
      *             PAYMENT STREAMS THAT ARE ADJUSTED BY NON INTEREST CHARGEABLE
      *             SERVICE CHARGES/INSURANCE, CAUSING THEM TO GO NEGATIVE
      **************************************************************************
       77  APR-STAT                      PIC XX VALUE "11".
       01  APR-WORK-AREA.
           05  APR-ID-AREA.
               10  APR-C-PROG-ID         PIC XX VALUE "12".
           05  APR-OUTPUT-AREA.
               10  APR-APR               PIC S9(3)V9(4) COMP-3.
           05  APR-INPUT-AREA.
               10  APR-UNITPER-CD        PIC X.
               10  APR-UNITPER-FREQ      PIC 999        COMP-3.
               10  APR-FINANCED          PIC S9(8)V99   COMP-3.
               10  APR-REGPYAMT          PIC S9(6)V99   COMP-3.
               10  APR-1STPYAMT          PIC S9(6)V99   COMP-3.
               10  APR-LASTPYAMT         PIC S9(6)V99   COMP-3 VALUE 0.
               10  APR-ORGTERM           PIC 9(4)       COMP-3.
               10  APR-JUNK              PIC 9(4)       COMP-3.
               10  APR-INTDATE           PIC 9(8)       COMP-3.
               10  APR-1STPYDATE         PIC 9(8)       COMP-3.
               10  APR-DISFRMLA          PIC X          VALUE "A".
           05  APR-INTERMEDIATE-WORK-AREA.
               10  APR-1ST-PAYMENT       PIC S9(6)V99   COMP-3.
               10  APR-PVTERM            PIC 9(4)       COMP-3.
               10  APR-UNITPER-PER-YEAR  PIC 9(3)V9(7)  COMP-3.
               10  APR-FULL-UNITPER      PIC 9(3)       COMP-3.
               10  APR-FRACT-UNITPER     PIC V9(9)      COMP-3.
               10  APR-DELTA             PIC V99        COMP-3
                                                        VALUE .1.
               10  APR-METHOD            PIC X.
           05  APR-WORKERS.
               10  APR-DAYS-IN-UNITPER   PIC 9(3)       COMP-3.
               10  REUBEN-FRACTERM       PIC 9V9(9)     COMP-3.
               10  APR-EXTCHG            PIC 9(4)V99    COMP-3.
      * HERE FOR LONPZ3.C TESTING ON SINGLE PAYS:
       01  APR-SPAY-FG                   PIC X.
       01  APR-PAYFREQ                   PIC 9.

      * WORKERS FOR
      * VERYANT COBOL VERSION OR APRZ
       77 APRZ-WS-F0                  USAGE DOUBLE.
       77 APRZ-WS-F1                  USAGE DOUBLE.
       77 APRZ-WS-ADJ                 USAGE DOUBLE.
       77 APRZ-WS-R                   USAGE DOUBLE.
       77 APRZ-WS-CNTR                PIC 9(3).

       01 APRZ-APR-FIN-VARS.
          05 APRZ-WS-APRW             USAGE DOUBLE.
          05 APRZ-WS-APRW-DIV         USAGE DOUBLE.
          05 APRZ-WS-AF-R             USAGE DOUBLE.
          05 APRZ-WS-I                USAGE DOUBLE.
          05 APRZ-WS-I1               USAGE DOUBLE.
          05 APRZ-WS-REG-ANN          USAGE DOUBLE.
          05 APRZ-WS-LAST-FAC         USAGE DOUBLE.

       01 APRZ-WS-VARS.
          05 APRZ-WS-APR              USAGE DOUBLE.
          05 APRZ-WS-FINANCED         USAGE DOUBLE.
          05 APRZ-WS-REGPYAMT         USAGE DOUBLE.
          05 APRZ-WS-FSTPYAMT         USAGE DOUBLE.
          05 APRZ-WS-LASTPYAMT        USAGE DOUBLE.
          05 APRZ-WS-REG-TERM         USAGE DOUBLE.
          05 APRZ-WS-LAST-TERM        USAGE DOUBLE.
          05 APRZ-WS-UNITPER-PER-YEAR USAGE DOUBLE.
          05 APRZ-WS-FULL-UNITPER     USAGE DOUBLE.
          05 APRZ-WS-FRACT-UNITPER    USAGE DOUBLE.
          05 APRZ-WS-DELTA            USAGE DOUBLE.
          05 APRZ-WS-METHOD           PIC X.
