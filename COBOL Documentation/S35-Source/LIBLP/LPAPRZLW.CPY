      * COPYMEMBER: LIBLP/LPAPRZLW
      ***************************************************************
      *    LPAPRZLW
      *    APR CALCULATION WORKERS FOR CALCZL.C ONLY
      *           (DIRECT LOAN CALCULATOR)
      * REV:
      *  JTG 041690 ADDED APR-METHOD FOR U.S. RULE
      *                      VALUES: A - ACTUARIAL
      *                              U - U.S. RULE
      *  JTG 060590 ADDED APR-SPAY-FG   (PACESETER) SINGLE PAYS
      *  JTG 091391 ADDED APR-SPAY-DAYS (PACESETER) SINGLE PAYS
      *  MJD 130222 ADDED WORKERS FOR VERYANTS APRL COBOL ROUTINE THAT
      *             REPLACES PFS "C" ROUTINE.
      *  BAH 161026 CHNAGED INTDATE AND 1STPYDATE TO 9(8)
      ***************************************************************
       77  APR-STAT                   PIC XX VALUE "11".
       01  APR-WORK-AREA.
           05  APR-ID-AREA.
               10  APR-C-PROG-ID      PIC XX VALUE "16".
           05  APR-OUTPUT-AREA.
               10  APR-APR            PIC S9(3)V9(4)  COMP-3.
           05  APR-INPUT-AREA.
               10  APR-FINANCED       PIC 9(8)V99     COMP-3.
               10  APR-REGPYAMT       PIC 9(6)V99     COMP-3.
               10  APR-1STPYAMT       PIC 9(6)V99     COMP-3.
               10  APR-NOREGPY        PIC 9(4)        COMP-3.
               10  APR-NOODDPY        PIC 9(4)        COMP-3.
               10  APR-INTDATE        PIC 9(8)        COMP-3.
               10  APR-1STPYDATE      PIC 9(8)        COMP-3.
               10  APR-PAYFREQ        PIC 99V99       COMP-3.
               10  APR-METHOD         PIC X.
           05  APR-INTERMEDIATE-WORK-AREA.
               10  APR-1ST-PAYMENT    PIC 9(6)V99     COMP-3.
               10  APR-PVTERM         PIC 9(4)        COMP-3.
               10  APR-ELAPSED-MONTHS PIC 9(4)        COMP-3.
               10  APR-FRACTERM       PIC V9(9)       COMP-3.
               10  APR-DELTA          PIC V99         COMP-3
                                                      VALUE .1.
           05  APR-WORKERS.
               10  APR-EXTCHG         PIC 9(4)V99     COMP-3.
               10  APR-SPAY-FG        PIC X           VALUE " ".
               10  APR-SPAY-DAYS      PIC 9(4)        COMP-3.
               10  APR-REUBEN         PIC 9V9(9)      COMP-3.

      * WORKERS FOR VERYANT APRL COBOL ROUTINE.
       77 APRL-F0                USAGE DOUBLE.
       77 APRL-F1                USAGE DOUBLE.
       77 APRL-ADJ               USAGE DOUBLE.
       77 APRL-R                 USAGE DOUBLE.
       77 APRL-CNTR                 PIC 9(5).

       01 APR-FIN-VARS.
          05 APRL-AF-R           USAGE DOUBLE.
          05 APRL-I              USAGE DOUBLE.
          05 APRL-I1             USAGE DOUBLE.
          05 APRL-ANN            USAGE DOUBLE.
          05 APRL-APRLW          USAGE DOUBLE.
          05 APRL-DIV-BY         USAGE DOUBLE.

       01 APRL-VARS.
          05 APRL-APRL           USAGE DOUBLE.
          05 APRL-FINANCED       USAGE DOUBLE.
          05 APRL-REGPYAMT       USAGE DOUBLE.
          05 APRL-PAYFREQ        USAGE DOUBLE.
          05 APRL-FST-PAYMENT    USAGE DOUBLE.
          05 APRL-PVTERM         USAGE DOUBLE.
          05 APRL-ELAPSED-MONTHS USAGE DOUBLE.
          05 APRL-FRACTERM       USAGE DOUBLE.
          05 APRL-DELTA          USAGE DOUBLE.
          05 APRL-METHOD         PIC X.
