      * COPYMEMBER: LIBLP/LFPAPW
      *-------------------------------------------------------------------
      *    PAYMENT DISTRIBUTION AMORTIZED (Z) INTEREST WORKERS
      *    (LFPAPW)
      *
      * REV:
      *-------------------------------------------------------------------

       01  LFPAP-FIELDS.
      * IN:
           03  LFPAP-TODAY                  PIC 9(8)     COMP.
           03  LFPAP-TRCD                   PIC XX.
           03  LFPAP-TRAMT                  PIC S9(7)V99 COMP.

           03  LFPAP-INTBAL                 PIC S9(7)V99 COMP.
           03  LFPAP-CURBAL                 PIC S9(7)V99 COMP.
           03  LFPAP-LCBAL                  PIC S9(7)V99 COMP.
           03  LFPAP-OTHBAL                 PIC S9(7)V99 COMP.

           03  LFPAP-EXTRA-PRIN-LFP         PIC S9(7)V99 COMP.
           03  LFPAP-REMAIN-INT-SINCE-LFP   PIC S9(7)V99 COMP.
           03  LFPAP-REMAIN-PRIN-SINCE-LFP  PIC S9(7)V99 COMP.
      *------------------------------------------------------------------------
      * IN/OUT:
           03  LFPAP-TOTPAYMNTD             PIC S9(7)V99 COMP.
           03  LFPAP-TOTEXCPAYMNTD          PIC S9(7)V99 COMP.

      * OUT:
           03  LFPAP-APINT                  PIC S9(7)V99 COMP.
           03  LFPAP-APCUR                  PIC S9(7)V99 COMP.
           03  LFPAP-APLC                   PIC S9(7)V99 COMP.
           03  LFPAP-APOTH                  PIC S9(7)V99 COMP.

           03  LFPAP-APCURINT               PIC S9(7)V99 COMP.
           03  LFPAP-APCURPRIN              PIC S9(7)V99 COMP.

           03  LFPAP-APPDUEINT              PIC S9(7)V99 COMP.
           03  LFPAP-APPDUEPRIN             PIC S9(7)V99 COMP.

           03  LFPAP-EXTRA-PRIN             PIC S9(7)V99 COMP.
           03  LFPAP-INDU-INTEREST          PIC S9(7)V99 COMP.

      *------------------------------------------------------------------------
      * WORKERS:
           03  LFPAP-SAVE-LN-CURBAL         PIC S9(7)V99 COMP.
           03  LFPAP-SAVE-LN-INTPDTH-DATE   PIC 9(8)     COMP.
           03  LFPAP-PERIOD                 PIC X.
