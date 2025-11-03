      * COPYMEMBER: LIBWI/REALPYW
      *******************************************************************
      *    MORTGAGE REAL ESTATE PAYMENT WINDOW WORKERS
      * REV:
      *******************************************************************
      *---------------------------------------------------------------
      *    DISPLAY REQUESTED DISPLAY PER: SUBX
      *        1 -  PAYMENT DEFAULT DISTRIBUTION
      *        2 -  PAYMENT APPLIED DISTRIBUTION
      *---------------------------------------------------------------
       01  REALPYW-FIELDS.
           03  REALPYW-PAYDATE              PIC 9(8)     COMP.
           03  REALPYW-TRCD                 PIC XX.
           03  REALPYW-REFNO                PIC X(5).
           03  REALPYW-TRAMT                PIC S9(7)V99 COMP.

           03  REALPYW-TABLE                OCCURS 2.
               05  REALPYW-POST-REGPY       PIC S9(7)V99 COMP.
               05  REALPYW-POST-EXTRA-PRIN  PIC S9(7)V99 COMP.
               05  REALPYW-POST-OTH         PIC S9(7)V99 COMP.
               05  REALPYW-POST-LCHG        PIC S9(7)V99 COMP.

               05  REALPYW-PDUE-APINT       PIC S9(7)V99 COMP.
               05  REALPYW-PDUE-APPRIN      PIC S9(7)V99 COMP.
               05  REALPYW-CUR-APINT        PIC S9(7)V99 COMP.
               05  REALPYW-CUR-APPRIN       PIC S9(7)V99 COMP.
               05  REALPYW-AP-EXTRA-PRIN    PIC S9(7)V99 COMP.
               05  REALPYW-APOTH            PIC S9(7)V99 COMP.
               05  REALPYW-APLC             PIC S9(7)V99 COMP.

               05  REALPYW-OTHBAL           PIC S9(7)V99 COMP.
               05  REALPYW-LCBAL            PIC S9(7)V99 COMP.
               05  REALPYW-INTBAL           PIC S9(7)V99 COMP.
               05  REALPYW-CURBAL           PIC S9(7)V99 COMP.

               05  REALPYW-INT-SINCE-LFP    PIC S9(7)V99 COMP.
               05  REALPYW-PRIN-SINCE-LFP   PIC S9(7)V99 COMP.

