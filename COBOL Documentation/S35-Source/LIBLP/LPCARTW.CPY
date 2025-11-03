      * COPYMEMBER: LIBLP/LPCARTW
      ***************************************************************
      *    LPCARTW
      *    CART CALCULATION WORKERS
      * REV:
      *  JTG 042093 MADE CART-1STFAC 9V9(X) AND NOT V9(X)
      *  JTG 042197 MADE CHANGES TO HANDLE NON MONTHLY PAPER
      ***************************************************************
       01  CART-WORK-AREA.
           03  CART-OUTPUT-AREA.
               05  CART-PAYMENT        PIC 9(6)V99     COMP.
               05  CART-BALLOON        PIC S9(6)V99    COMP.

           03  CART-INPUT-AREA.
               05  CART-FINANCED       PIC 9(8)V99     COMP.
               05  CART-SUM-REGPYAMT   PIC 9(6)V99     COMP.
               05  CART-AMTDUE         PIC 9(6)V99     COMP.
               05  CART-TERM           PIC 9(4)        COMP.
               05  CART-INTDATE        PIC 9(8)        COMP.
               05  CART-1STPYDATE      PIC 9(8)        COMP.
               05  CART-RATE           PIC 9(3)V9(3)   COMP.
               05  CART-DISFRMLA       PIC X.
               05  CART-UNITPER-CD     PIC X.
               05  CART-UNITPER-FREQ   PIC 9(3)        COMP.
               05  CART-INTMETHOD      PIC 999         COMP  VALUE 0.
               05  CART-NEG-FG         PIC X                 VALUE " ".

           03  CART-WORK-AREA.
               05  CART-TERM-1         PIC 9(4)        COMP.
               05  CART-W              PIC 9(4)        COMP.
               05  CART-F              PIC V9(9)       COMP.
               05  CART-METHOD         PIC X.
               05  CART-ERRCD          PIC X.

               05  CART-I              PIC V9(15)      COMP.
               05  CART-I1             PIC 9V9(15)     COMP.
               05  CART-ADNI-1         PIC 9(3)V9(15)  COMP.
               05  CART-ANI-2          PIC 9(3)V9(15)  COMP.
               05  CART-1STFAC         PIC 9V9(15)     COMP.
               05  CART-PV             PIC 9(8)V99     COMP.
               05  CART-PV1            PIC 9(8)V99     COMP.
               05  CART-PV2            PIC 9(8)V99     COMP.
               05  CART-PMT            PIC 9(8)V99     COMP.
               05  CART-ADJ            PIC S9(8)V9(7)  COMP.
               05  CART-ADJ-POS        PIC 9(8)V9(7)   COMP.
               05  CART-COUNT          PIC 9(2)        COMP.
               05  CART-DELTA          PIC V99         COMP  VALUE .01.
