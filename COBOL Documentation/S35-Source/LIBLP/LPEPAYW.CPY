      * COPYMEMBER: LIBLP/LPEPAYW
      *******************************************************
      *               (LPEPAYW)
      * REV:
      *  JTG 021097 CHANGED EPAY-TERM TO EPAY-ORGTERM
      *             ADDED   EPAY-UNITPER-CD
      *                     EPAY-UNITPER-FREQ
      *******************************************************
       01  EPAY-WORK-AREA.
      * INPUT AREA:
           03  EPAY-PVALUE         PIC 9(8)V99     COMP    VALUE 0.
           03  EPAY-RATE           PIC 9(3)V9(3)   COMP    VALUE 0.
           03  EPAY-ORGTERM        PIC 9(4)        COMP    VALUE 0.
           03  EPAY-INTDATE        PIC 9(8)        COMP    VALUE 0.
           03  EPAY-INTMETHOD      PIC 999         COMP    VALUE 0.
           03  EPAY-1STPYDATE      PIC 9(8)        COMP.
           03  EPAY-UNITPER-CD     PIC X.
           03  EPAY-UNITPER-FREQ   PIC 9(3)        COMP    VALUE 1.
           03  EPAY-DISFRMLA       PIC X                   VALUE "C".

      * OUTPUT AREA:
           03  EPAY-PAYMNT         PIC S9(6)V99    COMP    VALUE 0.

      * WORKERS:
           03  EPAYF               PIC S9V9(17)    COMP.
           03  EPAYT               PIC S999        COMP.
           03  EPAYI               PIC S999V9(15)  COMP.
           03  EPAYADJ             PIC S999V9(15)  COMP.
           03  EPAYPW              PIC S999V9(15)  COMP.
