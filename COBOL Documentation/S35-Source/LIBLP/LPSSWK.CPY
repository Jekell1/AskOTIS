      * COPYMEMBER: LIBLP/LPSSWK
      *************************************************************
      *    LPSSWK          SSFILE WORKERS
      * REV:
      *************************************************************
       78  SSMTD             VALUE 1.
       78  SSYTD             VALUE 2.

       01  CREDIT-SCORE-BUCKET           PIC 9(4).
           88  SCORE-BUCKET1             VALUE 0   THRU 1.
           88  SCORE-BUCKET2             VALUE 2   THRU 488.
           88  SCORE-BUCKET3             VALUE 489 THRU 534.
           88  SCORE-BUCKET4             VALUE 535 THRU 562.
           88  SCORE-BUCKET5             VALUE 563 THRU 591.
           88  SCORE-BUCKET6             VALUE 592 THRU 612.
           88  SCORE-BUCKET7             VALUE 613 THRU 639.
           88  SCORE-BUCKET8             VALUE 640 THRU 9999.

      * SUBSCRIPTS:
       01  CUR-SUB           PIC 99.
       01  ORIG-SUB          PIC 99.
       01  DEL-SUB           PIC 99.
       01  SCORE-SUB         PIC 99.
       01  MTD-SUB           PIC 99.
       01  SSSUB1            PIC 9(3)  COMP.
       01  SSSUB2            PIC 9(3)  COMP.
       01  SSSUB3            PIC 9(3)  COMP.

