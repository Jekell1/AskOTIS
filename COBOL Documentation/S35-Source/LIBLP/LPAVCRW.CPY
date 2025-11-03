      * COPYMEMBER: LIBLP/LPAVCRW
      *-----------------------------------------------------------------------
      *    COMPUTE AVAILABLE CREDIT WORKERS
      * REV:
      *  JTG 051089 CHANGED WORKERS TO COMP
      *  JTG 052190 ADDED AVCR-RESTRICT-FG (GA)
      *  JTG 092491 ADDED AVCR-WORKER FOR PAID AHEAD
      *             MONIES TO DETERMINE (GA) SERVICE CHARGE
      *  JTG 022593 ADDED AVCR-LPTRCD & AVCR-DEF-STOP
      *  BAH 111496 ADDED AVCR-CURRENT-AVAIL FOR WDT
      *  JTG 010426 ADDED AVCR-WORKER2  RE: GEORGIA GILA CHANGE EFF: 04/13/01
      *                                                          WORLD PR#263
      *-----------------------------------------------------------------------
       01  AVCR-WORKERS.
      * INPUT FIELDS:
           03  AVCR-PAYDATE         PIC 9(8)     COMP-3.
           03  AVCR-MAKERCD         PIC X.
           03  AVCR-LPTRCD          PIC XX       VALUE "PO".
           03  AVCR-POFF-FG         PIC X        VALUE " ".
           03  AVCR-DEF-STOP   PIC X        VALUE " ".

      *****03  AVCR-POFF-NETDUE     PIC S9(7)V99 COMP.
      * COMPUTATION OF THE PAYOFF ROUTINE CAN BE AVOIDED
      * WHEN THE NET PAYOFF AMOUNT IS AVAILABLE.
      * MOVE POFF-NETDUE TO AVCR-POFF-NETDUE, ALONG WITH
      * SETTING AVCR-POFF-FG TO "Y".

      * OUTPUT FIELDS:
           03  AVCR-POFF-NETDUE     PIC S9(7)V99 COMP-3.
           03  AVCR-AVAILCREDIT     PIC S9(7)V99 COMP-3.
           03  AVCR-RESTRICT-FG     PIC X.
           03  AVCR-CURRENT-AVAIL   PIC S9(7)V99 COMP-3.

      * WORK FIELDS:
           03  AVCR-WORKER          PIC S9(7)V99 COMP-3.
           03  AVCR-WORKER2         PIC S9(7)V99 COMP-3.

