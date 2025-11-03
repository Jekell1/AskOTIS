      * COPYMEMBER: LIBLP/LPEADJW
      ****************************************************
      *    WORKERS FOR PRORATION OF REBATE FOR
      *    EARNING ADJUSTMENTS:
      *               INTEREST(1)
      *               1ST PAY EXT(2)
      *               SERVICE CHARGE(3)
      *               MAINTFEE(4)
      * REV: JTG 051089 CHANGED WORKERS TO COMP
      ****************************************************
       01  EADJ-WORKERS.
      * INPUT FIELDS:
           03  EADJ-REBATE       PIC S9(7)V99   COMP VALUE 0.

      * OUTPUT FIELDS:
           03  EADJ-ADJUST       OCCURS 4 TIMES
                                 PIC S9(7)V99   COMP.

           03  EADJ-ADJUST-FG    OCCURS 4 TIMES
                                 PIC X.

      * WORK FIELDS:
           03  EADJ-FACTOR       PIC S9(3)V9(7) COMP.
           03  EADJ-SUB          PIC 9          COMP.
