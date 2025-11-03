      * COPYMEMBER: LIBLP/LPSRCDW
      *---------------------------------------------------
      *                  (LPSRCDW)
      *    GET SUBSCRIPT FOR SOURCE CODE TABLES WORKERS
      * REV:
      *  MJD 012495 ADDED SRCD-FG TO INDICATE WHETHER TO UPDATE
      *               PERIOD OF PL TABLE.
      *  MJD ADDED 88 LEVELS FOR SRCD (DONE AS PART OF WORLD PR#215)
      *---------------------------------------------------
       01  SRCD               PIC 99       COMP.
           88 SRCD-IS-NB      VALUE 1.
           88 SRCD-IS-SF      VALUE 2.
           88 SRCD-IS-FB      VALUE 3.
           88 SRCD-IS-PB      VALUE 4.
           88 SRCD-IS-PBD     VALUE 5.
           88 SRCD-IS-PBI     VALUE 6.
           88 SRCD-IS-RN      VALUE 7.
           88 SRCD-IS-SC      VALUE 8.
           88 SRCD-IS-OT      VALUE 9.
           88 SRCD-IS-RB      VALUE 10.
           88 SRCD-IS-RO      VALUE 11.
       01  SRCD-WORKER        PIC S9(7)V99 COMP.
       01  SRCD-FG            PIC X.
           88 SRCD-IS-PL      VALUE "Y".
           88 SRCD-NOT-PL     VALUE "N".

