      * COPYMEMBER: LIBLP/LPSCHDW
      *----------------------------------------------
      *            LPSCHDW
      *    EXPANDED PAYMENT SCHEDULE.
      *
      * REV:
      *----------------------------------------------
       01  SCHD-TERM                 PIC 999     COMP.
       01  SCHD-PAYMENT-TABLE.
           03  SCHD-AMT              OCCURS 360
                                     PIC 9(6)V99 COMP.
       01  SCHD-SUB                  PIC 999     COMP.
       01  SCHD-SUB2                 PIC 999     COMP.
       01  SCHD-SUB3                 PIC 999     COMP.
