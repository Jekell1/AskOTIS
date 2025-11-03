       REBATE-EP2-REBATE-EXTCHG.
           PERFORM REBATE-GET-EXTDAYS.
           IF REB-TOTAL-EXTDAYS > 0
              COMPUTE REB-REBATE ROUNDED = LN-INTCHG +
                       REB-UNUSED-EXTDAYS / REB-TOTAL-EXTDAYS *
                                                           LN-EXTCHG.

