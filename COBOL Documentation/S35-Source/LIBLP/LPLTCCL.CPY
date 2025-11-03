      * COPYMEMBER: LIBLP/LPLTCCL
      **************************************************************************
      *    LPLTCCL    COLLATERAL TRAILER FILE CLEAR ROUTINE
      *
      * REV:
      *  06/05/2015 BAH NEW
      **************************************************************************
       CLEAR-LTC-FILE SECTION.
           MOVE SPACES TO LTC-REC.

           MOVE 0 TO LTC-LTOUCH-DATE.

           PERFORM VARYING LTCSUB FROM 1 BY 1 UNTIL LTCSUB > 3
              MOVE 0 TO LTC-MILEAGE(LTCSUB)
                        LTC-VALUE (LTCSUB)
                        LTC-SELLPR(LTCSUB)
                        LTC-MISC-AMT (LTCSUB)
                        LTC-VIN-CHG-DATE(LTCSUB).
