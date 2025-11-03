      * COPYMEMBER: LIBLP/LPLTPCL
      **************************************************************************
      *    LPLTPCL    PAYMENT SCHEDULE TRAILER FILE CLEAR ROUTINE
      *
      * REV:
      *  06/19/2015 BAH NEW
      *  160614 CS  CHANGED OCCURS FROM 17 TO 41 FOR 40 YEARS OF ESCROW
      **************************************************************************
       CLEAR-LTP-FILE SECTION.
           MOVE SPACES TO LTP-REC.

           MOVE 0 TO LTP-LTOUCH-DATE.

           PERFORM VARYING LTPSUB FROM 1 BY 1 UNTIL LTPSUB > 41
              MOVE 0 TO LTP-SCHLD-TERM(LTPSUB)
                        LTP-SCHLD-PAYMNT(LTPSUB).
