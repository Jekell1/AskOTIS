      * COPYMEMBER: LIBLP/LPTRWCL

      ***********************************************************************
      *    LPTRWCL    CASH BALANCING FILE CLEAR ROUTINE
      *
      * REV:
      * BAH 20210426 REMOVED FUTURE FIELDS
      ***********************************************************************
       CLEAR-TRW-FILE SECTION.
           MOVE SPACES TO TRW-REC.

           MOVE 0 TO TW-BRNO TW-DATE TW-TILLNO.
           MOVE 0 TO TW-LTOUCH-DATE.

           PERFORM VARYING TRWSUB FROM 1 BY 1 UNTIL TRWSUB > 20
              MOVE 0 TO TW-AMOUNT(TRWSUB) TW-OVERSHORT(TRWSUB)
                        TW-TIME(TRWSUB)
           END-PERFORM.

