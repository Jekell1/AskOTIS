      *******************************************
      *    COMPUTE EFFECTIVE RATE
      *******************************************
       APRS-GET-EFFRATE SECTION.
           MOVE 0 TO APRS-EFFRATE.
           IF SP-CAL-RATETYPE(CTBL) = "S" OR "M" OR "Z" OR "G"
              MOVE APRS-SMPRATE TO APRS-EFFRATE
           ELSE
           IF SP-CAL-RATETYPE(CTBL) = "A"
              PERFORM APRS-EFFRATE-ADDON
           ELSE
           IF SP-CAL-RATETYPE(CTBL) = "D"
              PERFORM APRS-EFFRATE-DISCOUNT.

       APRS-GET-EFFRATE-EXIT.
           EXIT.

      *-------------------------------------------------
      *    COMPUTE EFFECTIVE DISCOUNT RATE
      *-------------------------------------------------
