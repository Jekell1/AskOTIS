      * COPYMEMBER: LIBGB/ZONEDTIMEIO
      ******************************************
      *              (ZONEDTIMEIO)
      * DESC:  GET/FORMAT CLOCK TIME FOR TIME ZONE
      * IN  :  TIME-MILITARY-FG (Y OR N)
      *        TIME-ZONE-FACTOR: MOVE BR-TIME-ZONE-FACTOR
      *                               TO TIME-ZONE-FACTOR
      * OUT :  TIME-NOW, TIME-EDIT
      * NOTE:  ADJUSTS CURRENT SYSTEM TIME UP OR BACK BASED
      *        ON + OR - FACTOR, BUT DOESN'T ATTEMPT TO
      *        CROSS MIDNIGHT...FOR EX. IF SYSTEM TIME IS
      *        22:30 (10:30 PM) & FACTOR IS +3, ROUTINE
      *        WILL ONLY INCREMENT TIME TO 23:30 (11:30)
      *        SINCE DATE HAS ACTUALLY CHANGED.....
      * REV:
      ******************************************
       ZONED-GET-TIME SECTION.
           ACCEPT TIME-NOW FROM TIME.
           IF TIME-ZONE-FACTOR NOT NUMERIC
              MOVE 0 TO TIME-ZONE-FACTOR.
           IF TIME-ZONE-FACTOR NOT = 0
              COMPUTE SIGNED-HH = CUR-HH + TIME-ZONE-FACTOR
              IF SIGNED-HH < 0
                 MOVE 0 TO SIGNED-HH
              END-IF
              IF SIGNED-HH > 23
                 MOVE 23 TO SIGNED-HH
              END-IF
              MOVE SIGNED-HH TO CUR-HH.
           IF TIME-MILITARY-FG = "Y"
              MOVE " " TO T-AMPM
           ELSE
              IF H-AND-M NOT GREATER THAN 1159
                 MOVE "AM" TO T-AMPM
              ELSE
                 MOVE "PM" TO T-AMPM.
           IF TIME-MILITARY-FG = " "
              IF H-AND-M IS NOT LESS THAN 1300
                 SUBTRACT 1200 FROM H-AND-M.
           IF TIME-MILITARY-FG = " "
              IF CUR-HH = 0
                 MOVE 12 TO CUR-HH.
           MOVE CUR-HH TO T-HH.
           MOVE CUR-MM TO T-MM.
           MOVE ":" TO T-COL.
           MOVE " " TO T-SPACE.

       ZONED-GET-TIME-EXIT.
           MOVE " " TO TIME-MILITARY-FG.
