      *-----------------------------------------------------------------
       OPEN-SP1-FILE.
           PERFORM OPEN-SPA1-FILE.
           IF ( IO-FG = 0 )
               PERFORM OPEN-SPB1-FILE
               IF ( IO-FG = 0 )
                  PERFORM OPEN-SPC1-FILE
                  IF ( IO-FG = 0 )
                     MOVE " " TO IO-MULTI-FG
                  ELSE
                     MOVE "C" TO IO-MULTI-FG
               ELSE
                  MOVE "B" TO IO-MULTI-FG
            ELSE
               MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
