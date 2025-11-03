       OPEN-WK-OUTPUT.
           OPEN OUTPUT WK-FILE.
           IF ( IO-FG = 8 )
              GO TO OPEN-WK-OUTPUT.
           IF ( IO-FG = 7 )
              PERFORM CLOSE-WK-FILE
              GO TO OPEN-WK-OUTPUT.
           PERFORM CLOSE-WK-FILE.

      *********************************
      *   NAME: CDV-VERIFY
      *   DESC: VERIFY LP-TRCD & YIELD SUBSCRIPT
      *   IN  : LP-TRCD, CDV-NN
      *   OUT : CDV-SUB; 0 = NOT_FOUND
      *********************************
