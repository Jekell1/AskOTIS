      *-----------------------------------------------------------------
       OPEN-OP-FILE-OUTPUT.
           PERFORM LOAD-OP-FILE.
           PERFORM OPEN-IT.
           MOVE OPEN-PATH TO E-FILE.
           OPEN OUTPUT OP-FILE.
           IF ( IO-FG = 8 )
              GO TO OPEN-OP-FILE-OUTPUT.
           IF ( IO-FG = 7 )
              PERFORM CLOSE-OP-FILE
              GO TO OPEN-OP-FILE-OUTPUT.
      *-----------------------------------------------------------------
