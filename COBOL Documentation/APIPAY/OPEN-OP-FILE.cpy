      *-----------------------------------------------------------------
       OPEN-OP-FILE.
           PERFORM LOAD-OP-FILE.
           PERFORM OPEN-IT.
           MOVE OPEN-PATH TO E-FILE.
           OPEN I-O OP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OP-FILE.
           IF IO-FG = 7
              PERFORM CLOSE-OP-FILE
              GO TO OPEN-OP-FILE.
      *-----------------------------------------------------------------
