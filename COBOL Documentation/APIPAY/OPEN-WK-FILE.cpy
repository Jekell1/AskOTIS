       OPEN-WK-FILE.
           PERFORM OPEN-IT.
           MOVE TEMP-PATH TO E-FILE.
           OPEN I-O WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE.
           UNLOCK WK-FILE.
