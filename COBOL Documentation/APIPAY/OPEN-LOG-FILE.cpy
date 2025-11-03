       OPEN-LOG-FILE.
           OPEN INPUT LOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-LOG-FILE.
           IF IO-FG = 7
              CLOSE LOG-FILE
              GO TO OPEN-LOG-FILE.
