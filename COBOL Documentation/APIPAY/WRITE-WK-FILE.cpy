       WRITE-WK-FILE.
           PERFORM WRITE-IT.
           MOVE TEMP-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           WRITE WK-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-FILE.
           UNLOCK WK-FILE.
