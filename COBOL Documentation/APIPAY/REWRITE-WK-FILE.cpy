       REWRITE-WK-FILE.
           PERFORM REWRITE-IT.
           MOVE TEMP-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           REWRITE WK-REC.
           IF IO-FG = 8
              GO TO REWRITE-WK-FILE.
           UNLOCK WK-FILE.
