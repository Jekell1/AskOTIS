       DELETE-WK-FILE.
           PERFORM DELETE-IT.
           MOVE TEMP-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           DELETE WK-FILE.
           IF IO-FG = 8
              GO TO DELETE-WK-FILE.
           UNLOCK WK-FILE.
