      * COPYMEMBER: LIBLP/LPWKR
       OPEN-WK-FILE.
           PERFORM OPEN-IT.
           MOVE TEMP-PATH TO E-FILE.
           OPEN INPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE.
       READ-WK-FILE.
           PERFORM READ-IT.
           MOVE TEMP-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           READ WK-FILE.
           IF IO-FG = 8
              GO TO READ-WK-FILE.
       CLOSE-WK-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.
