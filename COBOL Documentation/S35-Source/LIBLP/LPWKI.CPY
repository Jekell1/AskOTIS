      * COPYMEMBER: LIBLP/LPWKI
       OPEN-WK-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE TEMP-PATH TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE-OUTPUT.
           UNLOCK WK-FILE.
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
       READ-WK-FILE.
           PERFORM READ-IT.
           MOVE TEMP-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           READ WK-FILE.
           IF IO-FG = 8
              GO TO READ-WK-FILE.
       WRITE-WK-FILE.
           PERFORM WRITE-IT.
           MOVE TEMP-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           WRITE WK-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-FILE.
           UNLOCK WK-FILE.
       REWRITE-WK-FILE.
           PERFORM REWRITE-IT.
           MOVE TEMP-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           REWRITE WK-REC.
           IF IO-FG = 8
              GO TO REWRITE-WK-FILE.
           UNLOCK WK-FILE.
       DELETE-WK-FILE.
           PERFORM DELETE-IT.
           MOVE TEMP-PATH TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           DELETE WK-FILE.
           IF IO-FG = 8
              GO TO DELETE-WK-FILE.
           UNLOCK WK-FILE.
       CLOSE-WK-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.
