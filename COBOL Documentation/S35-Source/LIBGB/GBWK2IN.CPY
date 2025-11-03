      * COPYMEMBER: LIBGB/GBWK2IN
       OPEN-WK2-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE "WK2" TO E-FILE.
           OPEN OUTPUT WK2-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK2-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK2-FILE
              GO TO OPEN-WK2-FILE-OUTPUT.
           UNLOCK WK2-FILE.
       OPEN-WK2-FILE.
           PERFORM OPEN-IT.
           MOVE "WK2" TO E-FILE.
           OPEN I-O WK2-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK2-FILE.
           IF IO-FG = 7
              CLOSE WK2-FILE
              GO TO OPEN-WK2-FILE.
           UNLOCK WK2-FILE.
       READ-WK2-FILE.
           PERFORM READ-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           READ WK2-FILE.
           IF IO-FG = 8
              GO TO READ-WK2-FILE.
       READ-WK2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           READ WK2-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-WK2-FILE-NEXT.
       READ-WK2-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           READ WK2-FILE PREVIOUS.
           IF IO-FG = 8
              GO TO READ-WK2-FILE-PREVIOUS.
       START-WK2-FILE-LE.
           PERFORM START-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           START WK2-FILE KEY <= WK2-KEY.
           UNLOCK WK2-FILE.
       START-WK2-FILE-GREATER.
           PERFORM START-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           START WK2-FILE KEY > WK2-KEY.
           UNLOCK WK2-FILE.
       START-WK2-FILE.
           PERFORM START-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           START WK2-FILE KEY NOT < WK2-KEY.
           UNLOCK WK2-FILE.
       WRITE-WK2-FILE.
           PERFORM WRITE-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           WRITE WK2-REC.
           IF IO-FG = 8
              GO TO WRITE-WK2-FILE.
           UNLOCK WK2-FILE.
       REWRITE-WK2-FILE.
           PERFORM REWRITE-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           REWRITE WK2-REC.
           IF IO-FG = 8
              GO TO REWRITE-WK2-FILE.
           UNLOCK WK2-FILE.
       DELETE-WK2-FILE.
           PERFORM DELETE-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           DELETE WK2-FILE.
           IF IO-FG = 8
              GO TO DELETE-WK2-FILE.
           UNLOCK WK2-FILE.
       CLOSE-WK2-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK2-FILE.
