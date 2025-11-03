      * COPYMEMBER: LIBGB/GBWK3IN
       OPEN-WK3-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE "WK3" TO E-FILE.
           OPEN OUTPUT WK3-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK3-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK3-FILE
              GO TO OPEN-WK3-FILE-OUTPUT.
           UNLOCK WK3-FILE.
       OPEN-WK3-FILE.
           PERFORM OPEN-IT.
           MOVE "WK3" TO E-FILE.
           OPEN I-O WK3-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK3-FILE.
           IF IO-FG = 7
              CLOSE WK3-FILE
              GO TO OPEN-WK3-FILE.
           UNLOCK WK3-FILE.
       READ-WK3-FILE.
           PERFORM READ-IT.
           MOVE "WK3" TO E-FILE.
           MOVE WK3-KEY TO E-KEYX.
           READ WK3-FILE.
           IF IO-FG = 8
              GO TO READ-WK3-FILE.
       READ-WK3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE "WK3" TO E-FILE.
           MOVE WK3-KEY TO E-KEYX.
           READ WK3-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-WK3-FILE-NEXT.
       READ-WK3-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE "WK3" TO E-FILE.
           MOVE WK3-KEY TO E-KEYX.
           READ WK3-FILE PREVIOUS.
           IF IO-FG = 8
              GO TO READ-WK3-FILE-PREVIOUS.
       START-WK3-FILE-LE.
           PERFORM START-IT.
           MOVE "WK3" TO E-FILE.
           MOVE WK3-KEY TO E-KEYX.
           START WK3-FILE KEY <= WK3-KEY.
           UNLOCK WK3-FILE.
       START-WK3-FILE-GREATER.
           PERFORM START-IT.
           MOVE "WK3" TO E-FILE.
           MOVE WK3-KEY TO E-KEYX.
           START WK3-FILE KEY > WK3-KEY.
           UNLOCK WK3-FILE.
       START-WK3-FILE.
           PERFORM START-IT.
           MOVE "WK3" TO E-FILE.
           MOVE WK3-KEY TO E-KEYX.
           START WK3-FILE KEY NOT < WK3-KEY.
           UNLOCK WK3-FILE.
       WRITE-WK3-FILE.
           PERFORM WRITE-IT.
           MOVE "WK3" TO E-FILE.
           MOVE WK3-KEY TO E-KEYX.
           WRITE WK3-REC.
           IF IO-FG = 8
              GO TO WRITE-WK3-FILE.
           UNLOCK WK3-FILE.
       REWRITE-WK3-FILE.
           PERFORM REWRITE-IT.
           MOVE "WK3" TO E-FILE.
           MOVE WK3-KEY TO E-KEYX.
           REWRITE WK3-REC.
           IF IO-FG = 8
              GO TO REWRITE-WK3-FILE.
           UNLOCK WK3-FILE.
       DELETE-WK3-FILE.
           PERFORM DELETE-IT.
           MOVE "WK3" TO E-FILE.
           MOVE WK3-KEY TO E-KEYX.
           DELETE WK3-FILE.
           IF IO-FG = 8
              GO TO DELETE-WK3-FILE.
           UNLOCK WK3-FILE.
       CLOSE-WK3-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK3-FILE.
