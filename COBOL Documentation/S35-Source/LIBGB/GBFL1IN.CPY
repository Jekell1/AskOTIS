      * COPYMEMBER: LIBGB/GBFL1IN
       OPEN-FL1-FILE.
           PERFORM OPEN-IT.
           MOVE FL-PATH TO E-FILE.
           OPEN I-O FL-FILE.
           IF IO-FG = 8
             GO TO OPEN-FL1-FILE.
           IF IO-FG = 7
             CLOSE FL-FILE
             GO TO OPEN-FL1-FILE.
           UNLOCK FL-FILE.
       READ-FL1-FILE.
           PERFORM READ-IT.
           MOVE FL-PATH TO E-FILE.
           MOVE FL1-KEY TO E-KEYX.
           READ FL-FILE.
           IF IO-FG = 8
              GO TO READ-FL1-FILE.
       READ-FL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE FL-PATH TO E-FILE.
           MOVE FL1-KEY TO E-KEYX.
           READ FL-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-FL1-FILE-NEXT.
       START-FL1-FILE.
           PERFORM START-IT.
           MOVE FL-PATH TO E-FILE.
           MOVE FL1-KEY TO E-KEYX.
           START FL-FILE KEY NOT < FL1-KEY.
           UNLOCK FL-FILE.
       WRITE-FL1-FILE.
           PERFORM WRITE-IT.
           MOVE FL-PATH TO E-FILE.
           MOVE FL1-KEY TO E-KEYX.
           WRITE FL-REC.
           IF IO-FG = 8
             GO TO WRITE-FL1-FILE.
           UNLOCK FL-FILE.
       REWRITE-FL1-FILE.
           PERFORM REWRITE-IT.
           MOVE FL-PATH TO E-FILE.
           MOVE FL1-KEY TO E-KEYX.
           REWRITE FL-REC.
           IF IO-FG = 8
             GO TO REWRITE-FL1-FILE.
           UNLOCK FL-FILE.
       DELETE-FL1-FILE.
           PERFORM DELETE-IT.
           MOVE FL-PATH TO E-FILE.
           MOVE FL1-KEY TO E-KEYX.
           DELETE FL-FILE.
           IF IO-FG = 8
             GO TO DELETE-FL1-FILE.
           UNLOCK FL-FILE.
       CLOSE-FL1-FILE.
           PERFORM CLOSE-IT.
           CLOSE FL-FILE.
