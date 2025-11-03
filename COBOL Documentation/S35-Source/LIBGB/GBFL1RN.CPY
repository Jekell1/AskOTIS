      * COPYMEMBER: LIBGB/GBFL1RN
       OPEN-FL1-FILE.
           PERFORM OPEN-IT.
           MOVE FL-PATH TO E-FILE.
           OPEN INPUT FL-FILE.
           IF IO-FG = 8
              GO TO OPEN-FL1-FILE.
           IF IO-FG = 7
              CLOSE FL-FILE
              GO TO OPEN-FL1-FILE.
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
       CLOSE-FL1-FILE.
           PERFORM CLOSE-IT.
           CLOSE FL-FILE.
