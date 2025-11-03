      * COPYMEMBER: LIBGB/GBVGBIN
      * 2025.0307 SMR FIXED GO TO IN OPEN OUTPUT S35Q-172
       OPEN-VGB-FILE.
           PERFORM OPEN-IT.
           MOVE VGB-PATH       TO E-FILE.
           OPEN I-O VGB-FILE.
           IF IO-FG = 8
             GO TO OPEN-VGB-FILE.
           IF IO-FG = 7
             CLOSE VGB-FILE
             GO TO OPEN-VGB-FILE.
           UNLOCK VGB-FILE.
       OPEN-VGB-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE VGB-PATH       TO E-FILE.
           OPEN OUTPUT VGB-FILE.
           IF IO-FG = 8
             GO TO OPEN-VGB-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE VGB-FILE
             GO TO OPEN-VGB-FILE-OUTPUT.
           UNLOCK VGB-FILE.
       READ-VGB-FILE.
           PERFORM READ-IT.
           MOVE VGB-PATH       TO E-FILE.
           MOVE VGB-KEY        TO E-KEYX.
           READ VGB-FILE.
           IF IO-FG = 8
              GO TO READ-VGB-FILE.
       READ-VGB-FILE-NEXT.
           PERFORM READ-IT.
           MOVE VGB-PATH       TO E-FILE.
           MOVE VGB-KEY        TO E-KEYX.
           READ VGB-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-VGB-FILE-NEXT.
       START-VGB-FILE.
           PERFORM START-IT.
           MOVE VGB-PATH       TO E-FILE.
           MOVE VGB-KEY        TO E-KEYX.
           START VGB-FILE KEY NOT < VGB-KEY.
           UNLOCK VGB-FILE.
       WRITE-VGB-FILE.
           PERFORM WRITE-IT.
           MOVE VGB-PATH       TO E-FILE.
           MOVE VGB-KEY        TO E-KEYX.
           WRITE VGB-REC.
           IF IO-FG = 8
             GO TO WRITE-VGB-FILE.
           UNLOCK VGB-FILE.
       REWRITE-VGB-FILE.
           PERFORM REWRITE-IT.
           MOVE VGB-PATH       TO E-FILE.
           MOVE VGB-KEY        TO E-KEYX.
           REWRITE VGB-REC.
           IF IO-FG = 8
             GO TO REWRITE-VGB-FILE.
           UNLOCK VGB-FILE.
       CLOSE-VGB-FILE.
           PERFORM CLOSE-IT.
           CLOSE VGB-FILE.
