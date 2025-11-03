      * COPYMEMBER: LIBGB/GBVRC2RN
       OPEN-VRC2-FILE.
           PERFORM OPEN-IT.
           MOVE VRC-PATH       TO E-FILE.
           OPEN I-O VRC-FILE.
           IF IO-FG = 8
             GO TO OPEN-VRC2-FILE.
           IF IO-FG = 7
             CLOSE VRC-FILE
             GO TO OPEN-VRC2-FILE.
           UNLOCK VRC-FILE.
       READ-VRC2-FILE.
           PERFORM READ-IT.
           MOVE VRC-PATH        TO E-FILE.
           MOVE VRC2-KEY        TO E-KEYX.
           READ VRC-FILE.
           IF IO-FG = 8
              GO TO READ-VRC2-FILE.
       READ-VRC2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE VRC-PATH        TO E-FILE.
           MOVE VRC2-KEY        TO E-KEYX.
           READ VRC-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-VRC2-FILE-NEXT.
       START-VRC2-FILE.
           PERFORM START-IT.
           MOVE VRC-PATH        TO E-FILE.
           MOVE VRC2-KEY        TO E-KEYX.
           START VRC-FILE KEY NOT < VRC2-KEY.
           UNLOCK VRC-FILE.
       CLOSE-VRC2-FILE.
           PERFORM CLOSE-IT.
           CLOSE VRC-FILE.
