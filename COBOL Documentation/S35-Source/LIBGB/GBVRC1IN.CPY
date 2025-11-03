      * COPYMEMBER: LIBGB/GBVRC1IN
      * 2025.0307 SMR FIXED GO TO IN OPEN OUTPUT S35Q-172
       OPEN-VRC1-FILE.
           PERFORM OPEN-IT.
           MOVE VRC-PATH       TO E-FILE.
           OPEN I-O VRC-FILE.
           IF IO-FG = 8
             GO TO OPEN-VRC1-FILE.
           IF IO-FG = 7
             CLOSE VRC-FILE
             GO TO OPEN-VRC1-FILE.
           UNLOCK VRC-FILE.
       OPEN-VRC1-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE VRC-PATH       TO E-FILE.
           OPEN OUTPUT VRC-FILE.
           IF IO-FG = 8
             GO TO OPEN-VRC1-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE VRC-FILE
             GO TO OPEN-VRC1-FILE-OUTPUT.
           UNLOCK VRC-FILE.
       READ-VRC1-FILE.
           PERFORM READ-IT.
           MOVE VRC-PATH        TO E-FILE.
           MOVE VRC1-KEY        TO E-KEYX.
           READ VRC-FILE.
           IF IO-FG = 8
              GO TO READ-VRC1-FILE.
       READ-VRC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE VRC-PATH        TO E-FILE.
           MOVE VRC1-KEY        TO E-KEYX.
           READ VRC-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-VRC1-FILE-NEXT.
       START-VRC1-FILE.
           PERFORM START-IT.
           MOVE VRC-PATH        TO E-FILE.
           MOVE VRC1-KEY        TO E-KEYX.
           START VRC-FILE KEY NOT < VRC1-KEY.
           UNLOCK VRC-FILE.
       WRITE-VRC1-FILE.
           PERFORM WRITE-IT.
           MOVE VRC-PATH        TO E-FILE.
           MOVE VRC1-KEY        TO E-KEYX.
           WRITE VRC-REC.
           IF IO-FG = 8
             GO TO WRITE-VRC1-FILE.
           UNLOCK VRC-FILE.
       REWRITE-VRC1-FILE.
           PERFORM REWRITE-IT.
           MOVE VRC-PATH        TO E-FILE.
           MOVE VRC1-KEY        TO E-KEYX.
           REWRITE VRC-REC.
           IF IO-FG = 8
             GO TO REWRITE-VRC1-FILE.
           UNLOCK VRC-FILE.
       CLOSE-VRC1-FILE.
           PERFORM CLOSE-IT.
           CLOSE VRC-FILE.
