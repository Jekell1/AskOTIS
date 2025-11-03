      * COPYMEMBER: LIBLP/LPVBY1IN
      * 2025.0307 SMR FIXED GO TO IN OPEN OUTPUT S35Q-172
       OPEN-VBY1-FILE.
           PERFORM OPEN-IT.
           MOVE VBY-PATH       TO E-FILE.
           OPEN I-O VBY-FILE.
           IF IO-FG = 8
             GO TO OPEN-VBY1-FILE.
           IF IO-FG = 7
             CLOSE VBY-FILE
             GO TO OPEN-VBY1-FILE.
           UNLOCK VBY-FILE.
       OPEN-VBY1-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE VBY-PATH       TO E-FILE.
           OPEN OUTPUT VBY-FILE.
           IF IO-FG = 8
             GO TO OPEN-VBY1-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE VBY-FILE
             GO TO OPEN-VBY1-FILE-OUTPUT.
           UNLOCK VBY-FILE.
       READ-VBY1-FILE.
           PERFORM READ-IT.
           MOVE VBY-PATH        TO E-FILE.
           MOVE VBY1-KEY        TO E-KEYX.
           READ VBY-FILE.
           IF IO-FG = 8
              GO TO READ-VBY1-FILE.
       READ-VBY1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE VBY-PATH        TO E-FILE.
           MOVE VBY1-KEY        TO E-KEYX.
           READ VBY-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-VBY1-FILE-NEXT.
       START-VBY1-FILE.
           PERFORM START-IT.
           MOVE VBY-PATH        TO E-FILE.
           MOVE VBY1-KEY        TO E-KEYX.
           START VBY-FILE KEY NOT < VBY1-KEY.
           UNLOCK VBY-FILE.
       WRITE-VBY1-FILE.
           PERFORM WRITE-IT.
           MOVE VBY-PATH        TO E-FILE.
           MOVE VBY1-KEY        TO E-KEYX.
           WRITE VBY-REC.
           IF IO-FG = 8
             GO TO WRITE-VBY1-FILE.
           UNLOCK VBY-FILE.
       REWRITE-VBY1-FILE.
           PERFORM REWRITE-IT.
           MOVE VBY-PATH        TO E-FILE.
           MOVE VBY1-KEY        TO E-KEYX.
           REWRITE VBY-REC.
           IF IO-FG = 8
             GO TO REWRITE-VBY1-FILE.
           UNLOCK VBY-FILE.
       CLOSE-VBY1-FILE.
           PERFORM CLOSE-IT.
           CLOSE VBY-FILE.
