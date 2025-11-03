      * COPYMEMBER: LIBSP/SPVGQ1IN
      * 2025.0307 SMR FIXED GO TO IN OPEN OUTPUT S35Q-172
       OPEN-VGQ1-FILE.
           PERFORM OPEN-IT.
           MOVE VGQ-PATH       TO E-FILE.
           OPEN I-O VGQ-FILE.
           IF IO-FG = 8
             GO TO OPEN-VGQ1-FILE.
           IF IO-FG = 7
             CLOSE VGQ-FILE
             GO TO OPEN-VGQ1-FILE.
           UNLOCK VGQ-FILE.
       OPEN-VGQ1-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE VGQ-PATH       TO E-FILE.
           OPEN OUTPUT VGQ-FILE.
           IF IO-FG = 8
             GO TO OPEN-VGQ1-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE VGQ-FILE
             GO TO OPEN-VGQ1-FILE-OUTPUT.
           UNLOCK VGQ-FILE.
       READ-VGQ1-FILE.
           PERFORM READ-IT.
           MOVE VGQ-PATH        TO E-FILE.
           MOVE VGQ1-KEY        TO E-KEYX.
           READ VGQ-FILE.
           IF IO-FG = 8
              GO TO READ-VGQ1-FILE.
       READ-VGQ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE VGQ-PATH        TO E-FILE.
           MOVE VGQ1-KEY        TO E-KEYX.
           READ VGQ-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-VGQ1-FILE-NEXT.
       START-VGQ1-FILE.
           PERFORM START-IT.
           MOVE VGQ-PATH        TO E-FILE.
           MOVE VGQ1-KEY        TO E-KEYX.
           START VGQ-FILE KEY NOT < VGQ1-KEY.
           UNLOCK VGQ-FILE.
       START-VGQ1-FILE-GREATER.
           PERFORM START-IT.
           MOVE VGQ-PATH        TO E-FILE.
           MOVE VGQ1-KEY        TO E-KEYX.
           START VGQ-FILE KEY > VGQ1-KEY.
           UNLOCK VGQ-FILE.
       WRITE-VGQ1-FILE.
           PERFORM WRITE-IT.
           MOVE VGQ-PATH        TO E-FILE.
           MOVE VGQ1-KEY        TO E-KEYX.
           WRITE VGQ-REC.
           IF IO-FG = 8
             GO TO WRITE-VGQ1-FILE.
           UNLOCK VGQ-FILE.
       REWRITE-VGQ1-FILE.
           PERFORM REWRITE-IT.
           MOVE VGQ-PATH        TO E-FILE.
           MOVE VGQ1-KEY        TO E-KEYX.
           REWRITE VGQ-REC.
           IF IO-FG = 8
             GO TO REWRITE-VGQ1-FILE.
           UNLOCK VGQ-FILE.
       CLOSE-VGQ1-FILE.
           PERFORM CLOSE-IT.
           CLOSE VGQ-FILE.
       REMOVE-VGQ1-FILE.
           MOVE VGQ-PATH        TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
