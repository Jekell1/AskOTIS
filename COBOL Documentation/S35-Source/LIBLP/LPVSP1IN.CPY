      * COPYMEMBER: LIBLP/LPVSP1IN
      * 2025.0307 SMR FIXED GO TO IN OPEN OUTPUT S35Q-172
       OPEN-VSP1-FILE.
           PERFORM OPEN-IT.
           MOVE VSP-PATH       TO E-FILE.
           OPEN I-O VSP-FILE.
           IF IO-FG = 8
             GO TO OPEN-VSP1-FILE.
           IF IO-FG = 7
             CLOSE VSP-FILE
             GO TO OPEN-VSP1-FILE.
           UNLOCK VSP-FILE.
       OPEN-VSP1-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE VSP-PATH       TO E-FILE.
           OPEN OUTPUT VSP-FILE.
           IF IO-FG = 8
             GO TO OPEN-VSP1-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE VSP-FILE
             GO TO OPEN-VSP1-FILE-OUTPUT.
           UNLOCK VSP-FILE.
       READ-VSP1-FILE.
           PERFORM READ-IT.
           MOVE VSP-PATH        TO E-FILE.
           MOVE VSP1-KEY        TO E-KEYX.
           READ VSP-FILE.
           IF IO-FG = 8
              GO TO READ-VSP1-FILE.
       READ-VSP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE VSP-PATH        TO E-FILE.
           MOVE VSP1-KEY        TO E-KEYX.
           READ VSP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-VSP1-FILE-NEXT.
       START-VSP1-FILE.
           PERFORM START-IT.
           MOVE VSP-PATH        TO E-FILE.
           MOVE VSP1-KEY        TO E-KEYX.
           START VSP-FILE KEY NOT < VSP1-KEY.
           UNLOCK VSP-FILE.
       START-VSP1-FILE-GREATER.
           PERFORM START-IT.
           MOVE VSP-PATH        TO E-FILE.
           MOVE VSP1-KEY        TO E-KEYX.
           START VSP-FILE KEY > VSP1-KEY.
           UNLOCK VSP-FILE.
       WRITE-VSP1-FILE.
           PERFORM WRITE-IT.
           MOVE VSP-PATH        TO E-FILE.
           MOVE VSP1-KEY        TO E-KEYX.
           WRITE VSP-REC.
           IF IO-FG = 8
             GO TO WRITE-VSP1-FILE.
           UNLOCK VSP-FILE.
       REWRITE-VSP1-FILE.
           PERFORM REWRITE-IT.
           MOVE VSP-PATH        TO E-FILE.
           MOVE VSP1-KEY        TO E-KEYX.
           REWRITE VSP-REC.
           IF IO-FG = 8
             GO TO REWRITE-VSP1-FILE.
           UNLOCK VSP-FILE.
       CLOSE-VSP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE VSP-FILE.
       REMOVE-VSP1-FILE.
           MOVE VSP-PATH        TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
