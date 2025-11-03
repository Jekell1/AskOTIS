      * COPYMEMBER: LIBEX/EXXTRLI
      **************************************************************
      *                (EXXTRLI)
      **************************************************************
       OPEN-XTRL-FILE.
           PERFORM OPEN-IT.
           MOVE XTRL-PATH TO E-FILE.
           OPEN I-O XTRL-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRL-FILE.
           IF IO-FG = 7
             CLOSE XTRL-FILE
             GO TO OPEN-XTRL-FILE.
           UNLOCK XTRL-FILE.
       OPEN-XTRL-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XTRL-PATH TO E-FILE.
           OPEN OUTPUT XTRL-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRL-FILE.
           IF IO-FG = 7
             CLOSE XTRL-FILE
             GO TO OPEN-XTRL-FILE.
           UNLOCK XTRL-FILE.
       READ-XTRL-FILE.
           PERFORM READ-IT.
           MOVE XTRL-PATH TO E-FILE.
           MOVE XTRL-KEY TO E-KEYX.
           READ XTRL-FILE.
           IF IO-FG = 8
             GO TO READ-XTRL-FILE.
       WRITE-XTRL-FILE.
           PERFORM WRITE-IT.
           MOVE XTRL-PATH TO E-FILE.
           MOVE XTRL-KEY TO E-KEYX.
           WRITE XTRL-REC.
           IF IO-FG = 8
             GO TO WRITE-XTRL-FILE.
           UNLOCK XTRL-FILE.
       REWRITE-XTRL-FILE.
           PERFORM REWRITE-IT.
           MOVE XTRL-PATH TO E-FILE.
           MOVE XTRL-KEY TO E-KEYX.
           REWRITE XTRL-REC.
           IF IO-FG = 8
             GO TO REWRITE-XTRL-FILE.
           UNLOCK XTRL-FILE.
       DELETE-XTRL-FILE.
           PERFORM DELETE-IT.
           MOVE XTRL-PATH TO E-FILE.
           MOVE XTRL-KEY TO E-KEYX.
           DELETE XTRL-FILE.
           IF IO-FG = 8
             GO TO DELETE-XTRL-FILE.
           UNLOCK XTRL-FILE.
       CLOSE-XTRL-FILE.
           PERFORM CLOSE-IT.
           CLOSE XTRL-FILE.
