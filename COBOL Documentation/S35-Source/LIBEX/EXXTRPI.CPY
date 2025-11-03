      * COPYMEMBER: LIBEX/EXXTRPI
      **************************************************************
      *                (EXXTRPI)
      **************************************************************
       OPEN-XTRP-FILE.
           PERFORM OPEN-IT.
           MOVE XTRP-PATH TO E-FILE.
           OPEN I-O XTRP-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRP-FILE.
           IF IO-FG = 7
             CLOSE XTRP-FILE
             GO TO OPEN-XTRP-FILE.
           UNLOCK XTRP-FILE.
       OPEN-XTRP-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XTRP-PATH TO E-FILE.
           OPEN OUTPUT XTRP-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRP-FILE.
           IF IO-FG = 7
             CLOSE XTRP-FILE
             GO TO OPEN-XTRP-FILE.
           UNLOCK XTRP-FILE.
       READ-XTRP-FILE.
           PERFORM READ-IT.
           MOVE XTRP-PATH TO E-FILE.
           MOVE XTRP-KEY TO E-KEYX.
           READ XTRP-FILE.
           IF IO-FG = 8
             GO TO READ-XTRP-FILE.
       WRITE-XTRP-FILE.
           PERFORM WRITE-IT.
           MOVE XTRP-PATH TO E-FILE.
           MOVE XTRP-KEY TO E-KEYX.
           WRITE XTRP-REC.
           IF IO-FG = 8
             GO TO WRITE-XTRP-FILE.
           UNLOCK XTRP-FILE.
       REWRITE-XTRP-FILE.
           PERFORM REWRITE-IT.
           MOVE XTRP-PATH TO E-FILE.
           MOVE XTRP-KEY TO E-KEYX.
           REWRITE XTRP-REC.
           IF IO-FG = 8
             GO TO REWRITE-XTRP-FILE.
           UNLOCK XTRP-FILE.
       DELETE-XTRP-FILE.
           PERFORM DELETE-IT.
           MOVE XTRP-PATH TO E-FILE.
           MOVE XTRP-KEY TO E-KEYX.
           DELETE XTRP-FILE.
           IF IO-FG = 8
             GO TO DELETE-XTRP-FILE.
           UNLOCK XTRP-FILE.
       CLOSE-XTRP-FILE.
           PERFORM CLOSE-IT.
           CLOSE XTRP-FILE.
