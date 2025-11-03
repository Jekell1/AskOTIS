      * COPYMEMBER: LIBUP/UPATRWI
      **************************************************************
      *                ASCII TRANSACTION FILE
      **************************************************************
       OPEN-ATRW-FILE.
           PERFORM OPEN-IT.
           MOVE ATRW-PATH TO E-FILE.
           OPEN I-O ATRW-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRW-FILE.
           IF IO-FG = 7
             CLOSE ATRW-FILE
             GO TO OPEN-ATRW-FILE.
           UNLOCK ATRW-FILE.
       READ-ATRW-FILE.
           PERFORM READ-IT.
           MOVE ATRW-PATH TO E-FILE.
           MOVE ATRW-KEY TO E-KEYX.
           READ ATRW-FILE.
           IF IO-FG = 8
             GO TO READ-ATRW-FILE.
       WRITE-ATRW-FILE.
           PERFORM WRITE-IT.
           MOVE ATRW-PATH TO E-FILE.
           MOVE ATRW-KEY TO E-KEYX.
           WRITE ATRW-REC.
           IF IO-FG = 8
             GO TO WRITE-ATRW-FILE.
           UNLOCK ATRW-FILE.
       REWRITE-ATRW-FILE.
           PERFORM REWRITE-IT.
           MOVE ATRW-PATH TO E-FILE.
           MOVE ATRW-KEY TO E-KEYX.
           REWRITE ATRW-REC.
           IF IO-FG = 8
             GO TO REWRITE-ATRW-FILE.
           UNLOCK ATRW-FILE.
       CLOSE-ATRW-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRW-FILE.
      **********************************************
      * ASCII TRANS FILE DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ATRW-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ATRW-DONE-PATH TO E-FILE.
           OPEN OUTPUT ATRW-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRW-DONE-FILE.
           IF IO-FG = 7
             CLOSE ATRW-DONE-FILE
             GO TO OPEN-ATRW-DONE-FILE.
       CLOSE-ATRW-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRW-DONE-FILE.
