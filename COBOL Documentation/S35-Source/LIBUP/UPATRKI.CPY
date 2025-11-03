      * COPYMEMBER: LIBUP/UPATRKI
      **************************************************************
      *                ASCII TRANSACTION FILE
      **************************************************************
       OPEN-ATRK-FILE.
           PERFORM OPEN-IT.
           MOVE ATRK-PATH TO E-FILE.
           OPEN I-O ATRK-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRK-FILE.
           IF IO-FG = 7
             CLOSE ATRK-FILE
             GO TO OPEN-ATRK-FILE.
           UNLOCK ATRK-FILE.
       READ-ATRK-FILE.
           PERFORM READ-IT.
           MOVE ATRK-PATH TO E-FILE.
           MOVE ATRK-KEY TO E-KEYX.
           READ ATRK-FILE.
           IF IO-FG = 8
             GO TO READ-ATRK-FILE.
       WRITE-ATRK-FILE.
           PERFORM WRITE-IT.
           MOVE ATRK-PATH TO E-FILE.
           MOVE ATRK-KEY TO E-KEYX.
           WRITE ATRK-REC.
           IF IO-FG = 8
             GO TO WRITE-ATRK-FILE.
           UNLOCK ATRK-FILE.
       REWRITE-ATRK-FILE.
           PERFORM REWRITE-IT.
           MOVE ATRK-PATH TO E-FILE.
           MOVE ATRK-KEY TO E-KEYX.
           REWRITE ATRK-REC.
           IF IO-FG = 8
             GO TO REWRITE-ATRK-FILE.
           UNLOCK ATRK-FILE.
       CLOSE-ATRK-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRK-FILE.
      **********************************************
      * ASCII TRANS FILE DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ATRK-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ATRK-DONE-PATH TO E-FILE.
           OPEN OUTPUT ATRK-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRK-DONE-FILE.
           IF IO-FG = 7
             CLOSE ATRK-DONE-FILE
             GO TO OPEN-ATRK-DONE-FILE.
       CLOSE-ATRK-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRK-DONE-FILE.
