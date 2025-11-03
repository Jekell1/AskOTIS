      * COPYMEMBER: LIBUP/UPALTPI
      **************************************************************
      *                (UPALTPI)
      **************************************************************
       OPEN-ALTP-FILE.
           PERFORM OPEN-IT.
           MOVE ALTP-PATH TO E-FILE.
           OPEN I-O ALTP-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALTP-FILE.
           IF IO-FG = 7
             CLOSE ALTP-FILE
             GO TO OPEN-ALTP-FILE.
           UNLOCK ALTP-FILE.
       READ-ALTP-FILE.
           PERFORM READ-IT.
           MOVE ALTP-PATH TO E-FILE.
           MOVE ALTP-KEY TO E-KEYX.
           READ ALTP-FILE.
           IF IO-FG = 8
             GO TO READ-ALTP-FILE.
       WRITE-ALTP-FILE.
           PERFORM WRITE-IT.
           MOVE ALTP-PATH TO E-FILE.
           MOVE ALTP-KEY TO E-KEYX.
           WRITE ALTP-REC.
           IF IO-FG = 8
             GO TO WRITE-ALTP-FILE.
           UNLOCK ALTP-FILE.
       REWRITE-ALTP-FILE.
           PERFORM REWRITE-IT.
           MOVE ALTP-PATH TO E-FILE.
           MOVE ALTP-KEY TO E-KEYX.
           REWRITE ALTP-REC.
           IF IO-FG = 8
             GO TO REWRITE-ALTP-FILE.
           UNLOCK ALTP-FILE.
       CLOSE-ALTP-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALTP-FILE.
      **********************************************
      * ASCII LOAN TRAILER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ALTP-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ALTP-DONE-PATH TO E-FILE.
           OPEN OUTPUT ALTP-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALTP-DONE-FILE.
           IF IO-FG = 7
             CLOSE ALTP-DONE-FILE
             GO TO OPEN-ALTP-DONE-FILE.
       CLOSE-ALTP-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALTP-DONE-FILE.
