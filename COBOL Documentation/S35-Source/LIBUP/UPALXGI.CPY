      * COPYMEMBER: LIBUP/UPALXGI
      **************************************************************
      *                (UPALXGI)
      **************************************************************
       OPEN-ALXG-FILE.
           PERFORM OPEN-IT.
           MOVE ALXG-PATH TO E-FILE.
           OPEN I-O ALXG-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALXG-FILE.
           IF IO-FG = 7
             CLOSE ALXG-FILE
             GO TO OPEN-ALXG-FILE.
           UNLOCK ALXG-FILE.
       READ-ALXG-FILE.
           PERFORM READ-IT.
           MOVE ALXG-PATH TO E-FILE.
           MOVE ALXG-KEY TO E-KEYX.
           READ ALXG-FILE.
           IF IO-FG = 8
             GO TO READ-ALXG-FILE.
       WRITE-ALXG-FILE.
           PERFORM WRITE-IT.
           MOVE ALXG-PATH TO E-FILE.
           MOVE ALXG-KEY TO E-KEYX.
           WRITE ALXG-REC.
           IF IO-FG = 8
             GO TO WRITE-ALXG-FILE.
           UNLOCK ALXG-FILE.
       REWRITE-ALXG-FILE.
           PERFORM REWRITE-IT.
           MOVE ALXG-PATH TO E-FILE.
           MOVE ALXG-KEY TO E-KEYX.
           REWRITE ALXG-REC.
           IF IO-FG = 8
             GO TO REWRITE-ALXG-FILE.
           UNLOCK ALXG-FILE.
       CLOSE-ALXG-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALXG-FILE.
      **********************************************
      * ASCII LOAN PAYMENT TRAILER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ALXG-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ALXG-DONE-PATH TO E-FILE.
           OPEN OUTPUT ALXG-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALXG-DONE-FILE.
           IF IO-FG = 7
             CLOSE ALXG-DONE-FILE
             GO TO OPEN-ALXG-DONE-FILE.
       CLOSE-ALXG-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALXG-DONE-FILE.
