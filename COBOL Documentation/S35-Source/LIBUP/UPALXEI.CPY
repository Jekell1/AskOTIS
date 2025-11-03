      * COPYMEMBER: LIBUP/UPALXEI
      **************************************************************
      *                (UPALXEI)
      **************************************************************
       OPEN-ALXE-FILE.
           PERFORM OPEN-IT.
           MOVE ALXE-PATH TO E-FILE.
           OPEN I-O ALXE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALXE-FILE.
           IF IO-FG = 7
             CLOSE ALXE-FILE
             GO TO OPEN-ALXE-FILE.
           UNLOCK ALXE-FILE.
       READ-ALXE-FILE.
           PERFORM READ-IT.
           MOVE ALXE-PATH TO E-FILE.
           MOVE ALXE-KEY TO E-KEYX.
           READ ALXE-FILE.
           IF IO-FG = 8
             GO TO READ-ALXE-FILE.
       WRITE-ALXE-FILE.
           PERFORM WRITE-IT.
           MOVE ALXE-PATH TO E-FILE.
           MOVE ALXE-KEY TO E-KEYX.
           WRITE ALXE-REC.
           IF IO-FG = 8
             GO TO WRITE-ALXE-FILE.
           UNLOCK ALXE-FILE.
       REWRITE-ALXE-FILE.
           PERFORM REWRITE-IT.
           MOVE ALXE-PATH TO E-FILE.
           MOVE ALXE-KEY TO E-KEYX.
           REWRITE ALXE-REC.
           IF IO-FG = 8
             GO TO REWRITE-ALXE-FILE.
           UNLOCK ALXE-FILE.
       CLOSE-ALXE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALXE-FILE.
      **********************************************
      * ASCII LOAN PAYMENT TRAILER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ALXE-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ALXE-DONE-PATH TO E-FILE.
           OPEN OUTPUT ALXE-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALXE-DONE-FILE.
           IF IO-FG = 7
             CLOSE ALXE-DONE-FILE
             GO TO OPEN-ALXE-DONE-FILE.
       CLOSE-ALXE-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALXE-DONE-FILE.
