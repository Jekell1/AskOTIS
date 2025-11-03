      * COPYMEMBER: LIBUP/UPAGTI
      **************************************************************
      *                (UPAGTI)
      **************************************************************
       OPEN-AGT-FILE.
           PERFORM OPEN-IT.
           MOVE AGT-PATH TO E-FILE.
           OPEN I-O AGT-FILE.
           IF IO-FG = 8
             GO TO OPEN-AGT-FILE.
           IF IO-FG = 7
             CLOSE AGT-FILE
             GO TO OPEN-AGT-FILE.
           UNLOCK AGT-FILE.
       OPEN-AGT-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE AGT-PATH TO E-FILE.
           OPEN OUTPUT AGT-FILE.
           IF IO-FG = 8
             GO TO OPEN-AGT-FILE.
           IF IO-FG = 7
             CLOSE AGT-FILE
             GO TO OPEN-AGT-FILE.
           UNLOCK AGT-FILE.
       READ-AGT-FILE.
           PERFORM READ-IT.
           MOVE AGT-PATH TO E-FILE.
           MOVE AGT-KEY TO E-KEYX.
           READ AGT-FILE.
           IF IO-FG = 8
             GO TO READ-AGT-FILE.
       WRITE-AGT-FILE.
           PERFORM WRITE-IT.
           MOVE AGT-PATH TO E-FILE.
           MOVE AGT-KEY TO E-KEYX.
           WRITE AGT-REC.
           IF IO-FG = 8
             GO TO WRITE-AGT-FILE.
           UNLOCK AGT-FILE.
       REWRITE-AGT-FILE.
           PERFORM REWRITE-IT.
           MOVE AGT-PATH TO E-FILE.
           MOVE AGT-KEY TO E-KEYX.
           REWRITE AGT-REC.
           IF IO-FG = 8
             GO TO REWRITE-AGT-FILE.
           UNLOCK AGT-FILE.
       CLOSE-AGT-FILE.
           PERFORM CLOSE-IT.
           CLOSE AGT-FILE.
      **********************************************
      * ASCII GENERAL LEDGER TRANSACTIONS DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-AGT-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE AGT-DONE-PATH TO E-FILE.
           OPEN OUTPUT AGT-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-AGT-DONE-FILE.
           IF IO-FG = 7
             CLOSE AGT-DONE-FILE
             GO TO OPEN-AGT-DONE-FILE.
       CLOSE-AGT-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE AGT-DONE-FILE.
