      * COPYMEMBER: LIBUP/UPARCI
      **************************************************************
      *                (UPARCI)
      **************************************************************
       OPEN-ARC-FILE.
           PERFORM OPEN-IT.
           MOVE ARC-PATH TO E-FILE.
           OPEN I-O ARC-FILE.
           IF IO-FG = 8
             GO TO OPEN-ARC-FILE.
           IF IO-FG = 7
             CLOSE ARC-FILE
             GO TO OPEN-ARC-FILE.
           UNLOCK ARC-FILE.
       READ-ARC-FILE.
           PERFORM READ-IT.
           MOVE ARC-PATH TO E-FILE.
           MOVE ARC-KEY TO E-KEYX.
           READ ARC-FILE.
           IF IO-FG = 8
             GO TO READ-ARC-FILE.
       WRITE-ARC-FILE.
           PERFORM WRITE-IT.
           MOVE ARC-PATH TO E-FILE.
           MOVE ARC-KEY TO E-KEYX.
           WRITE ARC-REC.
           IF IO-FG = 8
             GO TO WRITE-ARC-FILE.
           UNLOCK ARC-FILE.
       REWRITE-ARC-FILE.
           PERFORM REWRITE-IT.
           MOVE ARC-PATH TO E-FILE.
           MOVE ARC-KEY TO E-KEYX.
           REWRITE ARC-REC.
           IF IO-FG = 8
             GO TO REWRITE-ARC-FILE.
           UNLOCK ARC-FILE.
       CLOSE-ARC-FILE.
           PERFORM CLOSE-IT.
           CLOSE ARC-FILE.
      **********************************************
      * ASCII DAILY STATUS  DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ARC-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ARC-DONE-PATH TO E-FILE.
           OPEN OUTPUT ARC-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ARC-DONE-FILE.
           IF IO-FG = 7
             CLOSE ARC-DONE-FILE
             GO TO OPEN-ARC-DONE-FILE.
       CLOSE-ARC-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ARC-DONE-FILE.
