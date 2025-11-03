      * COPYMEMBER: LIBUP/UPACAI
      **************************************************************
      *                ASCII CASH DRAWER
      **************************************************************
       OPEN-ACA-FILE.
           PERFORM OPEN-IT.
           MOVE ACA-PATH TO E-FILE.
           OPEN I-O ACA-FILE.
           IF IO-FG = 8
             GO TO OPEN-ACA-FILE.
           IF IO-FG = 7
             CLOSE ACA-FILE
             GO TO OPEN-ACA-FILE.
           UNLOCK ACA-FILE.
       OPEN-ACA-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE ACA-PATH TO E-FILE.
           OPEN OUTPUT ACA-FILE.
           IF IO-FG = 8
             GO TO OPEN-ACA-FILE.
           IF IO-FG = 7
             CLOSE ACA-FILE
             GO TO OPEN-ACA-FILE.
           UNLOCK ACA-FILE.
       READ-ACA-FILE.
           PERFORM READ-IT.
           MOVE ACA-PATH TO E-FILE.
           MOVE ACA-KEY TO E-KEYX.
           READ ACA-FILE.
           IF IO-FG = 8
             GO TO READ-ACA-FILE.
       WRITE-ACA-FILE.
           PERFORM WRITE-IT.
           MOVE ACA-PATH TO E-FILE.
           MOVE ACA-KEY TO E-KEYX.
           WRITE ACA-REC.
           IF IO-FG = 8
             GO TO WRITE-ACA-FILE.
           UNLOCK ACA-FILE.
       REWRITE-ACA-FILE.
           PERFORM REWRITE-IT.
           MOVE ACA-PATH TO E-FILE.
           MOVE ACA-KEY TO E-KEYX.
           REWRITE ACA-REC.
           IF IO-FG = 8
             GO TO REWRITE-ACA-FILE.
           UNLOCK ACA-FILE.
       CLOSE-ACA-FILE.
           PERFORM CLOSE-IT.
           CLOSE ACA-FILE.

      **********************************************
      * ASCII CASH DRAWER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ACA-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ACA-DONE-PATH TO E-FILE.
           OPEN OUTPUT ACA-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ACA-DONE-FILE.
           IF IO-FG = 7
             CLOSE ACA-DONE-FILE
             GO TO OPEN-ACA-DONE-FILE.
       CLOSE-ACA-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ACA-DONE-FILE.
