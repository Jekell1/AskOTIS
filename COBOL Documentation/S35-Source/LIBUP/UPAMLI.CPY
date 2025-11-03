      * COPYMEMBER: LIBUP/UPAMLI
      **************************************************************
      *                (UPAMLI)
      **************************************************************
       OPEN-AML-FILE.
           PERFORM OPEN-IT.
           MOVE AML-PATH TO E-FILE.
           OPEN I-O AML-FILE.
           IF IO-FG = 8
             GO TO OPEN-AML-FILE.
           IF IO-FG = 7
             CLOSE AML-FILE
             GO TO OPEN-AML-FILE.
           UNLOCK AML-FILE.
       READ-AML-FILE.
           PERFORM READ-IT.
           MOVE AML-PATH TO E-FILE.
           MOVE AML-KEY TO E-KEYX.
           READ AML-FILE.
           IF IO-FG = 8
             GO TO READ-AML-FILE.
       WRITE-AML-FILE.
           PERFORM WRITE-IT.
           MOVE AML-PATH TO E-FILE.
           MOVE AML-KEY TO E-KEYX.
           WRITE AML-REC.
           IF IO-FG = 8
             GO TO WRITE-AML-FILE.
           UNLOCK AML-FILE.
       REWRITE-AML-FILE.
           PERFORM REWRITE-IT.
           MOVE AML-PATH TO E-FILE.
           MOVE AML-KEY TO E-KEYX.
           REWRITE AML-REC.
           IF IO-FG = 8
             GO TO REWRITE-AML-FILE.
           UNLOCK AML-FILE.
       CLOSE-AML-FILE.
           PERFORM CLOSE-IT.
           CLOSE AML-FILE.
      **********************************************
      * ASCII MAIL LIST DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-AML-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE AML-DONE-PATH TO E-FILE.
           OPEN OUTPUT AML-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-AML-DONE-FILE.
           IF IO-FG = 7
             CLOSE AML-DONE-FILE
             GO TO OPEN-AML-DONE-FILE.
       CLOSE-AML-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE AML-DONE-FILE.
