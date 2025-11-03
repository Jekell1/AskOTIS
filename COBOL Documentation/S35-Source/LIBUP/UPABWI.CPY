      * COPYMEMBER: LIBUP/UPABWI
      **************************************************************
      *                ASCII BORROWER MASTER
      **************************************************************
       OPEN-ABW-FILE.
           PERFORM OPEN-IT.
           MOVE ABW-PATH TO E-FILE.
           OPEN I-O ABW-FILE.
           IF IO-FG = 8
             GO TO OPEN-ABW-FILE.
           IF IO-FG = 7
             CLOSE ABW-FILE
             GO TO OPEN-ABW-FILE.
           UNLOCK ABW-FILE.
       READ-ABW-FILE.
           PERFORM READ-IT.
           MOVE ABW-PATH TO E-FILE.
           MOVE ABW-KEY TO E-KEYX.
           READ ABW-FILE.
           IF IO-FG = 8
             GO TO READ-ABW-FILE.
       WRITE-ABW-FILE.
           PERFORM WRITE-IT.
           MOVE ABW-PATH TO E-FILE.
           MOVE ABW-KEY TO E-KEYX.
           WRITE ABW-REC.
           IF IO-FG = 8
             GO TO WRITE-ABW-FILE.
           UNLOCK ABW-FILE.
       REWRITE-ABW-FILE.
           PERFORM REWRITE-IT.
           MOVE ABW-PATH TO E-FILE.
           MOVE ABW-KEY TO E-KEYX.
           REWRITE ABW-REC.
           IF IO-FG = 8
             GO TO REWRITE-ABW-FILE.
           UNLOCK ABW-FILE.
       CLOSE-ABW-FILE.
           PERFORM CLOSE-IT.
           CLOSE ABW-FILE.

      **********************************************
      * ASCII BORROWER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ABW-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ABW-DONE-PATH TO E-FILE.
           OPEN OUTPUT ABW-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ABW-DONE-FILE.
           IF IO-FG = 7
             CLOSE ABW-DONE-FILE
             GO TO OPEN-ABW-DONE-FILE.
       CLOSE-ABW-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ABW-DONE-FILE.
