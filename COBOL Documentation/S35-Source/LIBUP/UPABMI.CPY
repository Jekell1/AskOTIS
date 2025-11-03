      * COPYMEMBER: LIBUP/UPABMI
      **************************************************************
      *                ASCII BORROWER MASTER
      **************************************************************
       OPEN-ABM-FILE.
           PERFORM OPEN-IT.
           MOVE ABM-PATH TO E-FILE.
           OPEN I-O ABM-FILE.
           IF IO-FG = 8
             GO TO OPEN-ABM-FILE.
           IF IO-FG = 7
             CLOSE ABM-FILE
             GO TO OPEN-ABM-FILE.
           UNLOCK ABM-FILE.
       WRITE-ABM-FILE.
           PERFORM WRITE-IT.
           MOVE ABM-PATH TO E-FILE.
           MOVE ABM-KEY TO E-KEYX.
           WRITE ABM-REC.
           IF IO-FG = 8
             GO TO WRITE-ABM-FILE.
           UNLOCK ABM-FILE.
       REWRITE-ABM-FILE.
           PERFORM REWRITE-IT.
           MOVE ABM-PATH TO E-FILE.
           MOVE ABM-KEY TO E-KEYX.
           REWRITE ABM-REC.
           IF IO-FG = 8
             GO TO REWRITE-ABM-FILE.
           UNLOCK ABM-FILE.
       CLOSE-ABM-FILE.
           PERFORM CLOSE-IT.
           CLOSE ABM-FILE.

      **********************************************
      * ASCII BORROWER MEMO DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ABM-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ABM-DONE-PATH TO E-FILE.
           OPEN OUTPUT ABM-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ABM-DONE-FILE.
           IF IO-FG = 7
             CLOSE ABM-DONE-FILE
             GO TO OPEN-ABM-DONE-FILE.
       CLOSE-ABM-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ABM-DONE-FILE.
