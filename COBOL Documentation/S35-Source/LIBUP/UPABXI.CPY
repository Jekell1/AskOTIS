      * COPYMEMBER: LIBUP/UPABXI
      **************************************************************
      *                ASCII BORROWER TRAILER
      **************************************************************
       OPEN-ABX-FILE.
           PERFORM OPEN-IT.
           MOVE ABX-PATH TO E-FILE.
           OPEN I-O ABX-FILE.
           IF IO-FG = 8
             GO TO OPEN-ABX-FILE.
           IF IO-FG = 7
             CLOSE ABX-FILE
             GO TO OPEN-ABX-FILE.
           UNLOCK ABX-FILE.
       WRITE-ABX-FILE.
           PERFORM WRITE-IT.
           MOVE ABX-PATH TO E-FILE.
           MOVE ABX-KEY TO E-KEYX.
           WRITE ABX-REC.
           IF IO-FG = 8
             GO TO WRITE-ABX-FILE.
           UNLOCK ABX-FILE.
       REWRITE-ABX-FILE.
           PERFORM REWRITE-IT.
           MOVE ABX-PATH TO E-FILE.
           MOVE ABX-KEY TO E-KEYX.
           REWRITE ABX-REC.
           IF IO-FG = 8
             GO TO REWRITE-ABX-FILE.
           UNLOCK ABX-FILE.
       CLOSE-ABX-FILE.
           PERFORM CLOSE-IT.
           CLOSE ABX-FILE.

      **********************************************
      * ASCII BORROWER TRAILER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ABX-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ABX-DONE-PATH TO E-FILE.
           OPEN OUTPUT ABX-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ABX-DONE-FILE.
           IF IO-FG = 7
             CLOSE ABX-DONE-FILE
             GO TO OPEN-ABX-DONE-FILE.
       CLOSE-ABX-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ABX-DONE-FILE.
