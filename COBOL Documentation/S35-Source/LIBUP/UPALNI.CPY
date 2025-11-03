      * COPYMEMBER: LIBUP/UPALNI
      **************************************************************
      *                (UPALNI)
      **************************************************************
       OPEN-ALN-FILE.
           PERFORM OPEN-IT.
           MOVE ALN-PATH TO E-FILE.
           OPEN I-O ALN-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALN-FILE.
           IF IO-FG = 7
             CLOSE ALN-FILE
             GO TO OPEN-ALN-FILE.
           UNLOCK ALN-FILE.
       WRITE-ALN-FILE.
           PERFORM WRITE-IT.
           MOVE ALN-PATH TO E-FILE.
           MOVE ALN-KEY TO E-KEYX.
           WRITE ALN-REC.
           IF IO-FG = 8
             GO TO WRITE-ALN-FILE.
           UNLOCK ALN-FILE.
       REWRITE-ALN-FILE.
           PERFORM REWRITE-IT.
           MOVE ALN-PATH TO E-FILE.
           MOVE ALN-KEY TO E-KEYX.
           REWRITE ALN-REC.
           IF IO-FG = 8
             GO TO REWRITE-ALN-FILE.
           UNLOCK ALN-FILE.
       CLOSE-ALN-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALN-FILE.
      **********************************************
      * ASCII LOAN MASTER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ALN-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ALN-DONE-PATH TO E-FILE.
           OPEN OUTPUT ALN-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALN-DONE-FILE.
           IF IO-FG = 7
             CLOSE ALN-DONE-FILE
             GO TO OPEN-ALN-DONE-FILE.
       CLOSE-ALN-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALN-DONE-FILE.
