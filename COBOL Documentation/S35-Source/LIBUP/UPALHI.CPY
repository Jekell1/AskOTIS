      * COPYMEMBER: LIBUP/UPALHI
      **************************************************************
      *                (UPALHI)
      **************************************************************
       OPEN-ALH-FILE.
           PERFORM OPEN-IT.
           MOVE ALH-PATH TO E-FILE.
           OPEN I-O ALH-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALH-FILE.
           IF IO-FG = 7
             CLOSE ALH-FILE
             GO TO OPEN-ALH-FILE.
           UNLOCK ALH-FILE.
       WRITE-ALH-FILE.
           PERFORM WRITE-IT.
           MOVE ALH-PATH TO E-FILE.
           MOVE ALH-KEY TO E-KEYX.
           WRITE ALH-REC.
           IF IO-FG = 8
             GO TO WRITE-ALH-FILE.
           UNLOCK ALH-FILE.
       REWRITE-ALH-FILE.
           PERFORM REWRITE-IT.
           MOVE ALH-PATH TO E-FILE.
           MOVE ALH-KEY TO E-KEYX.
           REWRITE ALH-REC.
           IF IO-FG = 8
             GO TO REWRITE-ALH-FILE.
           UNLOCK ALH-FILE.
       CLOSE-ALH-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALH-FILE.
      **********************************************
      * ASCII LOAN HISTORY DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ALH-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ALH-DONE-PATH TO E-FILE.
           OPEN OUTPUT ALH-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALH-DONE-FILE.
           IF IO-FG = 7
             CLOSE ALH-DONE-FILE
             GO TO OPEN-ALH-DONE-FILE.
       CLOSE-ALH-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALH-DONE-FILE.
