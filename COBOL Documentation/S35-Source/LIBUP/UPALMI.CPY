      * COPYMEMBER: LIBUP/UPALMI
      **************************************************************
      *                (EXALMI)
      **************************************************************
       OPEN-ALM-FILE.
           PERFORM OPEN-IT.
           MOVE ALM-PATH TO E-FILE.
           OPEN I-O ALM-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALM-FILE.
           IF IO-FG = 7
             CLOSE ALM-FILE
             GO TO OPEN-ALM-FILE.
           UNLOCK ALM-FILE.
       WRITE-ALM-FILE.
           PERFORM WRITE-IT.
           MOVE ALM-PATH TO E-FILE.
           MOVE ALM-KEY TO E-KEYX.
           WRITE ALM-REC.
           IF IO-FG = 8
             GO TO WRITE-ALM-FILE.
           UNLOCK ALM-FILE.
       REWRITE-ALM-FILE.
           PERFORM REWRITE-IT.
           MOVE ALM-PATH TO E-FILE.
           MOVE ALM-KEY TO E-KEYX.
           REWRITE ALM-REC.
           IF IO-FG = 8
             GO TO REWRITE-ALM-FILE.
           UNLOCK ALM-FILE.
       CLOSE-ALM-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALM-FILE.
      **********************************************
      * ASCII LOAN MEMO DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ALM-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ALM-DONE-PATH TO E-FILE.
           OPEN OUTPUT ALM-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALM-DONE-FILE.
           IF IO-FG = 7
             CLOSE ALM-DONE-FILE
             GO TO OPEN-ALM-DONE-FILE.
       CLOSE-ALM-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALM-DONE-FILE.
