      * COPYMEMBER: LIBUP/UPALXI
      **************************************************************
      *                (UPALXI)
      **************************************************************
       OPEN-ALX-FILE.
           PERFORM OPEN-IT.
           MOVE ALX-PATH TO E-FILE.
           OPEN I-O ALX-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALX-FILE.
           IF IO-FG = 7
             CLOSE ALX-FILE
             GO TO OPEN-ALX-FILE.
           UNLOCK ALX-FILE.
       READ-ALX-FILE.
           PERFORM READ-IT.
           MOVE ALX-PATH TO E-FILE.
           MOVE ALX-KEY TO E-KEYX.
           READ ALX-FILE.
           IF IO-FG = 8
             GO TO READ-ALX-FILE.
       WRITE-ALX-FILE.
           PERFORM WRITE-IT.
           MOVE ALX-PATH TO E-FILE.
           MOVE ALX-KEY TO E-KEYX.
           WRITE ALX-REC.
           IF IO-FG = 8
             GO TO WRITE-ALX-FILE.
           UNLOCK ALX-FILE.
       REWRITE-ALX-FILE.
           PERFORM REWRITE-IT.
           MOVE ALX-PATH TO E-FILE.
           MOVE ALX-KEY TO E-KEYX.
           REWRITE ALX-REC.
           IF IO-FG = 8
             GO TO REWRITE-ALX-FILE.
           UNLOCK ALX-FILE.
       CLOSE-ALX-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALX-FILE.
      **********************************************
      * ASCII LOAN PAYMENT TRAILER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ALX-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ALX-DONE-PATH TO E-FILE.
           OPEN OUTPUT ALX-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALX-DONE-FILE.
           IF IO-FG = 7
             CLOSE ALX-DONE-FILE
             GO TO OPEN-ALX-DONE-FILE.
       CLOSE-ALX-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALX-DONE-FILE.
