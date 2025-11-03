      * COPYMEMBER: LIBUP/UPATRCI
      **************************************************************
      *                ASCII TRANSACTION FILE
      **************************************************************
       OPEN-ATRC-FILE.
           PERFORM OPEN-IT.
           MOVE ATRC-PATH TO E-FILE.
           OPEN I-O ATRC-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRC-FILE.
           IF IO-FG = 7
             CLOSE ATRC-FILE
             GO TO OPEN-ATRC-FILE.
           UNLOCK ATRC-FILE.
       OPEN-ATRC-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE ATRC-PATH TO E-FILE.
           OPEN OUTPUT ATRC-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRC-FILE.
           IF IO-FG = 7
             CLOSE ATRC-FILE
             GO TO OPEN-ATRC-FILE.
           UNLOCK ATRC-FILE.
       READ-ATRC-FILE.
           PERFORM READ-IT.
           MOVE ATRC-PATH TO E-FILE.
           MOVE ATRC-KEY TO E-KEYX.
           READ ATRC-FILE.
           IF IO-FG = 8
             GO TO READ-ATRC-FILE.
       WRITE-ATRC-FILE.
           PERFORM WRITE-IT.
           MOVE ATRC-PATH TO E-FILE.
           MOVE ATRC-KEY TO E-KEYX.
           WRITE ATRC-REC.
           IF IO-FG = 8
             GO TO WRITE-ATRC-FILE.
           UNLOCK ATRC-FILE.
       REWRITE-ATRC-FILE.
           PERFORM REWRITE-IT.
           MOVE ATRC-PATH TO E-FILE.
           MOVE ATRC-KEY TO E-KEYX.
           REWRITE ATRC-REC.
           IF IO-FG = 8
             GO TO REWRITE-ATRC-FILE.
           UNLOCK ATRC-FILE.
       CLOSE-ATRC-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRC-FILE.
      **********************************************
      * ASCII TRANS FILE DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ATRC-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ATRC-DONE-PATH TO E-FILE.
           OPEN OUTPUT ATRC-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRC-DONE-FILE.
           IF IO-FG = 7
             CLOSE ATRC-DONE-FILE
             GO TO OPEN-ATRC-DONE-FILE.
       CLOSE-ATRC-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRC-DONE-FILE.
