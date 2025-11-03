      * COPYMEMBER: LIBUP/UPALTBI
      **************************************************************
      *                (UPALTBI)
      **************************************************************
       OPEN-ALTB-FILE.
           PERFORM OPEN-IT.
           MOVE ALTB-PATH TO E-FILE.
           OPEN I-O ALTB-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALTB-FILE.
           IF IO-FG = 7
             CLOSE ALTB-FILE
             GO TO OPEN-ALTB-FILE.
           UNLOCK ALTB-FILE.
       READ-ALTB-FILE.
           PERFORM READ-IT.
           MOVE ALTB-PATH TO E-FILE.
           MOVE ALTB-KEY TO E-KEYX.
           READ ALTB-FILE.
           IF IO-FG = 8
             GO TO READ-ALTB-FILE.
       WRITE-ALTB-FILE.
           PERFORM WRITE-IT.
           MOVE ALTB-PATH TO E-FILE.
           MOVE ALTB-KEY TO E-KEYX.
           WRITE ALTB-REC.
           IF IO-FG = 8
             GO TO WRITE-ALTB-FILE.
           UNLOCK ALTB-FILE.
       REWRITE-ALTB-FILE.
           PERFORM REWRITE-IT.
           MOVE ALTB-PATH TO E-FILE.
           MOVE ALTB-KEY TO E-KEYX.
           REWRITE ALTB-REC.
           IF IO-FG = 8
             GO TO REWRITE-ALTB-FILE.
           UNLOCK ALTB-FILE.
       CLOSE-ALTB-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALTB-FILE.

      **********************************************
      * ASCII LOAN TRAILER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ALTB-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ALTB-DONE-PATH TO E-FILE.
           OPEN OUTPUT ALTB-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALTB-DONE-FILE.
           IF IO-FG = 7
             CLOSE ALTB-DONE-FILE
             GO TO OPEN-ALTB-DONE-FILE.
       CLOSE-ALTB-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALTB-DONE-FILE.
