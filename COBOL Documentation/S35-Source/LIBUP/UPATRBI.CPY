      * COPYMEMBER: LIBUP/UPATRBI
      **************************************************************
      *                ASCII TRANSACTION FILE
      **************************************************************
       OPEN-ATRB-FILE.
           PERFORM OPEN-IT.
           MOVE ATRB-PATH TO E-FILE.
           OPEN I-O ATRB-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRB-FILE.
           IF IO-FG = 7
             CLOSE ATRB-FILE
             GO TO OPEN-ATRB-FILE.
           UNLOCK ATRB-FILE.
       READ-ATRB-FILE.
           PERFORM READ-IT.
           MOVE ATRB-PATH TO E-FILE.
           MOVE ATRB-KEY TO E-KEYX.
           READ ATRB-FILE.
           IF IO-FG = 8
             GO TO READ-ATRB-FILE.
       WRITE-ATRB-FILE.
           PERFORM WRITE-IT.
           MOVE ATRB-PATH TO E-FILE.
           MOVE ATRB-KEY TO E-KEYX.
           WRITE ATRB-REC.
           IF IO-FG = 8
             GO TO WRITE-ATRB-FILE.
           UNLOCK ATRB-FILE.
       REWRITE-ATRB-FILE.
           PERFORM REWRITE-IT.
           MOVE ATRB-PATH TO E-FILE.
           MOVE ATRB-KEY TO E-KEYX.
           REWRITE ATRB-REC.
           IF IO-FG = 8
             GO TO REWRITE-ATRB-FILE.
           UNLOCK ATRB-FILE.
       CLOSE-ATRB-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRB-FILE.

      **********************************************
      * ASCII TRANS FILE DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ATRB-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ATRB-DONE-PATH TO E-FILE.
           OPEN OUTPUT ATRB-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRB-DONE-FILE.
           IF IO-FG = 7
             CLOSE ATRB-DONE-FILE
             GO TO OPEN-ATRB-DONE-FILE.
       CLOSE-ATRB-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRB-DONE-FILE.
