      **************************************************************
      *                (UPAWRI)
      **************************************************************
       OPEN-AWR-FILE.
           PERFORM OPEN-IT.
           MOVE AWR-PATH TO E-FILE.
           OPEN I-O AWR-FILE.
           IF IO-FG = 8
             GO TO OPEN-AWR-FILE.
           IF IO-FG = 7
             CLOSE AWR-FILE
             GO TO OPEN-AWR-FILE.
           UNLOCK AWR-FILE.
       READ-AWR-FILE.
           PERFORM READ-IT.
           MOVE AWR-PATH TO E-FILE.
           MOVE AWR-KEY TO E-KEYX.
           READ AWR-FILE.
           IF IO-FG = 8
             GO TO READ-AWR-FILE.
       WRITE-AWR-FILE.
           PERFORM WRITE-IT.
           MOVE AWR-PATH TO E-FILE.
           MOVE AWR-KEY TO E-KEYX.
           WRITE AWR-REC.
           IF IO-FG = 8
             GO TO WRITE-AWR-FILE.
           UNLOCK AWR-FILE.
       REWRITE-AWR-FILE.
           PERFORM REWRITE-IT.
           MOVE AWR-PATH TO E-FILE.
           MOVE AWR-KEY TO E-KEYX.
           REWRITE AWR-REC.
           IF IO-FG = 8
             GO TO REWRITE-AWR-FILE.
           UNLOCK AWR-FILE.
       CLOSE-AWR-FILE.
           PERFORM CLOSE-IT.
           CLOSE AWR-FILE.
      **********************************************
      * ASCII RATING SHEET  DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-AWR-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE AWR-DONE-PATH TO E-FILE.
           OPEN OUTPUT AWR-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-AWR-DONE-FILE.
           IF IO-FG = 7
             CLOSE AWR-DONE-FILE
             GO TO OPEN-AWR-DONE-FILE.
       CLOSE-AWR-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE AWR-DONE-FILE.
