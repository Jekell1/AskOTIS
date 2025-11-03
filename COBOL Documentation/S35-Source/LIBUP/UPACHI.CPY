      * COPYMEMBER: LIBUP/UPACHI
      **************************************************************
      *                ASCII CHANGES/AUDIT FILE
      **************************************************************
       OPEN-ACH-FILE.
           PERFORM OPEN-IT.
           MOVE ACH-PATH TO E-FILE.
           OPEN I-O ACH-FILE.
           IF IO-FG = 8
             GO TO OPEN-ACH-FILE.
           IF IO-FG = 7
             CLOSE ACH-FILE
             GO TO OPEN-ACH-FILE.
           UNLOCK ACH-FILE.
       READ-ACH-FILE.
           PERFORM READ-IT.
           MOVE ACH-PATH TO E-FILE.
           MOVE ACH-KEY TO E-KEYX.
           READ ACH-FILE.
           IF IO-FG = 8
             GO TO READ-ACH-FILE.
       WRITE-ACH-FILE.
           PERFORM WRITE-IT.
           MOVE ACH-PATH TO E-FILE.
           MOVE ACH-KEY TO E-KEYX.
           WRITE ACH-REC.
           IF IO-FG = 8
             GO TO WRITE-ACH-FILE.
           UNLOCK ACH-FILE.
       REWRITE-ACH-FILE.
           PERFORM REWRITE-IT.
           MOVE ACH-PATH TO E-FILE.
           MOVE ACH-KEY TO E-KEYX.
           REWRITE ACH-REC.
           IF IO-FG = 8
             GO TO REWRITE-ACH-FILE.
           UNLOCK ACH-FILE.
       CLOSE-ACH-FILE.
           PERFORM CLOSE-IT.
           CLOSE ACH-FILE.
      **********************************************
      * ASCII CHANGES/AUDIT FILE DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ACH-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ACH-DONE-PATH TO E-FILE.
           OPEN OUTPUT ACH-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ACH-DONE-FILE.
           IF IO-FG = 7
             CLOSE ACH-DONE-FILE
             GO TO OPEN-ACH-DONE-FILE.
       CLOSE-ACH-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ACH-DONE-FILE.
