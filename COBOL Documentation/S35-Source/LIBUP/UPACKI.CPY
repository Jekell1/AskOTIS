      * COPYMEMBER: LIBUP/UPACKI
      **************************************************************
      *                ASCII CHECK FILE
      **************************************************************
       OPEN-ACK-FILE.
           PERFORM OPEN-IT.
           MOVE ACK-PATH TO E-FILE.
           OPEN I-O ACK-FILE.
           IF IO-FG = 8
             GO TO OPEN-ACK-FILE.
           IF IO-FG = 7
             CLOSE ACK-FILE
             GO TO OPEN-ACK-FILE.
           UNLOCK ACK-FILE.
       WRITE-ACK-FILE.
           PERFORM WRITE-IT.
           MOVE ACK-PATH TO E-FILE.
           MOVE ACK-KEY TO E-KEYX.
           WRITE ACK-REC.
           IF IO-FG = 8
             GO TO WRITE-ACK-FILE.
           UNLOCK ACK-FILE.
       REWRITE-ACK-FILE.
           PERFORM REWRITE-IT.
           MOVE ACK-PATH TO E-FILE.
           MOVE ACK-KEY TO E-KEYX.
           REWRITE ACK-REC.
           IF IO-FG = 8
             GO TO REWRITE-ACK-FILE.
           UNLOCK ACK-FILE.
       CLOSE-ACK-FILE.
           PERFORM CLOSE-IT.
           CLOSE ACK-FILE.
      **********************************************
      * ASCII CHECK TRAILER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ACK-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ACK-DONE-PATH TO E-FILE.
           OPEN OUTPUT ACK-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ACK-DONE-FILE.
           IF IO-FG = 7
             CLOSE ACK-DONE-FILE
             GO TO OPEN-ACK-DONE-FILE.
       CLOSE-ACK-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ACK-DONE-FILE.
