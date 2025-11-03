      * COPYMEMBER: LIBUP/UPACJI
      **************************************************************
       OPEN-ACJ-FILE.
           PERFORM OPEN-IT.
           MOVE ACJ-PATH TO E-FILE.
           OPEN I-O ACJ-FILE.
           IF IO-FG = 8
             GO TO OPEN-ACJ-FILE.
           IF IO-FG = 7
             CLOSE ACJ-FILE
             GO TO OPEN-ACJ-FILE.
           UNLOCK ACJ-FILE.
       WRITE-ACJ-FILE.
           PERFORM WRITE-IT.
           MOVE ACJ-PATH TO E-FILE.
           MOVE ACJ-KEY TO E-KEYX.
           WRITE ACJ-REC.
           IF IO-FG = 8
             GO TO WRITE-ACJ-FILE.
           UNLOCK ACJ-FILE.
       REWRITE-ACJ-FILE.
           PERFORM REWRITE-IT.
           MOVE ACJ-PATH TO E-FILE.
           MOVE ACJ-KEY TO E-KEYX.
           REWRITE ACJ-REC.
           IF IO-FG = 8
             GO TO REWRITE-ACJ-FILE.
           UNLOCK ACJ-FILE.
       CLOSE-ACJ-FILE.
           PERFORM CLOSE-IT.
           CLOSE ACJ-FILE.
      **********************************************
      * ASCII COLLECTOR JOURNAL DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ACJ-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ACJ-DONE-PATH TO E-FILE.
           OPEN OUTPUT ACJ-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ACJ-DONE-FILE.
           IF IO-FG = 7
             CLOSE ACJ-DONE-FILE
             GO TO OPEN-ACJ-DONE-FILE.
       CLOSE-ACJ-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ACJ-DONE-FILE.
