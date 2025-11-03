      * COPYMEMBER: LIBUP/UPAIXI
      **************************************************************
      *                (UPAIXI)
      **************************************************************
       OPEN-AIX-FILE.
           PERFORM OPEN-IT.
           MOVE AIX-PATH TO E-FILE.
           OPEN I-O AIX-FILE.
           IF IO-FG = 8
             GO TO OPEN-AIX-FILE.
           IF IO-FG = 7
             CLOSE AIX-FILE
             GO TO OPEN-AIX-FILE.
           UNLOCK AIX-FILE.
       WRITE-AIX-FILE.
           PERFORM WRITE-IT.
           MOVE AIX-PATH TO E-FILE.
           MOVE AIX-KEY TO E-KEYX.
           WRITE AIX-REC.
           IF IO-FG = 8
             GO TO WRITE-AIX-FILE.
           UNLOCK AIX-FILE.
       REWRITE-AIX-FILE.
           PERFORM REWRITE-IT.
           MOVE AIX-PATH TO E-FILE.
           MOVE AIX-KEY TO E-KEYX.
           REWRITE AIX-REC.
           IF IO-FG = 8
             GO TO REWRITE-AIX-FILE.
           UNLOCK AIX-FILE.
       CLOSE-AIX-FILE.
           PERFORM CLOSE-IT.
           CLOSE AIX-FILE.
      **********************************************
      * ASCII ALTERNATE INDEX DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-AIX-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE AIX-DONE-PATH TO E-FILE.
           OPEN OUTPUT AIX-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-AIX-DONE-FILE.
           IF IO-FG = 7
             CLOSE AIX-DONE-FILE
             GO TO OPEN-AIX-DONE-FILE.
       CLOSE-AIX-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE AIX-DONE-FILE.
