      * COPYMEMBER: LIBUP/UPALPI
      **************************************************************
      *                (UPALPI)
      **************************************************************
       OPEN-ALP-FILE.
           PERFORM OPEN-IT.
           MOVE ALP-PATH TO E-FILE.
           OPEN I-O ALP-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALP-FILE.
           IF IO-FG = 7
             CLOSE ALP-FILE
             GO TO OPEN-ALP-FILE.
           UNLOCK ALP-FILE.
       WRITE-ALP-FILE.
           PERFORM WRITE-IT.
           MOVE ALP-PATH TO E-FILE.
           MOVE ALP-KEY TO E-KEYX.
           WRITE ALP-REC.
           IF IO-FG = 8
             GO TO WRITE-ALP-FILE.
           UNLOCK ALP-FILE.
       REWRITE-ALP-FILE.
           PERFORM REWRITE-IT.
           MOVE ALP-PATH TO E-FILE.
           MOVE ALP-KEY TO E-KEYX.
           REWRITE ALP-REC.
           IF IO-FG = 8
             GO TO REWRITE-ALP-FILE.
           UNLOCK ALP-FILE.
       CLOSE-ALP-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALP-FILE.
      **********************************************
      * ASCII PAYMENT DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ALP-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ALP-DONE-PATH TO E-FILE.
           OPEN OUTPUT ALP-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALP-DONE-FILE.
           IF IO-FG = 7
             CLOSE ALP-DONE-FILE
             GO TO OPEN-ALP-DONE-FILE.
       CLOSE-ALP-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALP-DONE-FILE.
