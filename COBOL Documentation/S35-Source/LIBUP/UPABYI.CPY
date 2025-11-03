      * COPYMEMBER: LIBUP/UPABYAI
      **************************************************************
      *                (UPABYAI)
      **************************************************************
       OPEN-ABY-FILE.
           PERFORM OPEN-ABYA-FILE.
           IF IO-FG = 0
              PERFORM OPEN-ABYB-FILE.

       OPEN-ABYA-FILE.
           PERFORM OPEN-IT.
           MOVE ABYA-PATH TO E-FILE.
           OPEN I-O ABYA-FILE.
           IF IO-FG = 8
             GO TO OPEN-ABYA-FILE.
           IF IO-FG = 7
             CLOSE ABYA-FILE
             GO TO OPEN-ABYA-FILE.
           UNLOCK ABYA-FILE.
       OPEN-ABYB-FILE.
           PERFORM OPEN-IT.
           MOVE ABYB-PATH TO E-FILE.
           OPEN I-O ABYB-FILE.
           IF IO-FG = 8
             GO TO OPEN-ABYB-FILE.
           IF IO-FG = 7
             CLOSE ABYB-FILE
             GO TO OPEN-ABYB-FILE.
           UNLOCK ABYB-FILE.
       WRITE-ABY-FILE.
           PERFORM WRITE-ABYA-FILE.
           IF IO-FG = 0
              PERFORM WRITE-ABYB-FILE.

       WRITE-ABYA-FILE.
           PERFORM WRITE-IT.
           MOVE ABYA-PATH TO E-FILE.
           MOVE ABYA-KEY TO E-KEYX.
           WRITE ABYA-REC.
           IF IO-FG = 8
             GO TO WRITE-ABYA-FILE.
           UNLOCK ABYA-FILE.
       WRITE-ABYB-FILE.
           PERFORM WRITE-IT.
           MOVE ABYB-PATH TO E-FILE.
           MOVE ABYB-KEY TO E-KEYX.
           WRITE ABYB-REC.
           IF IO-FG = 8
             GO TO WRITE-ABYB-FILE.
           UNLOCK ABYB-FILE.
       CLOSE-ABY-FILE.
           PERFORM CLOSE-ABYA-FILE.
           PERFORM CLOSE-ABYB-FILE.
       CLOSE-ABYA-FILE.
           PERFORM CLOSE-IT.
           CLOSE ABYA-FILE.
       CLOSE-ABYB-FILE.
           PERFORM CLOSE-IT.
           CLOSE ABYB-FILE.
      **********************************************
      * ASCII BRANCH STAT  DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ABY-DONE-FILE.
           PERFORM OPEN-ABYA-DONE-FILE.
           IF IO-FG = 0
              PERFORM OPEN-ABYB-DONE-FILE.

       OPEN-ABYA-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ABYA-DONE-PATH TO E-FILE.
           OPEN OUTPUT ABYA-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ABYA-DONE-FILE.
           IF IO-FG = 7
             CLOSE ABYA-DONE-FILE
             GO TO OPEN-ABYA-DONE-FILE.
       OPEN-ABYB-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ABYB-DONE-PATH TO E-FILE.
           OPEN OUTPUT ABYB-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ABYB-DONE-FILE.
           IF IO-FG = 7
             CLOSE ABYB-DONE-FILE
             GO TO OPEN-ABYB-DONE-FILE.
       CLOSE-ABY-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ABYA-DONE-FILE.
           CLOSE ABYB-DONE-FILE.
