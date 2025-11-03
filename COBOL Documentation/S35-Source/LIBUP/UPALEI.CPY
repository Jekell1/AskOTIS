      * COPYMEMBER: LIBUP/UPALEI
      **************************************************************
      *                (UPALEI)
      **************************************************************
       OPEN-ALE-FILE.
           PERFORM OPEN-IT.
           MOVE ALE-PATH TO E-FILE.
           OPEN I-O ALE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALE-FILE.
           IF IO-FG = 7
             CLOSE ALE-FILE
             GO TO OPEN-ALE-FILE.
           UNLOCK ALE-FILE.
       WRITE-ALE-FILE.
           PERFORM WRITE-IT.
           MOVE ALE-PATH TO E-FILE.
           MOVE ALE-KEY TO E-KEYX.
           WRITE ALE-REC.
           IF IO-FG = 8
             GO TO WRITE-ALE-FILE.
           UNLOCK ALE-FILE.
       REWRITE-ALE-FILE.
           PERFORM REWRITE-IT.
           MOVE ALE-PATH TO E-FILE.
           MOVE ALE-KEY TO E-KEYX.
           REWRITE ALE-REC.
           IF IO-FG = 8
             GO TO REWRITE-ALE-FILE.
           UNLOCK ALE-FILE.
       CLOSE-ALE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALE-FILE.
      **********************************************
      * ASCII LOAN ENDORSER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ALE-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ALE-DONE-PATH TO E-FILE.
           OPEN OUTPUT ALE-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALE-DONE-FILE.
           IF IO-FG = 7
             CLOSE ALE-DONE-FILE
             GO TO OPEN-ALE-DONE-FILE.
       CLOSE-ALE-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALE-DONE-FILE.
