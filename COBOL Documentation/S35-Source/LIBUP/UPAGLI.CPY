      * COPYMEMBER: LIBUP/UPAGLI
      **************************************************************
      *                (UPAGLI)
      **************************************************************
       OPEN-AGL-FILE.
           PERFORM OPEN-IT.
           MOVE AGL-PATH TO E-FILE.
           OPEN I-O AGL-FILE.
           IF IO-FG = 8
             GO TO OPEN-AGL-FILE.
           IF IO-FG = 7
             CLOSE AGL-FILE
             GO TO OPEN-AGL-FILE.
           UNLOCK AGL-FILE.
       WRITE-AGL-FILE.
           PERFORM WRITE-IT.
           MOVE AGL-PATH TO E-FILE.
           MOVE AGL-KEY TO E-KEYX.
           WRITE AGL-REC.
           IF IO-FG = 8
             GO TO WRITE-AGL-FILE.
           UNLOCK AGL-FILE.
       REWRITE-AGL-FILE.
           PERFORM REWRITE-IT.
           MOVE AGL-PATH TO E-FILE.
           MOVE AGL-KEY TO E-KEYX.
           REWRITE AGL-REC.
           IF IO-FG = 8
             GO TO REWRITE-AGL-FILE.
           UNLOCK AGL-FILE.
       CLOSE-AGL-FILE.
           PERFORM CLOSE-IT.
           CLOSE AGL-FILE.
      **********************************************
      * ASCII GENERAL LEDGER  DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-AGL-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE AGL-DONE-PATH TO E-FILE.
           OPEN OUTPUT AGL-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-AGL-DONE-FILE.
           IF IO-FG = 7
             CLOSE AGL-DONE-FILE
             GO TO OPEN-AGL-DONE-FILE.
       CLOSE-AGL-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE AGL-DONE-FILE.
