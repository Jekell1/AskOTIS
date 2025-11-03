      * COPYMEMBER: LIBUP/UPACII
      **************************************************************
      *                ASCII CUSTOMER INFO
      **************************************************************
       OPEN-ACI-FILE.
           PERFORM OPEN-IT.
           MOVE ACI-PATH TO E-FILE.
           OPEN I-O ACI-FILE.
           IF IO-FG = 8
             GO TO OPEN-ACI-FILE.
           IF IO-FG = 7
             CLOSE ACI-FILE
             GO TO OPEN-ACI-FILE.
           UNLOCK ACI-FILE.
       WRITE-ACI-FILE.
           PERFORM WRITE-IT.
           MOVE ACI-PATH TO E-FILE.
           MOVE ACI-KEY TO E-KEYX.
           WRITE ACI-REC.
           IF IO-FG = 8
             GO TO WRITE-ACI-FILE.
           UNLOCK ACI-FILE.
       REWRITE-ACI-FILE.
           PERFORM REWRITE-IT.
           MOVE ACI-PATH TO E-FILE.
           MOVE ACI-KEY TO E-KEYX.
           REWRITE ACI-REC.
           IF IO-FG = 8
             GO TO REWRITE-ACI-FILE.
           UNLOCK ACI-FILE.
       CLOSE-ACI-FILE.
           PERFORM CLOSE-IT.
           CLOSE ACI-FILE.

      **********************************************
      * ASCII CUSTOMER INFO DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ACI-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ACI-DONE-PATH TO E-FILE.
           OPEN OUTPUT ACI-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ACI-DONE-FILE.
           IF IO-FG = 7
             CLOSE ACI-DONE-FILE
             GO TO OPEN-ACI-DONE-FILE.
       CLOSE-ACI-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ACI-DONE-FILE.
