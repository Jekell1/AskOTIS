      * COPYMEMBER: LIBSP/SPILI
       LOAD-IL-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( IL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( IL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO IL-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO IL-PATH-MACHINE
              MOVE IL-PATH-FORMAT          TO IL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( IL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO IL-PATH-BASE
              MOVE FILPATH-MACHINE         TO IL-PATH-MACHINE
              MOVE IL-PATH-FORMAT          TO IL-PATH.

       OPEN-IL-FILE-OUTPUT.
           PERFORM LOAD-IL-FILE.
           PERFORM OPEN-IT.
           MOVE IL-PATH TO E-FILE.
           OPEN OUTPUT IL-FILE.
           IF IO-FG = 8
             GO TO OPEN-IL-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE IL-FILE
             GO TO OPEN-IL-FILE-OUTPUT.
           UNLOCK IL-FILE.
       OPEN-IL-FILE.
           PERFORM LOAD-IL-FILE.
           PERFORM OPEN-IT.
           MOVE IL-PATH TO E-FILE.
           OPEN I-O IL-FILE.
           IF IO-FG = 8
             GO TO OPEN-IL-FILE.
           IF IO-FG = 7
             CLOSE IL-FILE
             GO TO OPEN-IL-FILE.
           UNLOCK IL-FILE.
       READ-IL-FILE.
           PERFORM READ-IT.
           MOVE IL-PATH TO E-FILE.
           MOVE IL-KEY TO E-KEYX.
           READ IL-FILE.
           IF IO-FG = 8
             GO TO READ-IL-FILE.
       WRITE-IL-FILE.
           PERFORM WRITE-IT.
           MOVE IL-PATH TO E-FILE.
           MOVE IL-KEY TO E-KEYX.
           WRITE IL-REC.
           IF IO-FG = 8
             GO TO WRITE-IL-FILE.
           UNLOCK IL-FILE.
       REWRITE-IL-FILE.
           PERFORM REWRITE-IT.
           MOVE IL-PATH TO E-FILE.
           MOVE IL-KEY TO E-KEYX.
           REWRITE IL-REC.
           IF IO-FG = 8
             GO TO REWRITE-IL-FILE.
           UNLOCK IL-FILE.
       DELETE-IL-FILE.
           PERFORM DELETE-IT.
           MOVE IL-PATH TO E-FILE.
           MOVE IL-KEY TO E-KEYX.
           DELETE IL-FILE.
           IF IO-FG = 8
             GO TO DELETE-IL-FILE.
           UNLOCK IL-FILE.
       CLOSE-IL-FILE.
           PERFORM CLOSE-IT.
           CLOSE IL-FILE.
