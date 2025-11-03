      * COPYMEMBER: LIBSP/SPIMI
       LOAD-IM-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( IM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( IM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO IM-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO IM-PATH-MACHINE
              MOVE IM-PATH-FORMAT          TO IM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( IM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO IM-PATH-BASE
              MOVE FILPATH-MACHINE         TO IM-PATH-MACHINE
              MOVE IM-PATH-FORMAT          TO IM-PATH.

       OPEN-IM-FILE.
           PERFORM LOAD-IM-FILE.
           PERFORM OPEN-IT.
           MOVE IM-PATH TO E-FILE.
           OPEN I-O IM-FILE.
           IF IO-FG = 8
             GO TO OPEN-IM-FILE.
           IF IO-FG = 7
             CLOSE IM-FILE
             GO TO OPEN-IM-FILE.
           UNLOCK IM-FILE.
       OPEN-IM-FILE-OUTPUT.
           PERFORM LOAD-IM-FILE.
           PERFORM OPEN-IT.
           MOVE IM-PATH TO E-FILE.
           OPEN OUTPUT IM-FILE.
           IF IO-FG = 8
             GO TO OPEN-IM-FILE.
           IF IO-FG = 7
             CLOSE IM-FILE
             GO TO OPEN-IM-FILE.
           UNLOCK IM-FILE.
       READ-IM-FILE.
           PERFORM READ-IT.
           MOVE IM-PATH TO E-FILE.
           MOVE IM-KEY TO E-KEYX.
           READ IM-FILE.
           IF IO-FG = 8
             GO TO READ-IM-FILE.
       WRITE-IM-FILE.
           PERFORM WRITE-IT.
           MOVE IM-PATH TO E-FILE.
           MOVE IM-KEY TO E-KEYX.
           WRITE IM-REC.
           IF IO-FG = 8
             GO TO WRITE-IM-FILE.
           UNLOCK IM-FILE.
       REWRITE-IM-FILE.
           PERFORM REWRITE-IT.
           MOVE IM-PATH TO E-FILE.
           MOVE IM-KEY TO E-KEYX.
           REWRITE IM-REC.
           IF IO-FG = 8
             GO TO REWRITE-IM-FILE.
           UNLOCK IM-FILE.
       DELETE-IM-FILE.
           PERFORM DELETE-IT.
           MOVE IM-PATH TO E-FILE.
           MOVE IM-KEY TO E-KEYX.
           DELETE IM-FILE.
           IF IO-FG = 8
             GO TO DELETE-IM-FILE.
           UNLOCK IM-FILE.
       CLOSE-IM-FILE.
           PERFORM CLOSE-IT.
           CLOSE IM-FILE.
