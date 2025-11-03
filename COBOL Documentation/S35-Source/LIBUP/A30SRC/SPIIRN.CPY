      * COPYMEMBER: LIBSP/SPIIRN
       LOAD-OII-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OII-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OII-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OII-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO OII-PATH-MACHINE
              MOVE OII-PATH-FORMAT          TO OII-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OII-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OII-PATH-BASE
              MOVE FILPATH-MACHINE         TO OII-PATH-MACHINE
              MOVE OII-PATH-FORMAT          TO OII-PATH.

       OPEN-OII-FILE.
*NEW*      PERFORM LOAD-OII-FILE.
           PERFORM OPEN-IT.
           MOVE OII-PATH TO E-FILE.
           OPEN INPUT OII-FILE.
           IF IO-FG = 8
             GO TO OPEN-OII-FILE.
           IF IO-FG = 7
             CLOSE OII-FILE
             GO TO OPEN-OII-FILE.
       READ-OII-FILE.
           PERFORM READ-IT.
           MOVE OII-PATH TO E-FILE.
           MOVE OII-KEY TO E-KEYX.
           READ OII-FILE.
           IF IO-FG = 8
             GO TO READ-OII-FILE.
       READ-OII-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OII-PATH TO E-FILE.
           MOVE OII-KEY TO E-KEYX.
           READ OII-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-OII-FILE-NEXT.
       WRITE-OII-FILE.
           PERFORM WRITE-IT.
           MOVE OII-PATH TO E-FILE.
           MOVE OII-KEY TO E-KEYX.
           WRITE II-REC.
           IF IO-FG = 8
             GO TO WRITE-OII-FILE.
       REWRITE-OII-FILE.
           PERFORM REWRITE-IT.
           MOVE OII-PATH TO E-FILE.
           MOVE OII-KEY TO E-KEYX.
           REWRITE II-REC.
           IF IO-FG = 8
             GO TO REWRITE-OII-FILE.
       DELETE-OII-FILE.
           PERFORM DELETE-IT.
           MOVE OII-PATH TO E-FILE.
           MOVE OII-KEY TO E-KEYX.
           DELETE OII-FILE.
           IF IO-FG = 8
             GO TO DELETE-OII-FILE.
       START-OII-FILE.
           PERFORM START-IT.
           MOVE OII-PATH TO E-FILE.
           MOVE OII-KEY TO E-KEYX.
           START OII-FILE KEY NOT < OII-KEY.
       CLOSE-OII-FILE.
           PERFORM CLOSE-IT.
           CLOSE OII-FILE.
