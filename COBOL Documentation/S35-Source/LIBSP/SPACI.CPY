      * COPYMEMBER: LIBSP/SPACI  ACH FILE, CREATED OUTPUT TO SEND TO BANK
       LOAD-AC-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( AC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( AC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO AC-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO AC-PATH-MACHINE
              MOVE AC-PATH-FORMAT          TO AC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( AC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO AC-PATH-BASE
              MOVE FILPATH-MACHINE         TO AC-PATH-MACHINE
              MOVE AC-PATH-FORMAT          TO AC-PATH.

       OPEN-AC-FILE.
           PERFORM LOAD-AC-FILE.
           PERFORM OPEN-IT.
           MOVE AC-PATH TO E-FILE.
           OPEN I-O AC-FILE.
           IF IO-FG = 8
             GO TO OPEN-AC-FILE.
           IF IO-FG = 7
             CLOSE AC-FILE
             GO TO OPEN-AC-FILE.
           UNLOCK AC-FILE.
       OPEN-AC-FILE-OUTPUT.
           PERFORM LOAD-AC-FILE.
           PERFORM OPEN-IT.
           MOVE AC-PATH TO E-FILE.
           OPEN OUTPUT AC-FILE.
           IF IO-FG = 8
             GO TO OPEN-AC-FILE.
           IF IO-FG = 7
             CLOSE AC-FILE
             GO TO OPEN-AC-FILE.
           UNLOCK AC-FILE.
       READ-AC-FILE.
           PERFORM READ-IT.
           MOVE AC-PATH TO E-FILE.
           MOVE AC-KEY TO E-KEYX.
           READ AC-FILE.
           IF IO-FG = 8
             GO TO READ-AC-FILE.
       WRITE-AC-FILE.
           PERFORM WRITE-IT.
           MOVE AC-PATH TO E-FILE.
           MOVE AC-KEY TO E-KEYX.
           WRITE AC-REC.
           IF IO-FG = 8
             GO TO WRITE-AC-FILE.
           UNLOCK AC-FILE.
       REWRITE-AC-FILE.
           PERFORM REWRITE-IT.
           MOVE AC-PATH TO E-FILE.
           MOVE AC-KEY TO E-KEYX.
           REWRITE AC-REC.
           IF IO-FG = 8
             GO TO REWRITE-AC-FILE.
           UNLOCK AC-FILE.
       DELETE-AC-FILE.
           PERFORM DELETE-IT.
           MOVE AC-PATH TO E-FILE.
           MOVE AC-KEY TO E-KEYX.
           DELETE AC-FILE.
           IF IO-FG = 8
             GO TO DELETE-AC-FILE.
           UNLOCK AC-FILE.
       CLOSE-AC-FILE.
           PERFORM CLOSE-IT.
           CLOSE AC-FILE.
