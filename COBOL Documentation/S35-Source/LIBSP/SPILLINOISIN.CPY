       LOAD-ILN-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ILN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ILN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ILN-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO ILN-PATH-MACHINE
              MOVE ILN-PATH-FORMAT         TO ILN-PATH ILLINOIS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ILN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ILN-PATH-BASE
              MOVE FILPATH-MACHINE         TO ILN-PATH-MACHINE
              MOVE ILN-PATH-FORMAT         TO ILN-PATH ILLINOIS-PATH.
       LOAD-ILU-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ILU-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ILU-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ILU-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO ILU-PATH-MACHINE
              MOVE ILU-PATH-FORMAT         TO ILU-PATH ILLINOIS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ILU-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ILU-PATH-BASE
              MOVE FILPATH-MACHINE         TO ILU-PATH-MACHINE
              MOVE ILU-PATH-FORMAT         TO ILU-PATH ILLINOIS-PATH.
       LOAD-ILM-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ILM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ILM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ILM-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO ILM-PATH-MACHINE
              MOVE ILM-PATH-FORMAT         TO ILM-PATH ILLINOIS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ILM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ILM-PATH-BASE
              MOVE FILPATH-MACHINE         TO ILM-PATH-MACHINE
              MOVE ILM-PATH-FORMAT         TO ILM-PATH ILLINOIS-PATH.
       OPEN-ILLINOIS-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE ILLINOIS-PATH TO E-FILE.
           OPEN OUTPUT ILLINOIS-FILE.
           IF IO-FG = 8
              GO TO OPEN-ILLINOIS-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE ILLINOIS-FILE
              GO TO OPEN-ILLINOIS-FILE-OUTPUT.
       WRITE-ILLINOIS-FILE.
           PERFORM WRITE-IT.
           MOVE ILLINOIS-PATH TO E-FILE.
           WRITE ILLINOIS-REC.
           IF IO-FG = 8
              GO TO WRITE-ILLINOIS-FILE.
       CLOSE-ILLINOIS-FILE.
           PERFORM CLOSE-IT.
           CLOSE ILLINOIS-FILE.
