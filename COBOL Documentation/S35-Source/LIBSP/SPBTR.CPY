      * COPYMEMBER: LIBSP/SPBTR
       LOAD-BT-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BT-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BT-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BT-ALL-MACHINE
              MOVE BT-ALL-PATH             TO BT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BT-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BT-ALL-BASE
              MOVE FILPATH-MACHINE         TO BT-ALL-MACHINE
              MOVE BT-ALL-PATH             TO BT-PATH.
       OPEN-BT-FILE.
           PERFORM LOAD-BT-FILE.
           PERFORM OPEN-IT.
           MOVE BT-IFN TO E-FILE.
           OPEN INPUT BT-FILE.
           IF IO-FG = 8
              GO TO OPEN-BT-FILE.
           IF IO-FG = 7
              CLOSE BT-FILE
              GO TO OPEN-BT-FILE.
       READ-BT-FILE.
           PERFORM READ-IT.
           MOVE BT-IFN TO E-FILE.
           READ BT-FILE.
           IF IO-FG = 8
              GO TO READ-BT-FILE.
       CLOSE-BT-FILE.
           PERFORM CLOSE-IT.
           CLOSE BT-FILE.
