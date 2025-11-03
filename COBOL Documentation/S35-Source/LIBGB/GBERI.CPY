      * COPYMEMBER: LIBGB/GBERI
       LOAD-ERRLOG-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ERRLOG-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ERRLOG-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ERRLOG-BASE
              MOVE EXT-FILPATH-MACHINE     TO ERRLOG-MACHINE
              MOVE ERRLOG-PATH-FORMAT      TO ERRLOG-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ERRLOG-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ERRLOG-BASE
              MOVE FILPATH-MACHINE         TO ERRLOG-MACHINE
              MOVE ERRLOG-PATH-FORMAT      TO ERRLOG-PATH.

       OPEN-ERRLOG-FILE-EXTEND.
           PERFORM LOAD-ERRLOG-FILE.
           OPEN EXTEND ERRLOG-FILE.
           IF IO-FG = 8
             GO TO OPEN-ERRLOG-FILE-EXTEND.
           IF IO-FG = 7
             CLOSE ERRLOG-FILE
             GO TO OPEN-ERRLOG-FILE-EXTEND.

       OPEN-ERRLOG-FILE-OUTPUT.
           PERFORM LOAD-ERRLOG-FILE.
           OPEN OUTPUT ERRLOG-FILE.
           IF IO-FG = 8
             GO TO OPEN-ERRLOG-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE ERRLOG-FILE
             GO TO OPEN-ERRLOG-FILE-OUTPUT.

       OPEN-ERRLOG-FILE.
           PERFORM LOAD-ERRLOG-FILE.
           OPEN INPUT ERRLOG-FILE.
           IF IO-FG = 8
             GO TO OPEN-ERRLOG-FILE.
           IF IO-FG = 7
             CLOSE ERRLOG-FILE
             GO TO OPEN-ERRLOG-FILE.
       WRITE-ERRLOG-FILE.
           WRITE ERRLOG-REC.
       CLOSE-ERRLOG-FILE.
           CLOSE ERRLOG-FILE.
