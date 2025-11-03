      * COPYMEMBER: LIBLP/LPPRINT
      * USED: GLDIST, GLRCAP, LONPR0, LPEOD5, RECAPJ
      *****************************
      *    LP ERROR PRINT FILE    *
      *****************************
       LOAD-ER-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ER-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ER-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ER-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO ER-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO ER-ALL-DATA
              MOVE ER-ALL-PATH             TO ER-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ER-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ER-ALL-BASE
              MOVE FILPATH-MACHINE         TO ER-ALL-MACHINE
              MOVE FILPATH-DATA            TO ER-ALL-DATA
              MOVE ER-ALL-PATH             TO ER-PATH.
       OPEN-ER-FILE.
           PERFORM LOAD-ER-FILE.
           PERFORM OPEN-IT.
           MOVE ER-PATH TO E-FILE.
           OPEN EXTEND ER-FILE.
           IF IO-FG = 8
             GO TO OPEN-ER-FILE.
           IF IO-FG = 7
             CLOSE ER-FILE
             GO TO OPEN-ER-FILE.
       OPEN-ER-FILE-OUTPUT.
           PERFORM LOAD-ER-FILE.
           PERFORM OPEN-IT.
           MOVE ER-PATH TO E-FILE.
           OPEN OUTPUT ER-FILE.
           IF IO-FG = 8
             GO TO OPEN-ER-FILE.
           IF IO-FG = 7
             CLOSE ER-FILE
             GO TO OPEN-ER-FILE.
       WRITE-ER-REC.
           PERFORM WRITE-IT.
           MOVE ER-PATH TO E-FILE.
           IF ER-FEED = 99
              WRITE ER-REC AFTER PAGE
            ELSE
              WRITE ER-REC AFTER ER-FEED.
           IF IO-FG = 8
              GO TO WRITE-ER-REC.
       CLOSE-ER-FILE.
           PERFORM CLOSE-IT.
           CLOSE ER-FILE.
