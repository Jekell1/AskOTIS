      * COPYMEMBER: LIBLP/LPOSM1RN
       LOAD-OSM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OSM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OSM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OSM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OSM-ALL-MACHINE
              MOVE OSM-ALL-PATH             TO OSM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OSM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OSM-ALL-BASE
              MOVE FILPATH-MACHINE         TO OSM-ALL-MACHINE
              MOVE OSM-ALL-PATH             TO OSM-PATH.
       OPEN-OSM1-FILE.
           PERFORM LOAD-OSM1-FILE.
           PERFORM OPEN-IT.
           MOVE OSM-PATH TO E-FILE.
           OPEN I-O OSM-FILE.
           IF IO-FG = 8
             GO TO OPEN-OSM1-FILE.
           IF IO-FG = 7
             CLOSE OSM-FILE
             GO TO OPEN-OSM1-FILE.
           UNLOCK OSM-FILE.
       READ-OSM1-FILE.
           PERFORM READ-IT.
           MOVE OSM-PATH TO E-FILE.
           MOVE OSM1-KEY TO E-KEYX.
           READ OSM-FILE.
           IF IO-FG = 8
              GO TO READ-OSM1-FILE.
       READ-OSM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OSM-PATH TO E-FILE.
           MOVE OSM1-KEY TO E-KEYX.
           READ OSM-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-OSM1-FILE-NEXT.
       START-OSM1-FILE.
           PERFORM START-IT.
           MOVE OSM-PATH TO E-FILE.
           MOVE OSM1-KEY TO E-KEYX.
           START OSM-FILE KEY NOT < OSM1-KEY.
           UNLOCK OSM-FILE.

       START-OSM1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE OSM-PATH TO E-FILE.
           MOVE OSM1-KEY TO E-KEYX.
           START OSM-FILE KEY NOT > OSM1-KEY.
       START-OSM1-FILE-LESS-THAN.
           PERFORM START-IT.
           MOVE OSM-PATH TO E-FILE.
           MOVE OSM1-KEY TO E-KEYX.
           START OSM-FILE KEY < OSM1-KEY.
       READ-OSM1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE OSM-PATH TO E-FILE.
           MOVE OSM1-KEY TO E-KEYX.
           READ OSM-FILE PREVIOUS.
           IF IO-FG = 8
              GO TO READ-OSM1-FILE-PREVIOUS.
       CLOSE-OSM1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OSM-FILE.
