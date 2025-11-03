      * COPYMEMBER: LIBLP/LPODL1RN
       LOAD-ODL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ODL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ODL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ODL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO ODL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO ODL-ALL-DATA
              MOVE ODL-ALL-PATH             TO ODL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ODL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ODL-ALL-BASE
              MOVE FILPATH-MACHINE         TO ODL-ALL-MACHINE
              MOVE FILPATH-DATA            TO ODL-ALL-DATA
              MOVE ODL-ALL-PATH             TO ODL-PATH.
       OPEN-ODL1-FILE.
           PERFORM LOAD-ODL1-FILE.
           PERFORM OPEN-IT.
           MOVE ODL-PATH TO E-FILE.
           OPEN INPUT ODL-FILE.
           IF IO-FG = 8
              GO TO OPEN-ODL1-FILE.
           IF IO-FG = 7
              CLOSE ODL-FILE
              GO TO OPEN-ODL1-FILE.
       READ-ODL1-FILE.
           PERFORM READ-IT.
           MOVE ODL-PATH TO E-FILE.
           MOVE ODL1-KEY TO E-KEYX.
           READ ODL-FILE.
           IF IO-FG = 8
              GO TO READ-ODL1-FILE.
       READ-ODL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ODL-PATH TO E-FILE.
           MOVE ODL1-KEY TO E-KEYX.
           READ ODL-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-ODL1-FILE-NEXT.
       START-ODL1-FILE.
           PERFORM START-IT.
           MOVE ODL-PATH TO E-FILE.
           MOVE ODL1-KEY TO E-KEYX.
           START ODL-FILE KEY NOT < ODL1-KEY.
       CLOSE-ODL1-FILE.
           PERFORM CLOSE-IT.
           CLOSE ODL-FILE.
