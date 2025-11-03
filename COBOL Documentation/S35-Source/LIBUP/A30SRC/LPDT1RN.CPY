      * COPYMEMBER: LIBLP/LPODT1RN
       LOAD-ODT1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ODT-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ODT-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ODT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO ODT-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO ODT-ALL-DATA
              MOVE ODT-ALL-PATH             TO ODT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ODT-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ODT-ALL-BASE
              MOVE FILPATH-MACHINE         TO ODT-ALL-MACHINE
              MOVE FILPATH-DATA            TO ODT-ALL-DATA
              MOVE ODT-ALL-PATH             TO ODT-PATH.
       OPEN-ODT1-FILE.
           PERFORM LOAD-ODT1-FILE.
           PERFORM OPEN-IT.
           MOVE ODT-PATH TO E-FILE.
           OPEN INPUT ODT-FILE.
           IF IO-FG = 8
              GO TO OPEN-ODT1-FILE.
           IF IO-FG = 7
              CLOSE ODT-FILE
              GO TO OPEN-ODT1-FILE.
       READ-ODT1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ODT-PATH TO E-FILE.
           MOVE ODT1-KEY TO E-KEYX.
           READ ODT-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-ODT1-FILE-NEXT.
       START-ODT1-FILE.
           PERFORM START-IT.
           MOVE ODT-PATH TO E-FILE.
           MOVE ODT1-KEY TO E-KEYX.
           START ODT-FILE KEY NOT < ODT1-KEY.
       CLOSE-ODT1-FILE.
           PERFORM CLOSE-IT.
           CLOSE ODT-FILE.
