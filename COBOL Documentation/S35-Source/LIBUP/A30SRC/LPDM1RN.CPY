      * COPYMEMBER: LIBLP/LPODM1RN
       LOAD-ODM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ODM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ODM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ODM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO ODM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO ODM-ALL-DATA
              MOVE ODM-ALL-PATH             TO ODM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ODM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ODM-ALL-BASE
              MOVE FILPATH-MACHINE         TO ODM-ALL-MACHINE
              MOVE FILPATH-DATA            TO ODM-ALL-DATA
              MOVE ODM-ALL-PATH             TO ODM-PATH.
       OPEN-ODM1-FILE.
           PERFORM LOAD-ODM1-FILE.
           PERFORM OPEN-IT.
           MOVE ODM-PATH TO E-FILE.
           OPEN INPUT ODM-FILE.
           IF IO-FG = 8
              GO TO OPEN-ODM1-FILE.
           IF IO-FG = 7
              CLOSE ODM-FILE
              GO TO OPEN-ODM1-FILE.
       READ-ODM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ODM-PATH TO E-FILE.
           MOVE ODM1-KEY TO E-KEYX.
           READ ODM-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-ODM1-FILE-NEXT.
       START-ODM1-FILE.
           PERFORM START-IT.
           MOVE ODM-PATH TO E-FILE.
           MOVE ODM1-KEY TO E-KEYX.
           START ODM-FILE KEY NOT < ODM1-KEY.
       CLOSE-ODM1-FILE.
           PERFORM CLOSE-IT.
           CLOSE ODM-FILE.
