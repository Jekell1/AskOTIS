      * COPYMEMBER: LIBLP/LPOLE1RN
       LOAD-OLE1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLE-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLE-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLE-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLE-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLE-ALL-DATA
              MOVE OLE-ALL-PATH             TO OLE-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLE-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLE-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLE-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLE-ALL-DATA
              MOVE OLE-ALL-PATH             TO OLE-PATH.
       OPEN-OLE1-FILE.
           PERFORM LOAD-OLE1-FILE.
           PERFORM OPEN-IT.
           MOVE OLE-PATH TO E-FILE.
           OPEN INPUT OLE-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLE1-FILE.
           IF IO-FG = 7
              CLOSE OLE-FILE
              GO TO OPEN-OLE1-FILE.
       READ-OLE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLE-PATH TO E-FILE.
           MOVE OLE1-KEY TO E-KEYX.
           READ OLE-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLE1-FILE-NEXT.
       START-OLE1-FILE.
           PERFORM START-IT.
           MOVE OLE-PATH TO E-FILE.
           MOVE OLE1-KEY TO E-KEYX.
           START OLE-FILE KEY NOT < OLE1-KEY.
       CLOSE-OLE1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLE-FILE.
