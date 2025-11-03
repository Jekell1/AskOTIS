      * COPYMEMBER: LIBLP/LPOCX1RN
       LOAD-OCX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OCX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OCX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OCX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OCX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OCX-ALL-DATA
              MOVE OCX-ALL-PATH             TO OCX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OCX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OCX-ALL-BASE
              MOVE FILPATH-MACHINE         TO OCX-ALL-MACHINE
              MOVE FILPATH-DATA            TO OCX-ALL-DATA
              MOVE OCX-ALL-PATH             TO OCX-PATH.
       OPEN-OCX1-FILE.
           PERFORM LOAD-OCX1-FILE.
           PERFORM OPEN-IT.
           MOVE OCX-PATH TO E-FILE.
           OPEN INPUT OCX-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCX1-FILE.
           IF IO-FG = 7
              CLOSE OCX-FILE
              GO TO OPEN-OCX1-FILE.
       READ-OCX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCX-PATH TO E-FILE.
           MOVE OCX1-KEY TO E-KEYX.
           READ OCX-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCX1-FILE-NEXT.
       START-OCX1-FILE.
           PERFORM START-IT.
           MOVE OCX-PATH TO E-FILE.
           MOVE OCX1-KEY TO E-KEYX.
           START OCX-FILE KEY NOT < OCX1-KEY.
       CLOSE-OCX1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCX-FILE.
