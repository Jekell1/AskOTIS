      * COPYMEMBER: LIBLP/LPOCHR
       LOAD-OCH-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUOCH THE PATH AT ALL  }- -
      *KC  IF ( OCHANGE-PATH-OVERRIDE = "Y" ) 
           IF ( OCH-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANOCHES }- - - - - -
      *KC  IF ( OCHANGE-PATH-OVERRIDE = SPACES ) 
           IF ( OCH-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OCH-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OCH-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OCH-ALL-DATA
      *KC     MOVE OCH-ALL-PATH            TO OCHANGE-PATH
              MOVE OCH-ALL-PATH            TO OCH-PATH
           ELSE
      *- - - -{ USE BRANOCH VALUES }- - - - - - - - - - - - - - - - - - -
      *KC  IF ( OCHANGE-PATH-OVERRIDE = "B" ) 
           IF ( OCH-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OCH-ALL-BASE
              MOVE FILPATH-MACHINE         TO OCH-ALL-MACHINE
              MOVE FILPATH-DATA            TO OCH-ALL-DATA
      *KC     MOVE OCH-ALL-PATH            TO OCHANGE-PATH.
              MOVE OCH-ALL-PATH            TO OCH-PATH.
       OPEN-OCH-FILE.
           PERFORM LOAD-OCH-FILE.
           PERFORM OPEN-IT.
      *KC  MOVE OCHANGE-PATH TO E-FILE.
           MOVE OCH-PATH TO E-FILE.
           OPEN INPUT OCH-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCH-FILE.
           IF IO-FG = 7
              CLOSE OCH-FILE
              GO TO OPEN-OCH-FILE.
       READ-OCH-FILE.
           PERFORM READ-IT.
      *KC  MOVE OCHANGE-PATH TO E-FILE.
           MOVE OCH-PATH TO E-FILE.
           MOVE OCH-KEY      TO E-KEYX.
           READ OCH-FILE.
           IF IO-FG = 8
              GO TO READ-OCH-FILE.
       READ-OCH-FILE-NEXT.
           PERFORM READ-IT.
      *KC  MOVE OCHANGE-PATH TO E-FILE.
           MOVE OCH-PATH TO E-FILE.
           READ OCH-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCH-FILE.
        START-OCH-FILE.
           PERFORM START-IT.
      *KC  MOVE OCHANGE-PATH TO E-FILE.
           MOVE OCH-PATH TO E-FILE.
           MOVE OCH-KEY TO E-KEYX.
           START OCH-FILE KEY NOT < OCH-KEY.
       CLOSE-OCH-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCH-FILE.
