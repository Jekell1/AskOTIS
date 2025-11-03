      * COPYMEMBER: LIBLP/LPOFDB1RN
      ******************************************************************
      * REV:
      * KEC 2016.0114 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ***NEW*** ADDED NEW OFDB-FILE; WAS IN FD-FILE.
      *
      ******************************************************************
       LOAD-OFDB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OFDB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OFDB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OFDB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OFDB-ALL-MACHINE
              MOVE OFDB-ALL-PATH            TO OFDB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OFDB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OFDB-ALL-BASE
              MOVE FILPATH-MACHINE         TO OFDB-ALL-MACHINE
              MOVE OFDB-ALL-PATH            TO OFDB-PATH.
       OPEN-OFDB1-FILE.
           PERFORM LOAD-OFDB1-FILE.
           PERFORM OPEN-IT.
           MOVE OFDB-PATH TO E-FILE.
           OPEN INPUT OFDB-FILE.
           IF IO-FG = 8
              GO TO OPEN-OFDB1-FILE.
           IF IO-FG = 7
              CLOSE OFDB-FILE
              GO TO OPEN-OFDB1-FILE.
       READ-OFDB1-FILE.
           PERFORM READ-IT.
           MOVE OFDB-PATH TO E-FILE.
           MOVE OFDB1-KEY TO E-KEYX.
           READ OFDB-FILE.
           IF IO-FG = 8
              GO TO READ-OFDB1-FILE.
       READ-OFDB1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OFDB-PATH TO E-FILE.
           MOVE OFDB1-KEY TO E-KEYX.
           READ OFDB-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OFDB1-FILE-NEXT.
       START-OFDB1-FILE.
           PERFORM START-IT.
           MOVE OFDB-PATH TO E-FILE.
           MOVE OFDB1-KEY TO E-KEYX.
           START OFDB-FILE KEY NOT < OFDB1-KEY.
       CLOSE-OFDB1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OFDB-FILE.
