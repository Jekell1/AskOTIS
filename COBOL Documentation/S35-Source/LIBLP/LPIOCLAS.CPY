      * COPYMEMBER: LIBLP/LPIOCLAS - CLASS TRANSFERS
      ************************************************************
      * REV:
      * KEC 130612 ***NEW***
      ************************************************************
       LOAD-CLAS-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CLAS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CLAS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CLAS-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO CLAS-PATH-MACHINE
              MOVE EXT-FILPATH-DATA        TO CLAS-PATH-DATA
              MOVE CLAS-PATH-FORMAT        TO CLAS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CLAS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CLAS-PATH-BASE
              MOVE FILPATH-MACHINE         TO CLAS-PATH-MACHINE
              MOVE FILPATH-DATA            TO CLAS-PATH-DATA
              MOVE CLAS-PATH-FORMAT        TO CLAS-PATH.

