      * COPYMEMBER: LIBSP/SPICI    IRS PAYEE FILE
       LOAD-IC-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( IC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( IC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO IC-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO IC-PATH-MACHINE
              MOVE IC-PATH-FORMAT          TO IC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( IC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO IC-PATH-BASE
              MOVE FILPATH-MACHINE         TO IC-PATH-MACHINE
              MOVE IC-PATH-FORMAT          TO IC-PATH.

       OPEN-IC-FILE-OUTPUT.
           PERFORM LOAD-IC-FILE.
           PERFORM OPEN-IT.
           MOVE IC-PATH TO E-FILE.
           OPEN OUTPUT IC-FILE.
           IF IO-FG = 8
             GO TO OPEN-IC-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE IC-FILE
             GO TO OPEN-IC-FILE-OUTPUT.
           UNLOCK IC-FILE.
       OPEN-IC-FILE.
           PERFORM LOAD-IC-FILE.
           PERFORM OPEN-IT.
           MOVE IC-PATH TO E-FILE.
           OPEN I-O IC-FILE.
           IF IO-FG = 8
             GO TO OPEN-IC-FILE.
           IF IO-FG = 7
             CLOSE IC-FILE
             GO TO OPEN-IC-FILE.
           UNLOCK IC-FILE.
       READ-IC-FILE.
           PERFORM READ-IT.
           MOVE IC-PATH TO E-FILE.
           MOVE IC-KEY TO E-KEYX.
           READ IC-FILE.
           IF IO-FG = 8
             GO TO READ-IC-FILE.
       WRITE-IC-FILE.
           PERFORM WRITE-IT.
           MOVE IC-PATH TO E-FILE.
           MOVE IC-KEY TO E-KEYX.
           WRITE IC-REC.
           IF IO-FG = 8
             GO TO WRITE-IC-FILE.
           UNLOCK IC-FILE.
       REWRITE-IC-FILE.
           PERFORM REWRITE-IT.
           MOVE IC-PATH TO E-FILE.
           MOVE IC-KEY TO E-KEYX.
           REWRITE IC-REC.
           IF IO-FG = 8
             GO TO REWRITE-IC-FILE.
           UNLOCK IC-FILE.
       DELETE-IC-FILE.
           PERFORM DELETE-IT.
           MOVE IC-PATH TO E-FILE.
           MOVE IC-KEY TO E-KEYX.
           DELETE IC-FILE.
           IF IO-FG = 8
             GO TO DELETE-IC-FILE.
           UNLOCK IC-FILE.
       CLOSE-IC-FILE.
           PERFORM CLOSE-IT.
           CLOSE IC-FILE.
