      * COPYMEMBER: LIBLP/LPOLTP1R
       LOAD-OLTP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLTP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLTP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLTP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLTP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLTP-ALL-DATA
              MOVE OLTP-ALL-PATH            TO OLTP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLTP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLTP-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLTP-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLTP-ALL-DATA
              MOVE OLTP-ALL-PATH            TO OLTP-PATH.

       GET-LT-P-REC.
           MOVE 0 TO IO-FG.
           IF NOT (LN-ACCTNO = OLTP-ACCTNO)
              MOVE LN-OWNBR  TO OLTP-BRNO
              MOVE LN-ACCTNO TO OLTP-ACCTNO
              MOVE 1         TO OLTP-SEQNO
              PERFORM OPEN-OLTP1-FILE
              PERFORM READ-OLTP1-FILE
              PERFORM CLOSE-OLTP1-FILE.
       OPEN-OLTP1-FILE.
           PERFORM LOAD-OLTP1-FILE.
           PERFORM OPEN-IT.
           MOVE OLTP-PATH TO E-FILE.
           OPEN INPUT OLTP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLTP1-FILE.
           IF IO-FG = 7
              CLOSE OLTP-FILE
              GO TO OPEN-OLTP1-FILE.
       READ-OLTP1-FILE.
           PERFORM READ-IT.
           MOVE OLTP-PATH TO E-FILE.
           MOVE OLTP1-KEY TO E-KEYX.
           READ OLTP-FILE.
           IF IO-FG = 8
              GO TO READ-OLTP1-FILE.
       START-OLTP1-FILE.
           PERFORM START-IT.
           MOVE OLTP-PATH TO E-FILE.
           MOVE OLTP1-KEY TO E-KEYX.
           START OLTP-FILE KEY NOT < OLTP1-KEY.

       READ-OLTP1-FILE-NEXT.          *> ONLY IN A32
           PERFORM READ-IT.
           MOVE OLTP-PATH TO E-FILE.
           MOVE OLTP1-KEY TO E-KEYX.
           READ OLTP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLTP1-FILE-NEXT.
       CLOSE-OLTP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLTP-FILE.
