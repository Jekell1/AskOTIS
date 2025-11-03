
       OPEN-STAGE-FILE.
           PERFORM OPEN-IT.
           MOVE STAGE-ALL-PATH TO E-FILE.
           OPEN I-O STAGE-FILE.
           IF IO-FG = 8
             GO TO OPEN-STAGE-FILE.
           IF IO-FG = 7
             CLOSE STAGE-FILE
             GO TO OPEN-STAGE-FILE.
           UNLOCK STAGE-FILE.
       OPEN-STAGE-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE STAGE-ALL-PATH TO E-FILE.
           OPEN OUTPUT STAGE-FILE.
           IF IO-FG = 8
             GO TO OPEN-STAGE-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE STAGE-FILE
             GO TO OPEN-STAGE-FILE-OUTPUT.
           UNLOCK STAGE-FILE.
       READ-STAGE-FILE.
           PERFORM READ-IT.
           MOVE STAGE-ALL-PATH TO E-FILE.
           MOVE STAGE-KEY  TO E-KEYX.
           READ STAGE-FILE.
           IF IO-FG = 8
              GO TO READ-STAGE-FILE.
       READ-STAGE2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE STAGE-ALL-PATH TO E-FILE.
           STRING STAGE-BRNO,STAGE-LN-NUMBER DELIMITED BY " "
               INTO E-KEYX.
           READ STAGE-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-STAGE2-FILE-NEXT.
       START-STAGE2-FILE.
           PERFORM START-IT.
           MOVE STAGE-ALL-PATH TO E-FILE.
           STRING STAGE-BRNO,STAGE-LN-NUMBER DELIMITED BY " "
               INTO E-KEYX.
           START STAGE-FILE KEY NOT < STAGE2-KEY.
       WRITE-STAGE-FILE.
           PERFORM WRITE-IT.
           MOVE STAGE-ALL-PATH TO E-FILE.
           MOVE STAGE-KEY TO E-KEYX.
           WRITE STAGE-REC.
           IF IO-FG = 8
             GO TO WRITE-STAGE-FILE.
           UNLOCK STAGE-FILE.
       REWRITE-STAGE-FILE.
           PERFORM REWRITE-IT.
           MOVE STAGE-ALL-PATH TO E-FILE.
           MOVE STAGE-KEY TO E-KEYX.
           REWRITE STAGE-REC.
           IF IO-FG = 8
             GO TO REWRITE-STAGE-FILE.
           UNLOCK STAGE-FILE.
       DELETE-STAGE-FILE.
           PERFORM DELETE-IT.
           MOVE STAGE-ALL-PATH TO E-FILE.
           MOVE STAGE-KEY TO E-KEYX.
           DELETE STAGE-FILE.
           IF IO-FG = 8
             GO TO DELETE-STAGE-FILE.
           UNLOCK STAGE-FILE.
       CLOSE-STAGE-FILE.
           PERFORM CLOSE-IT.
           CLOSE STAGE-FILE.
