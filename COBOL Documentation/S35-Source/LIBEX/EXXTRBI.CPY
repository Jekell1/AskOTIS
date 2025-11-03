      * COPYMEMBER: LIBEX/EXXTRBI
      **************************************************************
      *                (EXXTRBI)
      **************************************************************
       OPEN-XTRB-FILE.
           PERFORM OPEN-IT.
           MOVE XTRB-PATH TO E-FILE.
           OPEN I-O XTRB-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRB-FILE.
           IF IO-FG = 7
             CLOSE XTRB-FILE
             GO TO OPEN-XTRB-FILE.
           UNLOCK XTRB-FILE.
       OPEN-XTRB-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XTRB-PATH TO E-FILE.
           OPEN OUTPUT XTRB-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRB-FILE.
           IF IO-FG = 7
             CLOSE XTRB-FILE
             GO TO OPEN-XTRB-FILE.
           UNLOCK XTRB-FILE.
       READ-XTRB-FILE.
           PERFORM READ-IT.
           MOVE XTRB-PATH TO E-FILE.
           MOVE XTRB-KEY TO E-KEYX.
           READ XTRB-FILE.
           IF IO-FG = 8
             GO TO READ-XTRB-FILE.
       WRITE-XTRB-FILE.
           PERFORM WRITE-IT.
           MOVE XTRB-PATH TO E-FILE.
           MOVE XTRB-KEY TO E-KEYX.
           WRITE XTRB-REC.
           IF IO-FG = 8
             GO TO WRITE-XTRB-FILE.
           UNLOCK XTRB-FILE.
       REWRITE-XTRB-FILE.
           PERFORM REWRITE-IT.
           MOVE XTRB-PATH TO E-FILE.
           MOVE XTRB-KEY TO E-KEYX.
           REWRITE XTRB-REC.
           IF IO-FG = 8
             GO TO REWRITE-XTRB-FILE.
           UNLOCK XTRB-FILE.
       DELETE-XTRB-FILE.
           PERFORM DELETE-IT.
           MOVE XTRB-PATH TO E-FILE.
           MOVE XTRB-KEY TO E-KEYX.
           DELETE XTRB-FILE.
           IF IO-FG = 8
             GO TO DELETE-XTRB-FILE.
           UNLOCK XTRB-FILE.
       CLOSE-XTRB-FILE.
           PERFORM CLOSE-IT.
           CLOSE XTRB-FILE.
