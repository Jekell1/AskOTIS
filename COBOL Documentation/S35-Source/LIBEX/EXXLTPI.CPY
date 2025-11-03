      * COPYMEMBER: LIBEX/EXXLTPI
      **************************************************************
      *                (EXXLTPI)
      **************************************************************
       OPEN-XLTP-FILE.
           PERFORM OPEN-IT.
           MOVE XLTP-PATH TO E-FILE.
           OPEN I-O XLTP-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLTP-FILE.
           IF IO-FG = 7
             CLOSE XLTP-FILE
             GO TO OPEN-XLTP-FILE.
           UNLOCK XLTP-FILE.
       OPEN-XLTP-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLTP-PATH TO E-FILE.
           OPEN OUTPUT XLTP-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLTP-FILE.
           IF IO-FG = 7
             CLOSE XLTP-FILE
             GO TO OPEN-XLTP-FILE.
           UNLOCK XLTP-FILE.
       READ-XLTP-FILE.
           PERFORM READ-IT.
           MOVE XLTP-PATH TO E-FILE.
           MOVE XLTP-KEY TO E-KEYX.
           READ XLTP-FILE.
           IF IO-FG = 8
             GO TO READ-XLTP-FILE.
       WRITE-XLTP-FILE.
           PERFORM WRITE-IT.
           MOVE XLTP-PATH TO E-FILE.
           MOVE XLTP-KEY TO E-KEYX.
           WRITE XLTP-REC.
           IF IO-FG = 8
             GO TO WRITE-XLTP-FILE.
           UNLOCK XLTP-FILE.
       REWRITE-XLTP-FILE.
           PERFORM REWRITE-IT.
           MOVE XLTP-PATH TO E-FILE.
           MOVE XLTP-KEY TO E-KEYX.
           REWRITE XLTP-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLTP-FILE.
           UNLOCK XLTP-FILE.
       DELETE-XLTP-FILE.
           PERFORM DELETE-IT.
           MOVE XLTP-PATH TO E-FILE.
           MOVE XLTP-KEY TO E-KEYX.
           DELETE XLTP-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLTP-FILE.
           UNLOCK XLTP-FILE.
       CLOSE-XLTP-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLTP-FILE.
