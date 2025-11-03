      * COPYMEMBER: LIBEX/EXXLTII
      **************************************************************
      *                (EXXLTII)
      **************************************************************
       OPEN-XLTI-FILE.
           PERFORM OPEN-IT.
           MOVE XLTI-PATH TO E-FILE.
           OPEN I-O XLTI-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLTI-FILE.
           IF IO-FG = 7
             CLOSE XLTI-FILE
             GO TO OPEN-XLTI-FILE.
           UNLOCK XLTI-FILE.
       OPEN-XLTI-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLTI-PATH TO E-FILE.
           OPEN OUTPUT XLTI-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLTI-FILE.
           IF IO-FG = 7
             CLOSE XLTI-FILE
             GO TO OPEN-XLTI-FILE.
           UNLOCK XLTI-FILE.
       READ-XLTI-FILE.
           PERFORM READ-IT.
           MOVE XLTI-PATH TO E-FILE.
           MOVE XLTI-KEY TO E-KEYX.
           READ XLTI-FILE.
           IF IO-FG = 8
             GO TO READ-XLTI-FILE.
       WRITE-XLTI-FILE.
           PERFORM WRITE-IT.
           MOVE XLTI-PATH TO E-FILE.
           MOVE XLTI-KEY TO E-KEYX.
           WRITE XLTI-REC.
           IF IO-FG = 8
             GO TO WRITE-XLTI-FILE.
           UNLOCK XLTI-FILE.
       REWRITE-XLTI-FILE.
           PERFORM REWRITE-IT.
           MOVE XLTI-PATH TO E-FILE.
           MOVE XLTI-KEY TO E-KEYX.
           REWRITE XLTI-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLTI-FILE.
           UNLOCK XLTI-FILE.
       DELETE-XLTI-FILE.
           PERFORM DELETE-IT.
           MOVE XLTI-PATH TO E-FILE.
           MOVE XLTI-KEY TO E-KEYX.
           DELETE XLTI-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLTI-FILE.
           UNLOCK XLTI-FILE.
       CLOSE-XLTI-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLTI-FILE.
