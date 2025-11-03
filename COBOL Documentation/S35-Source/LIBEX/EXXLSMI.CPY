      * COPYMEMBER: LIBEX/EXXLSMI
      **************************************************************
      *                (EXXLSMI)
      **************************************************************
       OPEN-XLSM-FILE.
           PERFORM OPEN-IT.
           MOVE XLSM-PATH TO E-FILE.
           OPEN I-O XLSM-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLSM-FILE.
           IF IO-FG = 7
             CLOSE XLSM-FILE
             GO TO OPEN-XLSM-FILE.
           UNLOCK XLSM-FILE.
       OPEN-XLSM-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLSM-PATH TO E-FILE.
           OPEN OUTPUT XLSM-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLSM-FILE.
           IF IO-FG = 7
             CLOSE XLSM-FILE
             GO TO OPEN-XLSM-FILE.
           UNLOCK XLSM-FILE.
       READ-XLSM-FILE.
           PERFORM READ-IT.
           MOVE XLSM-PATH TO E-FILE.
           MOVE XLSM-KEY TO E-KEYX.
           READ XLSM-FILE.
           IF IO-FG = 8
             GO TO READ-XLSM-FILE.
       WRITE-XLSM-FILE.
           PERFORM WRITE-IT.
           MOVE XLSM-PATH TO E-FILE.
           MOVE XLSM-KEY TO E-KEYX.
           WRITE XLSM-REC.
           IF IO-FG = 8
             GO TO WRITE-XLSM-FILE.
           UNLOCK XLSM-FILE.
       REWRITE-XLSM-FILE.
           PERFORM REWRITE-IT.
           MOVE XLSM-PATH TO E-FILE.
           MOVE XLSM-KEY TO E-KEYX.
           REWRITE XLSM-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLSM-FILE.
           UNLOCK XLSM-FILE.
       DELETE-XLSM-FILE.
           PERFORM DELETE-IT.
           MOVE XLSM-PATH TO E-FILE.
           MOVE XLSM-KEY TO E-KEYX.
           DELETE XLSM-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLSM-FILE.
           UNLOCK XLSM-FILE.
       CLOSE-XLSM-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLSM-FILE.
