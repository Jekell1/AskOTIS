      * COPYMEMBER: LIBSP/SPTWERN
       OPEN-TRW-E-FILE.
           PERFORM OPEN-IT.
           MOVE TRW-E-PATH TO E-FILE.
           OPEN I-O TRW-E-FILE.
           IF IO-FG = 8
              GO TO OPEN-TRW-E-FILE.
           IF IO-FG = 7
              CLOSE TRW-E-FILE
              GO TO OPEN-TRW-E-FILE.
       OPEN-TRW-E-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE TRW-E-PATH TO E-FILE.
           OPEN OUTPUT TRW-E-FILE.
           IF IO-FG = 8
              GO TO OPEN-TRW-E-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE TRW-E-FILE
              GO TO OPEN-TRW-E-FILE-OUTPUT.
       READ-TRW-E-FILE.
           PERFORM READ-IT.
           MOVE TRW-E-PATH TO E-FILE.
           MOVE TRW-E-KEY TO E-KEYX.
           READ TRW-E-FILE.
           IF IO-FG = 8
              GO TO READ-TRW-E-FILE.
       WRITE-TRW-E-FILE.
           PERFORM WRITE-IT.
           MOVE TRW-E-PATH TO E-FILE.
           MOVE TRW-E-KEY TO E-KEYX.
           WRITE TRW-E-REC.
           IF IO-FG = 8
              GO TO WRITE-TRW-E-FILE.
       REWRITE-TRW-E-REC.
           PERFORM REWRITE-IT.
           MOVE TRW-E-PATH TO E-FILE.
           MOVE TRW-E-KEY TO E-KEYX.
           REWRITE TRW-E-REC.
           IF IO-FG = 8
              GO TO REWRITE-TRW-E-REC.
       CLOSE-TRW-E-FILE.
           PERFORM CLOSE-IT.
           CLOSE TRW-E-FILE.
