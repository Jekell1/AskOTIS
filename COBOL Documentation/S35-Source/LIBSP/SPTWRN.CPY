      * COPYMEMBER: LIBSP/SPTWRN
       OPEN-TRW-FILE.
           PERFORM OPEN-IT.
           MOVE TRW-PATH TO E-FILE.
           OPEN I-O TRW-FILE.
           IF IO-FG = 8
              GO TO OPEN-TRW-FILE.
           IF IO-FG = 7
              CLOSE TRW-FILE
              GO TO OPEN-TRW-FILE.
       OPEN-TRW-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE TRW-PATH TO E-FILE.
           OPEN OUTPUT TRW-FILE.
           IF IO-FG = 8
              GO TO OPEN-TRW-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE TRW-FILE
              GO TO OPEN-TRW-FILE-OUTPUT.
       READ-TRW-FILE.
           PERFORM READ-IT.
           MOVE TRW-PATH TO E-FILE.
           MOVE TRW-KEY TO E-KEYX.
           READ TRW-FILE.
           IF IO-FG = 8
              GO TO READ-TRW-FILE.
       WRITE-TRW-FILE.
           PERFORM WRITE-IT.
           MOVE TRW-PATH TO E-FILE.
           MOVE TRW-KEY TO E-KEYX.
           WRITE TRW-REC.
           IF IO-FG = 8
              GO TO WRITE-TRW-FILE.
       REWRITE-TRW-REC.
           PERFORM REWRITE-IT.
           MOVE TRW-PATH TO E-FILE.
           MOVE TRW-KEY TO E-KEYX.
           REWRITE TRW-REC.
           IF IO-FG = 8
              GO TO REWRITE-TRW-REC.
       CLOSE-TRW-FILE.
           PERFORM CLOSE-IT.
           CLOSE TRW-FILE.
