      * COPYMEMBER: LIBEX/EXXCJI
      **************************************************************
      *                (EXXCJI)
      **************************************************************
       OPEN-XCJ-FILE.
           PERFORM OPEN-IT.
           MOVE XCJ-PATH TO E-FILE.
           OPEN I-O XCJ-FILE.
           IF IO-FG = 8
             GO TO OPEN-XCJ-FILE.
           IF IO-FG = 7
             CLOSE XCJ-FILE
             GO TO OPEN-XCJ-FILE.
           UNLOCK XCJ-FILE.
       OPEN-XCJ-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XCJ-PATH TO E-FILE.
           OPEN OUTPUT XCJ-FILE.
           IF IO-FG = 8
             GO TO OPEN-XCJ-FILE.
           IF IO-FG = 7
             CLOSE XCJ-FILE
             GO TO OPEN-XCJ-FILE.
           UNLOCK XCJ-FILE.
       READ-XCJ-FILE.
           PERFORM READ-IT.
           MOVE XCJ-PATH TO E-FILE.
           MOVE XCJ-KEY TO E-KEYX.
           READ XCJ-FILE.
           IF IO-FG = 8
             GO TO READ-XCJ-FILE.
       WRITE-XCJ-FILE.
           PERFORM WRITE-IT.
           MOVE XCJ-PATH TO E-FILE.
           MOVE XCJ-KEY TO E-KEYX.
           WRITE XCJ-REC.
           IF IO-FG = 8
             GO TO WRITE-XCJ-FILE.
           UNLOCK XCJ-FILE.
       REWRITE-XCJ-FILE.
           PERFORM REWRITE-IT.
           MOVE XCJ-PATH TO E-FILE.
           MOVE XCJ-KEY TO E-KEYX.
           REWRITE XCJ-REC.
           IF IO-FG = 8
             GO TO REWRITE-XCJ-FILE.
           UNLOCK XCJ-FILE.
       DELETE-XCJ-FILE.
           PERFORM DELETE-IT.
           MOVE XCJ-PATH TO E-FILE.
           MOVE XCJ-KEY TO E-KEYX.
           DELETE XCJ-FILE.
           IF IO-FG = 8
             GO TO DELETE-XCJ-FILE.
           UNLOCK XCJ-FILE.
       CLOSE-XCJ-FILE.
           PERFORM CLOSE-IT.
           CLOSE XCJ-FILE.
