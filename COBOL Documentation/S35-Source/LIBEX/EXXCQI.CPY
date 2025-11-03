      * COPYMEMBER: LIBEX/EXXCQI
      **************************************************************
      *                (EXXCQI)
      **************************************************************
       OPEN-XCQ-FILE.
           PERFORM OPEN-IT.
           MOVE XCQ-PATH TO E-FILE.
           OPEN I-O XCQ-FILE.
           IF IO-FG = 8
             GO TO OPEN-XCQ-FILE.
           IF IO-FG = 7
             CLOSE XCQ-FILE
             GO TO OPEN-XCQ-FILE.
           UNLOCK XCQ-FILE.
       OPEN-XCQ-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XCQ-PATH TO E-FILE.
           OPEN OUTPUT XCQ-FILE.
           IF IO-FG = 8
             GO TO OPEN-XCQ-FILE.
           IF IO-FG = 7
             CLOSE XCQ-FILE
             GO TO OPEN-XCQ-FILE.
           UNLOCK XCQ-FILE.
       READ-XCQ-FILE.
           PERFORM READ-IT.
           MOVE XCQ-PATH TO E-FILE.
           MOVE XCQ-KEY TO E-KEYX.
           READ XCQ-FILE.
           IF IO-FG = 8
             GO TO READ-XCQ-FILE.
       WRITE-XCQ-FILE.
           PERFORM WRITE-IT.
           MOVE XCQ-PATH TO E-FILE.
           MOVE XCQ-KEY TO E-KEYX.
           WRITE XCQ-REC.
           IF IO-FG = 8
             GO TO WRITE-XCQ-FILE.
           UNLOCK XCQ-FILE.
       REWRITE-XCQ-FILE.
           PERFORM REWRITE-IT.
           MOVE XCQ-PATH TO E-FILE.
           MOVE XCQ-KEY TO E-KEYX.
           REWRITE XCQ-REC.
           IF IO-FG = 8
             GO TO REWRITE-XCQ-FILE.
           UNLOCK XCQ-FILE.
       DELETE-XCQ-FILE.
           PERFORM DELETE-IT.
           MOVE XCQ-PATH TO E-FILE.
           MOVE XCQ-KEY TO E-KEYX.
           DELETE XCQ-FILE.
           IF IO-FG = 8
             GO TO DELETE-XCQ-FILE.
           UNLOCK XCQ-FILE.
       CLOSE-XCQ-FILE.
           PERFORM CLOSE-IT.
           CLOSE XCQ-FILE.
