      * COPYMEMBER: LIBEX/EXXCLI
      **************************************************************
      *                (EXXCLI)
      **************************************************************
       OPEN-XCL-FILE.
           PERFORM OPEN-IT.
           MOVE XCL-PATH TO E-FILE.
           OPEN I-O XCL-FILE.
           IF IO-FG = 8
             GO TO OPEN-XCL-FILE.
           IF IO-FG = 7
             CLOSE XCL-FILE
             GO TO OPEN-XCL-FILE.
           UNLOCK XCL-FILE.
       OPEN-XCL-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XCL-PATH TO E-FILE.
           OPEN OUTPUT XCL-FILE.
           IF IO-FG = 8
             GO TO OPEN-XCL-FILE.
           IF IO-FG = 7
             CLOSE XCL-FILE
             GO TO OPEN-XCL-FILE.
           UNLOCK XCL-FILE.
       READ-XCL-FILE.
           PERFORM READ-IT.
           MOVE XCL-PATH TO E-FILE.
           MOVE XCL-KEY TO E-KEYX.
           READ XCL-FILE.
           IF IO-FG = 8
             GO TO READ-XCL-FILE.
       WRITE-XCL-FILE.
           PERFORM WRITE-IT.
           MOVE XCL-PATH TO E-FILE.
           MOVE XCL-KEY TO E-KEYX.
           WRITE XCL-REC.
           IF IO-FG = 8
             GO TO WRITE-XCL-FILE.
           UNLOCK XCL-FILE.
       REWRITE-XCL-FILE.
           PERFORM REWRITE-IT.
           MOVE XCL-PATH TO E-FILE.
           MOVE XCL-KEY TO E-KEYX.
           REWRITE XCL-REC.
           IF IO-FG = 8
             GO TO REWRITE-XCL-FILE.
           UNLOCK XCL-FILE.
       DELETE-XCL-FILE.
           PERFORM DELETE-IT.
           MOVE XCL-PATH TO E-FILE.
           MOVE XCL-KEY TO E-KEYX.
           DELETE XCL-FILE.
           IF IO-FG = 8
             GO TO DELETE-XCL-FILE.
           UNLOCK XCL-FILE.
       CLOSE-XCL-FILE.
           PERFORM CLOSE-IT.
           CLOSE XCL-FILE.

