      * COPYMEMBER: LIBLP/LPED1IN
      *
      * THIS IS JUST A WORK FILE IN /USR/TMP; DO NOT ADD A LOAD TO THIS
       OPEN-ED1-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE ED-PATH TO E-FILE.
           OPEN OUTPUT ED-FILE.
           IF IO-FG = 8
             GO TO OPEN-ED1-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE ED-FILE
             GO TO OPEN-ED1-FILE-OUTPUT.
       OPEN-ED1-FILE.
           PERFORM OPEN-IT.
           MOVE ED-PATH TO E-FILE.
           OPEN I-O ED-FILE.
           IF IO-FG = 8
             GO TO OPEN-ED1-FILE.
           IF IO-FG = 7
             CLOSE ED-FILE
             GO TO OPEN-ED1-FILE.
           UNLOCK ED-FILE.
       READ-ED1-FILE.
           PERFORM READ-IT.
           MOVE ED-PATH TO E-FILE.
           MOVE ED-KEY TO E-KEYX.
           READ ED-FILE.
           IF IO-FG = 8
              GO TO READ-ED1-FILE.
       READ-ED1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ED-PATH TO E-FILE.
           MOVE ED-KEY TO E-KEYX.
           READ ED-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-ED1-FILE-NEXT.
       START-ED1-FILE.
           PERFORM START-IT.
           MOVE ED-PATH TO E-FILE.
           MOVE ED-KEY TO E-KEYX.
           START ED-FILE KEY NOT < ED-KEY.
           UNLOCK ED-FILE.
       WRITE-ED1-FILE.
           PERFORM WRITE-IT.
           MOVE ED-PATH TO E-FILE.
           MOVE ED-KEY TO E-KEYX.
           WRITE ED-REC.
           IF IO-FG = 8
             GO TO WRITE-ED1-FILE.
           UNLOCK ED-FILE.
       REWRITE-ED1-FILE.
           PERFORM REWRITE-IT.
           MOVE ED-PATH TO E-FILE.
           MOVE ED-KEY TO E-KEYX.
           REWRITE ED-REC.
           IF IO-FG = 8
             GO TO REWRITE-ED1-FILE.
           UNLOCK ED-FILE.
       DELETE-ED1-FILE.
           PERFORM DELETE-IT.
           MOVE ED-PATH TO E-FILE.
           MOVE ED-KEY TO E-KEYX.
           DELETE ED-FILE.
           IF IO-FG = 8
             GO TO DELETE-ED1-FILE.
           UNLOCK ED-FILE.
       CLOSE-ED1-FILE.
           PERFORM CLOSE-IT.
           CLOSE ED-FILE.
