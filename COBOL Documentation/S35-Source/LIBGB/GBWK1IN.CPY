      * COPYMEMBER: LIBGB/GBW1IN
       OPEN-WK1-FILE.
           PERFORM OPEN-IT.
           MOVE WK1-IFN TO E-FILE.
           OPEN I-O WK1-FILE.
           IF IO-FG = 8
             GO TO OPEN-WK1-FILE.
           IF IO-FG = 7
             CLOSE WK1-FILE
             GO TO OPEN-WK1-FILE.
           UNLOCK WK1-FILE.
       OPEN-WK1-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK1-IFN TO E-FILE.
           OPEN OUTPUT WK1-FILE.
           IF IO-FG = 8
             GO TO OPEN-WK1-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE WK1-FILE
             GO TO OPEN-WK1-FILE-OUTPUT.
           UNLOCK WK1-FILE.
       READ-WK1-FILE.
           PERFORM READ-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           READ WK1-FILE.
           IF IO-FG = 8
              GO TO READ-WK1-FILE.
       READ-WK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           READ WK1-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-WK1-FILE-NEXT.
       START-WK1-FILE.
           PERFORM START-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           START WK1-FILE KEY NOT < WK1-KEY.
           UNLOCK WK1-FILE.
       WRITE-WK1-FILE.
           PERFORM WRITE-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           WRITE WK1-REC.
           IF IO-FG = 8
             GO TO WRITE-WK1-FILE.
           UNLOCK WK1-FILE.
       REWRITE-WK1-FILE.
           PERFORM REWRITE-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           REWRITE WK1-REC.
           IF IO-FG = 8
             GO TO REWRITE-WK1-FILE.
           UNLOCK WK1-FILE.
       DELETE-WK1-FILE.
           PERFORM DELETE-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           DELETE WK1-FILE.
           IF IO-FG = 8
             GO TO DELETE-WK1-FILE.
           UNLOCK WK1-FILE.

      *-----------------------------------------------------------------
       START-WK1-FILE-GREATER.
           PERFORM START-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           START WK1-FILE KEY > WK1-KEY.

      *-----------------------------------------------------------------
       START-WK1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           START WK1-FILE KEY NOT > WK1-KEY.

      *-----------------------------------------------------------------
       READ-WK1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           READ WK1-FILE PREVIOUS.

           IF ( IO-FG = 8 )
              GO TO READ-WK1-FILE-PREVIOUS.

      *-----------------------------------------------------------------
       CLOSE-WK1-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK1-FILE.
