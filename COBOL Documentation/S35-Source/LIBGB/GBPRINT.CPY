      * COPYMEMBER: LIBGB/GBPRINT
      **********
      * MJD 141015 ADDED CONTINUE TO EMPTY PARAGRAPHS ATTEMPTING TO
      *            FIX A STACK OVERFLOW BUG
      * BAH 2024.0419 REMOVED TRAP-INTERRUPT, INTERRUPTED-ONE-MOMENT,
      *               NPR-TRAP-INTERRUPT,  ENABLE-INTERRUPT,
      *               DISABLE-INTERRUPT, TEST-INTERRUPT
      **********
       OPEN-PR-FILE.
           PERFORM OPEN-IT.
           MOVE PRU-FILE TO E-FILE.
           OPEN OUTPUT PR-FILE.
           IF IO-FG = 8
              GO TO OPEN-PR-FILE.
           IF IO-FG = 7
              CLOSE PR-FILE
              GO TO OPEN-PR-FILE.
           PERFORM PRU-PREP.
       WRITE-PR-REC.
           PERFORM WRITE-IT.
           MOVE PRU-FILE TO E-FILE.
           IF LN-FEED = 99
              WRITE PR-REC AFTER PAGE
            ELSE
              WRITE PR-REC AFTER LN-FEED.
           IF IO-FG = 8
              GO TO WRITE-PR-REC.
       CLOSE-PR-FILE.
           PERFORM CLOSE-IT.
           CLOSE PR-FILE.

