       OPEN-HE-FILE.
           PERFORM OPEN-IT.
           MOVE HE-PATH TO E-FILE.
           OPEN INPUT HE-FILE.
           IF IO-FG = 8
              GO TO OPEN-HE-FILE.
           IF IO-FG = 7
              CLOSE HE-FILE
              GO TO OPEN-HE-FILE.
       READ-HE-FILE.
           PERFORM READ-IT.
           MOVE HE-PATH TO E-FILE.
           READ HE-FILE.
           IF IO-FG = 8
              GO TO READ-HE-FILE.
       CLOSE-HE-FILE.
           PERFORM CLOSE-IT.
           CLOSE HE-FILE.
