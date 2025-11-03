       OPEN-REPAY-NO-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE REPAY-NO-IFN TO E-FILE.
           OPEN OUTPUT REPAY-NO-FILE.
           IF IO-FG = 8
              GO TO OPEN-REPAY-NO-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE REPAY-NO-FILE
              GO TO OPEN-REPAY-NO-FILE-OUTPUT.
       WRITE-REPAY-NO-FILE.
           PERFORM WRITE-IT.
           MOVE REPAY-NO-IFN TO E-FILE.
           WRITE REPAY-NO-REC.
           IF IO-FG = 8
              GO TO WRITE-REPAY-NO-FILE.
       CLOSE-REPAY-NO-FILE.
           PERFORM CLOSE-IT.
           CLOSE REPAY-NO-FILE.
