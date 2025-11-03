       FILE-ERRORS-TERMINATE.
      *    A30, STARRED OFF, 7/26/12, BLM:
      *    IF WINDOWS-OPEN > ZERO
      *       CALL DELWIN
      *       SUBTRACT 1 FROM WINDOWS-OPEN
      *       GO TO FILE-ERRORS-TERMINATE.
           MOVE "E" TO ERRCD.
           IF FILE-STAT = "98"
              IF (FORM-PATHNAME(16:4) = "LONP" OR "XONP")
                 MOVE "Z" TO ERRCD.
           PERFORM CLOSE-FILES.
           GO TO EXIT-PROG.
