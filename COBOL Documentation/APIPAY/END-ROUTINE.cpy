       END-ROUTINE.
           MOVE TEMP-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           PERFORM CLOSE-OP-FILE.

           GO TO END-PROGRAM.

      ************************************
