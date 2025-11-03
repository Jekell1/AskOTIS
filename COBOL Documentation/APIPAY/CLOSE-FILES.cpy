       CLOSE-FILES.

      * OTIS MEMORY USAGE LOG
           IF ( EXT-TCLP-ACUMEM-UPDATE )
              CALL "ACUMEM" USING FORM-PATHNAME 
              CANCEL "ACUMEM".

      * OTIS TRACE USAGE LOG
           IF ( EXT-TCLP-TRACE-UPDATE ) 
              MOVE "PROGRAM FINISH (LIBGB/DECLARE)"
                TO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME
              CANCEL "TRACE".

           MOVE "CLOSE ERROR" TO E-MSG.
      *================================================================*
      * END COPYBOOK: LIBGB\DECLARE.CPY                                *
      *================================================================*
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           PERFORM CLOSE-LXG1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           CLOSE WK-FILE OP-FILE LOG-FILE.
      *================================================================*
      * EMBEDDED COPYBOOK: LIBGB\DECLAR2.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBGB/DECLAR2
      ******************************************************************
      * REV:
      * JKC 210702 ADDED SQL-DISCONNECT BEFORE 'STOP RUN'; WE FOUND THAT
      *            THE USE OF SQL-DISCONNECT WHEN EXITING THE
      *            APPLICATION IS ONLY NEEDED; DOING A SQL-DISCONNECT
      *            WHEN EXITING A PROGRAM WILL CAUSE ALL CURSORS TO BE
      *            CLOSED FROM THE CALLING PROGRAM WHICH EFFECTS IT 
      *            WHEN CONTINUING WITH A READ-NEXT.
      *
      * JKC 230907 COMMENTED OUT DECLARE-STOP PARAGRAPH; SEARCH FOUND
      *            NO 'PERFORM' OR 'GO TO' FOR IT, AND THERE WAS
      *            FALL-THRU LOGIC THAT IT COULD FLOW INTO IT.
      *
      ******************************************************************
      *DECLARE-STOP.
      * NEED TO BE LAST BEFORE 'STOP RUN'
      *    PERFORM SQL-DISCONNECT.
      *    STOP RUN.
