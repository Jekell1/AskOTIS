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
       END-DECLARE.
           EXIT.
       END DECLARATIVES.
