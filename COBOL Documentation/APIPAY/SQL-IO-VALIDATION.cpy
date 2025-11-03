       SQL-IO-VALIDATION.

      * GOOD
           IF ( E-MSG4  = "OPEN" ) AND 
              ( SQLCODE = 1      )
              EXIT PARAGRAPH
           ELSE
      * GOOD
           IF ( SQLCODE = 0      ) 
              EXIT PARAGRAPH.

           MOVE 9 TO IO-FG.

      * EOF OR ROW NOT FOUND
           IF (( E-MSG4  = "READ" )  OR
               ( E-MSG4  = "ROW " )) AND
              (  SQLCODE = 100     )
              EXIT PARAGRAPH.

      * DUPLICATE ROW
           IF ( E-MSG4   = "WRIT"  ) AND   
      * LIKE VISION "22,00"
              ( SQLSTATE = "23000" )
              MOVE "22"    TO FILE-STAT
              MOVE "00"    TO SECONDARY-FILE-STAT
              MOVE "22,00" TO FILE-STAT-DISP
                              E-STATUS-ERROR
              EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE E-FILE        TO E-FILEX ERRLOGW-FILE.
           MOVE FORM-PATHNAME TO E-PROG ERRLOGW-PROGX.
           MOVE E-MSG         TO ERRLOGW-TYPE.
           MOVE E-CODE        TO ERRLOGW-CODE.
           MOVE E-SUBCODE     TO ERRLOGW-SUBCODE.
           MOVE E-KEYX        TO ERRLOGW-KEY.
           MOVE "E"           TO ERRLOGW-ACTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CL/ERRSQL"   TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA
                                 SQLCA EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "E" TO ERRCD.
           IF FILE-STAT = "98"
              IF (FORM-PATHNAME(16:4) = "LONP" OR "XONP")
                 MOVE "Z" TO ERRCD.

      * DEFINED IN LIBGB/CONNECT_SQL.CPY
           PERFORM SQL-ERROR. 

      * ONLY CLOSE FILES/TABLES WHEN THERE IS AN SQL CONNECTION
      * OTHERWISE YOU CAN GET INTO AN INIFITE LOOP DUE TO THE
      * SQL-DISCONNECT IN SQL-ERROR WHICH THEN YOU CANNOT CLOSE CURSORS
      * IF THE CONNECTION HAS BEEN CLOSED.
      * NOTE: THIS WORKS FOR TABLES; NOT SURE WHAT HAPPENS WITH VISION
      *       WORK FILES, IMPORT & EXPORT EXTRACTION FILES; BELIEVE
      *       THEY SHOULD WORK FINE IF GOING RIGHT BACK INTO THE
      *       PROGRAM (THEORY IS THE CLOSING OF A PROGRAM WILL CLOSE
      *       THE FILES).                           BAH & JKC 2022-0214

      * CLOSE VISION FILES
           IF ( EXT-ACUSQL-CONNECT-STAT-GOOD )
              PERFORM CLOSE-FILES. 

      * FORCE EXIT OF PROGRAM
           GO TO EXIT-PROG.

