      * COPYMEMBER: LIBLP/LPFRVBY
      ******************************************************************
      *
      * IDENTIFICATION: LIBLP/LPFRVBY
      *
      * DESCRIPTION   : TEMPORARY VISION FILE CREATION - VBYFILE
      *                 VBYFILE IS TO HELP IMPROVE PERFORMANCE
      *                 AND RECOVERABILITY IN LP/EOMALL.
      *
      *=================================================================
      * REV :
      * JKC 2024-0829 ***NEW***
      *
      ******************************************************************
      *
      *   C R E A T E - V B Y - F I L E
      *
      *=================================================================
      * IN  : BY1-KEY
      * OUT : VBY-FILE                                    
      * DESC: LOAD SELECTED BY-REC INTO TEMPORARY VISION WORK FILE; THE
      *       REST OF THIS PROGRAM WILL USE THIS WORK FILE INSTEAD OF
      *       BYA/BYB FILES.
      *       THE EXISTING RECORDS COULD BE MODIFIED; NEW RECORDS
      *       WILL BE CREATED FOR NEXT MONTH.
      *       AT THE END OF PROGRAM WILL USE WK-BY1-FILE TO BE USED
      *       TO LOAD ASCII FILE TO BE LOADED INTO SQL.
      ******************************************************************
       CREATE-VBY1-FILE SECTION.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO VBY-PATH.
           PERFORM OPEN-VBY1-FILE-OUTPUT.
           PERFORM CLOSE-VBY1-FILE.
           PERFORM OPEN-VBY1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE BY-DATE-CCYYMM TO QBY1-WBEG-DATE-CCYYMM
                                  QBY1-WEND-DATE-CCYYMM.

           MOVE ZEROES         TO QBY1-WBEG-BRNO.
           MOVE ALL "9"        TO QBY1-WEND-BRNO.
           MOVE ZEROES         TO QBY1-WBEG-CLASS.
           MOVE ALL "9"        TO QBY1-WEND-CLASS.

           PERFORM OPEN-BY1-FILE.
           PERFORM START-BY1-FILE.

      *=================================================================
       CREATE-VBY1-NEXT-BY1.

           PERFORM READ-BY1-FILE-NEXT.

           IF ( IO-FG NOT = ZEROES )
              GO TO CREATE-VBY1-FILE-END.

           MOVE BY1-KEY TO VBY1-KEY.
           MOVE BY-REC  TO VBY-BY-REC.
           PERFORM WRITE-VBY1-FILE.

           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

           GO TO CREATE-VBY1-NEXT-BY1.

      *=================================================================
       CREATE-VBY1-FILE-END.

           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-VBY1-FILE. 

      *-----------------------------------------------------------------
       CREATE-VBY1-FILE-EXIT.
           EXIT.

