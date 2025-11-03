      * COPYMEMBER: LIBGB/GBFRVGB
      ******************************************************************
      *
      * IDENTIFICATION: LIBGB/GBFRVGB
      *
      * DESCRIPTION   : TEMPORARY VISION FILE CREATION - VGBFILE
      *                 VGBFILE IS TO HELP IMPROVE PERFORMANCE
      *                 AND RECOVERABILITY IN LP/EOMALL.
      *
      *=================================================================
      * REV :
      * JKC 2024-0829 ***NEW***
      *
      ******************************************************************
      *
      *   C R E A T E - V R C  - F I L E
      *
      *=================================================================
      * IN  : GB-KEY
      * OUT : VGB-FILE                                    
      * DESC: LOAD SELECTED GB-REC INTO TEMPORARY VISION WORK FILE; THE
      *       REST OF THIS PROGRAM WILL USE THIS WORK FILE INSTEAD OF
      *       GBFILE.
      *       THE EXISTING RECORDS COULD BE MODIFIED; NEW RECORDS
      *       WILL BE CREATED FOR NEXT MONTH.
      *       AT THE END OF PROGRAM WILL USE WK-GB-FILE TO BE USED
      *       TO LOAD ASCII FILE TO BE LOADED INTO SQL.
      ******************************************************************
       CREATE-VGB-FILE SECTION.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO VGB-PATH.
           PERFORM OPEN-VGB-FILE-OUTPUT.
           PERFORM CLOSE-VGB-FILE.
           PERFORM OPEN-VGB-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES         TO QGB-WBEG-BRNO QGB-WBEG-NO.
           MOVE ALL "9"        TO QGB-WEND-BRNO QGB-WEND-NO.

           PERFORM OPEN-GB-FILE.
           PERFORM START-GB-FILE.

      *=================================================================
       CREATE-VGB-NEXT-GB.

           PERFORM READ-GB-FILE-NEXT.
           IF ( IO-FG NOT = ZEROES )
              GO TO CREATE-VGB-FILE-END.

           MOVE GB-KEY  TO VGB-KEY.
           MOVE GB-REC  TO VGB-GB-REC.

           PERFORM WRITE-VGB-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

           GO TO CREATE-VGB-NEXT-GB.

      *=================================================================
       CREATE-VGB-FILE-END.

           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-VGB-FILE. 

      *-----------------------------------------------------------------
       CREATE-VGB-FILE-EXIT.
           EXIT.

