      * COPYMEMBER: LIBGB/GBFRVRC
      ******************************************************************
      *
      * IDENTIFICATION: LIBGB/GBFRVRC
      *
      * DESCRIPTION   : TEMPORARY VISION FILE CREATION - VRCFILE
      *                 VRCFILE IS TO HELP IMPROVE PERFORMANCE
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
      * IN  : RC1-KEY
      * OUT : VRC-FILE                                    
      * DESC: LOAD SELECTED RC-REC INTO TEMPORARY VISION WORK FILE; THE
      *       REST OF THIS PROGRAM WILL USE THIS WORK FILE INSTEAD OF
      *       RCFILE.
      *       THE EXISTING RECORDS COULD BE MODIFIED; NEW RECORDS
      *       WILL BE CREATED FOR NEXT MONTH.
      *       AT THE END OF PROGRAM WILL USE WK-RC1-FILE TO BE USED
      *       TO LOAD ASCII FILE TO BE LOADED INTO SQL.
      ******************************************************************
       CREATE-VRC1-FILE SECTION.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO VRC-PATH.
           PERFORM OPEN-VRC1-FILE-OUTPUT.
           PERFORM CLOSE-VRC1-FILE.
           PERFORM OPEN-VRC1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES         TO QRC1-WBEG-BRNO.
           MOVE ALL "9"        TO QRC1-WEND-BRNO.
           MOVE RC-TRANS-DATE  TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WBEG-TRANS-DATE
                                        QRC1-WEND-TRANS-DATE.

           PERFORM OPEN-RC1-FILE.
           PERFORM START-RC1-FILE.

      *=================================================================
       CREATE-VRC1-NEXT-RC1.

           PERFORM READ-RC1-FILE-NEXT.
           IF ( IO-FG NOT = ZEROES )
              GO TO CREATE-VRC1-FILE-END.

           MOVE RC1-KEY TO VRC1-KEY.
           MOVE RC2-KEY TO VRC2-KEY.
           MOVE RC-REC  TO VRC-RC-REC.

           PERFORM WRITE-VRC1-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

           GO TO CREATE-VRC1-NEXT-RC1.

      *=================================================================
       CREATE-VRC1-FILE-END.

           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-VRC1-FILE. 

      *-----------------------------------------------------------------
       CREATE-VRC1-FILE-EXIT.
           EXIT.

