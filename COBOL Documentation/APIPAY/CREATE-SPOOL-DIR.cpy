      ******************************************************************
      *
      *  CREATE SPOOL DIRECTORY IN /XXX/XXXX/R#/EO
      *
      * STORE SPOOL FILE NAME AS /XXX/XXXX/R$/EO/....
      * WE NEED A FULL PATH NAME HERE ELSE DIR WILL CHANGE WHEN WE
      * CHANGE DIRECTORIES TO ANOTHER BRANCH.
      *
      * CHANGED TO ACCESS AS ../../ORIG/R1/EO/STEPBPREF, WITH ORIG
      * BEING ORIG DATA PATH, FULL PATH TOO LONG FOR ACCESS-BUF
      ******************************************************************
       CREATE-SPOOL-DIR SECTION.
           MOVE " " TO ERRCD.
           MOVE EXT-FILPATH-BASE      TO EX-SPOOL-BASE.
           MOVE EXT-FILPATH-MACHINE   TO EX-SPOOL-MACHINE.
           MOVE HOLD-EXT-FILPATH-DATA TO EX-SPOOL-DATA-PATH.
           ACCEPT DATE-YYYYMMDD FROM CENTURY-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO EX-SPOOL-DATE.
           MOVE " "                   TO EX-SPOOL-FILE.
           MOVE EX-SPOOL              TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING DIR-ACCESS-BUF GIVING STAT-99
              IF STAT NOT = "00"
                 MOVE "CANNOT CREATE SPOOL DIRECTORY" TO LOG-MSG
                 MOVE 78                              TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 MOVE "E" TO ERRCD
                 GO TO EXIT-CREATE-SPOOL-DIR.

           MOVE "/"                TO EX-SPOOL-FILE.
           MOVE SAVE-EX-SPOOL-NAME TO EX-SPOOL-NAME.
           MOVE "   "              TO EX-SPOOL-SUF.

      *****************************************************************
      * THEY ARE ALLOWED TO RUN THIS  MORE THAN ONCE ON THE
      * SAME TRANSACTION DATE, SO CHECK TO SEE IF REPORT EXISTS, IF
      * SO, INCREMENT THE STEP # SO WE DON'T WIPE OUT PRIOR REPORTS
      *****************************************************************
           MOVE 100 TO STEP-COUNT.
       TRY-NEXT-STEP-NO.
           MOVE STEP-COUNT TO EX-SPOOL-STEP.
           MOVE EX-SPOOL TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
             IF (EX-SPOOL-NAME = "REPOEX")
                GO TO EXIT-CREATE-SPOOL-DIR
             ELSE
             IF STEP-COUNT < 999
                ADD 1 TO STEP-COUNT
                GO TO TRY-NEXT-STEP-NO.

       EXIT-CREATE-SPOOL-DIR.
           EXIT.

      ******************************************************************
