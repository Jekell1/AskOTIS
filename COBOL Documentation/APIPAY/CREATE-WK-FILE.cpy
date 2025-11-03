      ******************************************************************
       CREATE-WK-FILE SECTION.
      ******************************************************************

      *  CREATE WK-FILE, IF RUNNING FROM EOCRON, WORK FILE IN /USR/TMP
      *  WAS NOT CREATED IN LONPA0

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.

      * WK-FILE FOR LOAN PROCESSING USED PROCESS ID XL(PROCESS ID)

           MOVE PROCESS-ID-BUF TO USER-PID TEMP-PID.
           MOVE 9 TO IO-FG.
           PERFORM OPEN-IT.
           MOVE WK-IFN TO E-FILE.
       OPEN-WK-OUTPUT.
           OPEN OUTPUT WK-FILE.
           IF ( IO-FG = 8 )
              GO TO OPEN-WK-OUTPUT.
           IF ( IO-FG = 7 )
              PERFORM CLOSE-WK-FILE
              GO TO OPEN-WK-OUTPUT.
           PERFORM CLOSE-WK-FILE.

      *********************************
      *   NAME: CDV-VERIFY
      *   DESC: VERIFY LP-TRCD & YIELD SUBSCRIPT
      *   IN  : LP-TRCD, CDV-NN
      *   OUT : CDV-SUB; 0 = NOT_FOUND
      *********************************
