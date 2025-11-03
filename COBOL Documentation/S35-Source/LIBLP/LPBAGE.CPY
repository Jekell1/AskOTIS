      * COPYMEMBER: LIBLP/LPBAGE
      ********************************************************************
      *         COMPUTE BORROWERS AGE IN YEARS
      *
      * COMPUTE ELAPSED TIME FROM BAGE-DATE TO BAGE-TODAY
      *
      *   NAME: LPBAGE
      *   DESC: DETERMINES THE BORROWERS AGE
      *   IN  : BAGE-DATE      (BW-BDATE)
      *         BAGE-REFDATE   (BW-REFDATE)
      *         BAGE-TODAY     (TODAYS DATE)
      *
      *   OUT : BAGE-AGE       NO. OF YEARS OLD (AGE OF BORROWER)
      *
      *   COPY: LPBAGEW
      *REV:
      * JTG 000118 CHANGED ROUTINE TO USE JULIAN-CC AND NOT 50
      * JTG 000119 CHANGED ROUTINE TO USE USE TIM360 AND NOT TIM365
      * BAH 130430 ADDED BAGE-AGE-MONTHS FOR SP-INSREJECT-FG "E"
      *                                              CASHCO PR# 4261
      * BLM 170418 REMOVE LOGIC BASED ON JULIAN-CC, BW-BDATE IS 8 YYYYMMDD
      ********************************************************************
       BAGE-CALCULATION SECTION.
           MOVE 0 TO BAGE-AGE.

           IF BAGE-DATE = 0
              GO TO BAGE-CALCULATION-EXIT.

           MOVE BAGE-DATE  TO NUM-DATE.
           MOVE BAGE-TODAY TO SYS-DATE.
           PERFORM TIM360.

           COMPUTE BAGE-AGE = ELAPSED-MONTHS / 12.
           MOVE ELAPSED-MONTHS TO BAGE-AGE-MONTHS.

       BAGE-CALCULATION-EXIT.
           EXIT.
