      * COPYMEMBER: LIBGB/CAPASSWD
      *************************************************
       ENTER-CASH-DRAWER-PASSWORD SECTION.
      *************************************************
       ENTER-CASH-DRAWER-PASS.
           MOVE SPACES TO CAPASSWD-VDUSTATUS.
           MOVE SPACES TO CAPASSWD-BUF.

           MOVE "E  A040CAPASSWD." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUBUF TO CAPASSWD-BUF.

           IF VDUSTATUS NOT = "00"
              MOVE VDUSTATUS TO CAPASSWD-VDUSTATUS
              MOVE "V" TO CAPASSWD-OKAY
              GO TO EXIT-ENTER-DRAWER-PASSWORD.

           IF CAPASSWD-BUF = SPACES
              MOVE "VALID PASSWORD IS REQUIRED, TRY AGAIN " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CASH-DRAWER-PASS.

           PERFORM FIND-CASH-DRAWER.
           IF CAPASSWD-OKAY = "N"
              MOVE "VALID PASSWORD IS REQUIRED, TRY AGAIN " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CASH-DRAWER-PASS.

       EXIT-ENTER-DRAWER-PASSWORD.
           MOVE CAPASSWD-VDUSTATUS TO VDUSTATUS.

      * ----------------------------------------------------
      *     PASSWORD/TEST THE CASH DRAWER KEY BRNO,DATE,TILL
      *     "N"  =  NEW CASH DRAWER
      *     "O"  =  OLD CASH DRAWER
      *     "M"  =  MODIFY PASSWORD TO OLD DRAWER
      * ----------------------------------------------------
       FIND-CASH-DRAWER SECTION.
           MOVE " " TO CAPASSWD-OKAY.

      *    IF PATH-MACHINE NOT = " "
      *       MOVE PATH-MACHINE TO CA-ALL-MACHINE
      *       MOVE PATH-DATA TO CA-ALL-DATA
      *       MOVE CA-ALL-PATH TO CA-PATH.

           PERFORM OPEN-CA1-FILE.

           MOVE HOLD-CA1-KEY TO CA1-KEY.
           IF CA-DATE = 0
              MOVE 19000101  TO CA-DATE.

           MOVE CA-PATH-OWNBR        TO QCA1-WBEG-BRNO
                                        QCA1-WEND-BRNO.
           MOVE CA-DATE              TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QCA1-WBEG-DATE
                                        QCA1-WEND-DATE.
           MOVE 0            TO CA-TILLNO
                                QCA1-WBEG-TILLNO.
           MOVE ALL "9"              TO QCA1-WEND-TILLNO.

           PERFORM START-CA1-FILE.
       NEXT-CA-PASSWORD.
           PERFORM READ-CA1-FILE-NEXT.
           IF IO-FG = 9
              GO TO NO-MATCH-PASSWORD.

           IF CA-BRNO NOT = HOLD-CA-BRNO
              GO TO NO-MATCH-PASSWORD.

           IF CA-DATE NOT = HOLD-CA-DATE
              GO TO NO-MATCH-PASSWORD.

           IF CA-PASSWORD NOT = SPACES
              IF CA-PASSWORD = CAPASSWD-BUF
                 GO TO MATCH-PASSWORD.

           GO TO NEXT-CA-PASSWORD.

       MATCH-PASSWORD.
           MOVE "Y" TO CAPASSWD-OKAY.
           GO TO EXIT-FIND-CASH-DRAWER.
       NO-MATCH-PASSWORD.
           MOVE "N" TO CAPASSWD-OKAY.
       EXIT-FIND-CASH-DRAWER.
           PERFORM CLOSE-CA1-FILE.

           MOVE SPACES TO CAPASSWD-BUF WR-BUF.
           MOVE CAPASSWD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

