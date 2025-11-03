      * COPYMEMBER: LIBGB/PHONEIO
      ***********************************************************
      *                      PHONE-IO             
      ***********************************************************
      *  ENTER-PHONE: CONVERTS EDITED BUFFER TO TEL-BUF 9(10)
      *
      * REV: JTG 122188 ADDED NUMERIC EDIT TESTS
      ***********************************************************
       ENTER-PHONE.
           MOVE 0 TO TEL-BUF.
           MOVE VDUBUF TO TEL-EDIT.
           IF TEL-ED1 NUMERIC
              MOVE TEL-ED1 TO TEL-1.
           IF TEL-ED2 NUMERIC
              MOVE TEL-ED2 TO TEL-2.
           IF TEL-ED3 NUMERIC
              MOVE TEL-ED3 TO TEL-3.

      * IF DIDNT ENTER DASHES, JUST MOVE 10 NUMERIC NUMBERS, IGNORE LAST
      * 2 POSITIONS
           IF TEL-H1 NOT = "-"
              MOVE TEL-EDIT-NUM TO TEL-BUF.

      ************************************************************
      *  SEND-PHONE: MOVE 9(10) COMP-3 PHONE # FIELD TO TEL-BUF..
      *  EDITED X(14) PHONE # APPEARS IN TEL-EDIT 
      ************************************************************
       SEND-PHONE.
           MOVE TEL-1 TO TEL-ED1.
           MOVE TEL-2 TO TEL-ED2.
           MOVE TEL-3 TO TEL-ED3.
      ************************************************************
