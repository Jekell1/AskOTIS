      ******************************************************************
      * CALL-TMMSG
      *
      * THIS IS JUST A COMMENT TO DISPLAY ON LINE 24, NO BEEP, NO PRESS
      *  RETURN, JUST DISPLAY A MESSAGE
      *
      *  ******   WARNING   ******
      *
      * THIS SAME LOGIC ALSO APPEARS IN LIBGB/SCREEN (VDU-SEND-TMMSG)
      *  IF YOU CHANGE IT HERE CHANGE IT THERE AS WELL. /MJD
      ******************************************************************
       CALL-TMMSG.
           DISPLAY MESS
              LINE 024
              COL 001
              SIZE 61
              BOLD
              COLOR IS 5
              OUTPUT LEFT.

