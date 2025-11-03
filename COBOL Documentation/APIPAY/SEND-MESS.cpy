       SEND-MESS.
           IF ( EXT-API-SCREEN-DISABLE = "Y" )
              MOVE MESS TO EXT-API-MESS
           ELSE
              PERFORM ALARM
              DISPLAY MESSAGE BOX MESS.
       COPY "LIBGB/TMMSG.CPY".
       COPY "LIBGB/ERRMSG.CPY".

