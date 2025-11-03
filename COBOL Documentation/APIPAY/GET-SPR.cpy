      *================================================================*
      * END COPYBOOK: LIBGB\RMFILE.CPY                                *
      *================================================================*
       GET-SPR.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           MOVE SP1-KEY TO HOLD-SP1-KEY.
