      ***************************************************************
      * LIBGB/ERRMSG
      * REV:
      * 140507 CHANGED TO USE DISPLAY MESSAGE BOX INSTEAD OF 
      *        CALLING GB/ERRMSG, VERYANT
      *
      * JKC 2024-0501 ADDED USE OF EXT-API-SCREEN-DISABLE AND
      *         EXT-API-MESS FOR SUPPRESS SCREEN INTERACTIONS FOR
      *         USE IN API PROGRAMS WITHOUT SCREENS.
      *
      ***************************************************************
       CALL-ERRMSG.
           IF ( EXT-API-SCREEN-DISABLE NOT = "Y" )
              DISPLAY MESSAGE BOX ERRMSG-HEAD, X"0A"
                                  ERRMSG-MSG1, X"0A"
                                  ERRMSG-MSG2, X"0A"
                                  ERRMSG-MSG3.

      *    MOVE FORM-PATHNAME TO FORM-PATH.
      *    MOVE "CO" TO FORM-OBJ.
      *    MOVE "GB/ERRMSG" TO FORM-NAM.
      *    CALL FORM-PROGX USING ERRMSG-BUF.
      *    CANCEL FORM-PROGX.
      *    IF ERRMSG-STAT = "~~"
      *       DISPLAY OMITTED ERASE SCREEN
      *       STOP RUN.
      *    MOVE FORM-PATHNAME TO FORM-PATH.
