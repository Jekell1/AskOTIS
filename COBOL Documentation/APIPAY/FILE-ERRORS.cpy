      *    DISPLAY OMITTED WITH BEEP.
       FILE-ERRORS.
           MOVE E-FILE TO E-FILEX ERRLOGW-FILE.
           MOVE FORM-PATHNAME TO E-PROG ERRLOGW-PROGX.
           MOVE E-MSG TO ERRLOGW-TYPE.
           MOVE E-CODE TO ERRLOGW-CODE.
           MOVE E-SUBCODE TO ERRLOGW-SUBCODE.
           MOVE E-KEYX TO ERRLOGW-KEY.
           MOVE "E" TO ERRLOGW-ACTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE FORM-PATHNAME TO FORM-PATH.
           IF PIGGY-BACK-FLAG = "PIGGY"
              MOVE "SNDERR" TO PIGGY-BACK-FLAG
              GO TO FILE-ERRORS-TERMINATE.
           PERFORM ALARM.
           IF E-MSG4 = "OPEN" OR "WRIT" OR "REWR"
              MOVE "**** STOP ****"       TO ERRMSG-HEAD
              STRING "                    CALL HOME "
                     "OFFICE OR SUPPORT             "
                     DELIMITED BY SIZE INTO ERRMSG-MSG1
              STRING "AN ERROR HAS OCCURRED         "
                     "                DO NOT PROCEED"
                     DELIMITED BY SIZE INTO ERRMSG-MSG2
              MOVE "PRESS ENTER TO CONTINUE" TO ERRMSG-MSG3
              PERFORM CALL-ERRMSG.
           MOVE SPACES TO MESS.
      * THIS MUST STAY LIKE THIS, E-MSG HAS "OPEN ERROR" OR WHATEVER
      * IN IT, IT MUST GET MOVED AS BELOW
           MOVE ERROR-MSG-HEAD TO ERRMSG-HEAD.
           MOVE ERROR-MSG-STAT TO ERRMSG-MSG1.
           MOVE ERROR-MSG-PATH TO ERRMSG-MSG2.
      *    MOVE SPACES         TO ERRMSG-MSG3.
           MOVE ERROR-MSG-KEY  TO ERRMSG-MSG3.
           PERFORM CALL-ERRMSG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO MESS.
