      ******************************************************************
      * LINKAGE BUFFER USED FOR CALLING GB/HELP.CBL
      *  PUT THE HELP FILE NAME INTO HELP-LINK-FILE.
      *  PUT THE FIELD YOU WANT HELP ON IN HELP-LINK-FIELD.
      *  PUT THE HELP-FIELD BUFFER IN HELP-LINK-FIELD-INFO.

      * MJD 121219 WORKING ON GETTING HELP TO WORK.
      ******************************************************************
      *78  HELP-INFO-SIZE           VALUE 2400.
       01  HELP-LINK-BUF.
           03  HELP-LINK-PATH-INFO.
               05  HELP-LINK-DIR        PIC X(02).
               05  HELP-LINK-FILE       PIC X(20).
           03  HELP-LINK-FIELD      PIC X(15).
      *    03  HELP-LINK-INFO       PIC X(HELP-INFO-SIZE).
