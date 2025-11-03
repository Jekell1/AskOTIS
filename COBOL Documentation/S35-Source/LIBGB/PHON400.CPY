      * COPYMEMBER: LIBGB/PHON400
      *    **********************************
      *    *      PHONE DISPLAY WORKERS
      *    **********************************
       01  TEL-BUF         PIC 9(10).
       01  TEL-BUFX        REDEFINES TEL-BUF.
           03  TEL-1       PIC XXX.
           03  TEL-2       PIC XXX.
           03  TEL-3       PIC XXXX.
       01  TEL-EDIT.
           03  TEL-ED1     PIC XXX.
           03  TEL-H1      PIC X VALUE "-".
           03  TEL-ED2     PIC XXX.
           03  TEL-H2      PIC X VALUE "-".
           03  TEL-ED3     PIC XXXX.
       01  TEL-EDIT-X REDEFINES TEL-EDIT.
           03  TEL-EDIT-NUM PIC 9(10).
           03  FILLER       PIC XX.
