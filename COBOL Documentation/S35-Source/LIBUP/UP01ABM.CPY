      * COPYMEMBER: LIBUP/UP01ABM
      ******************************************************************
      *
      * ASCII EXTRACT BORROWER MEMO FILE
      * 
      *  TAB DELMITED
      *
      ******************************************************************
       78  ABM-SIZE                     VALUE 119.
       01  ABM-REC.
           03  ABM-BRNO             PIC 9(4).       
           03  FILLER               PIC X.
           03  ABM-SSNO             PIC 9(9).
           03  FILLER               PIC X.
           03  ABM-DATE-REVERSED    PIC 9(8).
           03  FILLER               PIC X.
           03  ABM-SEQ              PIC 9(3).
           03  FILLER               PIC X.
           03  ABM-LTOUCH-DATE      PIC 9(8).
           03  FILLER               PIC X.
           03  ABM-LTOUCH-TIME-HHMM PIC 9(4).
           03  FILLER               PIC X.
           03  ABM-LTOUCH-USERID    PIC X(10).
           03  FILLER               PIC X.
           03  ABM-MEMO             PIC X(60).
           03  FILLER               PIC X.
           03  ABM-MEMO-TYPE        PIC XX.
           03  FILLER               PIC X.
           03  ABM-CR               PIC X.
           03  ABM-LF               PIC X.
