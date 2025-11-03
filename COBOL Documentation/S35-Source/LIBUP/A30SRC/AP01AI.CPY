      * COPYMEMBER: LIBAP/AP01AI
      * A/P GENERAL LEDGER TABLE MAINTENANCE
      * 10/08/2015 BAH ADDED AI-KEY TO MAKE FILE INDEXED
      * 10/22/2015 BAH INCREASED FILLER, REMOVED AI-DATE,  PD0003
       78  AI-SIZE             VALUE 160.
       01  AI-REC.
           03  AI-KEY          PIC 9(4).
      *>>XDD DATE=YYYYMMDD
           03  AI-LTOUCH-DATE  PIC 9(8) COMP-3.
           03  AI-APGL         PIC 9(11).
           03  AI-BANK         PIC 9(11).
           03  AI-DISC         PIC 9(11).
           03  AI-DIR-PAY      PIC 9(11).
           03  AI-UNUSED       PIC X(107).
