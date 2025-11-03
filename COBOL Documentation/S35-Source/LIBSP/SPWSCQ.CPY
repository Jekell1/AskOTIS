      * COPYMEMBER: LIBSP/SPWSCQ
      *********************************************************
      *        LIBSP/SPWSCQ: CQFILE PATH WORKERS
      *********************************************************
       01  CQ-PATH-OVERRIDE    PIC X(01) VALUE SPACES.
       01  CQ-PATH             PIC X(35) VALUE SPACES.
       01  FILLER                    REDEFINES CQ-PATH.
           03  CQ-PATH-BEG           PIC X(13).
           03  CQ-PATH-OWNST         PIC X(02).
           03  CQ-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  CQ-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES CQ-PATH-FILE.
               05  FILLER            PIC X(03).
               05  CQ-PATH-SQL       PIC X(12).

       01  CQ-ALL-PATH.
           03  CQ-ALL-BASE     PIC X(09).
           03  FILLER                REDEFINES CQ-ALL-BASE.
               05  CQ-ALL-BASE-USR   PIC X(05).
               05  CQ-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  CQ-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  CQ-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)   VALUE "/SP/CQFILE".

      *-----------------------------------------------------------------
      *    SPWSCQ-CQ2-KEY-NON-COMP GROUP FIELD IS NEEDED FOR POPULATING
      *    E-KEYX; CANNOT STRING ALL CQ2-KEY FIELDS INTO E-KEYX BECAUSE
      *    CANNOT STRING CQ-START-TIME BECAUSE IT CONTAINS A COMP-3
      *    (STRING CANNOT HANDLE COMP).
      *    THIS IS USED IN LIBSP/SPCQ2IN AND LIBSP/SPCQ2RN.
      *-----------------------------------------------------------------
       01  SPWSCQ-CQ2-KEY-NON-COMP.
           03  SPWSCQ-CQ2-BRNO-NC             PIC 9(4).
           03  SPWSCQ-CQ2-COLLID-NC           PIC XXX.
           03  SPWSCQ-CQ2-REV-BEHAVIOR-RISK-NC PIC 9.
           03  SPWSCQ-CQ2-SEQ-FG-NC           PIC X.
           03  SPWSCQ-CQ2-DATE-NC             PIC 9(8).
           03  SPWSCQ-CQ2-START-TIME-NC       PIC S9(4).
           03  SPWSCQ-CQ2-REV-DEL-NC          PIC 9(5).
           03  SPWSCQ-CQ2-MISC-KEY-FLD-NC     PIC X(3).
           03  SPWSCQ-CQ2-REV-PDUE-DAYS-NC    PIC 9(5).
           03  SPWSCQ-CQ2-REV-BAL-NC          PIC 9(7).
           03  SPWSCQ-CQ2-SEQ-NC              PIC 9(3). 
 
      *-----------------------------------------------------------------
      *    USED TO LOAD/SAVE THE CQ2-KEY FIELDS FROM/TO THE HOLD-CQ2-KEY
      *    OR SAVE-CQ2-KEY FIELDS (SPWSCQ-CQ2-KEY).
      *    STANDARD ROUTINES ARE IN LIBSP/SPCQ2IN AND LIBSP/SPCQ2RN.
      *-----------------------------------------------------------------
       01  SPWSCQ-CQ2-KEY.
           03  SPWSCQ-CQ2-BRNO                PIC 9(4).
           03  SPWSCQ-CQ2-COLLID              PIC XXX.
           03  SPWSCQ-CQ2-REV-BEHAVIOR-RISK   PIC 9.
           03  SPWSCQ-CQ2-SEQ-FG              PIC X.
           03  SPWSCQ-CQ2-DATE                PIC 9(8).
           03  SPWSCQ-CQ2-START-TIME          PIC S9(4) COMP-3.
           03  SPWSCQ-CQ2-REV-DEL             PIC 9(5).
           03  SPWSCQ-CQ2-MISC-KEY-FLD        PIC X(3).
           03  SPWSCQ-CQ2-REV-PDUE-DAYS       PIC 9(5).
           03  SPWSCQ-CQ2-REV-BAL             PIC 9(7).
           03  SPWSCQ-CQ2-SEQ                 PIC 9(3). 
  
