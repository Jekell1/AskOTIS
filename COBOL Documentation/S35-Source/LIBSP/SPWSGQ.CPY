      * COPYMEMBER: LIBSP/SPWSGQ
      *********************************************************
      *        LIBSP/SPWSGQ: GQFILE PATH WORKERS
      *
      * GQ-PATH = /USR/DATA/R#/SP/GQFILE
      *           |----BEG----||-----FILE----|
      *                           |---SQL----|
      *
      * 190327 CS COMMENT SPWSGQ-GQ2-BRNO AND SPWSGQ-GQ2-BRNO-NC
      *********************************************************
       01  GQ-PATH-OVERRIDE    PIC X     VALUE SPACES.
       01  GQ-PATH             PIC X(35) VALUE SPACES.
       01  FILLER                    REDEFINES GQ-PATH.
           03  GQ-PATH-BEG           PIC X(13).
           03  GQ-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES GQ-PATH-FILE.
               05  FILLER            PIC X(03).
               05  GQ-PATH-SQL       PIC X(19).

       01  GQ-ALL-PATH.
           03  GQ-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES GQ-ALL-BASE.
               05  GQ-ALL-BASE-USR   PIC X(05).
               05  GQ-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  GQ-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/SP/GQFILE".


      *-----------------------------------------------------------------
      *    SPWSGQ-GQ2-KEY-NON-COMP GROUP FIELD IS NEEDED FOR POPULATING
      *    E-KEYX; CANNOT STRING ALL GQ2-KEY FIELDS INTO E-KEYX BECAUSE
      *    CANNOT STRING GQ-START-TIME BECAUSE IT CONTAINS A COMP-3
      *    (STRING CANNOT HANDLE COMP).
      *    THIS IS USED IN LIBSP/SPCQ2IN AND LIBSP/SPCQ2RN.
      *-----------------------------------------------------------------
       01  SPWSGQ-GQ2-KEY-NON-COMP.
           03  SPWSGQ-GQ2-COLLID-NC           PIC XXX.
           03  SPWSGQ-GQ2-SEQ-FG-NC           PIC X.
           03  SPWSGQ-GQ2-DATE-NC             PIC 9(8).
           03  SPWSGQ-GQ2-START-TIME-NC       PIC S9(4).
           03  SPWSGQ-GQ2-FUTURE-KEY1-NC      PIC X(8).
           03  SPWSGQ-GQ2-REV-DEL-NC          PIC 9(5).
           03  SPWSGQ-GQ2-MISC-KEY-FLD-NC     PIC X(3).
           03  SPWSGQ-GQ2-REV-PDUE-DAYS-NC    PIC 9(5).
           03  SPWSGQ-GQ2-REV-BAL-NC          PIC 9(7).
           03  SPWSGQ-GQ2-FUTURE-KEY2-NC      PIC X(8).
           03  SPWSGQ-GQ2-SEQ-NC              PIC 9(3). 
 
      *-----------------------------------------------------------------
      *    USED TO LOAD/SAVE THE GQ2-KEY FIELDS FROM/TO THE HOLD-GQ2-KEY
      *    OR SAVE-GQ2-KEY FIELDS (SPWSGQ-GQ2-KEY).
      *    STANDARD ROUTINES ARE IN LIBSP/SPGQ2IN AND LIBSP/SPGQ2RN.
      *-----------------------------------------------------------------
       01  SPWSGQ-GQ2-KEY.
           03  SPWSGQ-GQ2-COLLID              PIC XXX.
           03  SPWSGQ-GQ2-SEQ-FG              PIC X.
           03  SPWSGQ-GQ2-DATE                PIC 9(8).
           03  SPWSGQ-GQ2-START-TIME          PIC S9(4) COMP-3.
           03  SPWSGQ-GQ2-FUTURE-KEY1         PIC X(8).
           03  SPWSGQ-GQ2-REV-DEL             PIC 9(5).
           03  SPWSGQ-GQ2-MISC-KEY-FLD        PIC X(3).
           03  SPWSGQ-GQ2-REV-PDUE-DAYS       PIC 9(5).
           03  SPWSGQ-GQ2-REV-BAL             PIC 9(7).
           03  SPWSGQ-GQ2-FUTURE-KEY2         PIC X(8).
           03  SPWSGQ-GQ2-SEQ                 PIC 9(3). 
