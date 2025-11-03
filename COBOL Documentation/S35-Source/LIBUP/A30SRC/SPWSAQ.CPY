      * COPYMEMBER: LIBSP/SPWSAQ
*NEW* ******************************************************************
      *    LIBSP/SPWSAQ: AQFILE PATH WORKERS
*NEW* *    /USR/DATA/R#/SSBBBB/%%/%%FILE
*NEW* ******************************************************************
*NEW*  01  AQ-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
*NEW*  01  AQ-PATH                   PIC X(35)   VALUE SPACES.
*NEW*  01  AQ-ALL-PATH.
*NEW*      03  AQ-ALL-BASE           PIC X(09).
*NEW*      03  FILLER                REDEFINES AQ-ALL-BASE.
*NEW*          05  AQ-ALL-BASE-USR   PIC X(05).
*NEW*          05  AQ-ALL-BASE-DIR   PIC X(04).
*NEW*      03  FILLER                PIC X(01)   VALUE "/".
*NEW*      03  AQ-ALL-MACHINE        PIC X(02).
*NEW*      03  FILLER                PIC X(01)   VALUE "/".
*NEW*      03  AQ-ALL-DATA           PIC X(06).
*NEW*      03  FILLER                PIC X(10)   VALUE "/SP/AQFILE".
