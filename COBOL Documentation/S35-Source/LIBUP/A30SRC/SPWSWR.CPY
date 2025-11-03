      * COPYMEMBER: LIBSP/SPWSWR
*NEW* ******************************************************************
*NEW* *    LIBSP/SPWSWR: WRFILE PATH WORKERS
*NEW* *    /USR/DATA/R#/SP/WRFILE
*NEW* ******************************************************************
*NEW*  01  WR-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
*NEW*  01  WR-PATH                   PIC X(35)   VALUE SPACES.
*NEW*  01  WR-PATH-FORMAT.
*NEW*      03  WR-PATH-BASE          PIC X(09).
*NEW*      03  FILLER                REDEFINES WR-PATH-BASE.
*NEW*          05  WR-PATH-BASE-USR  PIC X(05).
*NEW*          05  WR-PATH-BASE-DIR  PIC X(04).
*NEW*      03  FILLER                PIC X(01)   VALUE "/".
*NEW*      03  WR-PATH-MACHINE       PIC X(02).
*NEW*      03  FILLER                PIC X(23)   VALUE "/SP/WRFILE".
