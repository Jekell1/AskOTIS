      * COPYMEMBER: LIBSP/SPWSII
*NEW* ******************************************************************
*NEW* *    LIBSP/SPWSII: IIFILE PATH WORKERS
*NEW* *    /USR/DATA/R#/GB/IIFILE
*NEW* ******************************************************************
*NEW*  01  II-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
*NEW*  01  II-PATH                   PIC X(35)   VALUE SPACES.
*NEW*  01  II-PATH-FORMAT.
*NEW*      03  II-PATH-BASE          PIC X(09).
*NEW*      03  FILLER                REDEFINES II-PATH-BASE.
*NEW*          05  II-PATH-BASE-USR  PIC X(05).
*NEW*          05  II-PATH-BASE-DIR  PIC X(04).
*NEW*      03  FILLER                PIC X(01)   VALUE "/".
*NEW*      03  II-PATH-MACHINE       PIC X(02).
*NEW*      03  FILLER                PIC X(23)   VALUE "/GB/IIFILE".
