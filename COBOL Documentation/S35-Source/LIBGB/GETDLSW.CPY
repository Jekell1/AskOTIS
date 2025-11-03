      * COPYMEMBER: LIBGB/GETDLSW DATE LIGHT SAVINGS
       01 GET-DLS-WORKERS.
          03  DLS-DATE                PIC 9(08).
          03  DLS-DATEX REDEFINES DLS-DATE.
              05  DLS-CC              PIC 9(02).
              05  DLS-YY              PIC 9(02).
              05  DLS-MM              PIC 9(02).
              05  DLS-DD              PIC 9(02).
          03  DLS-FG                  PIC X(01).
