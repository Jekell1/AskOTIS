      * COPYMEMBER: LIBSP/SPWSAC
      *********************************************************
      *    LIBSP/SPWSAC:       ACFILE PATH WORKERS
      *
      *  REV:
      *  MJD 070795 ADDED DATE SUFFIX.
      *  BLV 051500 ADDED MACHINE SUFFIX.
      *  BLV 020109 ASSIGN A NAME (AC-PATH-NAME) TO FILLER SO
      *             ACH OUTPUT FILE NAME MAY BE ASSIGNED IN DLRACH
      ******************************************************************
      *    /USR/DATA/R#/SP/ACFILE.######.R#
      ******************************************************************
       01  AC-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  AC-PATH                   PIC X(50)   VALUE SPACES.
       01  AC-PATH-FORMAT.
           03  AC-PATH-BASE          PIC X(09).
           03  FILLER                PIC X(01)   VALUE "/".
           03  AC-PATH-MACHINE       PIC X(02).
           03  FILLER                PIC X(04)   VALUE "/SP/".
           03  AC-PATH-FILENAME.
               05  AC-PATH-NAME      PIC X(7) VALUE "ACFILE.".
               05  AC-PATH-PART2.
                   07  AC-PATH-YR        PIC 99.
                   07  AC-PATH-MO        PIC 99.
                   07  AC-PATH-DA        PIC 99.
                   07  AC-PATH-DOT       PIC X.
                   07  AC-PATH-MACHINE2  PIC XX.
           03  FILLER                PIC X(10).

       01  AC-VOID-PATH.
           03  AC-VOID-PATH-BASE     PIC X(09).
           03  FILLER                PIC X(01)   VALUE "/".
           03  AC-VOID-PATH-MACHINE  PIC X(02).
           03  FILLER                PIC X(04)   VALUE "/SP/".
           03  AC-VOID-FILENAME.
               05  AC-VOID-NAME     PIC X(8) VALUE "DLRACHV.".
               05  AC-VOID-PART2.
                   07  AC-VOID-YR       PIC 99.
                   07  AC-VOID-MO       PIC 99.
                   07  AC-VOID-DA       PIC 99.
                   07  AC-VOID-DOT      PIC X.
                   07  AC-VOID-MACHINE2  PIC XX.
           03  FILLER               PIC X(9).
