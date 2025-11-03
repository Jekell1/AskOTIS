      *A30 CONVERSION COMPLETED
      * COPYMEMBER: LIBMN/PDMAINW
      ******************************************************************
      *
      * IDENTIFICATION:  LIBMN/PDMAINW
      *
      * DESCRIPTION   :  WORKERS FOR SCRATCH PAD FILE MAINTENANCE
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 000911 {PR#00000} PARADATA FINANCIAL [BARB & BETH]
      *             ***NEW*** ADDED PER BARB & BETH
      * BAH 120514 REMOVED SPECIAL-FG
      * MJD 131127 UPDATED FOR A30.  REMOVED CONAME FIELDS AND PUT
      *            REMAINING FIELDS TO BE PDMAIN-LINK.
      *
      *  CS 200129 THERE IS NO PAGE 12 SO REMOVE FROM FORM-SEL2
      ******************************************************************
      *
      *    A C T I O N / F O R M - N U M - W O R K E R S
      *
      *=================================================================
       01  ACTION                                    VALUE SPACES.
           03  CHKEY                   PIC X(04).
           03  CHNO                    PIC 9(02).
           03  CHNO-X                  REDEFINES CHNO
                                       PIC X(02).

       01  FORM-NUM1                   REDEFINES ACTION.
           03  FORM-KEY1               PIC X(04).
           03  FORM-P1                 PIC X(01).
           03  FORM-SEL1               PIC 9(01).
               88  FORM-CURRENT-PAGE-01    VALUE 01.
               88  FORM-CURRENT-PAGE-02    VALUE 02.
               88  FORM-CURRENT-PAGE-03    VALUE 03.
               88  FORM-CURRENT-PAGE-04    VALUE 04.
               88  FORM-CURRENT-PAGE-05    VALUE 05.
               88  FORM-CURRENT-PAGE-06    VALUE 06.
               88  FORM-CURRENT-PAGE-07    VALUE 07.
               88  FORM-CURRENT-PAGE-08    VALUE 08.
               88  FORM-CURRENT-PAGE-09    VALUE 09.

       01  FORM-NUM2                   REDEFINES ACTION.
           03  FORM-KEY2               PIC X(03).
           03  FORM-P2                 PIC X(01).
           03  FORM-SEL2               PIC 9(02).
               88  FORM-CURRENT-PAGE-10    VALUE 10.
               88  FORM-CURRENT-PAGE-11    VALUE 11.
               88  FORM-CURRENT-PAGE-12    VALUE 12.
               88  FORM-CURRENT-PAGE-91    VALUE 91.
               88  FORM-CURRENT-PAGE-92    VALUE 92.
               88  FORM-CURRENT-PAGE-93    VALUE 93.
               88  FORM-CURRENT-PAGE-94    VALUE 94.
               88  FORM-2-DIGITS           VALUE 10 11 
                                                 91 92 93 94.
       01  C-SYSDATE.
           03  C-SYSMO             PIC 9(02).
           03  FILLER              PIC X(01).
           03  C-SYSDAY            PIC 9(02).
           03  FILLER              PIC X(01).
           03  C-SYSYR             PIC 9(02).

      *=================================================================
      *
      *    C O N A M E - W O R K E R S
      *
      *=================================================================
       01  PDMAIN-LINK                 VALUE SPACES.
           03  FORM-NAME               PIC X(20).
           03  SAVEKEY                 PIC X(15).
           03  NEW-REC                 PIC X(01).
           03  ERRCD                   PIC X(01).
           03  FORMSEL                 PIC 9(02).
           03  FILLER                  REDEFINES FORMSEL.
               05  FILLER              PIC 9(01).
               05  FORMNUM             PIC 9(01).
           03  PDMAIN-WINDOW-HANDLE    PIC X(10).
