      * COPYMEMBER: LIBGB/GB01FL
      ***************************************************************
      *  FLFILE - FLAG FILE TO ORGANIZE/LOCATE GLOBAL & BRANCH FLAGS
      ***************************************************************
       78  FL-SIZE                 VALUE 225.
       01  FL-REC.
           03  FL1-KEY.
               05  FL-FIELD-NAME   PIC X(30).
           03  FL-DESC             PIC X(35).
           03  FL-PROG.
               05  FL-PROG-1ST5    PIC X(5).
               05  FL-PROG-6TH     PIC X.
           03  FL-PAGE             PIC 99.
           03  FL-FIELD-NO         PIC 99.
           03  FL-KEYWORDS.
               05  FL-KEYWORD      PIC X(15) OCCURS 10.
