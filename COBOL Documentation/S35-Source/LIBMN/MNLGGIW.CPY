      * COPYMEMBER: LIBMN/MNLGGIW
      ******************************************************************
      *
      * IDENTIFICATION:  LIBMN/MNLGGIW
      *
      * DESCRIPTION   :  MAINTENANCE LOG WORKERS - CHANGES TO GI-REC
      *
      *=================================================================
      * REV:
      * KEC 2007.0309 {PR#00000} PARADATA <A15>
      *               ***NEW***
      *
      ******************************************************************
      *
      *    M N L G G I - F I L E - W O R K E R S
      *
      ******************************************************************

      *- - - - - - - - - - - - - - - - - - - - - - -{ ORIGINAL RECORD  }
       COPY "LIBGL/GL01GI.CPY" REPLACING LEADING "GI" BY "GIOR".

      *- - - - - - - - - - - - - - - - - - - - - - - -{ CHANGED RECORD }
       COPY "LIBGL/GL01GI.CPY" REPLACING LEADING "GI" BY "GICH".

      ******************************************************************
      *
      *    M N L G G I - L M C - W O R K E R S
      *
      *=================================================================
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  MNLGGI-LMC-DETAIL-LINE      PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES MNLGGI-LMC-DETAIL-LINE.
           03  MNLGGI-LMC-BRNO         PIC 9(04).
           03  FILLER                  PIC X(01).
           03  MNLGGI-LMC-CLASS        PIC 9(02).
           03  FILLER                  PIC X(01).
           03  MNLGGI-LMC-DESC         PIC X(10).
           03  FILLER                  PIC X(01).

           03  MNLGGI-LMC-ORG-TEXT     PIC X(16).
           03  MNLGGI-LMC-ORG-AMT      REDEFINES MNLGGI-LMC-ORG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  MNLGGI-LMC-ORG-DATE     REDEFINES MNLGGI-LMC-ORG-TEXT
                                       PIC 99/99/99.
           03  MNLGGI-LMC-ORG-DLNO     REDEFINES MNLGGI-LMC-ORG-TEXT
                                       PIC ZZZ9.99.
           03  MNLGGI-LMC-ORG-GLNO     REDEFINES MNLGGI-LMC-ORG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  MNLGGI-LMC-ORG-NUM      REDEFINES MNLGGI-LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  MNLGGI-LMC-ORG-PHONE    REDEFINES MNLGGI-LMC-ORG-TEXT
                                       PIC 999/999/9999.
           03  MNLGGI-LMC-ORG-SSNO     REDEFINES MNLGGI-LMC-ORG-TEXT
                                       PIC 999/99/9999.

           03  MNLGGI-LMC-TO-X         PIC X(04).

           03  MNLGGI-LMC-CHG-TEXT     PIC X(16).
           03  MNLGGI-LMC-CHG-AMT      REDEFINES MNLGGI-LMC-CHG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  MNLGGI-LMC-CHG-DATE     REDEFINES MNLGGI-LMC-CHG-TEXT
                                       PIC 99/99/99.
           03  MNLGGI-LMC-CHG-DLNO     REDEFINES MNLGGI-LMC-CHG-TEXT
                                       PIC ZZZ9.99.
           03  MNLGGI-LMC-CHG-GLNO     REDEFINES MNLGGI-LMC-CHG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  MNLGGI-LMC-CHG-NUM      REDEFINES MNLGGI-LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  MNLGGI-LMC-CHG-PHONE    REDEFINES MNLGGI-LMC-CHG-TEXT
                                       PIC 999/999/9999.
           03  MNLGGI-LMC-CHG-SSNO     REDEFINES MNLGGI-LMC-CHG-TEXT
                                       PIC 999/99/9999.
