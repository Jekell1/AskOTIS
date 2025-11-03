      * COPYMEMBER: LIBGB/GB01VGB
      ******************************************************************
      *
      * DESCRIPTION: VISION VERSION OF THE GLOBAL FILE
      *
      * VGB-KEY   : VGB-BRNO, VGB-NO
      *
      *=================================================================
      * THE PURPOSE OF THE VGBFILE IS FOR LOADING A SPECIFIC RANGE OF
      * GB-REC FOR BETTER PERFORMANCE AND HELP
      * RECOVERABILITY OF A PROBLEM WITHIN LP/EOMALL.
      *
      *=================================================================
      * REV:
      * JKC 2024-0829 ***NEW***
      *
      ******************************************************************
       78  VGB-SIZE                                  VALUE 505.
       78  VGB-GB-SIZE                               VALUE 500.
       01  VGB-REC.
           03  VGB-KEY.
               05  VGB-BRNO            PIC 9(4).
               05  VGB-NO              PIC 9.
           03  VGB-GB-REC              PIC X(VGB-GB-SIZE).

