      * COPYMEMBER: LIBGB/GB01GR
      ******************************************************************
      *
      *  MEMBER NAME: LIBGB/GB01GR                             RSZ=0740
      *
      *  DESCRIPTION: BRANCH GROUP FILE
      *
      *  NOTES      :
      *
      *-----------------------------------------------------------------
      *  REV:
      *  KEC 031198 ADDED GR-LN-APPROVAL-LIMIT.
      *  MJD 990126 ----- UPDATED IN FLEXGEN -----
      *  BLM 161116 CHANGE COMP FIELD TO COMP-3, INCREASED UNUSED
      *             CHANGED REC SIZE FROM 672 TO 740
      ******************************************************************
       78  GR-SIZE                      VALUE 740.
       01  GR-REC.
           03  GR1-KEY.
               05  GR-ID                PIC X(4).
           03  GR-NAME                  PIC X(20).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    EXAMPLE:
      *        GR-LEVEL - 0 = MASTER GROUP
      *                   1 = CORPORATE
      *                   2 = COMPANY
      *                   3 = SUB-COMPANIES
      *                   4 = REGION
      *                   5 = SUB-REGION
      *                   6 = AREA
      *                   7 = SUB-AREA
      *                   8 = LOCALITY
      *                   9 = BRANCH
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  GR-LEVEL                 PIC 9.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  GR-MEMBERS               PIC X(400).
           03  GR-A REDEFINES GR-MEMBERS.
               05  GR-MEMBER-ID         PIC X(4) OCCURS 100.
           03  GR-N REDEFINES GR-MEMBERS.
               05  GR-MEMBER-LOC        PIC 9(4) OCCURS 100.
           03  GR-CONTACT               PIC X(20).
           03  GR-ADDR-1                PIC X(20).
           03  GR-ADDR-2                PIC X(20).
           03  GR-CITY                  PIC X(15).
           03  GR-STATE                 PIC X(4).
           03  GR-ZIP                   PIC X(10).
           03  GR-PHONE                 PIC 9(10).
           03  GR-TITLE                 PIC X(25).
           03  GR-DESC                  OCCURS 2
                                        PIC X(40).
           03  GR-CONTACT-NO            PIC 9(5)    COMP-3.
           03  GR-HOME-BRANCH           PIC 9(4).
           03  GR-LN-APPROVAL-LIMIT     PIC 9(7)    COMP-3.
           03  FILLER                   PIC X(100).
