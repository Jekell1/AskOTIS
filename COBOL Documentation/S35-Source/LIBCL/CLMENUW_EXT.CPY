      * COPYMEMBER: LIBCL/CLMENUW.EXT
      ***********************************************************************
      *                     LIBCL/CLMENUW.R_EXA.CPYT   F9/F10 INTERRUPT
      *
      * REV:
      *  JTG 123094 ADDED EXT-WI-CL-SYSDATE, HAD NO VALUE, HARD COPY PRINT NEEDS
      *  MJD 011097 ADDED EXT-WI-CL-PAY-SCHLD-FG.
      *  JTG 101797 EXPANDED EXT-WI-CL-WORKER FROM 74 TO 120 BYTES
      *  JTG 991124 ADDED WICL2 TO EXT-WI-CL-LIST AND ALL FORMS HAVING WICL
      *  JTG 991202 TEMPORARY REMOVED WICL2 UNTIL THE FIELD HAS NEW FORMS
      *             ADDED WICL2 TO EXT-WI-CL-LIST AND ALL FORMS HAVING WICL
      *  GLB 001113 ADDED EXT-WI-CL-VENDOR (JUST A NEW REDEFINITION)
      *  BLF 070814 ADDED EXT-WI-CL-ARCHIVE-FG, WORLD PR# 547
      *  BAH 121108 MADE EXTERNAL
      *  BAH 160114 ADDED EXT-WI-CL-OWNBR FOR LT FILES
      *
      * KEC 2016.0502 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         MOVED LIBCL/CLMENUW_HLD.CPY TO A .NO IN THE SV DIRECTORY
      *          AND NOW USING EXT-WI-CL-SIZE (NEW 78) TO BE USED IN
      *          PROGRAMS NEEDED TO HOLD/SAVE EXT-WI-CL-WORKER.
      *         CHANGED EXT-WI-CL-APPNO TO EXT-WI-CL-APP-NUMBER
      *          AND IS NOW 'PIC 9(8) COMP'.
      *         MADE EXT-WI-CL-NOTE-NO ITS OWN FIELD.
      *         MADE EXT-WI-CL-VENDOR ITS OWN FIELD.
      *         ADDED FILLER FOR FUTURE USE (EXT-WI-CL-SIZE = 200).
      *
      * KEC 2018-0822 LOAN APP MODULE REMOVAL FROM A30 [LARM30];
      *         LEAVING EXT-WI-CL-APP-NUMBER HERE IN CASE
      *         NEEDED IN THE FUTURE.
      *
      * KEC 2024-0404 REMOVED THE FOLLOWING:
      *          EXT-WI-CL-APP-NUMBER            PIC 9(9)       COMP-3.
      *          EXT-WI-CL-NOTE-NO               PIC 9(6)       COMP-3.
      *          EXT-WI-CL-VENDOR                PIC 9(6)       COMP-3.
      *          CHANGED FILLER FROM X(108) TO X(121).
      ******************************************************************

       78  EXT-WI-CL-SIZE                      VALUE 200.
       01  EXT-WI-CL-WORKER                    EXTERNAL.
           03  EXT-WI-CL-WK-PATH               PIC X(22).
           03  EXT-WI-CL-TRANS-DATE            PIC 9(8)       COMP-3.
           03  EXT-WI-CL-OWNBR                 PIC 9(4)       COMP-3.
           03  EXT-WI-CL-ACCTNO                PIC 9(8)       COMP-3.
           03  EXT-WI-CL-SSNO         OCCURS 4 PIC 9(9)       COMP-3.
           03  EXT-WI-CL-MAKERCD      OCCURS 4 PIC X.
           03  EXT-WI-CL-DLNO                  PIC 9(4)V99    COMP-3.
           03  EXT-WI-CL-SSNO-NO               PIC 9.
           03  EXT-WI-CL-SALEFIN               PIC X.
           03  EXT-WI-CL-PASSWD                PIC X(4).
           03  EXT-WI-CL-PAY-SCHLD-FG          PIC X.
           03  EXT-WI-CL-SYSDATE               PIC X(8).
           03  EXT-WI-CL-ARCHIVE-FG            PIC X.
           03  FILLER                          PIC X(121).
