      *COPY MEMBER LIBGB/GRBRLSW
      ******************************************************************
      * DESCRIPTION: WORKING STORAGE FOR LIBGB/GRBRLS_SQL
      *  WHICH CALLS STORED PROCEDURE SSP_TCLP_BRANCH_GROUP_LOGIC_SELECT
      *=================================================================
      * REV:
      * MB  2023-0821 *NEW*
      ******************************************************************
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  BGLS-RETURN-CODE            PIC S9(4) COMP-5.
       01  BGLS-PASS-BEG-BRGRP         PIC X(4).
       01  BGLS-PASS-END-BRGRP         PIC X(4).
       01  BGLS-RET-GRID               PIC X(4).
       01  BGLS-RET-KEY                PIC X(100).
       01  BGLS-RET-MEMBER-ID          PIC X(4).
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  BGLS-CURSOR-STAT            PIC X(2)   VALUE SPACES.
           88  BGLS-CURSOR-STAT-GOOD       VALUE "00".
           88  BGLS-CURSOR-STAT-BAD        VALUE "  " THRU "//"
                                                 "01" THRU "~~".
       01  BGLS-WORK-BRNO              PIC 9(4).
       01  BGLS-WORK-BRNO-TEXT         REDEFINES BGLS-WORK-BRNO
                                       PIC X(4).
       01  BGLS-BRANCHES-RETR-CTR      PIC 9(5)   VALUE 0.
       01  BGLS-BRANCH-TBL.
           03  BGLS-BRANCH-DATA        OCCURS 8000 TIMES.
               05  BGLS-BRANCH-BRNO    PIC X(4).
