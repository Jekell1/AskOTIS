      * COPYMEMBER: LIBGB/GRSERCH
      ******************************************************************
      * LIBGB/GRSERCH - ROUTINES USED WITH GB/GR-FILE
      * SEARCH-GR-INIT
      *
      *    INITIALIZE AND LOAD VARIABLES IN PREPARATION FOR
      *    FINDING BRANCHES. EXPECTS GR-ID TO BE SET TO THE
      *    GROUP ID THAT DEFINES THE BRANCHES TO READ.
      ******************************************************************
      ******************************************************************
      *         BUILDING GROUP STRUCTURE
      *
      *   NAME: GRSERCH
      *   DESC: THIS ROUTINE CHECKS THE CONTENTS OF WGR-WORD
      *         FOR FORMAT AND TYPE
      *
      *   IN  : WGR-ID
      *         WGR-MIN-LEVEL   - MINIMUM VALID LEVEL DESIRED
      *                           10 MEANS BRANCH NUMBER ONLY
      *   OUT : WGR-ERR:
      *            0   MEANS ALL OK
      *            1-4 POINT TO ERRORS IN WGR-MSG
      *         WGR-TYPE:
      *            "A" FOR ALPHA ENTERED
      *            "N" FOR NUMERIC ENTERED
      *         WGR-SRESULT:
      *            TELLS BRANCH NUMBER
      *            IF ENTERED LEVEL 9 GROUP OR BRANCH NUMBER
      *         WGR-LEVEL
      *            TELLS LEVEL OF ENTRY, 9 FOR BRANCH
      *
      *   USED:
      *   COPY: GRWORKW
      *
      * REV:
      *  061595 BLL EXPAND SID & SEARCH TABLES TO 500, AMTS COMP-3
      *  010918 BLV EXPAND SEARCH TABLES FROM 500 TO 700
      *  040107 BLV STORE GR-LEVEL IN WGR-SID-LEVEL TABLE; TO
      *             AVOID EXCESSIVE IO ON GRFILE IN GL FINANCIAL
      *             STATEMENTS WITH GROUP TOTALS
      *  040520 BLV EXPAND SEARCH TABLES FROM 700 TO 998
      *  061109 BLF EXPAND SEARCH TABLES FROM 998 TO 2000, FIX CHECK IN
      *             VERIFY-GROUP-MEMBER FOR EXPANSION, WORLD PR# XXXX
      *  100831 BLM ADD GR-ID TO MESSAGE "ERROR READING GR"
      *****************************************************************
       SEARCH-GR-INIT SECTION.
       SEARCH-GR-INIT-PARA.
           MOVE 0 TO WGR-SRESULT.
       SEARCH-GR-INIT1.
           MOVE LOW-VALUES TO WGR-SCURR-TABLE.
           MOVE SPACES TO WGR-SID-TABLE.
           MOVE LOW-VALUES TO WGR-SEARCH-TABLE.
           MOVE 1 TO WGR-SDEPTH WGR-SCURR(1).
           MOVE 0 TO WGR-SSTABLE.
           MOVE SPACES TO WGR-LEVEL-TAB.
           MOVE "Y" TO WGR-LEVEL-TABLE(9).
           MOVE GR-ID TO WGR-SID(1).
           MOVE 1 TO WGR-TDEX3.
           MOVE 0 TO WGR-TDEX1.
       SEARCH-GR-LOAD.
           ADD 1 TO WGR-TDEX1.
           IF WGR-TDEX1 > WGR-TDEX3
              GO TO EXIT-SEARCH-GR-INIT.
           IF WGR-SID(WGR-TDEX1) IS NUMERIC
              MOVE WGR-SID(WGR-TDEX1) TO WGR-WORD
              MOVE WGR-NUMB TO WGR-SLOC(WGR-TDEX1)
              GO TO SEARCH-GR-LOAD.
           MOVE WGR-SID(WGR-TDEX1) TO GR-ID.
           PERFORM READ-GR1-FILE.
           IF IO-FG NOT = 0
              MOVE "ERROR READING GROUP: &&&&" TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY GR-ID
              PERFORM SEND-MESS
              GO TO SEARCH-GR-LOAD.
      *
      *    UNLOCK GR-FILE.
      *
           IF GR-LEVEL = 9
              MOVE GR-MEMBER-LOC(1) TO WGR-SLOC(WGR-TDEX1)
              GO TO SEARCH-GR-LOAD.
           MOVE GR-LEVEL TO WGR-SID-LEVEL(WGR-TDEX1).
           IF GR-LEVEL > 0
              MOVE "Y" TO WGR-LEVEL-TABLE(GR-LEVEL)
           ELSE
              MOVE "Y" TO WGR-LEVEL-TABLE(10).
           MOVE 0 TO WGR-TDEX2.
       SEARCH-GR-LOAD2.
           ADD 1 TO WGR-TDEX2.
           IF WGR-TDEX2 > 100
              GO TO SEARCH-GR-LOAD.
           IF GR-MEMBER-ID(WGR-TDEX2) = SPACES
              MOVE WGR-TDEX3 TO WGR-SEND(WGR-TDEX1)
              GO TO SEARCH-GR-LOAD2.
           ADD 1 TO WGR-TDEX3.
           MOVE GR-MEMBER-ID(WGR-TDEX2) TO WGR-SID(WGR-TDEX3).
           IF WGR-SSTART(WGR-TDEX1) = 0
              MOVE WGR-TDEX3 TO WGR-SSTART(WGR-TDEX1).
           GO TO SEARCH-GR-LOAD2.
       EXIT-SEARCH-GR-INIT.
           EXIT.

      *********************************************************
      * SEARCH-GR-NEXT
      *
      *    RETURNS NEXT BRANCH WITHIN GROUP. ALSO SUPPLIES
      *    DEPTH (HOW MANY LEVELS DOWN WITHIN HEIRARCHY) AND
      *    PREDECESSOR (WHICH LEVELS ARE ABOVE BRANCH IN
      *    HIERARCHY) INFORMATION.
      *
      *    AT THE EXIT OF THIS ROUTINE
      *      WGR-SRESULT WILL BE THE BRANCH NUMBER FOUND.
      *      WGR-SID(WGR-TCURR) WILL BE THE ID OF THE BRANCH FOUND.
      *      WGR-SCURR(1) THRU WGR-SCURR(WGR-TPLUS) CAN BE USED AS
      *        SUBSCRIPTS INTO WGR-SID TO DETERMINE WHICH GROUPS
      *        PRECEDE THIS BRANCH.
      *      WGR-SCURR(WGR-SSTABLE+1) THRU WGR-SCURR(WGR-TPLUS) CAN
      *        BE USED AS SUBSCRIPTS INTO WGR-SID TO FIND OUT WHICH
      *        GROUPS HAVE CHANGED SINCE LAST BRANCH FOUND.
      *********************************************************
       SEARCH-GR-NEXT SECTION.
       SEARCH-GR-NEXT-PARA.
           MOVE 0 TO WGR-SRESULT.
           MOVE WGR-SDEPTH TO WGR-SSTABLE.
       SEARCH-GR-LOOP.
           IF WGR-SDEPTH < WGR-SSTABLE
              MOVE WGR-SDEPTH TO WGR-SSTABLE.
           IF WGR-SDEPTH = 0
              MOVE 0 TO WGR-SRESULT
              GO TO EXIT-SEARCH-GR-NEXT.
           MOVE WGR-SCURR(WGR-SDEPTH) TO WGR-TCURR.
           IF WGR-SSTART(WGR-TCURR) = 0
              MOVE WGR-SLOC(WGR-TCURR) TO WGR-SRESULT
              SUBTRACT 1 FROM WGR-SDEPTH
              GO TO EXIT-SEARCH-GR-NEXT.

           COMPUTE WGR-TPLUS = WGR-SDEPTH + 1.
           IF WGR-SCURR(WGR-TPLUS) = 0
              MOVE WGR-SSTART(WGR-TCURR) TO WGR-SCURR(WGR-TPLUS)
              ADD 1 TO WGR-SDEPTH
           ELSE
              ADD 1 TO WGR-SCURR(WGR-TPLUS)
              IF WGR-SCURR(WGR-TPLUS) > WGR-SEND(WGR-TCURR)
                 MOVE 0 TO WGR-SCURR(WGR-TPLUS)
                 SUBTRACT 1 FROM WGR-SDEPTH
              ELSE
                 ADD 1 TO WGR-SDEPTH.
           GO TO SEARCH-GR-LOOP.
       EXIT-SEARCH-GR-NEXT.
           EXIT.

      *********************************************************
      * SEARCH-GR-VERIFY
      *
      *    VERIFY THAT GIVEN LOCATION IS AMONG THE LIST OF
      *    THOSE TO PRINT. BEFORE CALLING LOAD THE LOCATION
      *    TO BE VERIFIED INTO WGR-SRESULT. IF THE LOCATION
      *    IS NOT WITHIN THE LIST OF THOSE WITHIN THE INITIALIZED
      *    GROUP THEN WGR-ERR IS SET TO NON-ZERO.
      *    BLV 031106 CHANGE TO GET OUT IF SUB > WGR-TDEX3,
      *               WHY GO THROUGH LOOP 700 TIMES???
      *********************************************************
       SEARCH-GR-VERIFY SECTION.
       SEARCH-GR-VERIFY-PARA.
           MOVE 1 TO WGR-TDEX1.
       SEARCH-VERIFY-LOOP.
           IF WGR-SRESULT = WGR-SLOC(WGR-TDEX1)
              MOVE 0 TO WGR-ERR
              GO TO EXIT-SEARCH-GR-VERIFY.
           ADD 1 TO WGR-TDEX1.
      *    IF WGR-TDEX1 < 701
      *       GO TO SEARCH-VERIFY-LOOP.
           IF WGR-TDEX1 <= WGR-TDEX3
              GO TO SEARCH-VERIFY-LOOP.
           MOVE 1 TO WGR-ERR.
       EXIT-SEARCH-GR-VERIFY.
           EXIT.
      *********************************************************
      * VERIFY-GROUP-MEMBER
      *
      *    VERIFY THAT GIVEN GROUP IS WITHIN HIERARCHY OF
      *    LOADED GROUP. BEFORE CALLING LOAD THE GROUP
      *    TO BE VERIFIED INTO WGR-TEST-GROUP. IF THE GROUP
      *    IS NOT WITHIN THE LIST OF THOSE WITHIN THE INITIALIZED
      *    GROUP THEN WGR-ERR IS SET TO NON-ZERO.
      *********************************************************
       VERIFY-GROUP-MEMBER SECTION.
       VERIFY-MEMBER.
           MOVE 1 TO WGR-TDEX1.
       VERIFY-MEMBER-LOOP.
           IF WGR-TEST-GROUP = WGR-SID(WGR-TDEX1)
              MOVE 0 TO WGR-ERR
              GO TO EXIT-VERIFY-MEMBER.
           ADD 1 TO WGR-TDEX1.
           IF WGR-TDEX1 < 2001
              GO TO VERIFY-MEMBER-LOOP.
           MOVE 1 TO WGR-ERR.
       EXIT-VERIFY-MEMBER.
           EXIT.
