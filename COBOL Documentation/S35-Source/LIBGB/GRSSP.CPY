      * COPYMEMBER: LIBGB/GRSSP
      ******************************************************************
      * BRANCHLIST TABLE CAN HOLD UP TO 1500 BRANCHES- SEE BRANCHLIST-TABLE-MAX
      * PROCESSING DOES NOT HAVE TO SEND 1500 BRANCHES PER LOOP- CONTROLLED BY 
      *    BRANCHLIST-PGM-PROC-MAX
      * BRANCHLIST-PGM-PROC-MAX CAN NOT BE > BRANCHLIST-TABLE-MAX
      *
      *****
      * IN PARAGRAPH NEXT-BRANCH-NO GO TO END-LOAD-BRANCH-TABLE WHEN NO MORE
      *    BRANCHES
      * NEXT-BRANCH-NO.
      *     PERFORM READ-BR-FILE-NEXT.
      *     IF IO-FG = 9 OR BR-NO > END-BRANCH
      *         GO TO END-LOAD-BRANCH-TABLE.
      *=================================================================
      * REV:
      * MB  2023-0821 *NEW*
      ******************************************************************
       LOAD-BRANCH-TABLE.
           ADD 1 TO BRANCHLIST-PROC-BRANCH-CTR
                    BRANCHLIST-SUB.

           MOVE BR-NO TO BRANCHLIST-WORK-BRNO.

           MOVE BRANCHLIST-WORK-BRNO-TEXT TO 
                              BRANCHLIST-BRNO(BRANCHLIST-SUB).

           IF BRANCHLIST-SUB > 1
               MOVE ","   TO BRANCHLIST-COMMA(BRANCHLIST-SUB).

           IF BRANCHLIST-PROC-BRANCH-CTR < BRANCHLIST-PGM-PROC-MAX
               GO TO NEXT-GROUP-LOC.

       END-LOAD-BRANCH-TABLE.
           MOVE BRANCHLIST-TBL TO PASS-BRANCHLIST.
