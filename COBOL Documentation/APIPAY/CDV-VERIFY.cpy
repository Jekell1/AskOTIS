      *********************************
      *   NAME: CDV-VERIFY
      *   DESC: VERIFY LP-TRCD & YIELD SUBSCRIPT
      *   IN  : LP-TRCD, CDV-NN
      *   OUT : CDV-SUB; 0 = NOT_FOUND
      *********************************
       CDV-VERIFY SECTION.
           MOVE 0 TO CDV-SUB.
       CDV-VERIFY-NEXT.
           ADD 1 TO CDV-SUB.
           MOVE CDV-CD(CDV-SUB) TO STAT.
           IF STAT = LP-TRCD
              GO TO CDV-VERIFY-EXIT.
           IF STAT NOT = " "
              GO TO CDV-VERIFY-NEXT.
           MOVE 0 TO CDV-SUB.
       CDV-VERIFY-EXIT.
           EXIT.

      *================================================================*
      * EMBEDDED COPYBOOK: LIBGB\TIMEIO.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBGB/TIMEIO
      ******************************************************************
      *              (TIMEIO)
      *   GET/FORMAT CLOCK TIME
      *=================================================================
      * REV:
      * JTG 020594 ADDED TIME-MILITARY-FG
      *
      * JKC 2020-1026 ADDED TIMER-START AND TIMER-STOP SECTIONS.
      *          USED FOR GB/TIMER.CBL.
      *
      ******************************************************************
