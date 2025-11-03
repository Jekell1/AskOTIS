      * COPYMEMBER: LIBLP/LPTBNO
      ****************************************
      *                                      *
      *   SET THE NUMBER OF ELEMENTS IN:     *
      *      SP-CAL-RATETBL                  *
      *      SP-EDT-STPTBL                   *
      *      SP-INSVERTBL                    *
      *                                      *
      *   OUT:                               *
      *      SP-CAL-NORATES()                *
      *      SP-EDT-NOEDITS()                *
      *      SP-INSNOVER()                   *
      ****************************************
       COMPUTE-SP-TABLE-NUMS SECTION.

           PERFORM COMPUTE-NORATES.
           PERFORM COMPUTE-NOEDITS.
           PERFORM COMPUTE-INSNOVER.

      *************************************************
       COMPUTE-NORATES SECTION.
      *************************************************
           SET CTBL TO 1.
           SET CSTP TO 1.
           MOVE 0 TO SP-CAL-NORATES(CTBL).
       NEXT-NORATES.
           IF SP-CAL-RATE(CTBL CSTP) = 0 AND
              SP-CAL-BREAK(CTBL CSTP) = 0
              GO TO COMPUTE-NORATES-2.
           ADD 1 TO SP-CAL-NORATES(CTBL).
           IF CSTP < 5
              SET CSTP UP BY 1
              GO TO NEXT-NORATES.
       COMPUTE-NORATES-2.
           IF CTBL = 2
              GO TO EXIT-COMPUTE-NORATES.
           SET CTBL TO 2.
           SET CSTP TO 1.
           MOVE 0 TO SP-CAL-NORATES(CTBL).
           GO TO NEXT-NORATES.
       EXIT-COMPUTE-NORATES.
           EXIT.

      ***********************************************
       COMPUTE-NOEDITS SECTION.
      ***********************************************
           SET ETBL TO 1.
           SET ESTP TO 1.
           MOVE 0 TO SP-EDT-NOEDITS(ETBL).
       NEXT-NOEDITS.
           IF SP-EDT-TYPE-CODES(ETBL ESTP) = SPACES
              GO TO COMPUTE-NOEDITS-2.
           ADD 1 TO SP-EDT-NOEDITS(ETBL).
           IF ESTP < 6
              SET ESTP UP BY 1
              GO TO NEXT-NOEDITS.
       COMPUTE-NOEDITS-2.
           IF ETBL = 2
              GO TO EXIT-COMPUTE-NOEDITS.

           SET ETBL TO 2.
           SET ESTP TO 1.
           MOVE 0 TO SP-EDT-NOEDITS(ETBL).

           GO TO NEXT-NOEDITS.
       EXIT-COMPUTE-NOEDITS.
           EXIT.

      *******************************************
       COMPUTE-INSNOVER SECTION.
      *******************************************
           SET ILEV TO 1.
           SET IVER TO 1.
           MOVE 0 TO SP-INSNOVER(ILEV).
       NEXT-INSNOVER.
           IF SP-INSMINAGE(ILEV IVER) = 0 AND
              SP-INSMAXAGE(ILEV IVER) = 0 AND
              SP-INSMAXCOV(ILEV IVER) = 0 AND
              SP-INSMAXTRM(ILEV IVER) = 0
              GO TO COMPUTE-INSNOVER-2.
           ADD 1 TO SP-INSNOVER(ILEV).
      * IVER USED TO OCCUR 14, NOW 5
           IF IVER < 5
              SET IVER UP BY 1
              GO TO NEXT-INSNOVER.
       COMPUTE-INSNOVER-2.
           IF ILEV = 2
              GO TO COMPUTE-INSNOVER-3.
           IF ILEV = 3
              GO TO COMPUTE-INSNOVER-4.
           IF ILEV = 4
              GO TO EXIT-COMPUTE-INSNOVER.
           SET ILEV TO 2.
           SET IVER TO 1.
           MOVE 0 TO SP-INSNOVER(ILEV).
           GO TO NEXT-INSNOVER.
       COMPUTE-INSNOVER-3.
           SET ILEV TO 3.
           SET IVER TO 1.
           MOVE 0 TO SP-INSNOVER(ILEV).
           GO TO NEXT-INSNOVER.
       COMPUTE-INSNOVER-4.
           SET ILEV TO 4.
           SET IVER TO 1.
           MOVE 0 TO SP-INSNOVER(ILEV).
           GO TO NEXT-INSNOVER.
       EXIT-COMPUTE-INSNOVER.
           EXIT.
