      * COPYMEMBER: LIBLP/LPSETTYW
      *****************************************************
      *               (LPSETTYW)
      *    SET TY-DATE WORKERS
      * REV:
      * JTG 030797 ADDED TYNET & TYVOL & TYREC & TYCON
      *                   TYONH & TYRED & TYSOL
      * BLM 160310 REMOVE DATE WORKERS, NOT NEEDED WITH
      *            NEW TY DATE FORMAT, PD0003
      * BLM 160310 ALSO REMOVED TYONH, TYRED, TYSOL,
      *            NOT USED
      *****************************************************
       78  TYNET                   VALUE 1.
       78  TYVOL                   VALUE 2.

       78  TYREC                   VALUE 1.
       78  TYCON                   VALUE 2.

       01  TYSUB1                  PIC 99 COMP VALUE 1.
       01  TYSUB2                  PIC 99 COMP VALUE 1.
       01  TYSUB3                  PIC 99 COMP VALUE 1.
